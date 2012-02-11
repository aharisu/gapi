(use srfi-1)
(use srfi-13)
(use util.list)
(use ginfo)
(use term.readline)

;;---------------------
;;Functions related to state
;;---------------------

(define (do-nothing . unused) (undefined))
(define *state-list* '())
(define-macro (define-state name in exec out completion)
  `(set! *state-list* (acons (quote ,name)
                           (cons ,in (cons ,exec (cons ,out ,completion)))
                           *state-list*)))
(define (get-state name)
  (assoc-ref *state-list* name #f))
(define (state-in state)
  (car state))
(define (state-exec state)
  (cadr state))
(define (state-out state)
  (caddr state))
(define (state-completion state)
  (cdddr state))


;;---------------------
;;Definition of unit state
;;---------------------

(define enable-text-format? #t)

(define (make-underline-text text)
  (string-append
    (if enable-text-format? "\x1b[4m" "")
    text
    (if enable-text-format? "\x1b[0m" "")))

(define (make-bold-text text)
  (string-append
    (if enable-text-format? "\x1b[1m" "")
    text
    (if enable-text-format? "\x1b[0m" "")))

(define (print-align-text text-list left-space-text 
                          :key (pos -1) (newline? #t))
  (unless (null? text-list)
    (if enable-text-format?
      (let1 space-size (string-length left-space-text)
        (when (> 0 pos)
          (display left-space-text)
          (display (car text-list)))
        (receive (h w) (rl-get-screen-size)
          (let loop ([text-list (if (> 0 pos) (cdr text-list) text-list)]
                     [len (if (> 0 pos)
                            (+ space-size (string-length (car text-list)))
                            pos)])
            (if (null? text-list)
              (when newline?
                (newline))
              (if (< w (+ 1 len (string-length (car text-list))))
                (begin
                  (newline)
                  (display left-space-text)
                  (display (car text-list))
                  (loop (cdr text-list) (+ space-size (string-length (car text-list)))))
                (begin
                  (display " ")
                  (display (car text-list))
                  (loop (cdr text-list) (+ 1 len (string-length (car text-list))))))))))
      (print left-space-text (string-join text-list) " "))))

(define (print-description name description-list)
  (print (make-bold-text name))
  (if (null? description-list)
    (print "No doccument.")
    (for-each
      (cut print <>)
      description-list)))


(define-class <gapi-context> (<convert-context>) ())
(define-constant gapi-context (make <gapi-context> :port (standard-output-port)))

(define-method output ((context <gapi-context>) (unit <unit-proc>))
  (display "(")
  (display (make-bold-text (ref unit 'name)))
  (print-align-text (map (cut param-name <>) (ref unit 'param))
                    (make-string (+ 2 (string-length (ref unit 'name))) #\space)
                    :pos (+ 1 (string-length (ref unit 'name)))
                    :newline? #f)
  (print ")")
  (for-each
    (lambda (p)
      (unless (null? (param-acceptable p))
        (print (make-underline-text (param-name p)))
        (print-align-text (param-acceptable p) "    ")))
    (ref unit 'param))
  )

(define-method output ((context <gapi-context>) (unit <unit-class>))
  (let1 delimiter " :: "
    (display (make-bold-text (ref unit 'name)))
    (unless (null? (ref unit 'supers))
      (display delimiter)
      (print-align-text (ref unit 'supers)
                        "    "
                        :pos (+ (string-length (ref unit 'name))
                                (string-length delimiter))
                        :newline? #f)
      )
    (newline)
    (for-each
      (lambda (s)
        (print (make-underline-text (param-name s)))
        (print-align-text (param-acceptable s) "    "))
      (ref unit 'slots))
    ))

(define-method output ((context <gapi-context>) (unit <unit-var>))
  (print (make-bold-text (ref unit 'name)))
  )

(define (show unit)
  (with-output-to-port (slot-ref gapi-context 'port)
                       (lambda ()
                         (output gapi-context unit))))

(define (substring-match-list text l)
  (let1 len (string-length text)
    (filter
      (lambda (name) (and (<= len (string-length name))
                       (string=? text (substring name 0 len))))
      l)))

(define-macro (or-eq? eq? x . any)
  `(or ,@(map (lambda (y) `(,eq? ,x ,y)) any)))

(define (params-without-optional params)
  (filter
    (lambda (p)
      (let1 name (param-name p)
        (not (or (string=? "." name) (eq? (string-ref name 0) #\:)))))
    params))

(define-method unit-exec-line ((unit <unit-top>) line)
  (if (string=? line (ref unit 'name))
    (begin
      (print-description (ref unit 'name) (ref unit 'description))
      #t)
    #f))

(define-method unit-exec-line ((unit <unit-proc>) line)
  (cond
    [(next-method) #t]
    [(find (lambda (p) (string=? line (param-name p)))
           (params-without-optional (ref unit 'param)))
     => (lambda (p) 
          (print-description (param-name p) (param-description p))
          #t)]
    [(or-eq? string=? line "r" "#r" "#return")
     (print-description "return" (ref unit 'return))
     #t]
    [else #f]))

(define-method unit-exec-line ((unit <unit-class>) line)
  (cond
    [(next-method) #t]
    [(find (lambda (p) (string=? line (param-name p))) (ref unit 'slots))
     => (lambda (p) 
          (print-description (param-name p) (param-description p))
          #t)]
    [else #f]))

(define-method unit-all-info ((unit <unit-top>))
  (print-description (ref unit 'name) (ref unit 'description)))

(define-method unit-all-info ((unit <unit-proc>))
  (next-method)
  (print-description "return" (ref unit 'return))
  (for-each (lambda (p) (print-description (param-name p) (param-description p))) 
            (params-without-optional (ref unit 'param))))

(define-method unit-all-info ((unit <unit-class>))
  (next-method)
  (for-each (lambda (p) (print-description (param-name p) (param-description p))) 
            (ref unit 'slots)))

(define-method unit-candidate ((unit <unit-top>))
  (cons (ref unit 'name) '()))

(define-method unit-candidate ((unit <unit-proc>))
  (cons "#return" (append (map 
                           (cut param-name <>) 
                           (params-without-optional (ref unit 'param)))
                         (next-method))))

(define-method unit-candidate ((unit <unit-class>))
  (append (map (cut param-name <>) (ref unit 'slots))
          (next-method)))

;;
;;definination of unit state 
(let ([cur-unit #f])
  (define-state unit
                ;;in action
                (lambda (unit) 
                  (rl-set-prompt! :ps1 "more info> " :ps2 "..... ")
                  (show unit)
                  (set! cur-unit unit))
                ;;line exec
                (lambda (line) 
                  (cond 
                    [(unit-exec-line cur-unit line)]
                    [(or-eq? string=? line "a" "#a" "#all") (unit-all-info cur-unit)]
                    [(or-eq? string=? line "q" "e" "#q" "#quit")
                     ;;transition to initialize state
                     (cons 'init '())]
                    [else (print "Not found symbol.")]))
                ;;out action
                do-nothing
                ;;completation
                (let1 candidate #f
                  (lambda (text state)
                    (when (zero? state)
                      (set! candidate (substring-match-list 
                                        text 
                                        (cons* "#all" "#quit" 
                                               (unit-candidate cur-unit)))))
                    (if (null? candidate)
                      #f
                      (begin0 (car candidate)
                        (set! candidate (cdr candidate))))))
                ))


;;---------------------
;;Definition of initial state
;;---------------------

(define *docs*  '())

(define (match-unit-list cmp)
  (append-map
    (lambda (doc)
      (filter
        (lambda (unit) (cmp (ref unit 'name)))
        (ref doc 'units)))
    *docs*))

(define (unit-candidate-list text)
  (map
    (cut ref <> 'name)
    (match-unit-list
      (let1 len (string-length text)
        (lambda (name)
          (and (<= len (string-length name))
            (string=? text (substring name 0 len))))))))

(define (exact-match-unit-list text)
  (match-unit-list (cut string=? text <>)))

;;---------------------
;;Functions related to command
;;---------------------
(define *commands* '())
(define-macro (define-cmd cmd valid-arg exec completion)
  `(set! *commands* (acons (string-append "#" (symbol->string (quote ,cmd)))
                         (cons ,valid-arg (cons ,exec ,completion))
                         *commands*)))
(define (get-command cmd)
  (assoc-ref *commands* cmd #f))
(define (cmd-valid-arg cmd)
  (car cmd))
(define (cmd-exec cmd)
  (cadr cmd))
(define (cmd-completion cmd)
  (cddr cmd))
(define (get-all-command)
  (cons "#exit" (map car *commands*)))

(define (variable-length-argument num) #t)

(define (load-info module)
  (unless (any
            (lambda (doc) (equal? (ref doc 'name) module))
            *docs*)
    (guard (e [(<geninfo-warning> e) (print "Error: " (ref e 'message))])
      (set! *docs* (cons (geninfo module) *docs*)))))

;;
;;definetion of use command
(define-cmd use 
            variable-length-argument
            ;;execute
            (lambda modules
              (for-each
                (lambda (module) (load-info (string->symbol module)))
                modules))
            ;;library completion
            (let1 module-list #f
              (lambda (text)
                (unless module-list
                  (set! module-list (append-map
                                      (lambda (s) 
                                        (library-map s (lambda (m p) (symbol->string m))))
                                      '(* *.* *.*.* *.*.*.* *.*.*.*.*))))
                (substring-match-list
                  text
                  module-list)))
            )

;;
;;definetion of use load
(define-cmd load
            variable-length-argument
            ;;execute
            (lambda files
              (for-each
                load-info
                files))
            ;;file completion
            (lambda (text)
              (let loop ([state 0]
                         [l '()])
                (cond
                  [(rl-filename-completion text state)
                   => (lambda (file) (loop (+ 1 state) (cons file l)))]
                  [else (reverse l)]))
              ))

;;
;;definination of initial state
(define-state init
              ;;in action
              (lambda () (rl-set-prompt! :ps1 "gapi> " :ps2 "..... "))
              ;;line execution
              (lambda (line)
                (if (eq? (string-ref line 0) #\#)
                  (let* ([tokens (string-split line #[\s])]
                         [cmd (get-command (car tokens))])
                    (if cmd
                      (if ((cmd-valid-arg cmd) (length (cdr tokens)))
                        (apply (cmd-exec cmd) (cdr tokens))
                        (print (format #f "Invalid argument.")))
                      (print (format #f "Unkown command [~a]." line))))
                  (let1 units (exact-match-unit-list line)
                    (case (length units)
                      [(0) (print "Not found.")]
                      [(1) ;;transition to unit state
                       (cons 'unit (list (car units)))]
                      [else ;TODO multiple found. it does wath?
                        (print "Multiple found.")]))))
              ;;out execution
              do-nothing
              ;;completion
              (let1 candidate #f
                (lambda (text state) 
                  (when (zero? state)
                    (let1 tokens (string-split (string-trim 
                                                 (rl-get-line-text rl-point))
                                               #[\s])
                      (set! candidate
                        (if (and (< 1 (length tokens))
                              (eq? (string-ref (car tokens) 0) #\#))
                          (let1 cmd (get-command (car tokens))
                            (if cmd
                              ((cmd-completion cmd) text)
                              '()))
                          (append
                            (unit-candidate-list text)
                            (substring-match-list text (get-all-command)))))))
                  (if (null? candidate)
                    #f
                    (begin0
                      (car candidate)
                      (set! candidate (cdr candidate))))))
              )


;;---------------------
;;Entry point
;;---------------------

(define *cur-state* (get-state 'init))
;;exection first in action
((state-in *cur-state*))
(define (exec-line line)
  (let1 result ((state-exec *cur-state*) line)
    (when (pair? result)
      ;;transition
      (cond
        [(get-state (car result))
         => (lambda (state)
              ;;exec out action
              ((state-out state))
              ;;set new state
              (set! *cur-state* state)
              ;;exec in action
              (apply (state-in state) (cdr result)))]
        [else (error "State not found.")]))))

(define (exec-completion text state)
  ((state-completion *cur-state*) text state))


(define (main args)
  (let ([readline-port (make-readline-input-port)]
        [histfile (string-append (or (sys-getenv "HOME") ".")
                                 "/.gapi_history")])
    (rl-set-application-name! "gapi")
    (rl-set-basic-word-break-characters! " \t\n()',\";")
    (rl-set-basic-quote-characters! "\"")
    (rl-set-completion-list-function! exec-completion)
    ;;cancel filename completion
    (rl-set-completion-entry-function! (lambda (text state) #f))
    (rl-stifle-history! 100)

    (dynamic-wind
      (lambda ()
        (rl-read-history! histfile))
      (lambda ()
        (let loop ()
          (rl-set-prompt-state! 'ps1)
          (let1 line (read-line readline-port)
            (unless (eof-object? line)
              (let1 line (string-trim-both line)
                (unless (string=? line "#exit") 
                  (unless (zero? (string-length line))
                    (rl-commit-history!)
                    (exec-line line))
                  (loop)))))))
      (lambda ()
        (rl-write-history histfile)))
    0))

