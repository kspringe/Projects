#!/afs/cats.ucsc.edu/courses/cmps112-wm/usr/racket/bin/mzscheme -qr
;; $Id: sbi.scm,v 1.10 2019-01-15 14:10:54-08 - - $
;;
;; NAME
;;    sbi.scm - silly basic interpreter
;;
;; SYNOPSIS
;;    sbi.scm filename.sbir
;;
;; DESCRIPTION
;;    The file mentioned in argv[1] is read and assumed to be an SBIR
;;    program, which is the executed.  Currently it is only printed.
;;
;; Working on creating hash tables . . .
;;

;; Define the ports
(define *stdin* (current-input-port))
(define *stdout* (current-output-port))
(define *stderr* (current-error-port))

;; Run the file
(define *run-file*
    (let-values
        (((dirpath basepath root?)
            (split-path (find-system-path 'run-file))))
        (path->string basepath))
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Holds all functions
(define *function-table* (make-hash))

;; Give the value for key
(define (function-get key)
    (hash-ref *function-table* key))
    
;; Map key to value
(define (function-put! key value)
    (hash-set! *function-table* key value))
    
;; Map keys to functions, initialize
(for-each (lambda (pair)
    (function-put! (car pair) (cadr pair)))
    `(
        (log10_2 0.301029995663981195213738894724493026768189881)
        (sqrt_2  1.414213562373095048801688724209698078569671875)
        (e       2.718281828459045235360287471352662497757247093)
        (pi      3.141592653589793238462643383279502884197169399)
        (div     ,(lambda (x y) (floor (/ x y))))
        (log10   ,(lambda (x) (/ (log x) (log 10.0))))
        (log2    ,(lambda (x) (/ (log x) (log 2.0))))
        (log     ,log)
        (mod     ,(lambda (x y) (- x (* (div x y) y))))
        (quot    ,(lambda (x y) (truncate (/ x y))))
        (rem     ,(lambda (x y) (- x (* (quot x y) y))))
        (<>      ,(lambda (x y) (not (= x y))))
        (+       ,+)
        (-       ,-)
        (*       ,*)
        (^       ,expt)
        (ceil    ,ceiling)
        (floor   ,floor)
        (trunc   ,(lambda (x) (quotient x 1)))
        (exp     ,exp)
        (sqrt    ,sqrt)
        (abs     ,abs)
        (/       ,/)
        (<=      ,<=)
        (>=      ,>=)
        (=       ,=)
        (>       ,>)
        (<       ,<)
        (sin     ,sin)
        (cos     ,cos) 
        (tan     ,tan)
        (atan    ,atan)
        (asin    ,asin)
        (acos    ,acos)
        (round   ,round) 
     )
)

;; Holds value of each variable
(define *variable-table* (make-hash))

;; Initialized with the variables described in the section ‘‘builtin symbols’’
(for-each
   ( lambda (item) (hash-set! *variable-table* (car item) (cadr item)))
    `(
        (nan (/0.0 0.0))
        (eof 0.0)
        (pi  (acos -1.0))
        (e   (exp 1.0))
     )
)

;; Holds all arrays
(define *array-table* (make-hash))

;; Holds addresses of each line
(define *label-table* (make-hash))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Displays the list in stderr
(define (die list)
    (for-each (lambda (item) (display item *stderr*)) list)
    (newline *stderr*)
    (exit 1)
)

;; Displays usage message in stderr
(define (usage-exit)
    (die `("Usage: " ,*run-file* " filename"))
)

;; Read list from input file, display error message if failed
(define (readlist-from-inputfile filename)
    (let ((inputfile (open-input-file filename)))
         (if (not (input-port? inputfile))
             (die `(,*run-file* ": " ,filename ": open failed"))
             (let ((program (read inputfile)))
                  (close-input-port inputfile)
                         program))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (evaluate-expression input)
    (cond
        ((string? input) input) ;; Check if input is a string (returns #t or #f)                                                            STRING
        ((number? input) input) ;; Check if input is a number (returns #t or #f)                                                            NUMBER
        ((hash-has-key? *function-table* input) ;; Check if *function-table* has a key for it (returns #t or #f)
            (hash-ref *function-table* input)) ;; If #t, get the key                                                                        FUNCTION
        ((pair? input) ;; Check if input is a pair (returns #t or #f)
            (if (has-hash-key? *function-table* (car input)) ;; If #t, check if first element has a key in *function-table*
                (let((car (hash-ref *funtion-table* (car input)))) ;; The key of the first in the list
                    (cond
                        ((procedure? car) ;; Check if the first element is a procedure (returns #t or #f)
                            (apply car (map (lambda (x) (evaluate-expression x)) (cdr input)))) ;; If #t, apply procedure to the whole list recursively
                        ((vector? car) ;; Check if first element is a vector (returns #t or #f)
                            (vector-ref car (cadr input))) ;; If #t, get the element in the next position
                        ((number? car) car) ;; Check if the next element is also a number
                        (else ;; It's not a procedure, vector, or number . . .
                            ;; Print an error message!!!
                            (die "Issue with function-table!."))))            
                ;; There is no key in *function-table* that matches first element
                (die (list "Error: " 
                    (car input) " does not exist!\n")))) ;;                                                                                 LIST
    )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; If token is not eof, print?????
(define (dump-stdin)
    (let ((token (read)))
         (printf "token=~a~n" token)
         (when (not (eq? token eof)) (dump-stdin))))

;; Writes the program out line by line
(define (write-program-by-line filename program)
    (printf "==================================================~n")
    (printf "~a: ~s~n" *run-file* filename)
    (printf "==================================================~n")
    (printf "(~n")
    (for-each (lambda (line) (printf "~s~n" line)) program)
    (printf ")~n"))

;; Exit if list is null, otherwise read input file and print it line by line
(define (main arglist)
    (if (or (null? arglist) (not (null? (cdr arglist))))
        (usage-exit)
        (let* ((sbprogfile (car arglist))
               (program (readlist-from-inputfile sbprogfile)))
              (write-program-by-line sbprogfile program))))

;; Print the terminal port, then call the main function
(printf "terminal-port? *stdin* = ~s~n" (terminal-port? *stdin*))
(if (terminal-port? *stdin*)
    (main (vector->list (current-command-line-arguments)))
    (printf "sbi.scm: interactive mode~n"))
    
    
    
    
    
    
    
;; #!/afs/cats.ucsc.edu/courses/cmps112-wm/usr/racket/bin/mzscheme -qr
;; ;; $Id: symbols.scm,v 1.2 2014-10-31 17:35:08-07 - - $
;; 
;; ;;
;; ;; NAME
;; ;;    symbols - illustrate use of hash table for a symbol table
;; ;;
;; ;; DESCRIPTION
;; ;;    Put some entries into the symbol table and then use them.
;; ;;
;; 
;; ;;
;; ;; Create the symbol table and initialize it.
;; ;;
;; 
;; (define *symbol-table* (make-hash))
;; (define (symbol-get key)
;;         (hash-ref *symbol-table* key))
;; (define (symbol-put! key value)
;;         (hash-set! *symbol-table* key value))
;; 
;; (for-each
;;     (lambda (pair)
;;             (symbol-put! (car pair) (cadr pair)))
;;     `(
;; 
;;         (log10_2 0.301029995663981195213738894724493026768189881)
;;         (sqrt_2  1.414213562373095048801688724209698078569671875)
;;         (e       2.718281828459045235360287471352662497757247093)
;;         (pi      3.141592653589793238462643383279502884197169399)
;;         (div     ,(lambda (x y) (floor (/ x y))))
;;         (log10   ,(lambda (x) (/ (log x) (log 10.0))))
;;         (mod     ,(lambda (x y) (- x (* (div x y) y))))
;;         (quot    ,(lambda (x y) (truncate (/ x y))))
;;         (rem     ,(lambda (x y) (- x (* (quot x y) y))))
;;         (+       ,+)
;;         (^       ,expt)
;;         (ceil    ,ceiling)
;;         (exp     ,exp)
;;         (floor   ,floor)
;;         (log     ,log)
;;         (sqrt    ,sqrt)
;; 
;;      ))
;; 
;; ;; 
;; ;; What category of object is this?
;; ;;
;; 
;; (define (what-kind value)
;;     (cond ((real? value) 'real)
;;           ((vector? value) 'vector)
;;           ((procedure? value) 'procedure)
;;           (else 'other)))
;; 
;; ;;
;; ;; Main function.
;; ;;
;; 
;; (define (main argvlist)
;;     (symbol-put! 'n (expt 2.0 32.0))
;;     (symbol-put! 'a (make-vector 10 0.0))
;;     (vector-set! (symbol-get 'a) 3 (symbol-get 'pi))
;;     (printf "2 ^ 16 = ~s~n" ((symbol-get '^) 2.0 16.0))
;;     (printf "log 2 = ~s~n" ((symbol-get 'log) 2.0))
;;     (printf "log10 2 = ~s~n" ((symbol-get 'log10) 2.0))
;;     
;;     (newline)
;;     (printf "*symbol-table*:~n")
;;     (hash-for-each *symbol-table*
;;         (lambda (key value)
;;                 (printf "~s : ~s = ~s~n" key (what-kind value) value))
;;     ))
;; 
;; (main '())
;;     
