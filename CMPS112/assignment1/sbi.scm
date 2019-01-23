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
;;    program, which is then executed.
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

;; Displays list in stderr and exits
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

;; Check for labels and insert in label hash
(define (scan-labels program)
  (map (lambda (line)
         (when (and (not (null? line)) (>= (length line) 2)
                    (not (null? (cdr line))) (symbol? (cadr line)))
               (hash-set! labels-hash (cadr line) (caddr line))))
       program) ;; If not empty, and contains a symbol, store in hash
)

;; Recursively go through and find statements in hash
(define (interpret-program filename program)
    (scan-labels program) ;; Scan for labels first
    (map (lambda (line) 
            (if (not (pair? line))
                (interpret-program filename cdr(line))
                (hash-ref *label-table*)))
        program)
)

;; Look up the function in the tables, check the arguments, and apply function to list of results
(define (evaluate-expression input)
    (cond
        ((string? input) input) ;; Check if input is a string (returns #t or #f)                                                            STRING
        ((number? input) input) ;; Check if input is a number (returns #t or #f)                                                            NUMBER
        ((hash-has-key? *function-table* input) ;; Check if *function-table* has a key for it (returns #t or #f)
            (hash-ref *function-table* input)) ;; If #t, get the key                                                                        FUNCTION
        ((pair? input) ;; Check if input is a pair (returns #t or #f)
            (if (hash-has-key? *function-table* (car input)) ;; If #t, check if first element has a key in *function-table*
                (let((car (hash-ref *function-table* (car input)))) ;; The key of the first in the list
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
                    (car input) " does not exist!\n")))) ;;                                                                                 PAIR
    )
)

;; If token is not eof, keep reading
(define (dump-stdin)
    (let ((token (read)))
         (printf "token=~a~n" token)
         (when (not (eq? token eof)) (dump-stdin))))

;; Writes the header out
(define (write-program-by-line filename program)
    (printf "==================================================~n")
    (printf "~a: ~s~n" *run-file* filename)
    (printf "==================================================~n")
    (printf "(~n")
    (for-each (lambda (line) (printf "~s~n" line)) program)
    (printf ")~n"))

;; Print
(define (printFunc input)
 (map (lambda (x) (display (evaluate-expression x))) input)
  (newline)
)

;; Create an array of a certain size
(define (dimFunc input)
  (let((arr (make-vector (evaluate-expression (cadadr input)) (caadr input))))
  (symbol-put! (caadr input) (+ (evaluate-expression (cadadr input)) 1))
))

;; Input
(define (inputFunc input)
  (symbol-put! 'count 0)
  (if (null? (car input))
    (symbol-put! 'count -1)
    (begin
    (symbol-put! 'count (inputFunc input 0)))))

;; Let
(define (letFunc input)
  (symbol-put! (cadr input) (evaluate-expression (caddr input)))
)

;; Execute the lines based on the functions
(define (execute-function inst program lineNum)

    (when (eq? (car inst) 'print) ;; Print
         (printFunc (cdr inst))
         (parse-list program(+ lineNum 1)))

    (when (eq? (car inst) 'goto) ;; Goto
     (parse-list program (hash-ref *addr-table* (cadr inst))))
   
    (when (eq? (car inst) 'let) ;: Let
       (letFunc inst)
       (parse-list program(+ lineNum 1)))

    (when (eq? (car inst) 'if) ;; If
        (when (evaluate-expression  (cadr inst))
            (parse-list  program 
            (hash-ref *addr-table* (caddr inst)))
            ))

    (when (eq? (car inst) 'dim) ;; Dim 
       (dimFunc inst)
       (parse-list program(+ lineNum 1)))

    (when (eq? (car inst) 'input) ;; Input
    ((inputFunc inst)
       (parse-list program(+ lineNum 1)) 
    ))  
)

;; Parses the program recursively
(define (parse-list program lineNum)
   (when (> (length program) lineNum)
    (let((line (list-ref program lineNum)))
    (cond
      ((= (length line) 3)
       (set! line (cddr line))
       (execute-function (car line) program lineNum))
      ((and (= (length line) 2) (list? (cadr line)))
       (set! line (cdr line))
       (execute-function (car line) program lineNum))
      (else 
        (parse-list program (+ lineNum 1)))
    )))
)
     
;; Exit if list is null, otherwise parse and interpret and stuff              
(define (main arglist)
    (if (or (null? arglist) (not (null? (cdr arglist))))
        (usage-exit)
        (let* ((sbprogfile (car arglist))
              (program (readlist-from-inputfile sbprogfile)))
              (scan-labels program)
              (parse-list program 0)
        )
    )
)
            
;; Print the terminal port, then call the main function
(printf "terminal-port? *stdin* = ~s~n" (terminal-port? *stdin*))
(if (terminal-port? *stdin*)
    (main (vector->list (current-command-line-arguments)))
    (printf "sbi.scm: interactive mode~n"))
    
        
