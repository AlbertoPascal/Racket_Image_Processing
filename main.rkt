
(define (char-bin charcater)
#|
 this function will receive a character such as "A", convert it to its corresponding ascii "65" and then return its binary equivalent.
|#
    1
)
(define (prep-list sentence binary-arr)
;This function will receive a word or sentence, convert it into a list of characters and send char by char to its binary conversion.
    (if (> (string-length sentence) 0)
        ;(prep-list (cdr(sentece)) , (append binary-arr, list((char-bin (car((string->list sentence))) ) ) ) )
        (prep-list (cdr sentence) (list (append '(1) binary-arr)) )
        ;(prep-list (process (cdr (string->list sentence))) (append binary-arr (list (char-bin (car(string->list "hola crayola"))))) )
        binary-arr
        
    )
    
   
    
)

(define (process lst)
  (apply string-append                   ; append all the strings
         (map (lambda (e)                ; create a list of strings
                (if (char? e)            ; if it's a char
                    (string e)           ; convert it to string
                    (number->string e))) ; same if it's a number
              lst)))