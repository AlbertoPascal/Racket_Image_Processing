
(define (char-bin character)
#|
 this function will receive a character such as "A", convert it to its corresponding ascii "65" and then return its binary equivalent.
|#
    (write "I got ")
    (write character)
    "hola"
)
(define (prep-list sentence binary-arr)
;This function will receive a word or sentence, convert it into a list of characters and send char by char to its binary conversion.
;Function returns an array of strings containing the binary representation of each letter  
    (if (> (string-length sentence) 0)
        ;If I still have letters, I will return the letter's binary component.
        (prep-list (process (cdr (string->list sentence))) (append binary-arr (list (char-bin (car (string->list sentence))))))
        ;If I am out of letters, I need to return my array of "bits"
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