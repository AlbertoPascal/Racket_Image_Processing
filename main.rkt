
(define (char-bin character)
#|
 this function will receive a character such as "A", convert it to its corresponding ascii "65" and then return its binary equivalent.
|#
    ;(write (DecToBin (char->integer character)))
    (DecToBin (char->integer character))
)
(define (prep-list sentence binary-arr)
;This function will receive a word or sentence, convert it into a list of characters and send char by char to its binary conversion.
;Function returns an array of strings containing the binary representation of each letter  
    (if (> (string-length sentence) 0)
        ;If I still have letters, I will return the letter's binary component.
        (prep-list (Join-chars (cdr (string->list sentence))) (append binary-arr (list (char-bin (car (string->list sentence))))))
        ;If I am out of letters, I need to return my array of "bits"
        binary-arr
        
    )
    
)

(define (Join-chars lst)
  (apply string-append                   ; append all the strings
         (map (lambda (e)                ; create a list of strings
                (if (char? e)            ; if it's a char
                    (string e)           ; convert it to string
                    (number->string e)
                )
               ) ; same if it's a number
            lst
        )
    )
)

(define (DecToBin num)
    (if (< num 2)
        (number->string num); if number is no longer divisible, I return the number. 
        ;if it still is, I append the next calculation and the current result to make the effect of reading backwards.
        (string-append (DecToBin (truncate (/ num 2))) (number->string (modulo num 2)))
        ;Special Note: we can change modulo to reminder if we are going to use negative numbers.
    )

)