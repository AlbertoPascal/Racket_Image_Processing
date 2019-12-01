(define (read-image img-name)
    (let*
        (
            [in (open-input-file img-name)] ;we will define our input file reader
            [img_type (read-image-type in)] ; we store the image_type
            [mat_size (read-matrix-size in)] ; we store x and y coordinates from our matrix size
            [pixels (read-all-pixels in (* (car mat_size) (car (cdr mat_size))) '())]
        )
        ;(write img_type)
        ;(display "\n")
        ;(write mat_size)
        ;(display "\n")
        ;(write pixels)
        ;(display "\n")
        (close-input-port in) 
        (append (list img_type) (list mat_size) (list(convert-pixels-to-bin pixels '()) ) )
    ;Idea: leer los parámetros de la ppm image para saber cuantos renglones y columnas hay. A partir de
    ;ahí, hacer tercias y guardar esas listas de tercias dentro de otra lista para representar la lista de pixeles.
    ;(append (read-line in)
    )
)
(define (encode-msg msg img-name)
    (let*
        (
            [init_msg_length (string-length msg)]
            [img_data (read-image img-name)]
            [msg_arr (prep-list (string-append (~v init_msg_length) " " msg) '())]
            [img_type (car img_data)]
            [mat_size (car (cdr img_data))]
            [pixel_arr (car (cdr (cdr img_data)))]
        )
        (write msg_arr)
        (display "\n")
        (write img_type)
        (display "\n")
        (write mat_size)
        (display "\n")
        (write pixel_arr)
        (display "\n")

        (if (valid-encryption msg_arr pixel_arr)
            (write "I can encrypt this message")
            (write "message is too long for this image. Please try with a larger image")
        )


    )
)
(define (encrypt-message msg_arr pixel_arr new_pixel_arr)
    (if (> (length msg_arr) 0)
        ;send next letter to encryption. 
        1
        1
    )
)
(define (get_trimmed_pixel pixel)
    (if (null? (cdr pixel))
        '()
        (append (list (car pixel)) (get_trimmed_pixel (cdr pixel)))
    )
)
(define (add_next_bit char_bit trimmed_pixel)
    (string-append trimmed_pixel char_bit)
)
()
; (add_next_bit "1" (Join-chars (get_trimmed_pixel (string->list "100"))))

(define (valid-encryption msg_arr pixel_arr)
    (let*
        (
            [msg_length (* (length msg_arr) 8)]
            [chnl_length (length pixel_arr)]
            ;message includes length when written. 

        )
        
        (if (> msg_length chnl_length)
            #f
            #t
        )
    )
)

(define (read-image-type input) ;This method returns the image type. For example, P6
    (read input)
)
(define (read-matrix-size input); This method returns the matrix size. For example,  '(4 4)
    (append (list (read input)) (list (read input)))
)
(define (read-all-pixels input number_of_iter pixel_list) ;This method returns a list of lists containing pixels. 
    (if (> number_of_iter 0) ;If I still have pixels to read
        (read-all-pixels input (- number_of_iter 1) (append pixel_list (read-pixel input))) ;I keep reading
        pixel_list ; Else, I return my pixel list. Example '( (0 0 255) (240 130 244))
    )
)
;This method will extract all three R G B for a single pixel (next in file)
(define (read-pixel input) 
    (let*
        (
            [read-R (read input)]
            [read-G (read input)]
            [read-B (read input)]
        )
        (list (append (list read-R) (list read-G) (list read-B)))
        
    )
)
(define (char-bin character)
 ;this function will receive a character such as "A", convert it to its corresponding ascii "65" and then return its binary equivalent.
    ;(write (DecToBin (char->integer character)))
    (DecToBin (char->integer character))
)
(define (prep-list sentence binary-arr)
;This function will receive a word or sentence, convert it into a list of characters and send char by char to its binary conversion.
;Function returns an array of strings containing the binary representation of each letter  
    ;(write sentence)
    (if (> (string-length sentence) 0)
        ;If I still have letters, I will return the letter's binary component.
        (prep-list (Join-chars (cdr (string->list sentence))) (append binary-arr (list (format-bin-string (char-bin (car (string->list sentence)))))))
        ;If I am out of letters, I need to return my array of "bits"
        binary-arr
        
    )

)

(define (Join-chars lst)
  (apply string-append ; Here we will start appending all of our letters
         (map (lambda (e) ; I will receive each "char"
                (if (char? e) ; if it's a char already I convert it to string
                    (string e) ; This is for when I type in numbers, I still make them string
                    (number->string e)
                )
               ) 
            lst ;I will apply this to every char on my list
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
(define (format-bin-string binstring)
    (if (< (string-length binstring) 8)
        (format-bin-string (string-append "0" binstring))
        binstring
    )
)
(define (convert-pixels-to-bin pixels new_pixels)
    (if (> (length pixels) 0)
        (convert-pixels-to-bin (cdr pixels) (append new_pixels (convert-list-to-bin (car pixels) '())))
        ;(convert-pixles-to-bin (cdr pixles) (append new_pixels (convert-list-to-bin (car pixels) '())))
        new_pixels
    )
)

(define (convert-list-to-bin elem_list new_list)
    (if (> (length elem_list) 0)
        ;(cdr elem_list)
        (convert-list-to-bin (cdr elem_list) (append new_list (list (format-bin-string (DecToBin (car elem_list))))))
        new_list
    )
)

