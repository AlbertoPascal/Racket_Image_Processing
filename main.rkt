;NOTE: MAIN FUNCTIONS ARE (encode-msg msg input-ppm output-ppm) and (decode-msg "input-ppm" "")

(define (read-image img-name)
    (let*
        (
            [in (open-input-file img-name)] ;we will define our input file reader
            [img_type (read-image-type in)] ; we store the image_type
            [mat_size (read-matrix-size in)] ; we store x and y coordinates from our matrix size
            [max_value (read in)]
            [pixels (read-all-pixels in (* (car mat_size) (car (cdr mat_size))) '())]
        )
        ;(write img_type)
        ;(display "\n")
        ;(write mat_size)
        ;(display "\n")
        ;(write pixels)
        ;(display "\n")
        (close-input-port in) 
        (append (list img_type) (list mat_size) (list max_value) (list(convert-pixels-to-bin pixels '()) ) )
    ;Idea: leer los parámetros de la ppm image para saber cuantos renglones y columnas hay. A partir de
    ;ahí, hacer tercias y guardar esas listas de tercias dentro de otra lista para representar la lista de pixeles.
    ;(append (read-line in)
    )
)
(define (decode-msg img-input msg)
    (let*
        (
            [img_data (read-image img-input)]
            [img_type (car img_data)]
            [mat_size (car (cdr img_data))]
            [max_size (caddr img_data)]
            [pixel_arr (car (cdddr img_data))]
            [char_arr (convert-ascii-char (return-pixels-toDec (remake-all-letters (get_last_bits pixel_arr '()) '()) '()) '())]
            [msg_length (string->number (Join-chars (list (car char_arr))))]
            
        )
        
        (if (char-iso-control? (car char_arr))
            (write "This image has no hidden messages to decode")
            (decode_list_msg (trim_decoding_list char_arr) msg_length "")
        )
        
        
    )
)
(define (convert-ascii-char ascii_arr char_arr)
    (if (> (length ascii_arr) 0)
        (convert-ascii-char (cdr ascii_arr) (append char_arr (list (integer->char (car ascii_arr)))))
        char_arr
    )
)
(define (get_msg_length char_arr len)
    (if (or (empty? char_arr) (string=? (Join-chars (list (car char_arr)) ) " "))
        (Join-chars len)
        (get_msg_length (cdr char_arr) (string-append len (Join-chars(list (car char_arr)))))
    )
)
(define (trim_decoding_list char_list)
    (if (string=? (Join-chars (list (car char_list)) ) " ")
        (cdr char_list)
        (trim_decoding_list (cdr char_list))
    )
)
(define (decode_list_msg full_char_list msg_length secret_msg)
    (if (> msg_length 0)
        (decode_list_msg (cdr full_char_list) (- msg_length 1) (string-append secret_msg (Join-chars (list (car full_char_list)))))
        secret_msg
    )
)
(define (get_last_bits pixel_arr new_pixel_arr)
    (if (> (length pixel_arr) 0) 
        (get_last_bits (cdr pixel_arr) (append new_pixel_arr (list(Join-chars (list (car (cdddr (cddddr (string->list (car pixel_arr))) )))))))
        new_pixel_arr
    )
)
(define (remake-all-letters last_bit_arr letter_arr)
    (if (> (length last_bit_arr) 0)
        (remake-all-letters (cddddr (cddddr last_bit_arr)) (append letter_arr (list (remake-letter last_bit_arr "" 0))))
        letter_arr
    )

)

(define (remake-letter last_bit_arr letter curr_count)
    (if (< curr_count 8)
        (remake-letter (cdr last_bit_arr) (string-append letter  (car last_bit_arr)) (+ curr_count 1))
        letter
    )
)
(define (encode-msg msg img-name output_filename)
    (let*
        (
            [init_msg_length (string-length msg)]
            [img_data (read-image img-name)]
            [msg_arr (prep-list (string-append (~v init_msg_length) " " msg) '())]
            [img_type (car img_data)]
            [mat_size (car (cdr img_data))]
            [max_size (caddr img_data)]
            [pixel_arr (car (cdddr img_data))]
            [msg_length (* (length msg_arr) 8)]
            
        )
        ;(write msg_arr)
        ;(display "\n")
        ;(write img_type)
        ;(display "\n")
        ;(write mat_size)
        ;(display "\n")
        ;(write pixel_arr)
        ;(display "\n")

        (if (valid-encryption msg_arr pixel_arr)
            (write-img output_filename img_type max_size mat_size "hola" (encrypt-message msg_arr (trim_all_pixels pixel_arr msg_length '()) '()) (* (car mat_size) 3))
            (write "message is too long for this image. Please try with a larger image")
        )


    )
)

(define (write-img output_filename img-type max_size mat_size img-name pixel_arr col_size)
    (let*
        (
            [out (open-output-file output_filename #:exists 'truncate)]
            [writtable-pixels (map (lambda (e) (number->string e)) (return-pixels-toDec pixel_arr '()))]

        )
        (write pixel_arr)
        (display "\n")
        (display img-type out)
        (display "\n" out)
        (display (string-append (number->string (car mat_size)) " " (number->string (cadr mat_size)) "\n") out)
        (display max_size out)
        (display "\n" out)
        (write-all-pixels writtable-pixels col_size 1 out)
        (write writtable-pixels)
        (close-output-port out)
    )
    
)
(define (write-all-pixels pixel_arr columns curr_cols output)
    ;(write "my curr cols is ")
    ;(write curr_cols)
    ;(display "\n")
    (if (not (empty? pixel_arr))
        (if (> curr_cols columns)
            (display (string-append "\n" (car pixel_arr) " ") output)
            (display (string-append (car pixel_arr) " ") output)
        )
        '()
        
    )
    (if (> (length pixel_arr) 0)
        (if (> curr_cols columns)
            (write-all-pixels (cdr pixel_arr) columns 1 output)
            (write-all-pixels (cdr pixel_arr) columns (+ curr_cols 1) output)
        )
        (display (string-append "\n" "Finished creating image"))
    )
)

(define (return-pixels-toDec pixel_arr final_pixel_arr)
    (if (> (length pixel_arr) 0)
        (return-pixels-toDec (cdr pixel_arr) (append final_pixel_arr (list (BinToDec (car pixel_arr)))))
        final_pixel_arr
    )
)
(define (encrypt-message msg_arr pixel_arr new_pixel_arr)
    (if (> (length msg_arr) 0)
        (encrypt-message (cdr msg_arr) (cddddr (cddddr pixel_arr)) (append new_pixel_arr (letter-encryption (car msg_arr) pixel_arr '())))
        (append new_pixel_arr pixel_arr)
        
    )
)
(define (BinToDec numstring)
    
    (if (> (length (string->list numstring)) 0)
        (+ (* (string->number (Join-chars (list (car (string->list numstring))))) (expt 2 (- (length (string->list numstring)) 1))) (BinToDec (Join-chars (cdr (string->list numstring)))))
        0
    )
)

(define (letter-encryption letter pixel_arr new_pixel_arr)
    (if (> (length (string->list letter)) 0)
       (letter-encryption (Join-chars (cdr (string->list letter))) (cdr pixel_arr) (append new_pixel_arr  (list (add_next_bit (Join-chars (list (car (string->list letter)))) (car pixel_arr)))))
       new_pixel_arr
    )
)
(define (trim_all_pixels pixel_arr needed_channels new_pixel_arr)
    (if (> needed_channels 0)
        (trim_all_pixels (cdr pixel_arr) (- needed_channels 1) (append new_pixel_arr (list (Join-chars (get_trimmed_pixel (string->list(car pixel_arr)))))))
        (append new_pixel_arr pixel_arr)
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

;Joins separated characters into a string
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

;Converts a decimal number into binary (string)
(define (DecToBin num)
    (if (< num 2)
        (number->string num); if number is no longer divisible, I return the number. 
        ;if it still is, I append the next calculation and the current result to make the effect of reading backwards.
        (string-append (DecToBin (truncate (/ num 2))) (number->string (modulo num 2)))
        ;Special Note: we can change modulo to reminder if we are going to use negative numbers.
    )

)

;Adds '0's at the beggining of the number to complete 8 bit positions
(define (format-bin-string binstring)
    (if (< (string-length binstring) 8)
        (format-bin-string (string-append "0" binstring))
        binstring
    )
)

;Convert each RGB value of each pixel into binary
(define (convert-pixels-to-bin pixels new_pixels)
    (if (> (length pixels) 0)
        (convert-pixels-to-bin (cdr pixels) (append new_pixels (convert-list-to-bin (car pixels) '())))
        ;(convert-pixles-to-bin (cdr pixles) (append new_pixels (convert-list-to-bin (car pixels) '())))
        new_pixels
    )
)

;Converts every element of a given list of numbers into binary
(define (convert-list-to-bin elem_list new_list)
    (if (> (length elem_list) 0)
        ;(cdr elem_list)
        (convert-list-to-bin (cdr elem_list) (append new_list (list (format-bin-string (DecToBin (car elem_list))))))
        new_list
    )
)

