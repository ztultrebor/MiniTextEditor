;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname texteditor) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

; data definitions

; A Words is a structure:
;   (make-text String Img String).
; as used in a text editor, the prefix and suffix of text are determined by the
;   location of the cursor
(define-struct words [prefix cursor suffix])


; constants

(define WINDOW (empty-scene 400 20))
(define CURSOR-ON (rectangle 1 16 "solid" "black"))
(define CURSOR-OFF (rectangle 1 16 "solid" "white"))


; functions

; Words -> Words
; launches the program from some initial state 
(define (main txt)
  (big-bang txt
    [to-draw text-editor]
    [on-key edit]
    [on-tick flash-cursor 1/2]
    )
  )

; Words -> Img
; display the text editor window
(define (text-editor process)
  (place-image/align (beside (text (words-prefix process)12 "black")
                             (words-cursor process)
                             (text (words-suffix process) 12 "black"))
                     5 10 "left" "center" WINDOW))

; Words KeyEvent -> Words
; edit text with keystrokes
(check-expect (edit (make-words "Hello" CURSOR-ON "") "left") (make-words "Hell" CURSOR-ON "o"))
(check-expect (edit (make-words "Hell" CURSOR-ON "o world!") "left") (make-words "Hel" CURSOR-ON "lo world!"))
(check-expect (edit (make-words "" CURSOR-ON "Hello") "left") (make-words "" CURSOR-ON "Hello"))
(check-expect (edit (make-words "Hell" CURSOR-ON "o world!") "right") (make-words "Hello" CURSOR-ON " world!"))
(check-expect (edit (make-words "Hello" CURSOR-ON "") "right") (make-words "Hello" CURSOR-ON ""))
(check-expect (edit (make-words "Hell" CURSOR-ON "o world!") "\b") (make-words "Hel" CURSOR-ON "o world!"))
(check-expect (edit (make-words "" CURSOR-ON "Hello") "\b") (make-words "" CURSOR-ON "Hello"))
(check-expect (edit (make-words "Hell" CURSOR-ON "o world!") "a") (make-words "Hella" CURSOR-ON "o world!"))
(check-expect (edit (make-words "Hell" CURSOR-ON "o world!") "shift") (make-words "Hell" CURSOR-ON "o world!"))
(define (edit process ke)
  (make-words
   (cond
     [(and (or (key=? "\b" ke) (key=? "left" ke))
           (> (string-length (words-prefix process)) 0))
      (substring (words-prefix process)
                 0 (- (string-length (words-prefix process)) 1))]
     [(key=? "\b" ke) (words-prefix process)]
     [(and (key=? "right" ke)  (> (string-length (words-suffix process)) 0))
      (string-append (words-prefix process)
                     (string-ith (words-suffix process) 0))]
     [(= (string-length ke) 1) (string-append (words-prefix process) ke)]
     [else (words-prefix process)])
   (words-cursor process)
   (cond
     [(and (key=? "left" ke) (> (string-length (words-prefix process)) 0))
      (string-append (string-ith (words-prefix process)
                                 (- (string-length (words-prefix process)) 1))
                     (words-suffix process))]
     [(and (key=? "right" ke) (> (string-length (words-suffix process)) 0))
      (substring (words-suffix process) 1)]
     [else (words-suffix process)])))

; Words -> Words
; flash cursor
(define (flash-cursor process)
  (make-words (words-prefix process)
              (cond [(equal? CURSOR-ON (words-cursor process)) CURSOR-OFF]
                    [else CURSOR-ON])
              (words-suffix process)))


; actions

(main (make-words "" CURSOR-ON ""))