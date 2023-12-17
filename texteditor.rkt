;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname texteditor) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

; data definitions


; A 1String is a string of length 1
(define (1string? s)
  (and (string? s) (= (string-length s) 1)))
; checks
(check-expect (1string? "a") #t)
(check-expect (1string? "ab") #f)
(check-expect (1string? 7) #f)
(check-expect (1string? (cons "a" '())) #f)
#;
(define (fn-on-1string ch)
  (cond
    [(not (1string? ch)) (error "not a 1string")]
    [else ... ch]))


; A ListOf1String is one of:
;    - '()
;    - cons (String ListOfString)
(define (list-of-1string? lo1s)
  (and
   (list? lo1s)
   (or
    (empty? lo1s)
    (and
     (1string? (first lo1s))
     (list-of-1string? (rest lo1s))))))
; checks
(check-expect (list-of-1string? '()) #t)
(check-expect (list-of-1string? "a") #f)
(check-expect (list-of-1string? 7) #f)
(check-expect (list-of-1string? (cons "k" (cons "a" '()))) #t)
(check-expect (list-of-1string? (cons "k" (cons 7 '()))) #f)            
#;
(define (fn-on-list-of-1string lo1s)
  (cond
    [(not (list-of-1string? lo1s)) (error "not a list of 1strings")]
    [(empty? lo1s) ...]
    [else ... (fn-on-1string (first lo1s)) ...
          (fn-on-1string (fn-on-list-of-1string (rest lo1s)))]))
 
 
; A TextEditor is a structure:
;   (make-text ListOf1String Img ListOf1String).
; as used in a text editor, the prefix and suffix of text are determined by the
;   location of the cursor
(define-struct text-editor [prefix cursor suffix])
#;
(define (fn-on-text-editor txt-ed)
  (cond
    [(not (text-editor? txt-ed)) (error "not a text editor")]
    [else ... (fn-on-list-of-1string (text-editor-prefix txt-ed))
          ... (text-editor-cursor txt-ed)
          ...  (fn-on-list-of-1string (text-editor-suffix txt-ed))]))



; constants

(define WINDOWWIDTH 400)
(define WINDOW (empty-scene WINDOWWIDTH 20))
(define CURSOR-ON (rectangle 1 16 "solid" "black"))
(define CURSOR-OFF (rectangle 1 16 "solid" "white"))
(define INITIALSTATUS (make-text-editor '() CURSOR-ON '()))



; functions

(define (main txt-ed)
  ; TextEditor -> TextEditor
  ; launches the program from some initial state 
  (big-bang txt-ed
    [on-key edit]
    [to-draw editor-GUI]
    [on-tick flash-cursor 1/2]))


(define (edit txt-ed ke)
  ; TextEditor KeyEvent -> TextEditor
  ; edit text with keystrokes
  (cond
    [(key=? "\b" ke) (delete-text txt-ed)]
    [(key=? "left" ke) (move-left txt-ed)]
    [(key=? "right" ke) (move-right txt-ed)]
    [(key=? "\r" ke) txt-ed]
    [(key=? "\t" ke) txt-ed]
    [(key=? "shift" ke) txt-ed]
    [(key=? "rshift" ke) txt-ed]
    [(> (length (text-editor-prefix txt-ed)) 42) txt-ed]
    [else (insert-text txt-ed ke)]))


; TextEditor -> Img
; display the text editor window
(define (editor-GUI txt)
  (place-image/align
   (beside (text (reverse-implode (text-editor-prefix txt) "") 12 "black")
           (text-editor-cursor txt)
           (text (implode (text-editor-suffix txt)) 12 "black"))
   5 10 "left" "center" WINDOW))


; TextEditor -> TextEditor
; flash cursor
(define (flash-cursor txt)
  (make-text-editor (text-editor-prefix txt)
                    (cond [(equal? CURSOR-ON (text-editor-cursor txt))
                           CURSOR-OFF]
                          [else CURSOR-ON])
                    (text-editor-suffix txt)))


(define (insert-text txt-ed ke)
  ; TextEditor KeyEvent -> TextEditor
  ; type text with keystrokes
  (make-text-editor
   (cons ke (text-editor-prefix txt-ed))
   (text-editor-cursor txt-ed)
   (text-editor-suffix txt-ed)))
; checks
(check-expect (insert-text (make-text-editor '() CURSOR-ON '()) "p")
              (make-text-editor (cons "p" '()) CURSOR-ON '()))
(check-expect (insert-text (make-text-editor (cons "p" '()) CURSOR-ON '()) "r")
              (make-text-editor (cons "r" (cons "p" '())) CURSOR-ON '()))


(define (delete-text txt-ed)
  ; TextEditor -> TextEditor
  ; delete one character of text
  (cond
    [(empty? (text-editor-prefix txt-ed)) txt-ed]
    [else (make-text-editor
           (rest (text-editor-prefix txt-ed))
           (text-editor-cursor txt-ed)
           (text-editor-suffix txt-ed))]))
; checks
(check-expect (delete-text (make-text-editor '() CURSOR-ON '()))
              (make-text-editor '() CURSOR-ON '()))
(check-expect (delete-text (make-text-editor (cons "r" (cons "p" '()))
                                             CURSOR-ON '()))
              (make-text-editor (cons "p" '()) CURSOR-ON '()))


(define (move-left txt-ed)
  ; TextEditor -> TextEditor
  ; move cursor one character to the left
  (cond
    [(empty? (text-editor-prefix txt-ed)) txt-ed]
    [else (make-text-editor
           (rest (text-editor-prefix txt-ed))
           (text-editor-cursor txt-ed)
           (cons (first (text-editor-prefix txt-ed))
                 (text-editor-suffix txt-ed)))]))
; checks
(check-expect (move-left (make-text-editor '() CURSOR-ON (cons "p" '())))
              (make-text-editor '() CURSOR-ON (cons "p" '())))
(check-expect (move-left (make-text-editor (cons "r" (cons "p" '()))
                                           CURSOR-ON '()))
              (make-text-editor (cons "p" '()) CURSOR-ON (cons "r" '())))


(define (move-right txt-ed)
  ; TextEditor -> TextEditor
  ; move cursor one character to the left
  (cond
    [(empty? (text-editor-suffix txt-ed)) txt-ed]
    [else (make-text-editor
           (cons (first (text-editor-suffix txt-ed))
                 (text-editor-prefix txt-ed))
           (text-editor-cursor txt-ed)
           (rest (text-editor-suffix txt-ed)))]))
; checks
(check-expect (move-right (make-text-editor '() CURSOR-ON '()))
              (make-text-editor '() CURSOR-ON '()))
(check-expect (move-right (make-text-editor (cons "p" '())
                                            CURSOR-ON (cons "t" '())))
              (make-text-editor (cons "t" (cons "p" '())) CURSOR-ON '()))


(define (reverse-implode lo1s-from string-to)
  ;; ListOf1Strings String -> String
  ;; implode the list in reverse
  (cond
    [(empty? lo1s-from) string-to]
    [else (reverse-implode (rest lo1s-from)
                           (string-append (first lo1s-from) string-to))]))


; actions

(main INITIALSTATUS)