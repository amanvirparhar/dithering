#lang racket
(require racket/draw)

; read bitmap from disk, get width and height
(define bmp (read-bitmap "cat.bmp"))
(define width (send bmp get-width))
(define height (send bmp get-height))

; creates a buffer for all pixels
(define pixels-vec (make-bytes (* 4 width height)))
(send bmp get-argb-pixels 0 0 width height pixels-vec)

; struct for storing error diffusion weights and offsets
(struct dither-pixel (dy dx weight))

; my own dithering pattern - spreads error to 6 neighboring pixels
(define dither-pattern
  (list
    (dither-pixel 0 1 1/4)
    (dither-pixel 0 2 1/16)
    (dither-pixel 1 -1 1/8)
    (dither-pixel 1 0 1/4)
    (dither-pixel 1 1 1/8)
    (dither-pixel 2 0 1/16)))

; creates a buffer to store luminance values
(define grayscale-vec (make-bytes (* width height)))

; helpers to get and set luminance value at specific coords
(define (get-pixel-luminance i j)
  (bytes-ref grayscale-vec (+ j (* i width))))
(define (set-pixel-luminance i j value)
  (bytes-set! grayscale-vec (+ j (* i width)) value))

; grab rgba values and convert to grayscale using ITU-R BT.709 luminance formula
(for ([i height])
  (for ([j width])
    (define pixel-pos (* 4 (+ j (* i width)))) ; get pixel position in buffer
    (define luminance
      (min 
        (exact-round 
          (+ (* 0.2126 (bytes-ref pixels-vec (+ pixel-pos 1))) ; 2nd byte - red
             (* 0.7152 (bytes-ref pixels-vec (+ pixel-pos 2))) ; 3rd byte - green
             (* 0.0722 (bytes-ref pixels-vec (+ pixel-pos 3))) ; 4th byte - blue
          )
        ) 255))
    (set-pixel-luminance i j luminance)))

; converts each pixel to black or white and distributes error to neighbors
(for ([i height])
  (for ([j width])
    (define luminance (get-pixel-luminance i j))
    (define threshold-value (if (> luminance 127) 255 0)) ; halfway threshold at 127 - brighter pixels become white, darker become black
    (define quant-error (- luminance threshold-value)) ; calculates error between original and thresholded value

    ; spread the error to neighboring pixels according to dither pattern
    (for-each (λ (offset)
      (define neighbor-i (+ i (dither-pixel-dy offset)))
      (define neighbor-j (+ j (dither-pixel-dx offset)))
      (when (and (>= neighbor-i 0) (< neighbor-i height) (>= neighbor-j 0) (< neighbor-j width)) ; only update neighbors that are within image bounds
        (define curr-luminance (get-pixel-luminance neighbor-i neighbor-j))
        (define distributed-error (round (* quant-error (dither-pixel-weight offset))))
        (define new-luminance (max (min (+ curr-luminance distributed-error) 255) 0)) ; clamp new value between 0 and 255
        (set-pixel-luminance neighbor-i neighbor-j new-luminance))) ; set current pixel to threshold value
      dither-pattern)
    (set-pixel-luminance i j threshold-value)))

; convert back to bitmap format - set rgb channels all to same value for grayscale
(for ([i height])
  (for ([j width])
    (define pixel-pos (* 4 (+ j (* i width))))
    (define luminance (get-pixel-luminance i j))
    (bytes-set! pixels-vec (+ pixel-pos 1) luminance)
    (bytes-set! pixels-vec (+ pixel-pos 2) luminance)
    (bytes-set! pixels-vec (+ pixel-pos 3) luminance)
  ))

; write back to bitmap, and save dithered image to disk
(send bmp set-argb-pixels 0 0 width height pixels-vec)
(send bmp save-file "my-dither.bmp" 'bmp 100)