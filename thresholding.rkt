#lang racket
(require racket/draw)

; read bitmap from disk, get width and height
(define bmp (read-bitmap "cat.bmp"))
(define width (send bmp get-width))
(define height (send bmp get-height))

; creates a buffer for all pixels
(define pixels-vec (make-bytes (* 4 width height)))
(send bmp get-argb-pixels 0 0 width height pixels-vec)

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
    (define threshold-value (if (> luminance 127) 255 0)) ; halfway threshold at 127 - brighter pixels become white, darker become black
    (bytes-set! pixels-vec (+ pixel-pos 1) threshold-value)
    (bytes-set! pixels-vec (+ pixel-pos 2) threshold-value)
    (bytes-set! pixels-vec (+ pixel-pos 3) threshold-value)))

; write back to bitmap, and save dithered image to disk
(send bmp set-argb-pixels 0 0 width height pixels-vec)
(send bmp save-file "thresholding.bmp" 'bmp 100)