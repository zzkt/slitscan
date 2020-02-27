#! /usr/bin/env racket
#lang racket

;;; Temporal slice-stacking with FFmpeg (aka slitscan, etc+)
;;;
;;; Copyright (C) 2019 FoAM
;;;
;;;  This program is free software: you can redistribute it and/or modify
;;;  it under the terms of the GNU General Public License as published by
;;;  the Free Software Foundation, either version 3 of the License, or
;;;  (at your option) any later version.
;;;
;;;  This program is distributed in the hope that it will be useful,
;;;  but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;;  GNU General Public License for more details.
;;;
;;;  You should have received a copy of the GNU General Public License
;;;  along with this program.  If not, see <https://www.gnu.org/licenses/>.
;;;
;;; 
;;; Authors 
;;;
;;;  nik gaffney <nik@fo.am>
;;;
;;; Requirements
;;;
;;;  Racket, ffmpeg and zsh or bash (and probably works with other shells)
;;;
;;; Commentary
;;;
;;;  Mostly scaffolding around ffmpeg, based on a shell script from oioiiooixiii
;;;   http://oioiiooixiii.blogspot.com/2017/11/ffmpeg-temporal-slice-stacking-effect.html
;;; 
;;;  Further details, instructions and digression can be found at
;;;   https://github.com/zzkt/slitscan/
;;;
;;; Revision history
;;; 
;;;  - 2017-10-01 - shell script from oioiiooixiii 
;;;  - 2019-08-08 - cmprzs, convert, reconstruct
;;;  - 2019-08-10 - racket translation, info 


;; cli options
(define filename (make-parameter ""))
(define verbose? (make-parameter #f))
(define cleanup? (make-parameter #f))
(define noresize? (make-parameter #f))
(define in-folder (make-parameter ""))
(define out-folder (make-parameter ""))
(define loglevel (make-parameter "")) ;; leave unset by default

;; output options horiz/vert/both
(define hout-only? (make-parameter #f))
(define vout-only? (make-parameter #f))

(define output-width  (make-parameter 640))
(define output-height (make-parameter 360))

;; parse command line
(define getopt  
  (command-line
   #:program "slitscan"
   #:once-each
   (("-v" "--verbose")    "various verbose messages" (verbose? #t))
   (("-l" "--loglevel")   level "loglevel for ffmpeg e.g. quiet, error, warning, info, debug" (loglevel level))
   (("-c" "--cleanup")    "Clean up temporary/working files" (cleanup? #t))
   (("-n" "--noresize")   "do not resize the input file" (noresize? #t))   
   (("--horizontal")      "output a horizontal video only (default: both)" (hout-only? #t))
   (("--vertical")        "output a vertical video only (default: both)" (vout-only? #t))
   (("--width")           pixels "width of transform video" (output-width pixels))
   (("--height")          pixels "height of tansform video"  (output-height pixels))   
   (("-f" "--in-folder")  folder "input folder"  (in-folder folder))
   (("-o" "--out-folder") folder "output folder" (out-folder folder))
   #:args (video-file) ; rest of the args?
   (filename video-file)
   (if (file-exists? (filename))
       (printf "slitscanning: ~a\n" (filename))
       (raise-user-error 'slitscan "File '~a' does not exist." (filename)))))

;; emulation of $(...) shell syntax 
(define-syntax zout 
  (syntax-rules ()
    ((_ cmd) (string-trim (with-output-to-string (lambda () (system cmd)))))))

;; echoing verbosity
(define-syntax vecho
  (syntax-rules ()
    ((_ str ...) (when (verbose?) (printf str ...)))))

(cond ((and (verbose?) (string=? "" (loglevel))) (loglevel "info"))
      ((and (not (verbose?)) (string=? "" (loglevel))) (loglevel "error")))

;; variables/various
(define folder (if (not (string=? "" (in-folder)))
                   (in-folder) 
                   (zout "mktemp -d")))

(vecho "using folder '~a' for input\n" folder)

;(define fffeature "ffprobe -v quiet -select_streams v -of default=noprint_wrappers=1:nokey=1 -show_entries stream=")

(define fffeature "ffprobe -v error -of default=noprint_wrappers=1:nokey=1 -show_entries format=")

;; assume most/all codecs/formats/streams have a duration
(define duration (let ((f (string->number
                           (zout (format
                                  "~aduration \"~a\""
                                  fffeature (filename))))))
                   (if f f 0)))

(vecho "duration: ~a seconds\n" duration)

;; determine fps from input file or default to 24
(define fps (let ((f (string->number
                      (zout (format
                             "~ar_frame_rate \"~a\""
                             fffeature (filename))))))
              (if f f 24)))

(vecho "fps: ~a\n" fps)

;; number of frames. estimate for codecs/formats/streams without nb_frames 
(define frames (let ((f (string->number
                         (zout (format
                                "~anb_frames \"~a\""
                                fffeature (filename))))))
                 (if f f (ceiling (* duration fps)))))

(vecho "frames: ~a\n" frames)

;; output dimensions. change if/as required via opts...
(define w (output-width)) 
(define h (output-height))

(vecho "transform dimensions: ~ax~a\n" w h)

;; FFmpeg Filterchains
(define stemStart "select=gte(n\\,")
(define stemEnd "),format=yuv444p,split[horz][vert]")
(define horz (format "[horz]crop=in_w:1:0:n,tile=1x~a[horz]" h))
(define vert (format "[vert]crop=1:in_h:n:0,tile=~aX1[vert]" w))
(define merge "[0:v]null[horz];[1:v]null[vert]")
(define scale (format "scale=~a:~a" w h))

;; check resume?

;; rezise video
(define resized-video (string-append folder "/resized.mkv"))

;; resize video, or copy file if 'noresize?' option is set
(define (resize)
  (define ffmpeg-r1
    (format "ffmpeg -loglevel ~a -i \"~a\" -vf ~a -crf 10 \"~a\" 2>&1 | grep 'frame=' | tr \\n \\r; echo"
            (loglevel) (filename) scale resized-video)) 
  (vecho "resizing video: ~a\n" resized-video)
  (system ffmpeg-r1))


;; slice/crop/loop
(define (slice)
  (for ((i (range frames))) 
    (vecho "Frame: ~a of ~a\r" i frames)
    (define ffmpeg-s1 
      (format     
       "ffmpeg -loglevel ~a -i \"~a\" \
        -filter_complex \"~a~a~a;~a;~a\"  \
        -map '[horz]' \
          -vframes 1 ~a/horz_frame~a.png \
        -map '[vert]' \
          -vframes 1 ~a/vert_frame~a.png"
       (loglevel) resized-video
       stemStart i stemEnd horz vert
       folder i
       folder i))
    (system ffmpeg-s1))
  (vecho "\n"))


;; Join images (optional sharpening, upscale, etc. via 'merge' variable)
(define (assemble)
  (vecho "Creating output videos\n")

  (define ffmpeg-a0  ;; horizontal & vertical videos (from .sh) 
    (format   
     "ffmpeg -loglevel ~a \
      -r ~a -i ~a/horz_frame%d.png \
      -r ~a -i ~a/vert_frame%d.png \
      -filter_complex \"~a\" \
      -map '[horz]' \
        -r ~a \
        -crf 10 \
        \"~a_horizontal-smear.mkv\" \
      -map '[vert]' \
        -r ~a \
        -crf 10 \
        \"~a_vertical-smear.mkv\""
     (loglevel) 
     fps folder
     fps folder 
     merge 
     fps (filename)
     fps (filename)))

  (define ffmpeg-a1 ;; horizontal frame stack 
    (format   
     "ffmpeg -loglevel ~a -r ~a -i ~a/horz_frame%d.png \"~a_horizontal-smear.mkv\""
     (loglevel) fps folder (filename)))

  (define ffmpeg-a2 ;; vertical frame stack 
    (format   
     "ffmpeg -loglevel ~a -r ~a -i ~a/vert_frame%d.png \"~a_vertical-smear.mkv\""
     (loglevel) fps folder (filename)))
  
  ;; assemble
  (cond  ((hout-only?) (system ffmpeg-a1))  
         ((vout-only?) (system ffmpeg-a2))
         (else (system ffmpeg-a1)
               (system ffmpeg-a2))))

;; spatial extents (frames -> image) summary
(define (framecat start offset)
  (define montage-h1
    (format "montage ~a/horz_frame~a.png ~a/horz_frame~a.png ~a/horz_frame~a.png ~a/horz_frame~a.png ~a/horz_frame~a.png -tile x1 -mode concatenate ~a-slitscan-s~a.o~a.jpg"
            folder start
            folder (+ start offset)
            folder (+ start (* offset 2))
            folder (+ start (* offset 3))
            folder (+ start (* offset 4))
            (filename) start offset))
  (system montage-h1))

(define (image) (framecat 1 (/ frames 5)))
               
;; delete working files (or not) 
(define (cleanup)
  (if (cleanup?)
      (begin (system (format "rm -rf ~a" folder))
             (printf "Removed temp files at ~a\n" folder))
      (vecho "leaving working files at ~a\n" folder)))

;; output in 3 (or 4 (or 5)) steps
  
  (define (slitscan)
    (when (resize)   (printf "resized\n"))
    (when (slice)    (printf "sliced\n"))
    (when (assemble) (printf "assembled\n"))
    (when (cleanup)  (printf "done\n")))

  
  ;; (printf "The file '~a' does not exist.\n" (filename))

(slitscan)

;;; FIN
