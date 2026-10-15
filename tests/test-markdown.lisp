;;; test-markdown.lisp
;;;
;;; SPDX-License-Identifier: MIT
;;;
;;;; Tests for markdown rendering, and for the one thing a rendered link owes a
;;;; terminal: the URL, not only the look of a link. And for the width: a pane
;;;; draws what it was given, so a line that reaches its edge and goes on has
;;;; already been lost by the time a reader sees it.

(in-package #:tuition-tests)

(def-suite markdown-tests
  :description "Markdown rendering, and the OSC 8 hyperlinks it emits."
  :in tuition-tests)

(in-suite markdown-tests)

(defun %osc8-open (uri)
  "The OSC 8 sequence that opens a hyperlink to URI (ST-terminated)."
  (format nil "~C]8;;~A~C\\" #\Escape uri #\Escape))

(defun %rendered-lines (markdown &key (width 32))
  "MARKDOWN's lines as a pane would receive them, wrapped to WIDTH."
  (tuition:split-string-by-newline (render-markdown markdown :width width)))

(defun %all-lines-fit-p (lines width)
  "Does every line of LINES fit in WIDTH visible columns?"
  (every (lambda (line) (<= (visible-length line) width)) lines))

(defun %words-kept-p (lines words)
  "Does the concatenation of LINES still carry each of WORDS whole?
Concatenation and not joining: a wrap may fall between two words, and a word the
renderer cut in half would still read as present if the halves were joined."
  (let ((joined (format nil "~{~a~}" lines)))
    (every (lambda (word) (search word joined)) words)))

(defun %visible-line (line width)
  "LINE as a terminal shows it: the cells it draws, ANSI and all resolved."
  (let ((buf (parse-styled-string line width 1)))
    (with-output-to-string (s)
      (loop for x from 0 below width
            do (write-char (cell-char (screen-buffer-ref buf x 0)) s)))))

(test markdown-link-carries-its-url
  "A `[text](url)' link is rendered as text the terminal knows the URL of, which
is what makes it clickable rather than merely blue."
  (let ((rendered (render-markdown "See [the site](https://example.com/x) now." :width 80)))
    (is (search (%osc8-open "https://example.com/x") rendered))
    (is (search "the site" rendered))))

(test markdown-link-url-reaches-the-cells-parser
  "The cells parser -- what the renderer draws through -- records the URL on the
link's own cells, and on no other."
  (let* ((rendered (render-markdown "See [the site](https://example.com/x) now." :width 80))
         (buf (parse-styled-string rendered 80 2)))
    ;; "See " occupies columns 0-3, "the site" 4-11, and " now." follows.
    (is (null (cell-link (screen-buffer-ref buf 0 0))))
    (is (equal "https://example.com/x" (cell-link (screen-buffer-ref buf 4 0))))
    (is (equal "https://example.com/x" (cell-link (screen-buffer-ref buf 11 0))))
    (is (null (cell-link (screen-buffer-ref buf 12 0))))))

;;; ---------------------------------------------------------------------------
;;; The lines that are not paragraphs
;;;
;;; A quote, a heading and a code line are all lines a reader reads, and every
;;; one of them can be longer than the pane holding it. What says a wrapped line
;;; is still the same quotation, or the same heading, is what leads it -- so
;;; these tests ask two questions of each: does it fit, and does it still say
;;; what it is on the lines after the first.

(test markdown-wraps-a-quote-and-keeps-the-prefix-on-every-line
  "A quote that outgrows the width comes back on the next line, still a quote."
  (let* ((width 30)
         (lines (%rendered-lines "> the quick brown fox jumps over the lazy dog"
                                 :width width)))
    (is (> (length lines) 1))
    (is (%all-lines-fit-p lines width))
    (is (every (lambda (line) (search "│" line)) lines))
    (is (%words-kept-p lines '("quick" "brown" "fox" "jumps" "lazy" "dog")))))

(test markdown-wraps-a-heading-and-keeps-its-marker-on-every-line
  "A heading that outgrows the width is still a heading on the line after."
  (let* ((width 24)
         (lines (%rendered-lines "## Foundational software construction with agents"
                                 :width width)))
    (is (> (length lines) 1))
    (is (%all-lines-fit-p lines width))
    (is (every (lambda (line) (search "## " line)) lines))
    (is (%words-kept-p lines '("Foundational" "construction" "agents")))))

(test markdown-wraps-a-code-line-under-its-own-indentation
  "Code's spaces are content, so a wrapped code line continues under the line it
came from, and its tokens come through whole."
  (let* ((width 28)
         (markdown (format nil "```~%    (let ((x 1)) (print x) (print (* x 2)))~%```"))
         (lines (%rendered-lines markdown :width width)))
    (is (> (length lines) 1))
    (is (%all-lines-fit-p lines width))
    ;; The code-block margin is one column and the code indents four, so every
    ;; line of it -- the first and the ones that wrap under it -- begins in
    ;; column six, and the indentation is read back the way a terminal reads it.
    (is (every (lambda (line)
                 (string= "     " (subseq (%visible-line line width) 0 5)))
               lines))
    (is (%words-kept-p lines '("(let" "((x" "1))" "(print" "(*" "x" "2)))")))))

(test markdown-still-wraps-a-paragraph
  "The lines that are prose keep wrapping as they did -- and take no prefix."
  (let* ((width 24)
         (lines (%rendered-lines "the quick brown fox jumps over the lazy dog"
                                 :width width)))
    (is (> (length lines) 1))
    (is (%all-lines-fit-p lines width))
    (is (not (search "│" (format nil "~{~a~}" lines))))))
