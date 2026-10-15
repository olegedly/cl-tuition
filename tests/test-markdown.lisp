;;; test-markdown.lisp
;;;
;;; SPDX-License-Identifier: MIT
;;;
;;;; Tests for markdown rendering, and for the one thing a rendered link owes a
;;;; terminal: the URL, not only the look of a link.

(in-package #:tuition-tests)

(def-suite markdown-tests
  :description "Markdown rendering, and the OSC 8 hyperlinks it emits."
  :in tuition-tests)

(in-suite markdown-tests)

(defun %osc8-open (uri)
  "The OSC 8 sequence that opens a hyperlink to URI (ST-terminated)."
  (format nil "~C]8;;~A~C\\" #\Escape uri #\Escape))

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
