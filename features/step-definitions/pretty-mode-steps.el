;; This file contains your project specific step definitions. All
;; files in this directory whose names end with "-steps.el" will be
;; loaded automatically by Ecukes.

(Then "^the variable \\(.+\\) should be \\(.+\\)$"
      (lambda (name value)
        (let ((name (intern name))
              (value (intern value)))
          (cl-assert (eq (eval name) value) t))))

(Then "^the minor mode \\(.+\\) should be active$"
      (lambda (name)
        (let ((minor-mode (intern name)))
          (cl-assert
           (memq minor-mode minor-mode-list) t))))

(When "^I run \\(.+\\)$"
      (lambda (name)
        (let ((cmd (intern name)))
          (funcall cmd))))

(When "^I wait$"
      (lambda ()
        (sleep-for 1)))

(defun glyph-width (glyph)
  "how many real characters in the buffer does a glyph replace"
  (or (cdr (assoc glyph '((#x2a75 . 2) ; ⩵
                         (#x2a76 . 3) ; ⩶
                         (#x2245 . 2) ; ≅
                         (#x2247 . 2) ; ≇
                         (#x2264 . 2) ; ≤
                         (#x2265 . 2) ; ≥
                         (#x226a . 2) ; ≪
                         (#x226b . 2) ; ≫
                         (#x22d8 . 3) ; ⋘
                         (#x22d9 . 3) ; ⋙
                         (#x2260 . 2) ; ≠
                         (#x00ac . 3) ; ¬
                         (#x2227 . 3) ; ∧
                         (#x2228 . 2) ; ∨
                         (#x22c0 . 3) ; ⋀
                         (#x22c1 . 2) ; ⋁
                         (#x2208 . 2) ; ∈
                         (#x03bb . 8) ; λ
                         (#x2025 . 2) ; ‥
                         (#x2026 . 3) ; …
                         (#x203c . 2) ; ‼
                         (#x2237 . 2) ; ∷
                         (#x2190 . 2) ; ←
                         (#x2192 . 2) ; →
                         )))
     1))

(Then "^the buffer should appear as\\(?: \"\\(.+\\)\"\\|:\\)$"
      "Asserts that the buffer appears like this"
      (lambda (contents)
        (goto-char 1)
        (let ((pos 0))
          (while (not (eobp))
            (let* ((glyph (aref contents pos))
                   (p (point))
                   (comp (get-text-property p 'composition))
                   (gl (if comp (aref (caddr comp) 0) (char-after p))))
              (assert (= glyph gl) t)
              (redisplay)
              (forward-char (glyph-width glyph)))
            (setq pos (1+ pos))))))
