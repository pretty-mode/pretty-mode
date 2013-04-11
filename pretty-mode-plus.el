;;; pretty-mode-plus.el --- redisplay parts of the buffer as pretty symbols
;;; -*- coding: utf-8 -*-
;; Filename: pretty-mode-plus.el
;; Description: Redisplay parts of the buffer as pretty symbols.
;; Author: Arthur Danskin
;; Maintainer: Grant Rettke

;;; Commentary:
;; 
;; Minor mode for redisplaying parts of the buffer as pretty symbols
;; originally modified from Trent Buck's version at http://paste.lisp.org/display/42335,2/raw
;; Also includes code from `sml-mode'
;; See also http://www.emacswiki.org/cgi-bin/wiki/PrettyLambda
;; 
;; Released under the GPL. No implied warranties, etc. Use at your own risk.
;; Arthur Danskin <arthurdanskin@gmail.com>, March 2008
;;
;; Modifications made here only to mapped symbols by Grant Rettke <grettke@acm.org>, September 2012.
;; 
;; to install:
;; (require 'pretty-mode-plus)
;; and 
;; (global-pretty-mode 1)
;; or
;; (add-hook 'my-pretty-language-hook 'turn-on-pretty-mode)

;;; Code:

(require 'cl)

(defvar pretty-syntax-types '(?_ ?w))

;; modified from `sml-mode'
(defun pretty-font-lock-compose-symbol (alist)
  "Compose a sequence of ascii chars into a symbol."
  (let* ((start (match-beginning 0))
         (end (match-end 0))
         (syntax (char-syntax (char-after start))))
    (if (or (if (memq syntax pretty-syntax-types)
               (or (memq (char-syntax (char-before start)) pretty-syntax-types)
                  (memq (char-syntax (char-after end)) pretty-syntax-types))
             (memq (char-syntax (char-before start)) '(?. ?\\)))
           (memq (get-text-property start 'face)
                 '(font-lock-doc-face font-lock-string-face
                                      font-lock-comment-face)))
        (remove-text-properties start end '(composition))
      (compose-region start end (cdr (assoc (match-string 0) alist)))
;;;       (add-text-properties start end `(display ,repl)))
      ))
  ;; Return nil because we're not adding any face property.
  nil)

(defvar pretty-interaction-mode-alist
  '((inferior-scheme-mode . scheme-mode)
    (lisp-interaction-mode . emacs-lisp-mode)
    (inferior-lisp-mode . lisp-mode)
    (inferior-ess-mode . ess-mode)
    (inf-haskell-mode . haskell-mode)
    (tuareg-interactive-mode . tuareg-mode)
    (inferior-python-mode . python-mode)
    (inferior-octave-mode . octave-mode)
    (inferior-ruby-mode . ruby-mode))
  "Alist mapping from inferior process interaction modes to their
  corresponding script editing modes.")


(defun pretty-font-lock-keywords (alist)
  "Return a `font-lock-keywords' style entry for replacing
regular expressions with symbols. ALIST has the form ((STRING .
REPLACE-CHAR) ...)."
  (when alist
    `((,(regexp-opt (mapcar 'car alist))
       (0 (pretty-font-lock-compose-symbol
           ',alist))))))

(defun pretty-keywords (&optional mode)
  "Return the font-lock keywords for MODE, or the current mode if
MODE is nil. Return nil if there are no keywords."
  (let* ((mode (or mode major-mode))
         (kwds (cdr-safe
                (or (assoc mode pretty-patterns)
                   (assoc (cdr-safe
                           (assoc mode pretty-interaction-mode-alist))
                          pretty-patterns)))))
    (pretty-font-lock-keywords kwds)))

(defgroup pretty nil "Minor mode for replacing text with symbols "
  :group 'faces)

(define-minor-mode pretty-mode
  "Toggle Pretty minor mode.
With arg, turn Pretty minor mode on if arg is positive, off otherwise.

Pretty mode builds on `font-lock-mode'. Instead of highlighting
keywords, it replaces them with symbols. For example, lambda is
displayed as λ in lisp modes."
  :group 'pretty
                                        ;  :lighter " λ"
  (if pretty-mode
      (progn
        (font-lock-add-keywords nil (pretty-keywords) t)
        (when font-lock-mode
          (font-lock-fontify-buffer)))
    (font-lock-remove-keywords nil (pretty-keywords))
    (remove-text-properties (point-min) (point-max) '(composition nil))))

(defun turn-on-pretty-if-desired ()
  "Turn on `pretty-mode' if the current major mode supports it."
  (if (pretty-keywords)
      (pretty-mode 1)))

(define-globalized-minor-mode global-pretty-mode
  pretty-mode turn-on-pretty-if-desired
  :init-value t)

(defun turn-off-pretty-mode ()
  (interactive)
  (pretty-mode -1))


(defun turn-on-pretty-mode ()
  (interactive)
  (pretty-mode +1))

(defun pretty-compile-patterns (patterns)
  "Set pretty patterns in a convenient way.

PATTERNS should be of the form ((GLYPH (REGEXP MODE ...) ...)
...). GLYPH should be a character. MODE should be the name of a
major mode without the \"-mode\". Returns patterns in the form
expected by `pretty-patterns'"
  (let ((pretty-patterns))
    (loop for (glyph . pairs) in patterns do
          (loop for (regexp . major-modes) in pairs do
                (loop for mode in major-modes do
                      (let* ((mode (intern (concat (symbol-name mode)
                                                   "-mode")))
                             (assoc-pair (assoc mode pretty-patterns))
                             
                             (entry (cons regexp glyph)))
                        (if assoc-pair
                            (push entry (cdr assoc-pair))
                          (push (cons mode (list entry))
                                pretty-patterns))))))
    pretty-patterns))

(defvar pretty-patterns
  (let* ((lispy '(scheme emacs-lisp lisp clojure jess))
         (haskelly '(haskell literate-haskell))
         (mley (append haskelly '(tuareg sml)))
         (c-like '(c c++ perl sh python java ess ruby))
         (all (append lispy mley c-like (list 'octave))))
    (pretty-compile-patterns
     `(
       ;; Values taken directly from `The Unicode Standard, Version 5.2' documented
       ;; in `U0080.pdf', located at http://unicode.org/charts/PDF/U0080.pdf

       ;; 00AB « LEFT-POINTING DOUBLE ANGLE QUOTATION MARK
       (?\u00AB ("<<" ,@haskelly))

       ;; 00AC ¬ NOT SIGN
       (?\u00AC ("!" c c++ perl sh)
                ("not" ,@lispy ,@haskelly sml))

       ;; 00B2 ² SUPERSCRIPT TWO
       (?\u00B2 ("**2" python tuareg octave)
                ("^2" ,@haskelly))

       ;; 00B3 ³ SUPERSCRIPT THREE
       (?\u00B3 ("**3" python tuareg octave)
                ("^3" ,@haskelly))

       ;; 00BB » RIGHT-POINTING DOUBLE ANGLE QUOTATION MARK
       (?\u00BB (">>" ,@haskelly))

       ;; Values taken directly from `The Unicode Standard, Version 5.2' documented
       ;; in `U2000.pdf', located at http://unicode.org/charts/PDF/U2000.pdf

       ;; 2025 ‥ TWO DOT LEADER
       (?\u2025 (".." ,@haskelly ruby))

       ;; 2026 … HORIZONTAL ELLIPSIS
       (?\u2026 ("..." scheme))

       ;; 203C ‼ DOUBLE EXCLAMATION MARK
       (?\u203C ("!!" ,@haskelly))

       ;; Values taken directly from `The Unicode Standard, Version 5.2' documented
       ;; in `U2070.pdf', located at http://unicode.org/charts/PDF/U2070.pdf

       ;; 207F ⁿ SUPERSCRIPT LATIN SMALL LETTER N
       (?\u207F ("**n" python tuareg octave)
                ("^n" ,@haskelly))

       ;; 2080 ₀ SUBSCRIPT ZERO
       (?\u2080 ("[0]" ,@c-like)
                ("(0)" octave)
                (".(0)" tuareg)
                ("!!0" ,@haskelly))

       ;; 2081 ₁ SUBSCRIPT ONE
       (?\u2081 ("[1]" ,@c-like)
                ("(1)" octave)
                (".(1)" tuareg)
                ("!!1" ,@haskelly))

       ;; 2082 ₂ SUBSCRIPT TWO
       (?\u2082 ("[2]" ,@c-like)
                ("(2)" octave)
                (".(2)" tuareg)
                ("!!2" ,@haskelly))

       ;; 2083 ₃ SUBSCRIPT THREE
       (?\u2083 ("[3]" ,@c-like)
                ("(3)" octave)
                (".(3)" tuareg)
                ("!!3" ,@haskelly))

       ;; 2084 ₄ SUBSCRIPT FOUR
       (?\u2084 ("[4]" ,@c-like)
                ("(4)" octave)
                (".(4)" tuareg)
                ("!!4" ,@haskelly))

       ;; Values taken directly from `The Unicode Standard, Version 5.2' documented
       ;; in `U2100.pdf', located at http://unicode.org/charts/PDF/U2100.pdf

       ;; 2124 ℤ DOUBLE-STRUCK CAPITAL Z
       (?\u2124 ("Integer" ,@haskelly))

       ;; Values taken directly from `The Unicode Standard, Version 5.2' documented
       ;; in `U2190.pdf', located at http://unicode.org/charts/PDF/U2190.pdf

       ;; 2190 ← LEFTWARDS ARROW
       (?\u2190 ("<-" ,@mley ess ,@lispy))

       ;; 2191 ↑ UPWARDS ARROW
       (?\u2191 ("\\^" tuareg))

       ;; 2192 → RIGHTWARDS ARROW
       (?\u2192 ("->" ,@mley ess c c++ perl ,@lispy))

       ;; 21A0 ↠ RIGHTWARDS TWO HEADED ARROW
       (?\u21A0 ("->>" ,@lispy))

       ;; 21D2 ⇒ RIGHTWARDS DOUBLE ARROW
       (?\u21D2 ("=>" sml perl ruby ,@lispy ,@haskelly))

       ;; 21F9 ⇹ LEFT RIGHT ARROW WITH VERTICAL STROKE
       (?\u21F9 ("<|>" ,@haskelly))

       ;; Values taken directly from `The Unicode Standard, Version 5.2' documented
       ;; in `U2200.pdf', located at http://unicode.org/charts/PDF/U2200.pdf

       ;; 2200 ∀ FOR ALL
       (?\u2200 ("forall" ,@haskelly))

       ;; 2203 ∃ THERE EXISTS
       (?\u2203 ("exists" ,@haskelly))

       ;; 2205 ∅ EMPTY SET
       (?\u2205 ("nil" emacs-lisp ruby clojure)
                ("null" scheme java)
                ("'()" scheme)
                ("empty" scheme)
                ("NULL" c c++)
                ;; ("None" python)
                ("()" ,@mley)
                ("[]" ,@mley))

       ;; 2208 ∈ ELEMENT OF
       (?\u2208 ("`elem`" ,@haskelly)
                ("in" python))

       ;; 2209 ∉ NOT AN ELEMENT OF
       (?\u2209 ("`notElem`" ,@haskelly)
                ("not in" python))

       ;; 220F ∏ N-ARY PRODUCT
       (?\u220F ("product" ,@haskelly))

       ;; 2211 Σ N-ARY SUMMATION
       (?\u2211 ("sum" python ,@haskelly))

       ;; 221a √ SQUARE ROOT
       (?\u221A ("sqrt" ,@all))

       ;; 2227 ∧ LOGICAL AND
       (?\u2227 ("and" ,@lispy python ruby)
                ("andalso" sml)
                ("&&" c c++ perl ,@haskelly ruby))

       ;; 2228 ∨ LOGICAL OR
       (?\u2228 ("or" ,@lispy python ruby)
                ("orelse" sml)
                ("||" c c++ perl ,@haskelly ruby))

       ;; 2237 ∷ PROPORTION
       (?\u2237 ("::" ,@haskelly))

       ;; 2260 ≠ NOT EQUAL TO
       (?\u2260 ("!=" ,@c-like scheme octave)
                ("not=" clojure)
                ("<>" tuareg octave)
                ("~=" octave)
                ("/=" ,@haskelly))

       ;; 2261 ≡ IDENTICAL TO
       (?\u2261 ("==" ,@c-like ,@haskelly))

       ;; 2264 ≤ LESS-THAN OR EQUAL TO
       (?\u2264 ("<=" ,@all))

       ;; 2265 ≥ GREATER-THAN OR EQUAL TO
       (?\u2265 (">=" ,@all))

       ;; 22A5 ⊥ UP TACK
       (?\u22A5 ("undefined" ,@haskelly))

       ;; 22C0 ⋀ N-ARY LOGICAL AND
       (?\u22C0 ("and" ,@haskelly))

       ;; 22C1 ⋁ N-ARY LOGICAL OR
       (?\u22C1 ("or" ,@haskelly))

       ;; 22C5 ⋅ DOT OPERATOR
       (?\u22C5 ("\." ,@haskelly))

       ;; Values taken directly from `The Unicode Standard, Version 5.2' documented
       ;; in `U27C0.pdf', located at http://unicode.org/charts/PDF/U2900.pdf

       ;; 27E6 ⟦ MATHEMATICAL LEFT WHITE SQUARE BRACKET
       (?\u27E6 ("[|" ,@haskelly))

       ;; 27E7 ⟧ MATHEMATICAL RIGHT WHITE SQUARE BRACKET
       (?\u27E7 ("|]" ,@haskelly))

       ;; Values taken directly from `The Unicode Standard, Version 5.2' documented
       ;; in `U2900.pdf', located at http://unicode.org/charts/PDF/U2900.pdf

       ;; 2919 ⤙ LEFTWARDS ARROW-TAIL
       (?\u2919 ("-<" ,@haskelly))

       ;; 291A ⤚ RIGHTWARDS ARROW-TAIL
       (?\u291A (">-" ,@haskelly))

       ;; 291B ⤛ LEFTWARDS DOUBLE ARROW-TAIL
       (?\u291B ("-<<" ,@haskelly))

       ;; 291C ⤜ RIGHTWARDS DOUBLE ARROW-TAIL
       (?\u291C (">>-" ,@haskelly))

       ;; Values taken directly from `The Unicode Standard, Version 5.2' documented
       ;; in `U2980.PDF', located at http://unicode.org/charts/PDF/U2980.pdf

       ;; 2987 ⦇ Z NOTATION LEFT IMAGE BRACKET
       (?\u2987 ("(|" ,@haskelly))

       ;; 2988 ⦈ Z NOTATION RIGHT IMAGE BRACKET
       (?\u2988 ("|)" ,@haskelly))

       ;; 29FA ⧺ DOUBLE PLUS
       (?\u29FA ("++" ,@haskelly))

       ;; Values taken directly from `The Unicode Standard, Version 5.2' documented
       ;; in `U0370.pdf', located at http://unicode.org/charts/PDF/U0370.pdf

       ;; 0391 Α GREEK CAPITAL LETTER ALPHA
       (?\u0391 ("ALPHA" ,@all))

       ;; 0392 Β GREEK CAPITAL LETTER BETA
       (?\u0392 ("BETA" ,@all))

       ;; 0393 Γ GREEK CAPITAL LETTER GAMMA
       (?\u0393 ("GAMMA" ,@all))

       ;; 0394 Δ GREEK CAPITAL LETTER DELTA
       (?\u0394 ("DELTA" ,@all))

       ;; 0395 Ε GREEK CAPITAL LETTER EPSILON
       (?\u0395 ("EPSILON" ,@all))

       ;; 0396 Ζ GREEK CAPITAL LETTER ZETA
       (?\u0396 ("ZETA" ,@all))

       ;; 0397 Η GREEK CAPITAL LETTER ETA
       (?\u0397 ("ETA" ,@all))

       ;; 0398 Θ GREEK CAPITAL LETTER THETA
       (?\u0398 ("THETA" ,@all))

       ;; 0399 Ι GREEK CAPITAL LETTER IOTA
       (?\u0399 ("IOTA" ,@all))

       ;; 039A Κ GREEK CAPITAL LETTER KAPPA
       (?\u039A ("KAPPA" ,@all))

       ;; 039B Λ GREEK CAPITAL LETTER LAMDA
       (?\u039B ("LAMBDA" ,@all)
                ("FN" sml)
                ("FUN" tuareg))

       ;; 039C Μ GREEK CAPITAL LETTER MU
       (?\u039C ("MU" ,@all))

       ;; 039D Ν GREEK CAPITAL LETTER NU
       (?\u039D ("NU" ,@all))

       ;; 039E Ξ GREEK CAPITAL LETTER XI
       (?\u039E ("XI" ,@all))

       ;; 039F Ο GREEK CAPITAL LETTER OMICRON
       (?\u039F ("OMICRON" ,@all))

       ;; 03A0 Π GREEK CAPITAL LETTER PI
       (?\u03A0 ("PI" ,@all))

       ;; 03A1 Ρ GREEK CAPITAL LETTER RHO
       (?\u03A1 ("RHO" ,@all))

       ;; 03A3 Σ GREEK CAPITAL LETTER SIGMA
       (?\u03A3 ("SIGMA" ,@all))

       ;; 03A4 Τ GREEK CAPITAL LETTER TAU
       (?\u03A4 ("TAU" ,@all))

       ;; 03A5 Υ GREEK CAPITAL LETTER UPSILON
       (?\u03A5 ("UPSILON" ,@all))

       ;; 03A6 Φ GREEK CAPITAL LETTER PHI
       (?\u03A6 ("PHI" ,@all))

       ;; 03A7 Χ GREEK CAPITAL LETTER CHI
       (?\u03A7 ("CHI" ,@all))

       ;; 03A8 Ψ GREEK CAPITAL LETTER PSI
       (?\u03A8 ("PSI" ,@all))

       ;; 03A9 Ω GREEK CAPITAL LETTER OMEGA
       (?\u03A9 ("OMEGA" ,@all))

       ;; 03B1 α GREEK SMALL LETTER ALPHA
       (?\u03B1 ("alpha" ,@all)
                ("'a" ,@mley))

       ;; 03B2 β GREEK SMALL LETTER BETA
       (?\u03B2 ("beta" ,@all)
                ("'b" ,@mley))

       ;; 03B3 γ GREEK SMALL LETTER GAMMA
       (?\u03B3 ("gamma" ,@all)
                ("'c" ,@mley))

       ;; 03B4 δ GREEK SMALL LETTER DELTA
       (?\u03B4 ("delta" ,@all)
                ("'d" ,@mley))

       ;; 03B5 ε GREEK SMALL LETTER EPSILON
       (?\u03B5 ("epsilon" ,@all)
                ("'e" ,@mley))

       ;; 03B6 ζ GREEK SMALL LETTER ZETA
       (?\u03B6 ("zeta" ,@all))

       ;; 03B7 η GREEK SMALL LETTER ETA
       (?\u03B7 ("eta" ,@all))
       ;; Theta

       ;; 03B8 θ GREEK SMALL LETTER THETA
       (?\u03B8 ("theta" ,@all))

       ;; 03B9 ι GREEK SMALL LETTER IOTA
       (?\u03B9 ("iota" ,@all))

       ;; 03BA κ GREEK SMALL LETTER KAPPA
       (?\u03BA ("kappa" ,@all))

       ;; 03BB λ GREEK SMALL LETTER LAMDA
       (?\u03BB ("lambda" ,@all)
                ("fn" sml clojure)
                ("fun" tuareg)
                ("\\" ,@haskelly))

       ;; 03BC μ GREEK SMALL LETTER MU
       (?\u03BC ("mu" ,@all))

       ;; 03BD ν GREEK SMALL LETTER NU
       (?\u03BD ("nu" ,@all))

       ;; 03BE ξ GREEK SMALL LETTER XI
       (?\u03BE ("xi" ,@all))

       ;; 03BF ο GREEK SMALL LETTER OMICRON
       (?\u03BF ("omicron" ,@all))

       ;; 03C0 π GREEK SMALL LETTER PI
       (?\u03C0 ("pi" ,@all)
                ("M_PI" c c++))

       ;; 03C1 ρ GREEK SMALL LETTER RHO
       (?\u03C1 ("rho" ,@all))

       ;; 03C3 σ GREEK SMALL LETTER SIGMA
       (?\u03C3 ("sigma" ,@all))

       ;; 03C4 τ GREEK SMALL LETTER TAU
       (?\u03C4 ("tau" ,@all))

       ;; 03C5 υ GREEK SMALL LETTER UPSILON
       (?\u03C5 ("upsilon" ,@all))

       ;; 03C6 φ GREEK SMALL LETTER PHI
       (?\u03C6 ("phi" ,@all))

       ;; 03C7 χ GREEK SMALL LETTER CHI
       (?\u03C7 ("chi" ,@all))

       ;; 03C8 ψ GREEK SMALL LETTER PSI
       (?\u03C8 ("psi" ,@all))

       ;; 03C9 ω GREEK SMALL LETTER OMEGA
       (?\u03C9 ("omega" ,@all))

       )))
  "*List of pretty patterns.

Should be a list of the form ((MODE ((REGEXP . GLYPH) ...)) ...)")

(defun pretty-dont-use-pattern (regexp &rest languages)
  "Don't show a pretty symbol instead of REGEXP in LANGUAGES"
  (loop for language in languages do
        (let* ((mode (intern (concat (symbol-name language) "-mode")))
               (cell (assq mode pretty-patterns)))
          (setcdr cell
                  (remove* regexp (cdr cell)
                           :test 'equal
                           :key 'car)))))

(defun pretty-add-keywords (mode keywords)
  "Add pretty character KEYWORDS to MODE

MODE should be a symbol, the major mode command name, such as
`c-mode' or nil. If nil, pretty keywords are added to the current
buffer. KEYWORDS should be a list where each element has the
form (REGEXP . CHAR). REGEXP will be replaced with CHAR in the
relevant buffer(s)."
  (font-lock-add-keywords
   mode (mapcar (lambda (kw) `(,(car kw)
                          (0 (prog1 nil
                               (compose-region (match-beginning 0)
                                               (match-end 0)
                                               ,(cdr kw))))))
                keywords)))

(defun pretty-regexp (regexp glyph)
  "Replace REGEXP with GLYPH in buffer."
  (interactive "MRegexp to replace: 
MCharacter to replace with: ")
  (pretty-add-keywords nil `((,regexp . ,(string-to-char glyph))))
  (font-lock-fontify-buffer))




(provide 'pretty-mode-plus)
