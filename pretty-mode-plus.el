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

(defun ensure-list (arg)
  "Return ARG if it is a list or pack it inside one if it isn't."
  (if (listp arg)
      arg
    (list arg)))

(defun ensure-mode (mode)
  "Return MODE if it is a symbol ending in \"-mode\", or derive the
implied mode from MODE and return it."
  (let* ((name (if (stringp mode)
                   mode
                 (symbol-name mode)))
         (name (if (string= ":" (substring name 0 1))
                   (substring name 1)
                 name)))
    (intern (if (string= "mode"
                         (car (last (split-string name "-"))))
                name
              (concat name "-mode")))))

(defvar pretty-default-groups
  '(:relations :greek-capitals :logical :greek-capitals :greek-small)
  "A list of groups that should be activated by default.")

(defvar pretty-supported-modes
  '(ruby-mode
    ess-mode java-mode octave-mode tuareg-mode
    python-mode sml-mode jess-mode clojure-mode
    lisp-mode emacs-lisp-mode scheme-mode sh-mode
    perl-mode c++-mode c-mode
    literate-haskell-mode haskell-mode)
  "A list of all supported modes.")

(defun ensure-modes (modes)
  "Return a list of symbols ending in \"-mode\". If MODES is empty,
returns all modes, otherwise it calls `ensure-mode' on every member
of MODES."
  (if (null modes)
      pretty-supported-modes
    (mapcar* 'ensure-mode (ensure-list modes))))

(defvar pretty-active-groups
  (mapcar* (lambda (mode)
             (cons mode (copy-sequence pretty-default-groups)))
           pretty-supported-modes)
  "Alist mapping from a mode to a list of active groups for that
mode. An entry has the form (MODE . (GROUP1 ...)), where each
GROUP is a keyword.")

(defvar pretty-active-patterns
  (mapcar* (lambda (mode)
             (cons mode nil))
           pretty-supported-modes)
  "Alist mapping from a mode to a list of active patterns for that
mode that should be used, even though their group(s) aren't active.
An entry has the form (MODE . (PATTERN1 ...)), where each PATTERN
 is either a keyword or a string.")

(defvar pretty-deactivated-patterns
  (mapcar* (lambda (mode)
             (cons mode nil))
           pretty-supported-modes)
  "Alist mapping from a mode to a list of deactivated patterns for
that mode that should be not be used, even though their group(s) may
be active. An entry has the form (MODE . (PATTERN1 ...)), where each
PATTERN is either a keyword or a string.")

(defun pretty-is-active-pattern (regexp mode names groups)
  "Checks whether a given pattern is currently active according to the
pretty-active-patterns/groups and pretty-deactivated-patterns variables."
  (let ((agroups (assoc mode pretty-active-groups))
        (apatterns (assoc mode pretty-active-patterns))
        (dpatterns (assoc mode pretty-deactivated-patterns))
        (patterns (cons regexp names)))
    (or (intersection patterns apatterns)
       (and (subsetp groups agroups)
          (not (intersection patterns dpatterns))))
    ))

(defun pretty-activate-groups (modes groups)
  "Add GROUPS to each entry in `pretty-active-groups' for every entry
in MODES. If MODES is empty, assumes that all modes should be affected."
  (let ((modes (ensure-modes modes))
        (groups (ensure-list groups)))
    (loop for mode in modes do
          (let ((cell (assq mode pretty-active-groups)))
            (setcdr cell (union (cdr cell) groups))))))

(defun pretty-deactivate-groups (modes groups)
  "Remove all members of GROUPS from every entry in
`pretty-active-groups' associated with each entry in MODES. If MODES is
empty, assumes that all modes should be affected."
  (let ((modes (ensure-modes modes))
        (groups (ensure-list groups)))
    (loop for mode in modes do
          (let ((cell (assq mode pretty-active-groups)))
            (setcdr cell (set-difference (cdr cell) groups))))))

(defun pretty-activate-patterns (modes patterns)
  "Add PATTERNS to each entry in `pretty-active-patterns' for every entry
in MODES and remove them from `pretty-deactivated-patterns'. If MODES is
empty, assumes that all modes should be affected."
  (let ((modes (ensure-modes modes))
        (patterns (ensure-list patterns)))
    (loop for mode in modes do
          (let ((cell (assq mode pretty-active-patterns)))
            (setcdr cell (union (cdr cell) patterns)))
          (let ((cell (assq mode pretty-deactivated-patterns)))
            (setcdr cell (set-difference (cdr cell) patterns))))))

(defun pretty-deactivate-patterns (modes patterns)
  "Remove all members of PATTERNS from every entry in
`pretty-active-patterns' associated with each entry in MODES and add them
to `pretty-deactivated-patterns'. If MODES is empty, assumes that all
modes should be affected."
  (let ((modes (ensure-modes modes))
        (patterns (ensure-list patterns)))
    (loop for mode in modes do
          (let ((cell (assq mode pretty-active-patterns)))
            (setcdr cell (set-difference (cdr cell) patterns)))
          (let ((cell (assq mode pretty-deactivated-patterns)))
            (setcdr cell (union (cdr cell) patterns))))))

(defun pretty-keywords (&optional mode)
  "Return the font-lock keywords for MODE, or the current mode if
MODE is nil. Return nil if there are no keywords."
  (let* ((mode (or mode major-mode))
         (kwds (cdr-safe
                (or (assoc mode (pretty-patterns))
                   (assoc (cdr-safe
                           (assoc mode pretty-interaction-mode-alist))
                          (pretty-patterns))))))
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

PATTERNS should be of the form ((GLYPH NAMES GROUPS (REGEXP MODE ...) ...)
...). GLYPH should be a character. NAMES and GROUPS should both be lists of keywords,
MODE should be the name of a
major mode without the \"-mode\". Returns patterns in the form
expected by `pretty-patterns'"
  (let ((pretty-patterns))
    (loop for (glyph names groups . pairs) in patterns do
          (loop for (regexp . major-modes) in pairs do
                (loop for mode in major-modes do
                      (let* ((mode (ensure-mode mode))
                             (assoc-pair (assoc mode pretty-patterns))
                             (entry (cons regexp glyph)))
                        (when (pretty-is-active-pattern regexp mode
                                                        names groups)
                          (if assoc-pair
                              (push entry (cdr assoc-pair))
                            (push (cons mode (list entry))
                                  pretty-patterns)))))))
    pretty-patterns))

(defun pretty-patterns ()
    "*List of pretty patterns.

Should be a list of the form ((MODE ((REGEXP . GLYPH) ...)) ...)"
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
       (?\u00AB (:ll) (:relations)
                ("<<" ,@haskelly))

       ;; 00AC ¬ NOT SIGN
       (?\u00AC (:neg) (:logical)
                ("!" c c++ perl sh)
                ("not" ,@lispy ,@haskelly sml))

       ;; 00B2 ² SUPERSCRIPT TWO
       (?\u00B2 (:sup-2) (:superscripts)
                ("**2" python tuareg octave)
                ("^2" ,@haskelly))

       ;; 00B3 ³ SUPERSCRIPT THREE
       (?\u00B3 (:sup-3) (:superscripts)
                ("**3" python tuareg octave)
                ("^3" ,@haskelly))

       ;; 00BB » RIGHT-POINTING DOUBLE ANGLE QUOTATION MARK
       (?\u00BB (:gg) (:relations)
                (">>" ,@haskelly))

       ;; Values taken directly from `The Unicode Standard, Version 5.2' documented
       ;; in `U0370.pdf', located at http://unicode.org/charts/PDF/U0370.pdf

       ;; 0391 Α GREEK CAPITAL LETTER ALPHA
       (?\u0391 (:Alpha) (:greek-capitals)
                ("ALPHA" ,@all))

       ;; 0392 Β GREEK CAPITAL LETTER BETA
       (?\u0392 (:Beta) (:greek-capitals)
                ("BETA" ,@all))

       ;; 0393 Γ GREEK CAPITAL LETTER GAMMA
       (?\u0393 (:Gamma)  (:greek-capitals)
                ("GAMMA" ,@all))

       ;; 0394 Δ GREEK CAPITAL LETTER DELTA
       (?\u0394 (:Delta)  (:greek-capitals)
                ("DELTA" ,@all))

       ;; 0395 Ε GREEK CAPITAL LETTER EPSILON
       (?\u0395 (:Epsilon) (:greek-capitals)
                ("EPSILON" ,@all))

       ;; 0396 Ζ GREEK CAPITAL LETTER ZETA
       (?\u0396 (:Zeta) (:greek-capitals)
                ("ZETA" ,@all))

       ;; 0397 Η GREEK CAPITAL LETTER ETA
       (?\u0397 (:Eta) (:greek-capitals)
                ("ETA" ,@all))

       ;; 0398 Θ GREEK CAPITAL LETTER THETA
       (?\u0398 (:Theta) (:greek-capitals)
                ("THETA" ,@all))

       ;; 0399 Ι GREEK CAPITAL LETTER IOTA
       (?\u0399 (:Iota) (:greek-capitals)
                ("IOTA" ,@all))

       ;; 039A Κ GREEK CAPITAL LETTER KAPPA
       (?\u039A (:Kapp) (:greek-capitals)
                ("KAPPA" ,@all))

       ;; 039B Λ GREEK CAPITAL LETTER LAMDA
       (?\u039B (:Lambda) (:greek-capitals)
                ("LAMBDA" ,@all)
                ("FN" sml)
                ("FUN" tuareg))

       ;; 039C Μ GREEK CAPITAL LETTER MU
       (?\u039C (:Mu) (:greek-capitals)
                ("MU" ,@all))

       ;; 039D Ν GREEK CAPITAL LETTER NU
       (?\u039D (:Nu) (:greek-capitals)
                ("NU" ,@all))

       ;; 039E Ξ GREEK CAPITAL LETTER XI
       (?\u039E (:Xi) (:greek-capitals)
                ("XI" ,@all))

       ;; 039F Ο GREEK CAPITAL LETTER OMICRON
       (?\u039F (:Omicron) (:greek-capitals)
                ("OMICRON" ,@all))

       ;; 03A0 Π GREEK CAPITAL LETTER PI
       (?\u03A0 (:Pi) (:greek-capitals)
                ("PI" ,@all))

       ;; 03A1 Ρ GREEK CAPITAL LETTER RHO
       (?\u03A1 (:Rho) (:greek-capitals)
                ("RHO" ,@all))

       ;; 03A3 Σ GREEK CAPITAL LETTER SIGMA
       (?\u03A3 (:Sigma) (:greek-capitals)
                ("SIGMA" ,@all))

       ;; 03A4 Τ GREEK CAPITAL LETTER TAU
       (?\u03A4 (:Tau) (:greek-capitals)
                ("TAU" ,@all))

       ;; 03A5 Υ GREEK CAPITAL LETTER UPSILON
       (?\u03A5 (:Upsilon) (:greek-capitals)
                ("UPSILON" ,@all))

       ;; 03A6 Φ GREEK CAPITAL LETTER PHI
       (?\u03A6 (:Phi) (:greek-capitals)
                ("PHI" ,@all))

       ;; 03A7 Χ GREEK CAPITAL LETTER CHI
       (?\u03A7 (:Chi) (:greek-capitals)
                ("CHI" ,@all))

       ;; 03A8 Ψ GREEK CAPITAL LETTER PSI
       (?\u03A8 (:Psi) (:greek-capitals)
                ("PSI" ,@all))

       ;; 03A9 Ω GREEK CAPITAL LETTER OMEGA
       (?\u03A9 (:Omega) (:greek-capitals)
                ("OMEGA" ,@all))

       ;; 03B1 α GREEK SMALL LETTER ALPHA
       (?\u03B1 (:alpha) (:greek-small)
                ("alpha" ,@all)
                ("'a" ,@mley))

       ;; 03B2 β GREEK SMALL LETTER BETA
       (?\u03B2 (:beta) (:greek-small)
                ("beta" ,@all)
                ("'b" ,@mley))

       ;; 03B3 γ GREEK SMALL LETTER GAMMA
       (?\u03B3 (:gamma) (:greek-small)
                ("gamma" ,@all)
                ("'c" ,@mley))

       ;; 03B4 δ GREEK SMALL LETTER DELTA
       (?\u03B4 (:delta) (:greek-small)
                ("delta" ,@all)
                ("'d" ,@mley))

       ;; 03B5 ε GREEK SMALL LETTER EPSILON
       (?\u03B5 (:epsilon) (:greek-small)
                ("epsilon" ,@all)
                ("'e" ,@mley))

       ;; 03B6 ζ GREEK SMALL LETTER ZETA
       (?\u03B6 (:zeta) (:greek-small)
                ("zeta" ,@all))

       ;; 03B7 η GREEK SMALL LETTER ETA
       (?\u03B7 (:eta) (:greek-small)
                ("eta" ,@all))

       ;; 03B8 θ GREEK SMALL LETTER THETA
       (?\u03B8 (:theta) (:greek-small)
                ("theta" ,@all))

       ;; 03B9 ι GREEK SMALL LETTER IOTA
       (?\u03B9 (:iota) (:greek-small)
                ("iota" ,@all))

       ;; 03BA κ GREEK SMALL LETTER KAPPA
       (?\u03BA (:kappa) (:greek-small)
                ("kappa" ,@all))

       ;; 03BB λ GREEK SMALL LETTER LAMDA
       (?\u03BB (:lambda) (:greek-small)
                ("lambda" ,@all)
                ("fn" sml clojure)
                ("fun" tuareg)
                ("\\" ,@haskelly))

       ;; 03BC μ GREEK SMALL LETTER MU
       (?\u03BC (:mu) (:greek-small)
                ("mu" ,@all))

       ;; 03BD ν GREEK SMALL LETTER NU
       (?\u03BD (:nu) (:greek-small)
                ("nu" ,@all))

       ;; 03BE ξ GREEK SMALL LETTER XI
       (?\u03BE (:xi) (:greek-small)
                ("xi" ,@all))

       ;; 03BF ο GREEK SMALL LETTER OMICRON
       (?\u03BF (:omicron) (:greek-small)
                ("omicron" ,@all))

       ;; 03C0 π GREEK SMALL LETTER PI
       (?\u03C0 (:pi) (:greek-small)
                ("pi" ,@all)
                ("M_PI" c c++))

       ;; 03C1 ρ GREEK SMALL LETTER RHO
       (?\u03C1 (:rho) (:greek-small)
                ("rho" ,@all))

       ;; 03C3 σ GREEK SMALL LETTER SIGMA
       (?\u03C3 (:sigma) (:greek-small)
                ("sigma" ,@all))

       ;; 03C4 τ GREEK SMALL LETTER TAU
       (?\u03C4 (:tau) (:greek-small)
                ("tau" ,@all))

       ;; 03C5 υ GREEK SMALL LETTER UPSILON
       (?\u03C5 (:upsilon) (:greek-small)
                ("upsilon" ,@all))

       ;; 03C6 φ GREEK SMALL LETTER PHI
       (?\u03C6 (:phi) (:greek-small)
                ("phi" ,@all))

       ;; 03C7 χ GREEK SMALL LETTER CHI
       (?\u03C7 (:chi) (:greek-small)
                ("chi" ,@all))

       ;; 03C8 ψ GREEK SMALL LETTER PSI
       (?\u03C8 (:psi) (:greek-small)
                ("psi" ,@all))

       ;; 03C9 ω GREEK SMALL LETTER OMEGA
       (?\u03C9 (:omega) (:greek-small)
                ("omega" ,@all))

       ;; Values taken directly from `The Unicode Standard, Version 5.2' documented
       ;; in `U2000.pdf', located at http://unicode.org/charts/PDF/U2000.pdf

       ;; 2025 ‥ TWO DOT LEADER
       (?\u2025 (:dots-2) (:punctuation)
                (".." ,@haskelly ruby))

       ;; 2026 … HORIZONTAL ELLIPSIS
       (?\u2026 (:dots) (:punctuation)
                ("..." scheme))

       ;; 203C ‼ DOUBLE EXCLAMATION MARK
       (?\u203C () (:punctuation)
                ("!!" ,@haskelly))

       ;; Values taken directly from `The Unicode Standard, Version 5.2' documented
       ;; in `U2070.pdf', located at http://unicode.org/charts/PDF/U2070.pdf

       ;; 207F ⁿ SUPERSCRIPT LATIN SMALL LETTER N
       (?\u207F (:sup-n) (:superscripts)
                ("**n" python tuareg octave)
                ("^n" ,@haskelly))

       ;; 2080 ₀ SUBSCRIPT ZERO
       (?\u2080 (:sub-0) (:subscripts)
                ("[0]" ,@c-like)
                ("(0)" octave)
                (".(0)" tuareg)
                ("!!0" ,@haskelly))

       ;; 2081 ₁ SUBSCRIPT ONE
       (?\u2081 (:sub-1) (:subscripts)
                ("[1]" ,@c-like)
                ("(1)" octave)
                (".(1)" tuareg)
                ("!!1" ,@haskelly))

       ;; 2082 ₂ SUBSCRIPT TWO
       (?\u2082 (:sub-2) (:subscripts)
                ("[2]" ,@c-like)
                ("(2)" octave)
                (".(2)" tuareg)
                ("!!2" ,@haskelly))

       ;; 2083 ₃ SUBSCRIPT THREE
       (?\u2083 (:sub-3)  (:subscripts)
                ("[3]" ,@c-like)
                ("(3)" octave)
                (".(3)" tuareg)
                ("!!3" ,@haskelly))

       ;; 2084 ₄ SUBSCRIPT FOUR
       (?\u2084 (:sub-4)  (:subscripts)
                ("[4]" ,@c-like)
                ("(4)" octave)
                (".(4)" tuareg)
                ("!!4" ,@haskelly))

       ;; Values taken directly from `The Unicode Standard, Version 5.2' documented
       ;; in `U2100.pdf', located at http://unicode.org/charts/PDF/U2100.pdf

       ;; 2124 ℤ DOUBLE-STRUCK CAPITAL Z
       (?\u2124 (:Z) (:sets)
                ("Integer" ,@haskelly))

       ;; Values taken directly from `The Unicode Standard, Version 5.2' documented
       ;; in `U2190.pdf', located at http://unicode.org/charts/PDF/U2190.pdf

       ;; 2190 ← LEFTWARDS ARROW
       (?\u2190 (:leftarrow) (:arrows)
                ("<-" ,@mley ess ,@lispy))

       ;; 2191 ↑ UPWARDS ARROW
       (?\u2191 (:uparrow) (:arrows)
                ("\\^" tuareg))

       ;; 2192 → RIGHTWARDS ARROW
       (?\u2192 (:rightarrow) (:arrows)
                ("->" ,@mley ess c c++ perl ,@lispy))

       ;; 21A0 ↠ RIGHTWARDS TWO HEADED ARROW
       (?\u21A0 (:twoheadrightarrow) (:arrows)
                ("->>" ,@lispy))

       ;; 21D2 ⇒ RIGHTWARDS DOUBLE ARROW
       (?\u21D2 (:Rightarrow) (:arrows)
                ("=>" sml perl ruby ,@lispy ,@haskelly))

       ;; 21F9 ⇹ LEFT RIGHT ARROW WITH VERTICAL STROKE
       (?\u21F9 (:nleftrightarrow) (:arrows)
                ("<|>" ,@haskelly))

       ;; Values taken directly from `The Unicode Standard, Version 5.2' documented
       ;; in `U2200.pdf', located at http://unicode.org/charts/PDF/U2200.pdf

       ;; 2200 ∀ FOR ALL
       (?\u2200 (:forall) (:quantifiers)
                ("forall" ,@haskelly))

       ;; 2203 ∃ THERE EXISTS
       (?\u2203 (:exists) (:quantifiers)
                ("exists" ,@haskelly))

       ;; 2205 ∅ EMPTY SET
       (?\u2205 (:emptyset) (:sets)
                ("nil" emacs-lisp ruby clojure)
                ("null" scheme java)
                ("'()" scheme)
                ("empty" scheme)
                ("NULL" c c++)
                ;; ("None" python)
                ("()" ,@mley)
                ("[]" ,@mley))

       ;; 2208 ∈ ELEMENT OF
       (?\u2208 (:in) (:relations)
                ("`elem`" ,@haskelly)
                ("in" python))

       ;; 2209 ∉ NOT AN ELEMENT OF
       (?\u2209 (:notin) (:relations)
                ("`notElem`" ,@haskelly)
                ("not in" python))

       ;; 220F ∏ N-ARY PRODUCT
       (?\u220F (:prod) (:nary)
                ("product" ,@haskelly))

       ;; 2211 Σ N-ARY SUMMATION
       (?\u2211 (:sum) (:nary)
                ("sum" python ,@haskelly))

       ;; 221a √ SQUARE ROOT
       (?\u221A (:sqrt) (:arithmetic)
                ("sqrt" ,@all))

       ;; 2227 ∧ LOGICAL AND
       (?\u2227 (:wedge) ()
                ("and" ,@lispy python ruby)
                ("andalso" sml)
                ("&&" c c++ perl ,@haskelly ruby))

       ;; 2228 ∨ LOGICAL OR
       (?\u2228 (:vee) ()
                ("or" ,@lispy python ruby)
                ("orelse" sml)
                ("||" c c++ perl ,@haskelly ruby))

       ;; 2229 ∩ INTERSECTION
       (?\u2229 (:cap) (:relations)
                ("`intersect`" ,@haskelly)     ; Data.List
                ("`intersection`" ,@haskelly)) ; Data.Set

       ;; 222A ∪ UNION
       (?\u222A (:cup) (:relations)
                ("`union`" ,@haskelly)) ; Data.List, Data.Set

       ;; 2237 ∷ PROPORTION
       (?\u2237 (:Proportion) (:punctuation)
                ("::" ,@haskelly))

       ;; 2260 ≠ NOT EQUAL TO
       (?\u2260 (:neq) (:relations)
                ("!=" ,@c-like scheme octave)
                ("not=" clojure)
                ("<>" tuareg octave)
                ("~=" octave)
                ("/=" ,@haskelly))

       ;; 2261 ≡ IDENTICAL TO
       (?\u2261 (:equiv) (:relations)
                ("==" ,@c-like ,@haskelly))

       ;; 2264 ≤ LESS-THAN OR EQUAL TO
       (?\u2264 (:leq) (:relations)
                ("<=" ,@all))

       ;; 2265 ≥ GREATER-THAN OR EQUAL TO
       (?\u2265 (:geq) (:relations)
                (">=" ,@all))

       ;; 2282 ⊂ SUBSET OF
       (?\u2282 (:subset) (:relations)
                ("`isProperSubsetOf`" ,@haskelly)) ; Data.Set

       ;; 2286 ⊆ SUBSET OF OR EQUAL TO
       (?\u2286 (:subseteq) (:relations)
                ("`isSubsetOf`" ,@haskelly)) ; Data.Set

       ;; 22A5 ⊥ UP TACK
       (?\u22A5 (:bot) ()
                ("undefined" ,@haskelly))

       ;; 22C0 ⋀ N-ARY LOGICAL AND
       (?\u22C0 (:bigwedge) (:nary)
                ("and" ,@haskelly))

       ;; 22C1 ⋁ N-ARY LOGICAL OR
       (?\u22C1 (:bigvee) (:nary)
                ("or" ,@haskelly))

       ;; 22C3 ⋃ N-ARY UNION
       (?\u22C3 (:bigcup) (:nary)
                ("unions" ,@haskelly))  ; Data.Set

       ;; 22C5 ⋅ DOT OPERATOR
       (?\u22C5 (:.) (:punctuation)
                ("\." ,@haskelly))

       ;; 22D8 ⋘ VERY MUCH LESS-THAN
       (?\u22D8 (:lll) (:relations)
                ("<<<" ,@haskelly))     ; Control.Arrow

       ;; 22D9 ⋙ VERY MUCH GREATER-THAN
       (?\u22D9 (:rrr) (:relations)
                (">>>" ,@haskelly))     ; Control.Arrow

       ;; Values taken directly from `The Unicode Standard, Version 5.2' documented
       ;; in `U27C0.pdf', located at http://unicode.org/charts/PDF/U2900.pdf

       ;; 27E6 ⟦ MATHEMATICAL LEFT WHITE SQUARE BRACKET
       (?\u27E6 (:llbracket) (:parentheses)
                ("[|" ,@haskelly))

       ;; 27E7 ⟧ MATHEMATICAL RIGHT WHITE SQUARE BRACKET
       (?\u27E7 (:rrbracket) (:parentheses)
                ("|]" ,@haskelly))

       ;; Values taken directly from `The Unicode Standard, Version 5.2' documented
       ;; in `U2900.pdf', located at http://unicode.org/charts/PDF/U2900.pdf

       ;; 2919 ⤙ LEFTWARDS ARROW-TAIL
       (?\u2919 () (:arrows)
                ("-<" ,@haskelly))

       ;; 291A ⤚ RIGHTWARDS ARROW-TAIL
       (?\u291A () (:arrows)
                (">-" ,@haskelly))

       ;; 291B ⤛ LEFTWARDS DOUBLE ARROW-TAIL
       (?\u291B () (:arrows)
                ("-<<" ,@haskelly))

       ;; 291C ⤜ RIGHTWARDS DOUBLE ARROW-TAIL
       (?\u291C () (:arrows)
                (">>-" ,@haskelly))

       ;; Values taken directly from `The Unicode Standard, Version 5.2' documented
       ;; in `U2980.PDF', located at http://unicode.org/charts/PDF/U2980.pdf

       ;; 2987 ⦇ Z NOTATION LEFT IMAGE BRACKET
       (?\u2987 (:limg :Lparen) (:parentheses)
                ("(|" ,@haskelly))

       ;; 2988 ⦈ Z NOTATION RIGHT IMAGE BRACKET
       (?\u2988 (:rimg :Rparen) (:parentheses)
                ("|)" ,@haskelly))

       ;; 29F5 ⧵ REVERSE SOLIDUS OPERATOR
       (?\u29F5 (:setminus) (:relations)
                ("\\\\" ,@haskelly))

       ;; 29FA ⧺ DOUBLE PLUS
       (?\u29FA () ()
                ("++" ,@haskelly))

       ;; 29FB ⧻ TRIPLE PLUS
       (?\u29FB () ()
                ("+++" ,@haskelly))     ; Control.Arrow

       ;; Values taken directly from `The Unicode Standard, Version 5.2' documented
       ;; in `U2980.PDF', located at http://unicode.org/charts/PDF/U2980.pdf

       ;; 2AF4 ⫴ TRIPLE VERTICAL BAR BINARY RELATION
       (?\u2AF4 (:VERT) ()
                ("|||" ,@haskelly))     ; Control.Arrow

       ))))

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
