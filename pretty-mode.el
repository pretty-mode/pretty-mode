;;; pretty-mode.el --- Redisplay parts of the buffer as pretty symbols.
;;; -*- coding: utf-8 -*-

;; Copyright © March 2008 Arthur Danskin <arthurdanskin@gmail.com>
;; Copyright © January 2013 Grant Rettke <grettke@acm.org>
;; Copyright © April 2013 Dmitri Akatov <akatov@gmail.com>

;; Filename: pretty-mode.el
;; Description: Redisplay parts of the buffer as pretty symbols.
;; Author: Arthur Danskin <arthurdanskin@gmail.com>
;; Maintainer: Dmitri Akatov <akatov@gmail.com>
;; URL: https://github.com/akatov/pretty-mode
;; Keywords: pretty, unicode, symbols
;; Version: 2.0.2

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:
;;
;; Minor mode for redisplaying parts of the buffer as pretty symbols
;; originally modified from Trent Buck's version at http://paste.lisp.org/display/42335,2/raw
;; Also includes code from `sml-mode'
;; See also http://www.emacswiki.org/cgi-bin/wiki/PrettyLambda
;;
;; to install:
;;
;; (require 'pretty-mode)
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

(defun pretty-font-lock-keywords (alist)
  "Return a `font-lock-keywords' style entry for replacing
regular expressions with symbols. ALIST has the form ((STRING .
REPLACE-CHAR) ...)."
  (when alist
    `((,(regexp-opt (mapcar 'car alist))
       (0 (pretty-font-lock-compose-symbol
           ',alist))))))

(defvar pm/mode-aliases
  '((inferior-scheme-mode . scheme-mode)
    (lisp-interaction-mode . emacs-lisp-mode)
    (inferior-lisp-mode . lisp-mode)
    (inferior-ess-mode . ess-mode)
    (literate-haskell-mode . haskell-mode)
    (inf-haskell-mode . haskell-mode) ; is this still used??
    (inferior-haskell-mode . haskell-mode)
    (nrepl-mode . clojure-mode)
    (tuareg-interactive-mode . tuareg-mode)
    (inferior-python-mode . python-mode)
    (inferior-octave-mode . octave-mode)
    (js-mode . javascript-mode)
    (js2-mode . javascript-mode)
    (inferior-ruby-mode . ruby-mode))
  "Alist mapping from modes that should have the same substitution
patterns as to the mode they are mapping to. Usually these are
inferior process interaction modes corresponding to their main
script editing modes.")

(defun pm/ensure-list (arg)
  "Return ARG if it is a list or pack it inside one if it isn't."
  (if (listp arg)
      arg
    (list arg)))

(defun pm/ensure-mode (mode)
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

(defconst pm/default-groups
  '(:function
    :greek-capitals :greek-lowercase
    ;; turn on :greek manually
    :equality
    :ordering :ordering-double :ordering-triple
    :logic
    ;; turn on :logic-nary manually
    :nil
    :sets :sets-operations :sets-relations
    ;; turn on :sets-operations-nary manually
    :arrows :arrows-twoheaded
    ;; turn on :arrows-tails and :arrows-tails-double manually
    :arithmetic :arithmetic-double
    ;; turn on :arithmetic-triple and :arithmetic-nary manually
    :punctuation
    :subscripts :superscripts
    ;; turn on :sub-and-superscripts manually
    ;; turn on :parentheses manually
    ;; turn on :types manually
    )
  "A list of groups that should be activated by default.")

(defvar pm/supported-modes
  '(ruby-mode
    ess-mode java-mode octave-mode tuareg-mode
    python-mode sml-mode jess-mode clips-mode clojure-mode
    lisp-mode emacs-lisp-mode scheme-mode sh-mode
    perl-mode c++-mode c-mode haskell-mode
    javascript-mode coffee-mode groovy-mode)
  "A list of all supported modes.")

(defun pm/ensure-modes (modes)
  "Return a list of symbols ending in \"-mode\". If MODES is empty,
returns all modes, otherwise it calls `ensure-mode' on every member
of MODES."
  (if (null modes)
      pm/supported-modes
    (mapcar* 'pm/ensure-mode (pm/ensure-list modes))))

(defvar pm/active-groups
  nil
  "Alist mapping from a mode to a list of active groups for that
mode. An entry has the form (MODE . (GROUP1 ...)), where each
GROUP is a keyword.")

;; (defvar pm/active-patterns
;;   nil
;;   "Alist mapping from a mode to a list of active patterns for that
;; mode that should be used, even though their group(s) aren't active.
;; An entry has the form (MODE . (PATTERN1 ...)), where each PATTERN
;;  is either a keyword or a string.")

;; (defvar pm/deactivated-patterns
;;   nil
;;   "Alist mapping from a mode to a list of deactivated patterns for
;; that mode that should be not be used, even though their group(s) may
;; be active. An entry has the form (MODE . (PATTERN1 ...)), where each
;; PATTERN is either a keyword or a string.")

(defun pretty-defaults ()
  (setq pm/active-groups
        (mapcar* (lambda (mode)
                   (cons mode (copy-sequence pm/default-groups)))
                 pm/supported-modes))
  ;; (setq pm/active-patterns
  ;;       (mapcar* (lambda (mode)
  ;;                  (cons mode nil))
  ;;                pm/supported-modes))
  ;; (setq pm/deactivated-patterns
  ;;       (mapcar* (lambda (mode)
  ;;                  (cons mode nil))
  ;;                pm/supported-modes))
  )

(pretty-defaults)

(defun pretty-is-active-pattern (symbol-name groups name mode)
  "Checks whether a given pattern is currently active according to the
pretty-active-patterns/groups and pretty-deactivated-patterns variables."
  (let ((active-groups     (cdr (assoc mode pretty-active-groups)))
        (active-patterns   (cdr (assoc mode pretty-active-patterns)))
        (inactive-patterns (cdr (assoc mode pretty-deactivated-patterns)))
        (patterns          (list symbol-name name)))
    (or (intersection patterns active-patterns)
       (and (subsetp groups active-groups)
          (not (intersection patterns inactive-patterns))))))

(defun pretty-activate-groups (groups &optional modes)
  "Add GROUPS to each entry in `pretty-active-groups' for every entry
in MODES. If MODES is empty, assumes that all modes should be affected."
  (let ((modes (ensure-modes modes))
        (groups (ensure-list groups)))
    (loop for mode in modes do
          (let ((cell (assq mode pretty-active-groups)))
            (setcdr cell (union (cdr cell) groups))))))

(defun pretty-deactivate-groups (groups &optional modes)
  "Remove all members of GROUPS from every entry in
`pretty-active-groups' associated with each entry in MODES. If MODES is
empty, assumes that all modes should be affected."
  (let ((modes (ensure-modes modes))
        (groups (ensure-list groups)))
    (loop for mode in modes do
          (let ((cell (assq mode pretty-active-groups)))
            (setcdr cell (set-difference (cdr cell) groups))))))

(defun pretty-activate-patterns (patterns &optional modes)
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

(defun pretty-deactivate-patterns (patterns &optional modes)
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
                           (assoc mode pretty-modes-aliases))
                          (pretty-patterns))))))
    (pretty-font-lock-keywords kwds)))

(defgroup pretty nil "Minor mode for replacing text with symbols "
  :group 'faces)

;;;###autoload
(define-minor-mode pretty-mode
  "Toggle Pretty minor mode.
With arg, turn Pretty minor mode on if arg is positive, off otherwise.

Pretty mode builds on `font-lock-mode'. Instead of highlighting
keywords, it replaces them with symbols. For example, lambda is
displayed as λ in lisp modes."
  :group 'pretty
  :lighter " λ"
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

;;;###autoload
(define-globalized-minor-mode global-pretty-mode
  pretty-mode turn-on-pretty-if-desired
  :init-value t)

;;;###autoload
(defun turn-off-pretty-mode ()
  (interactive)
  (pretty-mode -1))

;;;###autoload
(defun turn-on-pretty-mode ()
  (interactive)
  (pretty-mode +1))

(defvar pm/patterns
  ;; Values taken directly from `The Unicode Standard, Version 5.2' documented
  ;; in `U0080.pdf', located at http://unicode.org/charts/PDF/U0080.pdf
  ;; in `U0370.pdf', located at http://unicode.org/charts/PDF/U0370.pdf
  ;; in `U2000.pdf', located at http://unicode.org/charts/PDF/U2000.pdf
  ;; in `U2070.pdf', located at http://unicode.org/charts/PDF/U2070.pdf
  ;; in `U2100.pdf', located at http://unicode.org/charts/PDF/U2100.pdf
  ;; in `U2190.pdf', located at http://unicode.org/charts/PDF/U2190.pdf
  ;; in `U2200.pdf', located at http://unicode.org/charts/PDF/U2200.pdf
  ;; in `U27C0.pdf', located at http://unicode.org/charts/PDF/U2900.pdf
  ;; in `U2900.pdf', located at http://unicode.org/charts/PDF/U2900.pdf
  ;; in `U2980.PDF', located at http://unicode.org/charts/PDF/U2980.pdf
  '(;; ordering
    (:<< "<<" ?\u00AB)           ; « LEFT-POINTING DOUBLE ANGLE QUOTATION MARK
    (:>> ">>" ?\u00BB)           ; » RIGHT-POINTING DOUBLE ANGLE QUOTATION MARK
    (:<= "<=" ?\u2264)           ; ≤ LESS-THAN OR EQUAL TO
    (:>= ">=" ?\u2265)           ; ≥ GREATER-THAN OR EQUAL TO
    (:<<< "<<<" ?\u22D8)         ; ⋘ VERY MUCH LESS-THAN
    (:>>> ">>>" ?\u22D9)         ; ⋙ VERY MUCH GREATER-THAN

    ;; equality
    (:!= "!=" ?\u2260)           ; ≠ NOT EQUAL TO
    (:not= "not=" ?\u2247)       ; ≠ NOT EQUAL TO
    (:<> "<>" ?\u2247)           ; ≠ NOT EQUAL TO
    (:~= "~=" ?\u2247)           ; ≠ NOT EQUAL TO
    (:== "==" ?\u2A75)           ; ⩵ TWO CONSECUTIVE EQUALS SIGNS
    (:=== "===" ?\u2A76)         ; ⩶ THREE CONSECUTIVE EQUALS SIGNS
    (:=~ "=~" ?\u2245)           ; ≅ APPROXIMATELY EQUAL TO
    (:!~ "!~" ?\u2247)           ; ≇ NEITHER APPROXIMATELY NOR ACTUALLY EQUAL TO

    ;; logic
    (:! "!" ?\u00AC)              ; ¬ NOT SIGN
    (:not "not" ?\u00AC)          ; ¬ NOT SIGN
    (:&& "&&" ?\u2227)            ; ∧ LOGICAL AND
    (:and "and" ?\u2227)          ; ∧ LOGICAL AND
    (:andalso "andlso" ?\u2227)          ; ∧ LOGICAL AND
    (:|| "||" ?\u2228)            ; ∨ LOGICAL OR
    (:or "or" ?\u2228)            ; ∨ LOGICAL OR
    (:||= "||=" ?\u22AB)          ; ⊫ DOUBLE VERTICAL BAR DOUBLE RIGHT TURNSTILE
    (:and-nary "and" ?\u22C0)     ; ⋀ N-ARY LOGICAL AND
    (:or-nary "or" ?\u22C1)       ; ⋁ N-ARY LOGICAL OR

    ;; sets
    (:in "in" ?\u2208)                  ; ∈ ELEMENT OF
    (:elem "`elem`" ?\u2208)            ; ∈ ELEMENT OF
    (:notElem "`notElem`" ?\u2209)      ; ∉ NOT AN ELEMENT OF
    (:not-in "not in" ?\u2209)          ; ∉ NOT AN ELEMENT OF
    (:intersect "`intersect`" ?\u2229)  ; ∩ INTERSECTION
    (:intersection "`intersection`" ?\u2229) ; ∩ INTERSECTION
    (:union "`union`" ?\u222A)               ; ∪ UNION
    (:isProperSubsetOf "`isProperSubsetOf`" ?\u2282) ; ⊂ SUBSET OF
    (:isSubsetOf "`isSubsetOf`" ?\u2286)             ; ⊆ SUBSET OF OR EQUAL TO
    (:unions "unions" ?\u22C3)                       ; ⋃ N-ARY UNION
    (:\\\\ "\\\\" ?\u29F5)              ; ⧵ REVERSE SOLIDUS OPERATOR

    ;; subscripts
    (:!!0 "!!0" ?\u2080)                ; ₀ SUBSCRIPT ZERO
    (:\[0\] "[0]" ?\u2080)              ; ₀ SUBSCRIPT ZERO
    (:\(0\) "(0)" ?\u2080)              ; ₀ SUBSCRIPT ZERO
    (:.\(0\) ".(0)" ?\u2080)            ; ₀ SUBSCRIPT ZERO
    (:!!1 "!!1" ?\u2081)                ; ₁ SUBSCRIPT ONE
    (:\[1\] "[1]" ?\u2081)              ; ₁ SUBSCRIPT ONE
    (:\(1\) "(1)" ?\u2081)              ; ₁ SUBSCRIPT ONE
    (:.\(1\) ".(1)" ?\u2081)            ; ₁ SUBSCRIPT ONE
    (:!!2 "!!2" ?\u2082)                ; ₂ SUBSCRIPT TWO
    (:\[2\] "[2]" ?\u2082)              ; ₂ SUBSCRIPT TWO
    (:\(2\) "(2)" ?\u2082)              ; ₂ SUBSCRIPT TWO
    (:.\(2\) ".(2)" ?\u2082)            ; ₂ SUBSCRIPT TWO
    (:!!3 "!!3" ?\u2083)                ; ₃ SUBSCRIPT THREE
    (:\[3\] "[3]" ?\u2083)              ; ₃ SUBSCRIPT THREE
    (:\(3\) "(3)" ?\u2083)              ; ₃ SUBSCRIPT THREE
    (:.\(3\) ".(3)" ?\u2083)            ; ₃ SUBSCRIPT THREE
    (:!!4 "!!4" ?\u2084)                ; ₄ SUBSCRIPT FOUR
    (:\[4\] "[4]" ?\u2084)              ; ₄ SUBSCRIPT FOUR
    (:\(4\) "(4)" ?\u2084)              ; ₄ SUBSCRIPT FOUR
    (:.\(4\) ".(4)" ?\u2084)            ; ₄ SUBSCRIPT FOUR

    ;; superscripts
    (:**2 "**2" ?\u00B2)                ; ² SUPERSCRIPT TWO
    (:^2 "^2" ?\u00B2)                  ; ² SUPERSCRIPT TWO
    (:**3 "**3" ?\u00B3)                ; ³ SUPERSCRIPT THREE
    (:^3 "^3" ?\u00B3)                  ; ³ SUPERSCRIPT THREE
    (:**n "**n" ?\u207F)                ; ⁿ SUPERSCRIPT LATIN SMALL LETTER N
    (:^n "^n" ?\u207F)                  ; ⁿ SUPERSCRIPT LATIN SMALL LETTER N
    
    ;; function
    (:function "function" ?\u03BB)      ; λ GREEK SMALL LETTER LAMDA
    (:lambda "lambda" ?\u03BB)          ; λ GREEK SMALL LETTER LAMDA
    (:\\ "\\" ?\u03BB)                  ; λ GREEK SMALL LETTER LAMDA
    (:FN "FN" ?\u039B)                  ; Λ GREEK CAPITAL LETTER LAMDA
    (:FUN "FUN" ?\u039B)                ; Λ GREEK CAPITAL LETTER LAMDA

    ;; greek letters
    ;;    ;;; 0391 Α GREEK CAPITAL LETTER ALPHA
    ;; (?\u0391 :Alpha (:greek :greek-capitals)
    ;;          (:ALPHA "ALPHA" ,@all)
    ;;          (:Alpha "Alpha" ,@all))

    ;;    ;;; 0392 Β GREEK CAPITAL LETTER BETA
    ;; (?\u0392 :Beta (:greek :greek-capitals)
    ;;          (:BETA "BETA" ,@all)
    ;;          (:Beta "Beta" ,@all))

    ;;    ;;; 0393 Γ GREEK CAPITAL LETTER GAMMA
    ;; (?\u0393 :Gamma (:greek :greek-capitals)
    ;;          (:GAMMA "GAMMA" ,@all)
    ;;          (:Gamma "Gamma" ,@all))

    ;;    ;;; 0394 Δ GREEK CAPITAL LETTER DELTA
    ;; (?\u0394 :Delta (:greek :greek-capitals)
    ;;          (:DELTA "DELTA" ,@all)
    ;;          (:Delta "Delta" ,@all))

    ;;    ;;; 0395 Ε GREEK CAPITAL LETTER EPSILON
    ;; (?\u0395 :Epsilon (:greek :greek-capitals)
    ;;          (:EPSILON "EPSILON" ,@all)
    ;;          (:Epsilon "Epsilon" ,@all))

    ;;    ;;; 0396 Ζ GREEK CAPITAL LETTER ZETA
    ;; (?\u0396 :Zeta (:greek :greek-capitals)
    ;;          (:ZETA "ZETA" ,@all)
    ;;          (:Zeta "Zeta" ,@all))

    ;;    ;;; 0397 Η GREEK CAPITAL LETTER ETA
    ;; (?\u0397 :Eta (:greek :greek-capitals)
    ;;          (:ETA "ETA" ,@all)
    ;;          (:Eta "Eta" ,@all))

    ;; ;; 0398 Θ GREEK CAPITAL LETTER THETA
    ;; (?\u0398 :Theta (:greek :greek-capitals)
    ;;          (:THETA "THETA" ,@all)
    ;;          (:Theta "Theta" ,@all))

    ;; ;; 0399 Ι GREEK CAPITAL LETTER IOTA
    ;; (?\u0399 :Iota (:greek :greek-capitals)
    ;;          (:IOTA "IOTA" ,@all)
    ;;          (:Iota "Iota" ,@all))

    ;; ;; 039A Κ GREEK CAPITAL LETTER KAPPA
    ;; (?\u039A :Kappa (:greek :greek-capitals)
    ;;          (:KAPPA "KAPPA" ,@all)
    ;;          (:Kappa "Kappa" ,@all))

    ;; ;; 039B Λ GREEK CAPITAL LETTER LAMDA
    ;; (?\u039B :Lambda (:greek :greek-capitals)
    ;;          (:LAMBDA "LAMBDA" ,@all)
    ;;          (:Lambda "Lambda" ,@all))

    ;; ;; 039C Μ GREEK CAPITAL LETTER MU
    ;; (?\u039C :Mu (:greek :greek-capitals)
    ;;          (:MU "MU" ,@all)
    ;;          (:Mu "Mu" ,@all))

    ;; ;; 039D Ν GREEK CAPITAL LETTER NU
    ;; (?\u039D :Nu (:greek :greek-capitals)
    ;;          (:NU "NU" ,@all)
    ;;          (:Nu "Nu" ,@all))

    ;; ;; 039E Ξ GREEK CAPITAL LETTER XI
    ;; (?\u039E :Xi (:greek :greek-capitals)
    ;;          (:XI "XI" ,@all)
    ;;          (:Xi "Xi" ,@all))

    ;; ;; 039F Ο GREEK CAPITAL LETTER OMICRON
    ;; (?\u039F :Omicron (:greek :greek-capitals)
    ;;          (:OMICRON "OMICRON" ,@all)
    ;;          (:Omicron "Omicron" ,@all))

    ;; ;; 03A0 Π GREEK CAPITAL LETTER PI
    ;; (?\u03A0 :Pi (:greek :greek-capitals)
    ;;          (:PI "PI" ,@all)
    ;;          (:Pi "Pi" ,@all))

    ;; ;; 03A1 Ρ GREEK CAPITAL LETTER RHO
    ;; (?\u03A1 :Rho (:greek :greek-capitals)
    ;;          (:RHO "RHO" ,@all)
    ;;          (:Rho "Rho" ,@all))

    ;; ;; 03A3 Σ GREEK CAPITAL LETTER SIGMA
    ;; (?\u03A3 :Sigma (:greek :greek-capitals)
    ;;          (:SIGMA "SIGMA" ,@all)
    ;;          (:Sigma "Sigma" ,@all))

    ;; ;; 03A4 Τ GREEK CAPITAL LETTER TAU
    ;; (?\u03A4 :Tau (:greek :greek-capitals)
    ;;          (:TAU "TAU" ,@all)
    ;;          (:Tau "Tau" ,@all))

    ;; ;; 03A5 Υ GREEK CAPITAL LETTER UPSILON
    ;; (?\u03A5 :Upsilon (:greek :greek-capitals)
    ;;          (:UPSILON "UPSILON" ,@all)
    ;;          (:Upsilon "Upsilon" ,@all))

    ;; ;; 03A6 Φ GREEK CAPITAL LETTER PHI
    ;; (?\u03A6 :Phi (:greek :greek-capitals)
    ;;          (:PHI "PHI" ,@all)
    ;;          (:Phi "Phi" ,@all))

    ;; ;; 03A7 Χ GREEK CAPITAL LETTER CHI
    ;; (?\u03A7 :Chi (:greek :greek-capitals)
    ;;          (:CHI "CHI" ,@all)
    ;;          (:Chi "Chi" ,@all))

    ;; ;; 03A8 Ψ GREEK CAPITAL LETTER PSI
    ;; (?\u03A8 :Psi (:greek :greek-capitals)
    ;;          (:PSI "PSI" ,@all)
    ;;          (:Psi "Psi" ,@all))

    ;; ;; 03A9 Ω GREEK CAPITAL LETTER OMEGA
    ;; (?\u03A9 :Omega (:greek :greek-capitals)
    ;;          (:OMEGA "OMEGA" ,@all)
    ;;          (:Omega "Omega" ,@all))

    ;; ;; 03B1 α GREEK SMALL LETTER ALPHA
    ;; (?\u03B1 :alpha (:greek :greek-lowercase)
    ;;          (:alpha "alpha" ,@all)
    ;;          (:\'a "'a" ,@mley))

    ;; ;; 03B2 β GREEK SMALL LETTER BETA
    ;; (?\u03B2 :beta (:greek :greek-lowercase)
    ;;          (:beta "beta" ,@all)
    ;;          (:\'b "'b" ,@mley))

    ;; ;; 03B3 γ GREEK SMALL LETTER GAMMA
    ;; (?\u03B3 :gamma (:greek :greek-lowercase)
    ;;          (:gamma "gamma" ,@all)
    ;;          (:\'c "'c" ,@mley))

    ;; ;; 03B4 δ GREEK SMALL LETTER DELTA
    ;; (?\u03B4 :delta (:greek :greek-lowercase)
    ;;          (:delta "delta" ,@all)
    ;;          (:\'d "'d" ,@mley))

    ;; ;; 03B5 ε GREEK SMALL LETTER EPSILON
    ;; (?\u03B5 :epsilon (:greek :greek-lowercase)
    ;;          (:epsilon "epsilon" ,@all)
    ;;          (:\'e "'e" ,@mley))

    ;; ;; 03B6 ζ GREEK SMALL LETTER ZETA
    ;; (?\u03B6 :zeta (:greek :greek-lowercase)
    ;;          (:zeta "zeta" ,@all))

    ;; ;; 03B7 η GREEK SMALL LETTER ETA
    ;; (?\u03B7 :eta (:greek :greek-lowercase)
    ;;          (:eta "eta" ,@all))

    ;; ;; 03B8 θ GREEK SMALL LETTER THETA
    ;; (?\u03B8 :theta (:greek :greek-lowercase)
    ;;          (:theta "theta" ,@all))

    ;; ;; 03B9 ι GREEK SMALL LETTER IOTA
    ;; (?\u03B9 :iota (:greek :greek-lowercase)
    ;;          (:iota "iota" ,@all))

    ;; ;; 03BA κ GREEK SMALL LETTER KAPPA
    ;; (?\u03BA :kappa (:greek :greek-lowercase)
    ;;          (:kappa "kappa" ,@all))

    ;; ;; 03BB λ GREEK SMALL LETTER LAMDA
    ;; (?\u03BB :lambda (:greek :greek-lowercase)
    ;;          (:lambda "lambda" ,@all))

    ;; ;; 03BC μ GREEK SMALL LETTER MU
    ;; (?\u03BC :mu (:greek :greek-lowercase)
    ;;          (:mu "mu" ,@all))

    ;; ;; 03BD ν GREEK SMALL LETTER NU
    ;; (?\u03BD :nu (:greek :greek-lowercase)
    ;;          (:nu "nu" ,@all))

    ;; ;; 03BE ξ GREEK SMALL LETTER XI
    ;; (?\u03BE :xi (:greek :greek-lowercase)
    ;;          (:xi "xi" ,@all))

    ;; ;; 03BF ο GREEK SMALL LETTER OMICRON
    ;; (?\u03BF :omicron (:greek :greek-lowercase)
    ;;          (:omicron "omicron" ,@all))

    ;; ;; 03C0 π GREEK SMALL LETTER PI
    ;; (?\u03C0 :pi (:greek :greek-lowercase)
    ;;          (:pi "pi" ,@all)
    ;;          (:M_PI "M_PI" c c++))

    ;; ;; 03C1 ρ GREEK SMALL LETTER RHO
    ;; (?\u03C1 :rho (:greek :greek-lowercase)
    ;;          (:rho "rho" ,@all))

    ;; ;; 03C3 σ GREEK SMALL LETTER SIGMA
    ;; (?\u03C3 :sigma (:greek :greek-lowercase)
    ;;          (:sigma "sigma" ,@all))

    ;; ;; 03C4 τ GREEK SMALL LETTER TAU
    ;; (?\u03C4 :tau (:greek :greek-lowercase)
    ;;          (:tau "tau" ,@all))

    ;; ;; 03C5 υ GREEK SMALL LETTER UPSILON
    ;; (?\u03C5 :upsilon (:greek :greek-lowercase)
    ;;          (:upsilon "upsilon" ,@all))

    ;; ;; 03C6 φ GREEK SMALL LETTER PHI
    ;; (?\u03C6 :phi (:greek :greek-lowercase)
    ;;          (:phi "phi" ,@all))

    ;; ;; 03C7 χ GREEK SMALL LETTER CHI
    ;; (?\u03C7 :chi (:greek :greek-lowercase)
    ;;          (:chi "chi" ,@all))

    ;; ;; 03C8 ψ GREEK SMALL LETTER PSI
    ;; (?\u03C8 :psi (:greek :greek-lowercase)
    ;;          (:psi "psi" ,@all))

    ;; ;; 03C9 ω GREEK SMALL LETTER OMEGA
    ;; (?\u03C9 :omega (:greek :greek-lowercase)
    ;;          (:omega "omega" ,@all))

    ;; punctuation
    (:.. ".." ?\u2025)                  ; ‥ TWO DOT LEADER
    (:... "..." ?\u2026)                ; … HORIZONTAL ELLIPSIS
    (:!! "!!" ?\u203C)                  ; ‼ DOUBLE EXCLAMATION MARK
    (:. "\." ?\u2218)                   ; ∘ RING OPERATOR
    (::: "::" ?\u2237)                  ; ∷ PROPORTION

    ;; arrows
    (:<- "<-" ?\u2190)                  ; ← LEFTWARDS ARROW
    (:^ "\\^" ?\u2191)                  ; ↑ UPWARDS ARROW
    (:-> "->" ?\u2192)                  ; → RIGHTWARDS ARROW
    (:->> "->>" ?\u21A0)                ; ↠ RIGHTWARDS TWO HEADED ARROW
    (:=> "=>" ?\u21D2)                  ; ⇒ RIGHTWARDS DOUBLE ARROW
    (:<=> "<=>" ?\u21D4)                ; ⇔ LEFT RIGHT DOUBLE ARROW
    (:-< "-<" ?\u2919)                  ; ⤙ LEFTWARDS ARROW-TAIL
    (:>- ">-" ?\u291A)                  ; ⤚ RIGHTWARDS ARROW-TAIL
    (:-<< "-<<" ?\u291B)                ; ⤛ LEFTWARDS DOUBLE ARROW-TAIL
    (:>>- ">>-" ?\u291C)                ; ⤜ RIGHTWARDS DOUBLE ARROW-TAIL

    ;; quantifiers
    (:forall "forall" ?\u2200)          ; ∀ FOR ALL
    (:exists "exists" ?\u2203)          ; ∃ THERE EXISTS

    ;; nil
    (:nil "nil" ?\u2205)                ; ∅ EMPTY SET
    (:null "null" ?\u2205)              ; ∅ EMPTY SET
    (:NULL "NULL" ?\u2205)              ; ∅ EMPTY SET
    (:None "None" ?\u2205)              ; ∅ EMPTY SET
    (:empty "empty" ?\u2205)            ; ∅ EMPTY SET
    (:\(\) "()" ?\u2205)                ; ∅ EMPTY SET
    (:\'\(\) "'()" ?\u2205)             ; ∅ EMPTY SET
    (:\[\] "[]" ?\u2205)                ; ∅ EMPTY SET

    ;; arithmetic
    (:sqrt "sqrt" ?\u221A)              ; √ SQUARE ROOT
    (:Math.sqrt "Math.sqrt" ?\u221A)    ; √ SQUARE ROOT
    (:++ "++" ?\u29FA)                  ; ⧺ DOUBLE PLUS
    (:+++ "+++" ?\u29FB)                ; ⧻ TRIPLE PLUS
    (:product "product" ?\u220F)        ; ∏ N-ARY PRODUCT
    (:sum "sum" ?\u2211)                ; Σ N-ARY SUMMATION

    ;; undefined
    (:undefined "undefined" ?\u22A5)    ; ⊥ UP TACK
    (:void-0 "void 0" ?\u22A5)          ; ⊥ UP TACK

    ;; parentheses
    (:\[| "[|" ?\u27E6)              ; ⟦ MATHEMATICAL LEFT WHITE SQUARE BRACKET
    (:|\] "|]" ?\u27E7)              ; ⟧ MATHEMATICAL RIGHT WHITE SQUARE BRACKET
    (:\(| "(|" ?\u2987)              ; ⦇ Z NOTATION LEFT IMAGE BRACKET
    (:|\) "|)" ?\u2988)              ; ⦈ Z NOTATION RIGHT IMAGE BRACKET

    ;; types
    (:Integer "Integer" ?\u2124)        ; ℤ DOUBLE-STRUCK CAPITAL Z

    ;; other
    (:||| "|||" ?\u2AF4)                ; ⫴ TRIPLE VERTICAL BAR BINARY RELATION
    )
  "mapping from common symbol names to their regexp and glyph")

(defun pm/names (patterns)
  "converts a list of names to a list of pattern / glyph pairs"
  (mapcar (lambda (pattern)
            (let* ((triple (assq pattern pm/patterns))
                   (pair (cdr triple))
                   (regexp (car pair))
                   (glyph (cadr pair)))
              (cons regexp glyph)))
          patterns))

(defvar pm/mapping
  (let* (;; common symbol groups
         (bitshift (list (cons :bitshift (pm/names '(:>> :<<)))))
         (bitshift-with-unsigned (list (cons :bitshift (pm/names '(:>> :<< :>>>)))))
         (equality (list (cons :equality (pm/names '(:== :!=)))))
         (logic-symbols (list (cons :logic-symbols (pm/names '(:&& :|| :!)))))
         (logic-words (list (cons :logic-words (pm/names '(:and :or :not)))))
         (ordering (list (cons :ordering (pm/names '(:>= :<=)))))
         (null (list (cons :null (pm/names '(:null)))))
         (null-nil (list (cons :null (pm/names '(:nil)))))
         (arrows-right (listp (cons :arrows (pm/names '(:-> :=>)))))
         (arrows-lisp (list (cons :arrows (pm/names '(:-> :->> :=>)))))
         (superscripts-** (list (cons :superscripts (pm/names '(:**2 :**3 :**n)))))
         (function-lambda (list (cons :function (pm/names '(:lambda)))))

         ;; common language groups
         (c-like (append
                  (list
                   (cons :subscripts (pm/names '(:\[0\] :\[1\] :\[2\] :\[3\] :\[4\]))))
                  logic-symbols bitshift equality ordering))
         (ml-like (append
                   (list (cons :null (pm/names '(:\[\] :\(\)))))
                   ordering))
         (lisp-like (append logic-words arrows-lisp ordering)))
    (list
     ;; popular languages
     (cons :javascript (append
                        (list
                         (cons :arithmetic (pm/names '(:++ :Math.sqrt)))
                         (cons :function (pm/names '(:function)))
                         (cons :sets (pm/names '(:in)))
                         (cons :undefined (pm/names '(:undefined :void-0)))
                         (cons :equality (pm/names '(:=== :!== :== :!=))))
                        null c-like))
     (cons :ruby (append
                  (list (cons :equality (pm/names '(:== :=== :!= :=~ :!~ :||=)))
                        (cons :arithmetic (pm/names '(:Math.sqrt)))
                        (cons :punctuation (pm/names '(:.. :... :::))))
                  null-nil function-lambda superscripts-** logic-words c-like))
     (cons :java (append null c-like))
     (cons :python (append
                    (list (cons :sets (pm/names '(:in :not-in)))
                          (cons :null (pm/names '(:None))))
                    superscripts-** logic-words bitshift equality ordering))
     (cons :sh c-like)
     ;; (cons :php nil) ; missing
     (cons :c (append
               (list (cons (:null (pm/names '(:NULL)))))
               c-like))
     (cons :c++ c-like)
     (cons :perl c-like)
     ;; (cons :objective-c nil) ; missing

     ;; other languages
     (cons :coffee (append
                    (list
                     (cons :arithmetic (pm/names '(:Math.sqrt)))
                     (cons :equality (pm/names '(:== :!= :||=)))
                     (cons :sets (pm/names '(:in)))
                     (cons :undefined (pm/names '(:undefined))))
                    null logic-words c-like))
     (cons :clips lisp-like)
     (cons :clojure (append
                     (list (cons :function (pm/names '(:fn)))
                           (cons :equality (pm/names '(:not=))))
                     null-nil lisp-like))
     (cons :emacs-lisp (append null-nil function-lambda lisp-like))
     (cons :ess c-like)
     (cons :groovy c-like)     
     (cons :haskell (append
                     (list
                      (cons :arrows-control-arrow
                            (pm/names '(:<+> :+++ :|||)))
                      (cons :arrows-control-category
                            (pm/names '(:<<< :>>>)))
                      (cons :arrows-monads
                            (pm/names '(:<< :>>)))
                      (cons :arrows
                            (pm/names '(:<- :-> :=> :-< :>- :-<< :>>-)))
                      (cons :equality '(("==" . ?\u2A75)
                                        ("/=" . ?\u2260)))
                      (cons :logic-words (pm/names '(:not :and-nary :or-nary)))
                      (cons :logic-symbols (pm/names '(:&& :||)))
                      (cons :sets (pm/names '(:elem :notElem)))
                      (cons :sets-lists (pm/names '(:intersect :union :++)))
                      (cons :sets-sets (pm/names '(:intersection
                                                   :union
                                                   :isProperSubsetOf
                                                   :isSubsetOf
                                                   :unions
                                                   :\\\\)))
                      (cons :subscripts (pm/names '(:!!0 :!!1 :!!2 :!!3 :!!4)))
                      (cons :superscripts (pm/names '(:^2 :^3 :^n)))
                      (cons :function (pm/names '(:\\)))
                      (cons :punctuation (pm/names '(:.. :!! :. :::)))
                      (cons :types (pm/names '(:Integer)))
                      (cons :quantifiers (pm/names '(:forall :exists)))
                      (cons :arithmetic (pm/names '(:sqrt :product :sum)))
                      (cons :undefined (pm/names '(:undefined)))
                      (cons :parentheses (pm/names '(:\[| :|\] :\(| :|\))))
                      )
                     arrows-right
                     ml-like))
     (cons :jess lisp-like)
     (cons :lisp (append function-lambda lisp-like))
     (cons :octave (append
                    (list (cons :subscripts (pm/names '(:\(0\) :\(1\) :\(2\) :\(3\) :\(4\))))
                          (cons :equality (pm/names '(:<> :~= :!=))))
                    superscripts-** ordering))
     ;; (cons :scala c-like) ; missing
     (cons :scheme (append
                    (list (cons :equality (pm/names '(:!=)))
                          (cons :punctuation (pm/names '(:...)))
                          (cons :null (pm/names '(:null :\'\(\) :empty))))
                    function-lambda lisp-like))
     (cons :sml (append
                 (list (cons :logic-words (pm/names '(:not :andalso :orelse)))
                       (cons :function (pm/names '(:fn :FN))))
                 ml-like))
     (cons :tuareg (append
                    (list (cons :subscripts (pm/names '(:.\(0\) :.\(1\) :.\(2\) :.\(3\) :.\(4\))))
                          (cons :function (pm/names '(:fun :FUN)))
                          (cons :equality (pm/names '(:<>))))
                    superscripts-** ml-like))
     ))
  "Alist mapping from modes to mappings. An entry has the form
 (MODE . GROUP_MAPPINGS), where each GROUP_MAPPINGS is a mapping from
a group name to Regexp / Glyph pairs. Each entry of GROUP_MAPPINGS has
the form (GROUP . ((REGEXP . GLYPH) ...))")


;; TODO: arrows
;; '(
;;        ;;; Arrows

;;   ;; 2190 ← LEFTWARDS ARROW
;;   (?\u2190 :leftarrow (:arrows)
;;            (:<- "<-" ,@mley ess ,@lispy))

;;   ;; 2191 ↑ UPWARDS ARROW
;;   (?\u2191 :uparrow (:arrows)
;;            (:\\^ "\\^" tuareg))

;;   ;; 2192 → RIGHTWARDS ARROW
;;   (?\u2192 :rightarrow (:arrows)
;;            (:-> "->" ,@mley ess c c++ perl ,@lispy coffee groovy))

;;   ;; 21A0 ↠ RIGHTWARDS TWO HEADED ARROW
;;   (?\u21A0 :twoheadrightarrow (:arrows :arrows-twoheaded)
;;            (:->> "->>" ,@lispy))

;;   ;; 21D2 ⇒ RIGHTWARDS DOUBLE ARROW
;;   (?\u21D2 :Rightarrow (:arrows)
;;            (:=> "=>" sml perl ruby ,@lispy haskell coffee))

;;   ;; 21D4 ⇔ LEFT RIGHT DOUBLE ARROW
;;   (?\u21D4 :eftrightarrow (:arrows)
;;            (:<=> "<=>" groovy))


;;   )

(defun pretty-patterns ()
  "Set pretty patterns in a convenient way."
  (let ((pretty-patterns))
    (loop for (mode . group-to-regexp-glyph-pairs) in pm/mapping do
          (let* ((mode (pm/ensure-mode mode))
                 (active-groups (cdr (assq mode pm/active-groups)))
                 (pretty-patterns (push (list mode) pretty-patterns))
                 (assoc-pair (car pretty-patterns)))
            (loop for (group . regexp-glyph-pairs) in group-to-regexp-glyph-pairs do
                  (when (memq group active-groups)
                    (mapcar (lambda (pair)
                              (push pair (cdr assoc-pair)))
                            regexp-glyph-pairs)))))
    pretty-patterns))


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

(provide 'pretty-mode)

;;; pretty-mode.el ends here
