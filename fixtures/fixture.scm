#!r6rs

(import (rnrs base (6)))

;;; Ordering
(assert '<=) ; 2264 ≤ LESS-THAN OR EQUAL TO
(assert '>=) ; 2265 ≥ GREATER-THAN OR EQUAL TO
;;; Equality
(assert '!=) ; 2260 ≠ NOT EQUAL TO
;;; Logic
(assert 'not) ; 00AC ¬ NOT SIGN
(assert 'and) ; 2227 ∧ LOGICAL AND
(assert 'or) ; 2228 ∨ LOGICAL OR
;;; Sets
;;; Subscripts and Superscripts
;;; Function
(assert 'lambda) ; 03BB λ GREEK SMALL LETTER LAMDA
;;; Greek Letters
(assert 'ALPHA) ; 0391 Α GREEK CAPITAL LETTER ALPHA
(assert 'Alpha) 
(assert 'BETA) ; 0392 Β GREEK CAPITAL LETTER BETA
(assert 'Beta) 
(assert 'GAMMA) ; 0393 Γ GREEK CAPITAL LETTER GAMMA
(assert 'Gamma)
(assert 'DELTA) ; 0394 Δ GREEK CAPITAL LETTER DELTA
(assert 'Delta) 
(assert 'EPSILON) ; 0395 Ε GREEK CAPITAL LETTER EPSILON
(assert 'Epsilon) 
(assert 'ZETA) ; 0396 Ζ GREEK CAPITAL LETTER ZETA
(assert 'Zeta)
(assert 'ETA) ; 0397 Η GREEK CAPITAL LETTER ETA
(assert 'Eta)
(assert 'THETA) ; 0398 Θ GREEK CAPITAL LETTER THETA
(assert 'Theta)
(assert 'IOTA) ; 0399 Ι GREEK CAPITAL LETTER IOTA
(assert 'Iota)
(assert 'KAPPA) ; 039A Κ GREEK CAPITAL LETTER KAPPA
(assert 'Kappa)
(assert 'LAMBDA) ; 039B Λ GREEK CAPITAL LETTER LAMBDA
(assert 'Lambda)
(assert 'MU) ; 039C Μ GREEK CAPITAL LETTER MU
(assert 'Mu)
(assert 'NU) ; 039D Ν GREEK CAPITAL LETTER NU
(assert 'Nu)
(assert 'XI) ; 039E Ξ GREEK CAPITAL LETTER XI
(assert 'Xi)
(assert 'OMICRON) ; 039F Ο GREEK CAPITAL LETTER OMICRON
(assert 'Omicron)
(assert 'PI) ; 03A0 Π GREEK CAPITAL LETTER PI
(assert 'Pi)
(assert 'RHO) ; 03A1 Ρ GREEK CAPITAL LETTER RHO
(assert 'Rho)
(assert 'SIGMA) ; 03A3 Σ GREEK CAPITAL LETTER SIGMA
(assert 'Sigma)
(assert 'TAU) ; 03A4 Τ GREEK CAPITAL LETTER TAU
(assert 'Tau)
(assert 'UPSILON) ; 03A5 Υ GREEK CAPITAL LETTER UPSILON
(assert 'Upsilon)
(assert 'PHI) ; 03A6 Φ GREEK CAPITAL LETTER PHI
(assert 'Phi)
(assert 'CHI) ; 03A7 Χ GREEK CAPITAL LETTER CHI
(assert 'Chi)
(assert 'PSI) ; 03A8 Ψ GREEK CAPITAL LETTER PSI
(assert 'Psi)
(assert 'OMEGA) ; 03A9 Ω GREEK CAPITAL LETTER OMEGA
(assert 'Omega)
(assert 'alpha) ; 03B1 α GREEK SMALL LETTER ALPHA
(assert 'beta) ; 03B2 β GREEK SMALL LETTER BETA
(assert 'gamma) ; 03B3 γ GREEK SMALL LETTER GAMMA
(assert 'delta) ; 03B4 δ GREEK SMALL LETTER DELTA
(assert 'epsilon) ; 03B5 ε GREEK SMALL LETTER EPSILON
(assert 'zeta) ; 03B6 ζ GREEK SMALL LETTER ZETA
(assert 'eta) ; 03B7 η GREEK SMALL LETTER ETA
(assert 'theta) ; 03B8 θ GREEK SMALL LETTER THETA
(assert 'iota) ; 03B9 ι GREEK SMALL LETTER IOTA
(assert 'kappa) ; 03BA κ GREEK SMALL LETTER KAPPA
(assert 'lambda) ; 03BB λ GREEK SMALL LETTER LAMBDA
(assert 'mu) ; 03BC μ GREEK SMALL LETTER MU
(assert 'nu) ; 03BD ν GREEK SMALL LETTER NU
(assert 'xi) ; 03BE ξ GREEK SMALL LETTER XI
(assert 'omicron) ; 03BF ο GREEK SMALL LETTER OMICRON
(assert 'pi) ; 03C0 π GREEK SMALL LETTER PI
(assert 'rho) ; 03C1 ρ GREEK SMALL LETTER RHO
(assert 'sigma) ; 03C3 σ GREEK SMALL LETTER SIGMA
(assert 'tau) ; 03C4 τ GREEK SMALL LETTER TAU
(assert 'upsilon) ; 03C5 υ GREEK SMALL LETTER UPSILON
(assert 'phi) ; 03C6 φ GREEK SMALL LETTER PHI
(assert 'chi) ; 03C7 χ GREEK SMALL LETTER CHI
(assert 'psi) ; 03C8 ψ GREEK SMALL LETTER PSI
(assert 'omega) ; 03C9 ω GREEK SMALL LETTER OMEGA
;;; Punctuation
(assert '...) ; 2026 … HORIZONTAL ELLIPSIS
;;; Types
;;; Arrows
(assert '<-) ; 2190 ← LEFTWARDS ARROW
(assert '->) ; 2192 → RIGHTWARDS ARROW
(assert '->>) ; 21A0 ↠ RIGHTWARDS TWO HEADED ARROW
(assert '=>) ; 21D2 ⇒ RIGHTWARDS DOUBLE ARROW
;;; Quantifiers
;;; Nil
(assert 'null) ; 2205 ∅ EMPTY SET "null"
(assert ''()) ; 2205 ∅ EMPTY SET "'()"
(assert 'empty) ; 2205 ∅ EMPTY SET  "empty"
;;; Arithmetic
;;; Undefined
;;; Parentheses
;;; Other
