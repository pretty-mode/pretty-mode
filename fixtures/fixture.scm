#!r6rs

(import (rnrs base (6)))

;;; Equality

(assert '!=) ; 2260 ≠ NOT EQUAL TO

;;; Logic

(assert 'not) ; 00AC ¬ NOT SIGN
(assert 'and) ; 2227 ∧ LOGICAL AND
(assert 'or) ; 2228 ∨ LOGICAL OR

;;; Arrows

(assert '<-) ; 2190 ← LEFTWARDS ARROW
(assert '->) ; 2192 → RIGHTWARDS ARROW
(assert '->>) ; 21A0 ↠ RIGHTWARDS TWO HEADED ARROW
(assert '=>) ; 21D2 ⇒ RIGHTWARDS DOUBLE ARROW
(assert '...) ; 2026 … HORIZONTAL ELLIPSIS
(assert 'null) ; 2205 ∅ EMPTY SET "null"
(assert ''()) ; 2205 ∅ EMPTY SET "'()"
(assert 'empty) ; 2205 ∅ EMPTY SET  "empty"
