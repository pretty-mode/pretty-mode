(ert-deftest ensure-list ()
  "Tests `ensure-list'"
  (should (equal nil
               (ensure-list nil)))
  (should (equal '(1 2)
               (ensure-list '(1 2))))
  (should (equal '(1)
               (ensure-list 1))))

(ert-deftest ensure-mode ()
  "Tests `ensure-mode'"
  (should (equal 'haskell-mode
               (ensure-mode 'haskell-mode)))
  (should (equal 'haskell-mode
               (ensure-mode 'haskell)))
  (should (equal 'haskell-mode
               (ensure-mode :haskell))))

(ert-deftest ensure-modes ()
  "Tests `ensure-modes'"
  (should (equal pretty-supported-modes
               (ensure-modes '())))
  (should (equal '(haskell-mode python-mode)
               (ensure-modes '(:haskell python)))))

;; TODO: make a macro to DRY up the following two:

(defun should-have-patterns (modes patterns)
  (let* ((modes (ensure-modes modes))
         (patterns (ensure-list patterns))
         (pp (pretty-patterns)))
    (cl-loop for mode in modes do
          (let ((pairs (cdr (assoc mode pp))))
            (cl-loop for pattern in patterns do
                  (should (assoc pattern pairs)))))))

(defun should-not-have-patterns (modes patterns)
  (let* ((modes (ensure-modes modes))
         (patterns (ensure-list patterns))
         (pp (pretty-patterns)))
    (cl-loop for mode in modes do
          (let ((pairs (cdr (assoc mode pp))))
            (cl-loop for pattern in patterns do
                  (should-not (assoc pattern pairs)))))))

(ert-deftest pretty-activate-groups ()
  "Tests the effects of running `pretty-activate-groups'."
  (pretty-defaults)
  (pretty-activate-groups :logic-nary)
  (should-have-patterns :haskell "and"))

(ert-deftest pretty-deactivate-groups ()
  "Tests the effects of running `pretty-deactivate-groups'."
  (pretty-defaults)
  (pretty-deactivate-groups '(:equality :punctuation) :haskell)
  (should-not-have-patterns :haskell '("\." "::" "!!" ".." "==" "/="))
  (should-have-patterns '(:c :c++) '("==" "!=")))

(ert-deftest test-pretty-subgroups ()
  "Tests the effects of (de)activating subgroups"
  (pretty-defaults)
  (pretty-activate-groups :greek '(:haskell :scheme))
  (pretty-deactivate-groups :greek-capitals)
  (should-have-patterns '(:haskell :scheme) '("alpha" "iota"))
  (should-not-have-patterns '(:c++ :clojure) '("beta" "gamma"))
  (should-not-have-patterns '(:haskell :scheme) '("ALPHA" "Alpha" "BETA")))

(ert-deftest pretty-activate-patterns-by-symbol-name ()
  "Tests the effects of running `pretty-activate-patterns' with their
LaTeX style symbol names."
  (pretty-defaults)
  (pretty-activate-patterns :bigwedge)
  (should-have-patterns :haskell "and")
  (should-not-have-patterns :haskell "or")
  (should-not-have-patterns :c++ "and"))

(ert-deftest pretty-activate-patterns-by-regexp-name ()
  "Tests the effects of running `pretty-activate-patterns' with their
regexp names."
  (pretty-defaults)
  (pretty-activate-patterns :or :haskell)
  (should-have-patterns :haskell "or")
  (should-not-have-patterns :c++ "or"))

(ert-deftest pretty-deactivate-patterns-by-symbol-name ()
  "Tests the effects of running `pretty-deactivate-patterns' with their
LaTeX style symbol names."
  (pretty-defaults)
  (pretty-deactivate-patterns :ll)
  (should-not-have-patterns :haskell "<<")
  (should-have-patterns :haskell ">>"))

(ert-deftest pretty-deactivate-patterns-by-regexp-name ()
  "Tests the effects of running `pretty-deactivate-patterns' with their
regexp names."
  (pretty-defaults)
  (pretty-deactivate-patterns :<<)
  (should-not-have-patterns :haskell "<<")
  (should-have-patterns :haskell '(">=" ">>")))

(pretty-defaults)
