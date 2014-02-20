(require 'f)

(defvar pretty-mode-support-path
  (f-dirname load-file-name))

(defvar pretty-mode-features-path
  (f-parent pretty-mode-support-path))

(defvar pretty-mode-root-path
  (f-parent pretty-mode-features-path))

(add-to-list 'load-path pretty-mode-root-path)

(require 'pretty-mode)
(require 'espuds)
(require 'ert)

;; language modes
(require 'clips-mode)
(require 'clojure-mode)
(require 'coffee-mode)
(load "ess-autoloads")
(require 'ess-site)
(require 'groovy-mode)
(require 'haskell-mode)
;; (require 'jess-mode)
(require 'js2-mode)
(require 'sml-mode)
(require 'tuareg)

(Setup
 ;; Before anything has run
 )

(Before
 ;; Before each scenario is run
 )

(After
 ;; After each scenario is run
 )

(Teardown
 ;; After when everything has been run
 )
