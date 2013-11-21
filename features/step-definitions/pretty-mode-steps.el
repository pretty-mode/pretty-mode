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
