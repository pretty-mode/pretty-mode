(require 'cl)
(require 'ert)
(require 'pretty-mode-plus)

(setq
 default-composition-text-properties
 '(("int x = 1 << 4;"                   ; int x = 1 ≪ 4;
    (11 . (5 2 [171])))
   ("int x = 15 >> 3;"                  ; int x = 15 ≫ 3;
    ( 12 .  (6 2 [187])))))

(ert-deftest c-syntax ()
  "test glyph substitution for C"
  (pretty-defaults)
  (loop for (text . properties) in default-composition-text-properties do
        (let ((buffer (generate-new-buffer "c-test")))
          (with-current-buffer buffer
            (switch-to-buffer buffer)
            (c-mode)
            (pretty-mode)
            (insert text)
            (redisplay)
            ;; (print (buffer-string))
            (loop for (pos . property) in properties do
                  (should
                   (equal property
                          (get-text-property pos 'composition))))
            (kill-buffer buffer)))))
