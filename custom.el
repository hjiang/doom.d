(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(safe-local-variable-values
   '((eval setq compile-command "bazel test //simpl:all")
     (eval setq flycheck-clang-include-path
      (list
       (expand-file-name "..")
       (expand-file-name "../bazel-oxid/external/gtest/googletest/include")))
     (eval setq flycheck-clang-include-path
      (list
       (expand-file-name "..")))
     (eval setq compile-command "bazel test //oxid:all")
     (eval setq flycheck-clang-include-path
      (list
       (expand-file-name ".")))
     (eval setq flycheck-clang-include-path
      (list
       (expand-file-name "~/myproject/include/")))
     (eval setq compile-command "bazel test //..."))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(doom-modeline-buffer-modified ((t (:foreground "orange")))))
