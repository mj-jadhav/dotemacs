(eval-after-load 'clojure-mode
  '(progn (add-hook 'clojure-mode-hook 'enable-paredit-mode)
          (add-hook 'clojure-mode-hook 'company-mode)
          (add-hook 'clojure-mode-hook 'eldoc-mode)))

(provide 'init-clojure)

;;; init-clojure.el ends here
