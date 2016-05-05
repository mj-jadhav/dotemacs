;; Add paredit minor mode to different mjor modes
(add-hook 'emacs-lisp-mode-hook 'enable-paredit-mode)

(eval-after-load 'paredit
  '(progn (define-key paredit-mode-map (kbd "C-o") 'paredit-open-round)))

(provide 'init-parens)

;;; init-parens.el ends here
