(eval-after-load 'cider-repl
  '(progn (add-hook 'cider-repl-mode-hook 'enable-paredit-mode)
          (add-hook 'cider-repl-mode-hook 'company-mode)
          (setq nrepl-buffer-name-show-port t)
          (setq cider-prefer-local-resources t)
          (setq cider-prompt-for-symbol nil)
          (setq cider-prompt-save-file-on-load nil)
          (setq cider-repl-history-size 1000)
          (setq cider-repl-history-file "~/.emacs.d/cider-history.dat")
          (define-key cider-repl-mode-map (kbd "C-c M-o") 'cider-repl-clear-buffer)))

(provide 'init-cider)

;;; init-cider.el ends here
