(require 'git-gutter)

;; add magit hook
(global-set-key (kbd "C-x g") 'magit-status)

;; Add git gutter mode

(global-git-gutter-mode t)
(bind-key "C-x q" 'git-gutter:revert-hunk)
(bind-key "C-c C-s" 'git-gutter:stage-hunk)
(bind-key "C-x p" 'git-gutter:previous-hunk)
(bind-key "C-x n" 'git-gutter:next-hunk)

(provide 'oh-my-git)
