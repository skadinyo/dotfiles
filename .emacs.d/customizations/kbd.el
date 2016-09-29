;;Overide paredit

(defun paredit-kill-region-or-backward-delete ()
  (interactive)
  (if (region-active-p)
      (kill-region (region-beginning) (region-end))
    (paredit-backward-delete)))

(eval-after-load 'paredit
  '(progn
     (define-key paredit-mode-map (kbd "<backspace>") 'paredit-kill-region-or-backward-delete)
     (define-key paredit-mode-map (kbd "<M-right>") 'paredit-forward)
     (define-key paredit-mode-map (kbd "<M-left>")  'paredit-backward)
     (define-key paredit-mode-map (kbd "<M-up>") nil)
     (define-key paredit-mode-map (kbd "<M-down>")  nil)
     (define-key paredit-mode-map (kbd "<C-right>")  'move-end-of-line)
     (define-key paredit-mode-map (kbd "<C-left>")  'move-beginning-of-line)
     (define-key paredit-mode-map (kbd "M-k") 'paredit-kill)
     (define-key paredit-mode-map (kbd "M-s") nil)
     (define-key paredit-mode-map (kbd "M-s M-d") 'paredit-forward-slurp-sexp)
     (define-key paredit-mode-map (kbd "M-s M-a") 'paredit-backward-slurp-sexp)
     
     (define-key paredit-mode-map (kbd "M-b M-n") 'paredit-forward-barf-sexp)
     (define-key paredit-mode-map (kbd "M-b M-v") 'paredit-backward-barf-sexp)

     (define-key paredit-mode-map (kbd "M-w M-w") 'paredit-splice-sexp)
     (define-key paredit-mode-map (kbd "M-w M-e") 'paredit-splice-sexp-killing-backward)
     (define-key paredit-mode-map (kbd "M-w M-q") 'paredit-splice-sexp-killing-forward)

     ))


;;Expand region
(require 'expand-region)
(global-set-key (kbd "<M-up>") 'er/expand-region)
(global-set-key (kbd "<M-down>") 'er/contract-region)
(delete-selection-mode 1)

;; Key binding to use "hippie expand" for text autocompletion
;; http://www.emacswiki.org/emacs/HippieExpand
(global-set-key (kbd "M-/") 'hippie-expand)
(setq hippie-expand-try-functions-list
      '(try-expand-dabbrev
        try-expand-dabbrev-all-buffers
        try-expand-dabbrev-from-kill
        try-complete-lisp-symbol-partially
        try-complete-lisp-symbol))


