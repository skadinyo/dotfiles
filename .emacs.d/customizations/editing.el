;; override the default keybindings in paredit
(eval-after-load 'paredit
  '(progn
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

;; Customizations relating to editing a buffer.

(require 'expand-region)
(global-set-key (kbd "<M-up>") 'er/expand-region)
(global-set-key (kbd "<M-down>") 'er/contract-region)
(delete-selection-mode 1)


;; Key binding to use "hippie expand" for text autocompletion
;; http://www.emacswiki.org/emacs/HippieExpand
(global-set-key (kbd "M-/") 'hippie-expand)

;; Lisp-friendly hippie expand
(setq hippie-expand-try-functions-list
      '(try-expand-dabbrev
        try-expand-dabbrev-all-buffers
        try-expand-dabbrev-from-kill
        try-complete-lisp-symbol-partially
        try-complete-lisp-symbol))

;; Highlights matching parenthesis
(show-paren-mode 1)

;; Highlight current line
;;(global-hl-line-mode 1)

;; Interactive search key bindings. By default, C-s runs
;; isearch-forward, so this swaps the bindings.
(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "C-r") 'isearch-backward-regexp)
(global-set-key (kbd "C-M-s") 'isearch-forward)
(global-set-key (kbd "C-M-r") 'isearch-backward)

;; Don't use hard tabs
(setq-default indent-tabs-mode nil)

;; When you visit a file, point goes to the last place where it
;; was when you previously visited the same file.
;; http://www.emacswiki.org/emacs/SavePlace
(require 'saveplace)
(setq-default save-place t)
;; keep track of saved places in ~/.emacs.d/places
(setq save-place-file (concat user-emacs-directory "places"))

;; Emacs can automatically create backup files. This tells Emacs to
;; put all backups in ~/.emacs.d/backups. More info:
;; http://www.gnu.org/software/emacs/manual/html_node/elisp/Backup-Files.html
(setq backup-directory-alist `(("." . ,(concat user-emacs-directory
                                               "backups"))))
(setq auto-save-default nil)


;; comments
(defun toggle-comment-on-line ()
  "comment or uncomment current line"
  (interactive)
  (comment-or-uncomment-region (line-beginning-position) (line-end-position)))
(global-set-key (kbd "C-;") 'toggle-comment-on-line)

;; use 2 spaces for tabs
(defun die-tabs ()
  (interactive)
  (set-variable 'tab-width 2)
  (mark-whole-buffer)
  (untabify (region-beginning) (region-end))
  (keyboard-quit))

;; fix weird os x kill error
(defun ns-get-pasteboard ()
  "Returns the value of the pasteboard, or nil for unsupported formats."
  (condition-case nil
      (ns-get-selection-internal 'CLIPBOARD)
    (quit nil)))

(setq electric-indent-mode nil)
