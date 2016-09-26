;;Attemp to replicate some intellij feature in emacs

;;load file in repl
;;command-shift-l
;;cider-load-buffer

;;change namespace
;;command-shift-n

;;split
;;??-;

;;wrap
;;C-w

;;Thread form
;;Cmd-shf-,
;;unthread form
;;Cmd-shf-.


;;move form up
;;Cmd-shf-up
;;move form down
;;Cmd-shf-down

;;send top form to repl
;;Cmd-shf-p

;;clear output repl
;;'cider-repl-clear-output-repl

;;tabs
;;Cmd-[/]

;;C-o
;;open file
;;'ido-find-file

;;C-b
;;change to buffer
;;'ido-switch-buffer

;;C-s
;;save file
;;'save-buffer

;;undo 
;;C-z
;;'undo

;;copy paste
;;'copy-region-as-kill
;;'yank

;;Move to repl
;;C-M-p

;;close tab
;;C-w

;;move to repl
;;'cider-move-te-repl-buffer

;;move to clojure buffer
;;'cider-move-to-clojure-buffer


(defun intellij-send-top-form-to-repl ()
  (interactive)
  (cider-insert-last-sexp-in-repl -1)
  (cider-switch-to-last-clojure-buffer))



(global-set-key (kbd "C-1") 'smex)
(global-set-key (kbd "C-z") 'undo)
;;(global-set-key (kbd "C-S-k") 'redo)
(global-set-key (kbd "C-s") 'save-buffer)
(global-set-key (kbd "C-o") 'ido-find-file)
(global-set-key (kbd "C-b") 'ido-switch-buffer)
(global-set-key (kbd "M-c") 'copy-region-as-kill)
(global-set-key (kbd "M-v") 'yank)
(global-set-key (kbd "<C-S-return>") 'other-window)
(global-set-key (kbd "C-S-l") 'cider-load-buffer)
(global-set-key (kbd "C-S-n") 'cider-repl-set-ns)
(global-set-key (kbd "C-S-p") 'intellij-send-top-form-to-repl)
(global-set-key (kbd "<M-up>") 'er/expand-region)
(global-set-key (kbd "<M-up>") 'er/expand-region)
(global-set-key (kbd "<M-up>") 'er/expand-region)
(global-set-key (kbd "<M-up>") 'er/expand-region)
