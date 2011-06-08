;; .emacs

;;; uncomment this line to disable loading of "default.el" at startup
;; (setq inhibit-default-init t)

;; turn on font-lock mode
(when (fboundp 'global-font-lock-mode)
  (global-font-lock-mode t))

(set-foreground-color "DeepSkyBlue3")
(set-background-color "black")

;Set color for all frames ?
;;(set-face-foreground "DeepSkyBlue3")
;;(set-face-background "black")

(mapcar 'expand-file-name ["~/.emacs.d/site-lisp/"])
;;merge exclude
;;Load site specific packages here.

(let ((default-directory "~/.emacs.d/site-lisp/"))
  (add-to-list 'load-path default-directory)
  (normal-top-level-add-subdirs-to-load-path))

(require 'etags-update)
(require 'emacs-browse-ext)
;;merge exclude

(global-set-key [f6] 'run-shell-command)
(global-set-key [f5] 'kill-buffer-and-window)
(global-set-key [f4] 'imenu)
(global-set-key [f3] 'other-window)
(global-set-key [f2] 'save-buffer)
(global-set-key [f1] 'next-buffer)
(global-set-key [mouse-5] 'go-forward)
(global-set-key [mouse-4] 'go-backwards)
(global-set-key (kbd "M-<left>") 'undo-last-tag-search)
(global-set-key (kbd "M-<right>") 'redo-last-tag-search)

;;merge exclude
(global-set-key (kbd "<XF86Favorites>") 'goto-line)
(global-set-key (kbd "<XF86HomePage>") 'list-tags-currentbuf-prefix)
(global-set-key (kbd "<XF86Search>" ) 'grep-current-buffer)
(global-set-key (kbd "<C-down-mouse-1>") 'find-tag-immediately)
;;merge exclude

;;Turn column number reporting on.
(column-number-mode 1)

;;Reload tag files without asking
(setq tags-revert-without-query 1)

;;Match parenthesis when encountered
(show-paren-mode 1)

;; enable visual feedback on selections
;(setq transient-mark-mode t)

;; default to better frame titles
(setq frame-title-format
      (concat  "%b - emacs@" (system-name)))

;; default to unified diffs
(setq diff-switches "-u")

;; always end a file with a newline
;(setq require-final-newline 'query)

(defvar rs-command 
  "cd ~/repository/lonipipeline_blade/lib/plugins/torque; make all; make jar&" 
  "Command to run in a shell for the `run-shell-command'")

(setq rs-command "cd ~/repository/lonipipeline_blade/lib/plugins/torque; make all; make jar&")

(defun run-shell-command ()
  (interactive)
  (shell-command rs-command)
  (delete-window))

(defun kill-buffer-and-window()
  (interactive)
  (kill-buffer)
  (delete-window))

;;(interactive) must be set for commandp to pass. this is necessary so 
;;we can bind
;;these functions to the mouse wheel.
(defun go-forward() 
  (interactive)
  (forward-line 3))
(defun go-backwards() 
  (interactive)
  (forward-line -3))

;;merge exclude
(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(inhibit-startup-screen t))
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 )
;;merge exclude

;; (defun list-tags-currentbuf-prefix ()
;;   "List the tags matching the current buffer's name as a prefix."
;;   (interactive)

;;   ;Create a new annoymous function to extract the prefix of this buffer's filename
;;   (let ((extract-sourcename-prefix 	
;; 	(lambda (filename) 
;; 	  (let* ((split-result (split-string filename "\\."))
;; 		 (buffer-name-prefix (pop split-result)))
	    
;; 	    ;;Empty filename or e.g .emacs condition.
;; 	    (if (string= buffer-name-prefix "") 
;; 		(if split-result (setq buffer-name-prefix 
;; 				       (concat "." (pop split-result))))
	      
;; 	      (dotimes (i (- (length split-result) 1))
;; 		(setq buffer-name-prefix (concat buffer-name-prefix
;; 						 "."
;; 						 (nth i split-result))))
;; 	      buffer-name-prefix)))))
;;     (tags-apropos (extract-sourcename-prefix (buffer-name)))))















  
