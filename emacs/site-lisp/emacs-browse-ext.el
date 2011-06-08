;;;; emacs-browse.ext.el
;;
;; Code for better browsing.

;;; Code:

;;Highlight minor mode code

(define-minor-mode sexp-highlight-mode
"Highlight all occurences of a sexp for a buffer when ctrl+clicked."
:init-value 1
:lighter " shi"
:global 1
:group 'fjohnson-custom
(if sexp-highlight-mode 
    (global-set-key (kbd "<mouse-1>") 'highlight-cursor-sexp)
  (if last-highlight-regexp (progn (unhighlight-regexp last-highlight-regexp)
				   (setq last-highlight-regexp 'nil)
				   (global-unset-key (kbd "<mouse-1>"))))))

(defvar last-highlight-regexp 'nil "A variable not to be fiddled with.
Used by `highlight-cursor-sexp' to remember the last highlighted sexp so
that when a new sexp is selected the previous one is unhighlighted.")

(defun highlight-cursor-sexp (event)
  "Highlight the current sexp under the mouse cursor as well as any other
occurrences in the buffer. EVENT is a mouse event."

  (interactive "e")
  (mouse-set-point event)
  (if last-highlight-regexp (unhighlight-regexp last-highlight-regexp))    
  (let ((sexp (thing-at-point 'sexp)))
    (if sexp				;Do not highlight 'nil!
    (progn (setq last-highlight-regexp sexp)
	   (highlight-regexp sexp)))))

;;Code for tag navigation by clicking on a sexp

;;Rings for holding tag search history.
(defvar tag-history-backwards-ring (make-ring 40)
  "Ring for holding the history of tags searched with `find-tag-immediately'.")

(defvar tag-history-forwards-ring (make-ring 40)
  "Ring for holding the history of undone tags searched for with 
`find-tag-immediatley'.")

(defun find-tag-immediately (event)
  "Jump to the tag contained in the expression under the cursor"

  (interactive "e")
  (mouse-set-point event)
  
  (let ((sexp-tag (thing-at-point 'sexp)))
    (ring-insert-at-beginning tag-history-backwards-ring ;Save tag search
			      sexp-tag)
    (find-tag sexp-tag)))	;Saved mark added to M-. ring of marks.

(defun undo-last-tag-search ()
  "Undoes the last tag location jump"

  (interactive)
  (if (> (ring-length tag-history-backwards-ring) 0)

      ;;Remove last tag search and pop back to the last location.
      ;;The previous tag searched is saved but the mark is removed from the
      ;;M-. ring of marks.

      (progn (ring-insert-at-beginning tag-history-forwards-ring 
				       (ring-remove tag-history-backwards-ring))
	     (pop-tag-mark))
    (minibuffer-message "Nothing left to undo.")
    ))

(defun redo-last-tag-search ()
  "Jumps to the last tag location that was just undone."

  (interactive)
  (if (> (ring-length tag-history-forwards-ring) 0)

      ;;find-tag will add the latest tag search mark to the M-. ring of marks.
      (let ((redo-tag (ring-remove tag-history-forwards-ring)))
	(ring-insert-at-beginning tag-history-backwards-ring
				  redo-tag)
	(find-tag redo-tag))
    (minibuffer-message "Nothing left to redo.")
    ))

;;Other functions (grep current buffer, list tags for buffer)

(defun grep-current-buffer (pattern)
  "Run grep on the current buffer, but only if it is tied to a file.
This function relies on the variable `grep-command'."
  (interactive "sSearch: ")
  (if (buffer-file-name)
      (let ((cargs (list pattern (buffer-file-name)))
	    (commandstring grep-command))
	(grep (dolist (arg cargs commandstring)
		(setq commandstring (concat commandstring 
					    " "
					    arg)))))))

(defun list-tags-currentbuf ()
  "List the tags for the current buffer."
  (interactive)
  (list-tags (buffer-name)))

(defun list-tags-currentbuf-prefix ()
  "List the tags matching the current buffer's name as a prefix."
  (interactive)
  (tags-apropos (parse-filename-or-extension (buffer-file-name))))

(defun parse-filename-or-extension (filename &optional ext)
  "Extract the basename out of the absolute path to a
file. Return nil if supplied an invalid filename Otherwise Parse
the basename into its prefix and extension components. If EXT is
not 'nil then return the extension instead. If a file does not
have an extension return the filename as it is regardless of
whether an extension or prefix was asked for. An explaination
of the rational behind how this function parses is given now.

Steps and Rules:

1) Get the full path of a buffers name if it exists.  

2) If it does not exist, bounce.  

3) Now get the basename of the filename.  
It should be impossible that the basename is \"\" I dont see how
it could be true.

4)Once we have the basename we want to extract the name of the file
without the extension.

A file only has an extension if it has one or more of any
character, then a period, and then at least one other
character. Two last conditions are that an extension does not end
in a . and an extension does not start with a .

Some files are inheriently ambigious and I have chosen to parse
the extension as text that occurs after the last . that has text
after it.

For example, should emacs..a be parsed as (emacs. , .a)
or (emacs, ..a)?  emacs...a be parsed as (emacs, ...a)
or (emacs., ..a) or (emacs.., .a)

What about file.more.c? Is this (file, .more.c) or (file.more,
.c)?  And file..more.c? Is this (file.., more.c) or (file.,
.more.c) or (file..more, .c)?

Examples of files with no extension.

.emacs
emacs
.robots.
files.c.
.c

Examples of files with extensions

files.c -> file, .c
files.c.c -> files.c, .c
.files.c -> .files
..files.crrrrrazy -> ..files, .crrrrrazy
..files..crazy.c -> ..files..crazy, .c
..emacs --> ., .emacs
..hi.. --> ., .hi..
emacs..a -> emacs., .a

A regular expression for a unix path grouping prefix and basename is...
\"(/.*/)*(.+)\"

In emacs that is translated to...
\"\\(/.*/\\)*\\(.+\\)\" Pretty unreadable...

Now to separate the actual file name and extension we can use the following re
\"(.+)(\.[^.].*)\"

In emacs that is \"\\(.+\\)\\(\\.[^.].*\\)\"
"

  (if (string-match "\\(/.*/\\)*\\(.+\\)" filename)
      (let ((basename (match-string 2 filename)))
	(if (string-match "\\(.+\\)\\(\\.[^.].*\\)" basename)
	    (if ext (match-string 2 basename)
	      (match-string 1 basename))
	  basename))))

(provide 'emacs-browse-ext)
;;emacs-browse-ext.el ends here