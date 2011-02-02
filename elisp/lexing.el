;; FIXME: it would probably be better to use some built-in Emacs-y
;; customisation stuff to do this bit. And to rewrite
;; transition-function to use it without having this indirection.
(defun classification-to-face (classification)
  (cond ((eq classification 'comment)     'font-lock-comment-face)
	((eq classification 'keyword)     'font-lock-keyword-face)
	((eq classification 'punctuation) 'font-lock-variable-name-face)
	((eq classification 'identifier)  'default)
	((eq classification 'whitespace)  'default)
	((eq classification 'constant)    'font-lock-constant-face)
	((eq classification 'operator)    'font-lock-keyword-face)
	((eq classification 'constructor) 'font-lock-constant-face)))

;; lexer state consists of:
;;   the start of the current lexeme
;;   the current state
;;   the last good match (optional)

;; for each character, update the current state by applying the
;; transition-function.
;;   on error:  if we have a last good match, go for it and apply the appropriate property
;;              if no match, report error
;;   on move:   just move to the new state
;;   on accept: record the new match and move to the new state
(defun lex-buffer ()
  (interactive)
  (catch 'abort-lexing
    (let ((position       1)
	  (final-position (+ (buffer-size) 1))
	  (state          0)
	  (lexeme-start   1)
	  (current-match  nil))
      (while (<= position final-position)
	(if (= position final-position)
	    (cond ((= lexeme-start position)
		   (setq position (1+ position)))
		  ((null current-match)
		   (progn
		     (message "Lexing failure at EOF")
		     (put-text-property lexeme-start final-position 'face 'default)
		     (throw 'abort-lexing t)))
		  (t
		   (let ((lexeme-end  (car current-match))
			 (lexeme-type (cdr current-match)))
		     (put-text-property lexeme-start lexeme-end 'face (classification-to-face lexeme-type))
		     (setq position lexeme-end)
		     (setq current-match nil)
		     (setq state 0)
		     (setq lexeme-start position))))
	  (let ((result (transition-function state (char-after position))))
	    (cond ((eq (car result) 'change)
		   (setq state (car (cdr result)))
		   (setq position (1+ position)))
		  ((eq (car result) 'accept)
		   (setq position (1+ position))
		   (setq state (car (cdr result)))
		   (setq current-match (cons position (car (cdr (cdr result))))))
		  ((eq (car result) 'error)
		   (if (null current-match)
		       ;; Lexing failure, abort (FIXME: draw a pretty underline)
		       (progn
			 (message (format "Lexing failure at position %d" position))
			 (put-text-property lexeme-start final-position 'face 'default)
			 (throw 'abort-lexing t))
		     ;; Lexing success
		     (let ((lexeme-end  (car current-match))
			   (lexeme-type (cdr current-match)))
		       (put-text-property lexeme-start lexeme-end 'face (classification-to-face lexeme-type))
		       (setq position lexeme-end)
		       (setq current-match nil)
		       (setq state 0)
		       (setq lexeme-start position)))))))))))

;; (defun do-lexing (start end len)
;;   (lex-buffer))

;; (defun install-lexing ()
;;   (interactive)
;;   (lex-buffer)
;;   (add-hook 'after-change-functions
;; 	    'do-lexing
;; 	    nil
;; 	    t))
