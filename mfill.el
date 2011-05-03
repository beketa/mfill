;;; mfill.el --- Fill Japanese text using morphological analyzer.

;; For debugging.
;;
;; (save-excursion
;;   (goto-char 10)
;;   (let ((end (progn (forward-paragraph) (skip-chars-backward " \t\n") (point)))
;;         (beg (progn (backward-paragraph) (skip-chars-forward " \t\n") (point))))
;;     (mfill-make-word-splittability-list (mfill-get-word-class-list beg end))))

;; Explicitly load kinsoku.el to initialize kinsoku char category.
(load "kinsoku")

(defvar mfill-mecab-program-path "C:/Program Files/MeCab/bin/mecab.exe")

(defun mfill-get-word-class-list (beg end)
  (let ((text (buffer-substring-no-properties beg end)))
    (with-temp-buffer
      (insert text)
      (fill-delete-newlines (point-min) (copy-marker (point-max) t)
			    (current-justification) nil nil)
      (call-process-region (point-min) (point-max) mfill-mecab-program-path t t nil "-O" "chasen")
      ;; Escape special charactors.
      (goto-char (point-min))
      (while (search-forward "\\" nil t)
	(replace-match "\\\\" t t))
      (goto-char (point-min))
      (while (search-forward "\"" nil t)
	(replace-match "\\\"" t t))
      
      (goto-char (point-min))
      (insert "(\n")
      (while (re-search-forward "^\\([^\t]\+\\)\t[^\t]\+\t[^\t]\+\t\\([^\t]\+\\).*$" nil t)
	(replace-match "(\"\\1\" . \"\\2\")"))
      (insert ")")
      (preceding-sexp))))

(defun mfill-word-class-splittability (word1 class1 word2 class2)
  (if (and word1 class1 word2 class2)
      (cond ((or (string-match "記号-句点" class1)
		 (string-match "記号-読点" class1))
	     1)
	    ((and
	      ;; word1 does not end with gyoumatsu kinsoku.
	      (not (aref (char-category-set (aref word1 (1- (length word1)))) ?<))
	      ;; word2 does not start with gyoutou kinsoku.
	      (not (aref (char-category-set (aref word2 0)) ?>))
	      
	      (not (string-match "接頭詞" class1))
	      
	      (not (string-match "非自立" class2))
	      (not (string-match "接尾" class2))
	      (not (string-match "助詞" class2))
	      (not (string-match "助動詞" class2))
	      
	      (not (and (string-match "名詞-サ変接続" class1)
			(string-match "動詞" class2)))
	      (not (and (string-match "名詞" class1)
			(string-match "名詞" class2)))
	      )
	     1)
	    (t 0))
    0))

(defun mfill-make-word-splittability-list (word-class-list)
  (if (null word-class-list)
      '()
    (let ((word (caar word-class-list))
	  (class (cdar word-class-list))
	  (prev-word nil)
	  (prev-class nil)
	  (word-class-list (cdr word-class-list))
	  result)
      (while word-class-list
	(setq result (cons (cons word (mfill-word-class-splittability prev-word prev-class
								      word class))
			   result)
	      prev-word word
	      prev-class class
	      word (caar word-class-list)
	      class (cdar word-class-list)
	      word-class-list (cdr word-class-list)))
      (nreverse result))))

(defun mfill-fill-paragraph ()
  (interactive
   (save-excursion
     (move-to-left-margin)
     (when (zerop (forward-paragraph))
       (skip-chars-backward " \t\n")
       (let* ((end (copy-marker (point) t))
	      (beg (progn (backward-paragraph) (skip-chars-forward " \t\n") (point)))
	      (words (mfill-make-word-splittability-list (mfill-get-word-class-list beg end)))
	      (splittable-point nil)
	      (prev-words nil))
	 ;; TODO: indentation, fill prefix, ...
	 (fill-delete-newlines beg end (current-justification) nil nil)
	 (goto-char beg)
	 (while words
	   (let ((word (caar words))
		 (splittability (cdar words)))
	     (while (not (string= (buffer-substring-no-properties (point) (+ (point) (length word)))
				  word))
	       (forward-char))
	     (if (<= (current-fill-column) (current-column))
		 
		 (if (null splittable-point)
		     ;; There is no splittable point before.
		     (insert "\n")
		   (goto-char splittable-point)
		   (insert "\n")
		   (setq words prev-words)
		   (setq splittable-point nil
			 prev-words nil))
	       
	       (if (>= splittability 1)
		   (setq splittable-point (point)
			 prev-words words)))
	     
	     (setq words (cdr words))))))
       )))

(provide 'mfill)
