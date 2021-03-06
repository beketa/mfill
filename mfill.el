;;; mfill.el --- Fill Japanese text using morphological analyzer.

;; For debugging.
;;
;; (save-excursion
;;   (goto-char 10)
;;   (let ((end (progn (forward-paragraph)
;;                     (skip-chars-backward " \t\n")
;;                     (point)))
;;         (beg (progn (backward-paragraph)
;;                     (skip-chars-forward " \t\n")
;;                     (point))))
;;     (mfill-make-word-splittability-list
;;      (mfill-get-word-class-list beg end))))

;; Explicitly load kinsoku.el to initialize kinsoku char category.
(load "kinsoku")

(defvar mfill-mecab-program-path "C:/Program Files/MeCab/bin/mecab.exe")

(defun mfill-get-word-class-list (beg end)
  (let ((text (buffer-substring-no-properties beg end)))
    (with-temp-buffer
      (insert text)
      (fill-delete-newlines (point-min) (copy-marker (point-max) t)
			    (current-justification) nil nil)
      (call-process-region (point-min) (point-max)
			   mfill-mecab-program-path t t nil "-O" "chasen")
      ;; Escape special charactors.
      (goto-char (point-min))
      (while (search-forward "\\" nil t)
	(replace-match "\\\\" t t))
      (goto-char (point-min))
      (while (search-forward "\"" nil t)
	(replace-match "\\\"" t t))
      
      (goto-char (point-min))
      (insert "(\n")
      (while (re-search-forward
	      "^\\([^\t]\+\\)\t[^\t]\+\t[^\t]\+\t\\([^\t]\+\\).*$" nil t)
	(replace-match "(\"\\1\" . \"\\2\")"))
      (insert ")")
      (preceding-sexp))))

(defun mfill-word-class-splittability (word1 class1 word2 class2)
  (if (and word1 class1 word2 class2)
      (cond ((or (string-match "�L��-��_" class1)
		 (string-match "�L��-�Ǔ_" class1))
	     1)
	    ((and
	      ;; word1 does not end with gyoumatsu kinsoku.
	      (not (aref (char-category-set (aref word1 (1- (length word1))))
			 ?<))
	      ;; word2 does not start with gyoutou kinsoku.
	      (not (aref (char-category-set (aref word2 0)) ?>))
	      
	      (not (string-match "�ړ���" class1))
	      
	      (not (string-match "�񎩗�" class2))
	      (not (string-match "�ڔ�" class2))
	      (not (string-match "����" class2))
	      (not (string-match "������" class2))
	      
	      (not (and (string-match "����-�T�ϐڑ�" class1)
			(string-match "����" class2)))
	      (not (and (string-match "����" class1)
			(string-match "����" class2)))
	      )
	     1)
	    (t 0))
    0))

(defun mfill-make-word-splittability-list (word-class-list)
  (if (null word-class-list)
      '()
    (let* ((prev-word (caar word-class-list))
	   (prev-class (cdar word-class-list))
	   (result (list (cons prev-word 0)))
	   (word-class-list (cdr word-class-list))
	   (word nil)
	   (class nil))
      (while word-class-list
	(setq word (caar word-class-list)
	      class (cdar word-class-list)
	      result (cons (cons word (mfill-word-class-splittability
				       prev-word prev-class
				       word class))
			   result)
	      prev-word word
	      prev-class class
	      word-class-list (cdr word-class-list)))
      (nreverse result))))

(defun mfill-fill-paragraph ()
  (interactive
   (save-excursion
     (move-to-left-margin)
     (when (zerop (forward-paragraph))
       (skip-chars-backward " \t\n")
       (let* ((end (copy-marker (point) t))
	      (beg (progn (backward-paragraph)
			  (skip-chars-forward " \t\n")
			  (point)))
	      (words (mfill-make-word-splittability-list
		      (mfill-get-word-class-list beg end)))
	      (splittable-point nil)
	      (prev-words nil))
	 ;; TODO: indentation, fill prefix, ...
	 (fill-delete-newlines beg end (current-justification) nil nil)
	 (goto-char beg)
	 (while words
	   (let ((word (caar words))
		 (splittability (cdar words)))
	     (while (not (string= (buffer-substring-no-properties
				   (point) (+ (point) (length word)))
				  word))
	       ;; There is a white space or something before this word.
	       (forward-char)
	       ;; And if word does not start with gyoutou kinsoku, we can
	       ;; insert line break here.
	       (if (not (aref (char-category-set (aref word 0)) ?>))
		   (setq splittability 1)))
	     (cond ((<= (current-fill-column) (current-column))
		    (if (null splittable-point)
			;; There is no splittable point before.
			(insert "\n")
		      (goto-char splittable-point)
		      (insert "\n")
		      (setq words prev-words
			    word (caar words)))
		    (setq splittable-point nil
			  prev-words nil))
		   (t
		    (if (>= splittability 1)
			(setq splittable-point (point)
			      prev-words words))))

	     (forward-char (length word))
	     (setq words (cdr words))))

	 ;; Here the point is at the end of the last word of paragraph.
	 ;; Check if the last word exceeds fill column and fold if necessary.
	 (end-of-line)
	 (when (and (<= (current-fill-column) (current-column))
		    splittable-point)
	   (goto-char splittable-point)
	   (insert "\n"))
	 
	 )))))

(provide 'mfill)
