
(defun contain(item sequence)
	
	(if (equal item (car sequence))
		(return 1))
	
	(if (equal nil sequence)
		(return 0))
	(contain item (cdr sequence)))
(defun tokenize(line)
	
	;;(let )
	;;(alphabet (#\a #\b #\c #\d #\e #\f #\g #\h #\i #\j #\k #\l #\m #\n #\o #\p #\q #\r #\s #\t #\u #\v #\w #\x #\y #\z #\A #\B #\C #\D #\E #\F #\G #\H #\I #\J #\K #\L #\M #\N #\P #\P #\Q #\R #\S #\T #\U #\V #\W #\X #\Y #\Z)))
	(setq other "")
	(setq flag_asterix_end T)
	(setq flag_asterix Nil)
	(setq flag_comment Nil)
	(setq flag_quote Nil)
	(setq _is_digit 0)
	(setq _is_alpha 0)
	(loop for x from 0 to (- (length line) 1)
		do(defparameter *operator* (subseq line x (+ x 1)))
			;;(format t "~a mustafa " *operator*)

			(cond ( (and (string-equal *operator* #\*) (equal flag_asterix_end T))
				
					(setf flag_asterix T)
					(setf flag_asterix_end Nil))
				((and (string-equal *operator* #\*) (equal flag_asterix T)) 
					(setf flag_asterix_end T)
					(setf flag_asterix Nil)
					(print "OP_DBL")
					(terpri))
				((and (not (string-equal *operator* #\*)) (equal flag_asterix T))
					 
					(setf flag_asterix_end T)
					(setf flag_asterix Nil)
					(print "OP_MULT")
					(terpri)))
			(if (string-equal *operator*  #\+)
				(progn
					(print "OP_PLUS")))
			(if (string-equal *operator* #\-)
				(progn
					(print "OP_MINUS")))
			(if (string-equal *operator* #\/)
				(progn
					(print "OP_DIV")))
			
			(if (string-equal *operator* #\()
				(progn
					(print "OP_OP")
					))
			(if (string-equal *operator* #\))
				(progn
					(print "OP_CP")
					(terpri)))
			(if (and (string-equal *operator* #\") (equal flag_quote Nil))
				(progn
					(print "OP_OC")
					(terpri)))
			(if (and (string-equal *operator* #\") (equal flag_quote T))
				(progn
					(print "OP_CP")
					(terpri)))
			(if (string-equal *operator* #\,)
				(progn
					(print "OP_COMMA")
					(terpri)))
			(if (string-equal *operator* #\;)
				(progn
					
					(setf flag_comment T)
					(terpri)))
			(if (and (string-equal *operator* #\;) (equal flag_comment T))
				(progn
					(print "COMMENT")
					(terpri)
					(return flag_comment)))
				
			(cond( (not (string-equal *operator* (string " ")))
				(progn 
							
					(setq other (concatenate 'string other (string *operator*))))))
					
			 (if (or (string-equal *operator* #\SPACE) (string-equal *operator* #\newline) (string-equal *operator* #\tab))
				(progn
					
					(cond
					 	((string-equal other "and")    (print "KW_AND"))
					 	((string-equal other "or") 	(print "KW_OR"))
					 	((string-equal other "not")	(print "KW_NOT"))
					 	((string-equal other "equal")	(print "KW_EQUAL"))
					 	((string-equal other "less")	(print "KW_LESS"))
					 	((string-equal other "list")	(print "KW_NIL"))
					 	((string-equal other "append") (print "KW_APPEND"))
					 	((string-equal other "concat") (print "KW_CONCAT"))
					 	((string-equal other "set")	(print "KW_SET"))
					 	((string-equal other "deffun") (print "KW_DEFFUN"))
					 	((string-equal other "for")	(print "KW_FOR"))
					 	((string-equal other "if")	(print "KW_IF"))
					 	((string-equal other "exit")	(print "KW_EXIT"))
					 	((string-equal other "load")	(print "KW_LOAD"))
					 	((string-equal other "disp")	(print "KW_DISP"))
					 	((string-equal other "true")	(print "KW_TRUE"))
					 	((string-equal other "false")	(print "KW_FALSE")))
					 	
					
					(loop for y from 0 to (- (length other) 1)
				 		do(defparameter *literal* (subseq other y (+ y 1)))
				 		  (if (digit-char-p (coerce *literal* 'character))
				 		  	(progn
				 				(setf _is_digit (+ _is_digit 1)))))
				 				
				 	(loop for y from 0 to (- (length other) 1)
				 		do(defparameter *literal* (subseq other y (+ y 1)))
				 		  (if (alpha-char-p (coerce *literal* 'character))
				 		  	(progn
				 				(setf _is_alpha (+ _is_alpha 1)))))	  
				 	(if (equal _is_digit (length other))
				 		(print "VALUE"))		
					(if (equal _is_alpha (length other))
				 		(print "IDENTIFIER"))
				 	(setf _is_digit 0)
				 	(setf _is_alpha 0)
					(setf other ""))
					
					)
				
				
		)
)
	
	

;; reading from file

(defun read_file(file_name)
	(let ((in (open file_name :if-does-not-exist nil)))
	    (when in
	    	(loop for line = (read-line in nil)
	    	
	    	while line do (tokenize line))
	    	(close in)
	 )
)
)



(defun gppinterpreter()
	
	(format t "Are you using terminal or file?(1 for terminal 2 for file or 3 for exit)~%")
	(defvar *choice*(read))
	
	(when (not(= *choice* 3))
		(if (= *choice* 1)
	 	   (progn
 			(format t "Terminal input=>")
 			(defvar *terminal_input* (read-line))
 			(tokenize *terminal_input*)))
		(if (= *choice* 2)
 		   (progn
			(format t "File name=>")
			(defvar *file_name* (read-line))
			(read_file *file_name*)))))		
(gppinterpreter)
