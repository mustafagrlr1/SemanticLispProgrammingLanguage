
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
	(setq _value1 0)
	(setq _value2 0)
	(defparameter *list* (list))
	(defparameter *mylist* (list))
	
	(loop for x from 0 to (- (length line) 1)
		do(defparameter *operator* (subseq line x (+ x 1)))
			;;(format t "~a mustafa " *operator*)
			(setq *flag* 0)
			(setq *done* 0)
			(cond ( (and (string-equal *operator* #\*) (equal flag_asterix_end T))
					(setf *flag* 1)
					(setf flag_asterix T)
					(setf flag_asterix_end Nil))
				((and (string-equal *operator* #\*) (equal flag_asterix T)) 
					(setf *flag* 1)
					(setf flag_asterix_end T)
					(setf flag_asterix Nil)
					(push "OP_DBL" *list*)
					(terpri))
				((and (not (string-equal *operator* #\*)) (equal flag_asterix T))
					(setf *flag* 1)
					(setf flag_asterix_end T)
					(setf flag_asterix Nil)
					(push "OP_MULT" *list*)))
			(if (string-equal *operator*  #\+)
				(progn
					(setf *flag* 1)
					(push "OP_PLUS" *list*)))
			(if (string-equal *operator* #\-)
				(progn
					(setf *flag* 1)
					(push "OP_MINUS" *list*)))
			(if (string-equal *operator* #\/)
				(progn
					(setf *flag* 1)
					(push "OP_DIV" *list*)))
			
			(if (string-equal *operator* #\()
				(progn
					(setf *flag* 1)
					(push "OP_OP" *list*)))
			(if (string-equal *operator* #\))
				(progn
					(setf *flag* 1)
					(push "OP_CP" *list*)))
			(if (and (string-equal *operator* #\") (equal flag_quote Nil))
				(progn
					(setf *flag* 1)
					(push "OP_OC" *list*)))
			(if (and (string-equal *operator* #\") (equal flag_quote T))
				(progn
					(setf *flag* 1)
					(push "OP_CP" *list*)))
			(if (string-equal *operator* #\,)
				(progn
					(setf *flag* 1)
					(push "OP_COMMA" *list*)))
			(if (string-equal *operator* #\;)
				(progn
					(setf *flag* 1)		
					(setf flag_comment T)))
			(if (and (string-equal *operator* #\;) (equal flag_comment T))
				(progn
					(setf *flag* 1)
					(push "COMMENT" *list*)
					(return flag_comment)))
				
			(if (and (equal *flag* 0) (not (string-equal *operator* (string " "))))
				(progn 	
					(setq other (concatenate 'string other (string *operator*)))))
					
			 (if(and (not (equal other "")) (or (string-equal *operator* #\SPACE) (string-equal *operator* #\newline) (string-equal *operator* #\tab)))
				(progn
					(cond
					 	((string-equal other "and")    (push "KW_AND" *list*) (setf *done* 1) )
					 	((string-equal other "or") 	(push "KW_OR" *list*) (setf *done* 1))
					 	((string-equal other "not")	(push "KW_NOT" *list*) (setf *done* 1))
					 	((string-equal other "equal")	(push "KW_EQUAL" *list*) (setf *done* 1))
					 	((string-equal other "less")	(push "KW_LESS" *list*) (setf *done* 1))
					 	((string-equal other "list")	(push "KW_NIL" *list*) (setf *done* 1))
					 	((string-equal other "append") (push "KW_APPEND" *list*) (setf *done* 1))
					 	((string-equal other "concat") (push "KW_CONCAT" *list*) (setf *done* 1))
					 	((string-equal other "set")	(push "KW_SET" *list*) (setf *done* 1))
					 	((string-equal other "deffun") (push "KW_DEFFUN" *list*) (setf *done* 1))
					 	((string-equal other "for")	(push "KW_FOR" *list*) (setf *done* 1))
					 	((string-equal other "if")	(push "KW_IF" *list*) (setf *done* 1))
					 	((string-equal other "exit")	(push "KW_EXIT" *list*) (setf *done* 1))
					 	((string-equal other "load")	(push "KW_LOAD" *list*) (setf *done* 1))
					 	((string-equal other "disp")	(push "KW_DISP" *list*) (setf *done* 1))
					 	((string-equal other "true")	(push "KW_TRUE" *list*) (setf *done* 1))
					 	((string-equal other "false")	(push "KW_FALSE" *list*) (setf *done* 1)))
					 	
					
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
				 	(if(and (equal *done* 0) (equal _is_digit (length other)))
						(progn
				 		(push "VALUE" *list*)
						(push other *list*)))		
					(if(and (equal *done* 0) (equal _is_alpha (length other)))
				 		(push "IDENTIFIER" *list*))
				 	(setf _is_digit 0)
				 	(setf _is_alpha 0)
					(setf other ""))
					
					)
				
				
		)
		(setf *list* (reverse *list*)) 
		


		(if (string-equal (nth 0 *list*) "COMMENT")
			(progn
				(exit)))
		
		(if (string-equal (nth 0 *list*) "OP_OP")
			(progn
				(if (string-equal (nth 1 *list*) "EXIT")
					(progn
						(if (string-equal (nth 2 *list*) "OP_CP")
							(progn
								(exit)))))
				(if (string-equal (nth 1 *list*) "OP_PLUS")
					(progn
						(if(and (string-equal (nth 2 *list*) "VALUE") (string-equal (nth 4 *list*) "VALUE"))				
							(progn	
								(if (string-equal (nth 6 *list*) "OP_CP")
									(progn
										(format t "Syntax OK!")
										(terpri)
										(format t "Result = ~d ~%" (+ (parse-integer (nth 3 *list*)) (parse-integer (nth 5 *list*))))
									))))))
				(if (string-equal (nth 1 *list*) "OP_MINUS")
					(progn
						(if(and (string-equal (nth 2 *list*) "VALUE") (string-equal (nth 4 *list*) "VALUE"))				
							(progn	
								(if (string-equal (nth 6 *list*) "OP_CP")
									(progn
										(format t "Syntax OK!")
										(terpri)
										(format t "Result = ~d ~%" (- (parse-integer (nth 3 *list*)) (parse-integer (nth 5 *list*))))
										))))))				
				(if (string-equal (nth 1 *list*) "OP_MULT")
					(progn
						(if(and (string-equal (nth 2 *list*) "VALUE") (string-equal (nth 4 *list*) "VALUE"))				
							(progn	
								(if (string-equal (nth 6 *list*) "OP_CP")
									(progn
										(format t "Syntax OK!")
										(terpri)
										(format t "Result = ~d ~%" (* (parse-integer (nth 3 *list*)) (parse-integer (nth 5 *list*))))
										))))))
				(if (string-equal (nth 1 *list*) "OP_DIV")
					(progn
						(if(and (string-equal (nth 2 *list*) "VALUE") (string-equal (nth 4 *list*) "VALUE"))				
							(progn	
								(if (string-equal (nth 6 *list*) "OP_CP")
									(progn
										(format t "Syntax OK!")
										(terpri)
										(format t "Result = ~d ~%" (/ (parse-integer (nth 3 *list*)) (parse-integer (nth 5 *list*))))
										))))))
				(if (string-equal (nth 1 *list*) "OP_DBMULT")
					(progn
						(if(and (string-equal (nth 2 *list*) "VALUE") (string-equal (nth 4 *list*) "VALUE"))				
							(progn	
								(if (string-equal (nth 6 *list*) "OP_CP")
									(progn
										(format t "Syntax OK!")
										(terpri)
										(format t "Result = ~d ~%" (** (parse-integer (nth 3 *list*)) (parse-integer (nth 5 *list*))))
										))))))

				(if (string-equal (nth 1 *list*) "KW_LESS")
					(progn
						(if(and (string-equal (nth 2 *list*) "VALUE") (string-equal (nth 4 *list*) "VALUE"))
							(progn	
								(if (string-equal (nth 6 *list*) "OP_CP")
										(progn
											(format t "Syntax OK!")
											(terpri)
											(format t "Result = ~d ~%" (< (parse-integer (nth 3 *list*)) (parse-integer (nth 5 *list*))))
											))))))
				(if (string-equal (nth 1 *list*) "KW_AND")
					(progn
						(format t "Syntax OK!")
						(terpri)
						(if(and (string-equal (nth 2 *list*) "VALUE") (string-equal (nth 3 *list*) "VALUE"))
							(progn	
								(if (string-equal (nth 4 *list*) "OP_CP")
										(progn
											
											(if (and (nth 3 *list*) (nth 5 *list*))
												(format t "Result = ~d ~%" "true")
                        						(format t "Result = ~d ~%" "false"))
											
											))))))
				(if (string-equal (nth 1 *list*) "KW_OR")
					(progn
						(format t "Syntax OK!")
						(terpri)
						(if(and (string-equal (nth 2 *list*) "VALUE") (string-equal (nth 3 *list*) "VALUE"))
							(progn	
								(if (string-equal (nth 4 *list*) "OP_CP")
										(progn
											
											(if (and (nth 3 *list*) (nth 5 *list*))
												(format t "Result = ~d ~%" "true")
                        						(format t "Result = ~d ~%" "false"))
											))))))
				(if (string-equal (nth 1 *list*) "KW_NOT")
					(progn
						(format t "Syntax OK!")
						(terpri)
						(if(string-equal (nth 2 *list*) "VALUE")
							(progn	
								(if (string-equal (nth 4 *list*) "OP_CP")
										(progn
											
											(if (not (nth 3 *list*))
												(format t "Result = ~d ~%" "true")
                        						(format t "Result = ~d ~%" "false"))
											))))))
				
				(if (string-equal (nth 1 *list*) "KW_EQUAL")
					(progn
						(if(and (string-equal (nth 2 *list*) "VALUE") (string-equal (nth 4 *list*) "VALUE"))
							(progn	
							(format t "Syntax OK!")
							(terpri)
							(if (equal (nth 3 *list*) (nth 5 *list*))
								(format t "Result = ~d ~%" "true")
                        		(format t "Result = ~d ~%" "false"))))
						(if(and (string-equal (nth 2 *list*) "KW_TRUE") (string-equal (nth 3 *list*) "KW_TRUE"))	
							(progn
								(format t "Syntax OK!")
								(terpri)
								(format t "Result = ~d ~%" "true")))
						(if(and (string-equal (nth 2 *list*) "KW_TRUE") (string-equal (nth 3 *list*) "KW_FALSE"))	
							(progn
								(format t "Syntax OK!")
								(terpri)
								(format t "Result = ~d ~%" "false")))
						(if(and (string-equal (nth 2 *list*) "KW_FALSE") (string-equal (nth 3 *list*) "KW_FALSE"))	
							(progn
								(format t "Syntax OK!")
								(terpri)
								(format t "Result = ~d ~%" "true")))
						(if(and (string-equal (nth 2 *list*) "KW_FALSE") (string-equal (nth 3 *list*) "KW_TRUE"))	
							(progn
								(format t "Syntax OK!")
								(terpri)
								(format t "Result = ~d ~%" "false")))))
						
			))
)
	
	

(defun lexer (*filename* *args*)
    (with-open-file (my-stream 
        *filename*
        :direction :output
        :if-does-not-exist :create
        :if-exists :supersede)

        (if (equal *args* NIL) 
            (progn
                (loop
                    do 
                    (defparameter *line* (read-line))
                    (tokenize *line*)
                )
            )
            (progn
                (let ((in (open *args* :if-does-not-exist nil)))
                    (when in
                        (loop for line = (read-line in nil)
                            while line 
                                do (tokenize line)
                        )
                        (close in)
                    )
                )
            )
        )
    )
)

(lexer "ders.txt" (nth 0 *args*))
