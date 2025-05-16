;;=====================================================================
;; LISP READER & LEXER - new version 160514
;;=====================================================================

;;=====================================================================
;; Help functions
;;=====================================================================
;; ctos         convert a character to a string
;; str_con      concatenate 2 strings str, c
;; whitespace   is c whitespace?
;;=====================================================================

(defun ctos (c)        (make-string 1 :initial-element c))
(defun str-con (str c) (concatenate 'string str (ctos c)))
(defun whitespace (c)  (member c '(#\Space #\Tab #\Newline)))

;;=====================================================================
;; get-wspace   remove whitespace
;;=====================================================================

(defun get-wspace (ip)
   (setf c (read-char ip nil 'EOF))
   (cond
           ((whitespace c)  (get-wspace ip))
           (t                             c)
   )
)

;;=====================================================================
;; Read an Identifier         Compare this with C's do-while construct
;;=====================================================================

(defun get-name (ip lexeme c)
   (setf lexeme (str-con lexeme c))
   (setf c      (read-char ip nil 'EOF))
   (cond
                ((alphanumericp c)  (get-name ip lexeme c))
                (t                  (list        c lexeme))
   )
)

;;=====================================================================
;; Read a Number              Compare this with C's do-while construct
;;=====================================================================

(defun get-number (ip lexeme c)
   (setf lexeme (str-con lexeme c))
   (setf c      (read-char ip nil 'EOF))
   (cond
         ((not (null (digit-char-p c)))  (get-number ip lexeme c))
         (t                              (list          c lexeme))
   )
  )

;;=====================================================================
;; Read a single character or ":="
;;=====================================================================

(defun get-symbol (ip lexeme c)
   (setf lexeme (str-con lexeme c))
   (setf c1 c)
   (setf c (read-char ip nil 'EOF))
   (cond
         ((and (char= c1 #\:) (char= c #\=))  (get-symbol ip lexeme c))
         (t                                   (list          c lexeme))
   )
)

;;=====================================================================
;; Read a Lexeme                       lexeme is an "accumulator"
;;                                     Compare this with the C version
;;=====================================================================

(defun get-lex (state)
   (setf lexeme "")
   (setf ip (pstate-stream   state))
   (setf c  (pstate-nextchar state))
   (if (whitespace c) (setf c (get-wspace ip)))
   (cond
         ((eq c 'EOF)                     (list 'EOF ""))
         ((alpha-char-p c)                (get-name   ip lexeme c))
         ((not (null (digit-char-p c)))   (get-number ip lexeme c))
         (t                               (get-symbol ip lexeme c))
   )
)

;;=====================================================================
; map-lexeme(lexeme) returns a list: (token, lexeme)
;;=====================================================================

(defun map-lexeme (lexeme)
(format t "Symbol: ~S ~%" lexeme)
   (list (cond
         ((string=   lexeme "program")  'PROGRAM)
         ((string=   lexeme "var")      'VAR)
         ((string=   lexeme "input")    'INPUT)
         ((string=   lexeme "output")   'OUTPUT)
         ((string=   lexeme "begin")   'BEGIN)
         ((string=   lexeme "end")     'END)
         ((string=   lexeme "boolean") 'BOOLEAN)
         ((string=   lexeme  "integer") 'INTEGER)
         ((string=   lexeme "real")     'REAL)
         ((string=   lexeme ":=")   'ASSIGN)
         ((string=   lexeme "undef")  'UNDEF)
         ((string=   lexeme "predef")  'PREDEF)
         ((string=   lexeme "tempty") 'TEMPTY)
         ((string=   lexeme "error")   'ERROR)
         ((string=   lexeme "type") 'TYPE)
         ((string=   lexeme  "$") 'DOLLAR)
         ((string=   lexeme   "(")  'LPAREN)
         ((string=   lexeme   ")")  'RPAREN)
         ((string=   lexeme   "*") 'STAR)
         ((string=   lexeme   "+")  'PLUS)
         ((string=   lexeme   ",")  'COMMA)
         ((string=   lexeme   "-")  'DASH)
         ((string=   lexeme   ".")  'DOT)
         ((string=   lexeme   "/")  'SLASH)
         ((string=   lexeme   ":")  'COLON)
         ((string=   lexeme   ";")  'SEMICOLON)
         ((string=   lexeme   "=")  'EQUAL)
         ((string=   lexeme "")	        'EOF)
         ((is-id     lexeme)            'ID)
         ((is-number lexeme)            'NUM)
         (t                             'UNKNOWN)
         )
    lexeme)
)

;;=====================================================================
; ID is [A-Z,a-z][A-Z,a-z,0-9]*          number is [0-9][0-9]*
;;=====================================================================

(defun is-id (str)
   (and (alpha-char-p (char str 0)) (every #'alphanumericp str))
)

(defun is-number (str)
   (every #'digit-char-p str)
)

;;=====================================================================
; THIS IS THE PARSER PART
;;=====================================================================

;;=====================================================================
; Create a stucture - parse state descriptor
;;=====================================================================
; lookahead is the list (token, lexeme)
; stream    is the input filestream
; nextchar  is the char following the last lexeme read
; status    is the parse status (OK, NOTOK)
; symtab    is the symbol table
;;=====================================================================

(defstruct pstate
    (lookahead)
    (stream)
    (nextchar)
    (status)
    (symtab)
)

;;=====================================================================
; Constructor for the structure pstate - initialise
; stream to the input file stream (ip)
;;=====================================================================

(defun create-parser-state (ip)
   (make-pstate
      :stream        ip
      :lookahead     ()
      :nextchar      #\Space
      :status        'OK
      :symtab        ()
    )
)

;;=====================================================================
; SYMBOL TABLE MANIPULATION
;;=====================================================================

;;=====================================================================
; token  - returns the token  from (token lexeme)(reader)
; lexeme - returns the lexeme from (token lexeme)(reader)
;;=====================================================================

(defun token  (state)
   (first (pstate-lookahead state))
)

(defun lexeme (state)
   (second (pstate-lookahead state))
)

;;=====================================================================
; symbol table manipulation: add + lookup + display
;;=====================================================================

(defun symtab-add (state id)
    (if (not(eq (token state) 'ID))
        (synerr1 state 'ID)
    )
    (if (symtab-member state id)
        (semerr1 state)
        (setf (pstate-symtab state) (append (pstate-symtab state) (list id)))

    )
)

(defun symtab-member (state id)
    (find id (pstate-symtab state) :test #'equal)
)

(defun symtab-display (state)
   (format t "------------------------------------------------------~%")
   (format t "Symbol Table is: ~S ~%" (pstate-symtab state))
   (format t "------------------------------------------------------~%")
)

;;=====================================================================
; Error functions: Syntax & Semantic
;;=====================================================================

(defun synerr1 (state symbol)
    (format t "*** Syntax error:   Expected ~8S found ~8S ~%"
           symbol (lexeme state))
    (setf (pstate-status state) 'NOTOK)
)

(defun synerr2 (state)
    (format t "*** Syntax error:   Expected TYPE     found ~S ~%"
           (lexeme state))
    (setf (pstate-status state) 'NOTOK)
)

(defun synerr3 (state)
    (format t "*** Syntax error:   Expected OPERAND  found ~S ~%"
           (lexeme state))
    (setf (pstate-status state) 'NOTOK)
)

(defun semerr1 (state)
    (format t "*** Semantic error: ~S already declared.~%"
                (lexeme state))
    (setf (pstate-status state) 'NOTOK)
)

(defun semerr2 (state)
    (format t "*** Semantic error: ~S not declared.~%"
          (lexeme state))
    (setf (pstate-status state) 'NOTOK)
)

(defun semerr3 (state symbol)
    (format t "*** Semantic error: found ~8S expected ~S.~%"
          (lexeme state) symbol)
    (setf (pstate-status state) 'NOTOK)
)

;;=====================================================================
; The return value from get-token is always a list. (token lexeme)
;;=====================================================================

(defun get-token (state)
  (let    ((result (get-lex state)))
    (setf (pstate-nextchar  state) (first result))
    (setf (pstate-lookahead state) (map-lexeme (second result)))
  )
 )

;;=====================================================================
; match compares lookahead with symbol (the expected token)
; if symbol == lookahead token ==> get next token else Syntax error
;;=====================================================================

(defun match (state symbol)
   (if (eq symbol (token state))
       (get-token  state)
       (synerr1    state symbol)
       )
)

;;=====================================================================
; THE GRAMMAR RULES
;;=====================================================================

;;=====================================================================
; <stat-part>     --> begin <stat-list> end .
; <stat-list>     --> <stat> | <stat> ; <stat-list>
; <stat>          --> <assign-stat>
; <assign-stat>   --> id := <expr>
; <expr>          --> <term>     | <term> + <expr>
; <term>          --> <factor>   | <factor> * <term>
; <factor>        --> ( <expr> ) | <operand>
; <operand>       --> id | number
;;=====================================================================

(defun id-exists (state)
    ( if ( not ( symtab-member state ( lexeme state ) ) )
        ( semerr2 state )
    )
)

(defun expr-aux(state)
    (match state 'PLUS)
    (expr state)
)

(defun expr(state)
    (term state)
    (if (eq (token state) 'PLUS)
        (expr-aux state)
    )
)

(defun term-aux(state)
    (match state 'STAR)
    (term state)
)

(defun term(state)
    (factor state)
    (if (eq (token state) 'STAR)
        (term-aux state)
    )
)

(defun factor-aux(state)
    (match state 'LPAREN)
    (expr state)
    (match state 'RPAREN)
)

(defun factor(state)
    (if (eq (token state) 'LPAREN)
        (factor-aux state)
        (operand state)
    )
)

(defun operand (state)
    (cond
        ((eq (token state) 'ID) (id-exists state) (match state 'ID))
        ((eq (token state) 'NUM) (match state 'NUM))
        (t (synerr3 state))
    )
)


(defun assign-state (state)
    (if (eq (token state) 'ID)
        (id-exists state)
    )

    (match state 'ID)
    (match state 'ASSIGN)
    (expr state)
)

(defun stat (state)
    (assign-state state)
)

(defun stat-list-aux (state)
    (match state 'SEMICOLON)
    (stat-list state)
)

(defun stat-list (state)
    (stat state)
    (if (eq (token state) 'SEMICOLON)
        (stat-list-aux state)
    )
)

(defun stat-part (state)
    (match state 'BEGIN)
    (stat-list state)
    (match state 'END)
    (match state 'DOT)
)

;;=====================================================================
; <var-part>     --> var <var-dec-list>
; <var-dec-list> --> <var-dec> | <var-dec><var-dec-list>
; <var-dec>      --> <id-list> : <type> ;
; <id-list>      --> id | id , <id-list>
; <type>         --> integer | real | boolean
;;=====================================================================

(defun typ (state)
    (cond   ((eq (token state) 'REAL)
                (match state 'REAL)
            )
            ((eq (token state) 'INTEGER)
                (match state 'INTEGER)
            )
            ((eq (token state) 'BOOLEAN)
                (match state 'BOOLEAN)
            )
            (t
                (synerr2 state)
            )
    )
)

(defun id-list-aux (state) (match state 'COMMA) (id-list state))

(defun id-list (state)
    (symtab-add state (lexeme state))

    (match state 'ID)

    (if (eq (token state) 'COMMA)
        (id-list-aux state)
    )
)

(defun var-dec (state)
    (id-list state)
    (match state 'COLON)
    (typ state)
    (match state 'SEMICOLON)
)

(defun var-dec-list (state)
    (var-dec state)
    (if (eq (token state) 'ID)
        (var-dec-list state)
    )
)

(defun var-part (state)
    (match state 'VAR)
    (var-dec-list state)
)

;;=====================================================================
; <program-header>
;;=====================================================================

(defun program-header (state)
    (match state 'PROGRAM)
    (match state 'ID)
    (match state 'LPAREN)
    (match state 'INPUT)
    (match state 'COMMA)
    (match state 'OUTPUT)
    (match state 'RPAREN)
    (match state 'SEMICOLON)
)

;;=====================================================================
; <EOF-validator>
;;=====================================================================
(defun EOF-validator-aux (state)
   (semerr3 state 'EOF)
   (get-token state)
   (EOF-validator state)
)

(defun EOF-validator (state)
    (if (not(eq (token state) 'EOF))
        (EOF-validator-aux state)
    )
)

;;=====================================================================
; <program> --> <program-header><var-part><stat-part>
;;=====================================================================
(defun program (state)
   (program-header state)
   (var-part       state)
   (stat-part      state)
   (EOF-validator  state)
   (symtab-display state)
)

;;=====================================================================
; THE PARSER - parse a file
;;=====================================================================

;;=====================================================================
; Test parser for file name input
;;=====================================================================

(defun parse (filename)
   (format t "~%------------------------------------------------------")
   (format t "~%--- Parsing program: ~S " filename)
   (format t "~%------------------------------------------------------~%")
   (with-open-file (ip (open filename) :direction :input)
      (setf state (create-parser-state ip))
      (setf (pstate-nextchar state) (read-char ip nil 'EOF))
      (get-token state)
      (program   state)
      )
   (if (eq (pstate-status state) 'OK)
      (format t "Parse Successful. ~%")
      (format t "Parse Fail. ~%")
      )
   (format t "------------------------------------------------------~%")
)

;;=====================================================================
; THE PARSER - parse all the test files
;;=====================================================================

(defun parse-all ()
   (mapcar #'parse
      (sort (directory "testfiles/*.pas") #'string-lessp :key #'namestring))
)

;;=====================================================================
; THE PARSER - test all files
;;=====================================================================

(parse-all)

;;=====================================================================
; THE PARSER - test a single file
;;=====================================================================

;(parse "testfiles/testok1.pas")

;;=====================================================================
; THE PARSER - end of code
;;=====================================================================
