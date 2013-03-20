;Written by Michael Robertson (mjr127)
;EECS 325 Parser Project 1

;loads the parser
(load "verySimpleParser.scm")

;base function the user uses, calls the interpret statement list function
(define interpret
	(lambda (filename)
		(lookup 'return (interpret_statement_list (parser filename) (newenvironment)))))
		
;returns the environment
;takes a parsetree as input
(define interpret_statement_list
	(lambda (parsetree environment)
		(cond
			((null? parsetree) environment)
				(else (interpret_statement_list (cdr parsetree) (interpret_statement (car parsetree) environment))))))
				
;this will interpret individual statements
;statements we need to interpret:
;	var
;	=
;	if
;	return
(define interpret_statement
	(lambda (statement environment)
		(cond
			((null? statement) environment)
			((eq? '= (car statement)) (interpret_assign statement environment))
			((eq? 'var (car statement)) (interpret_declare statement environment))
			((eq? 'if (car statement)) (interpret_if statement environment))
			((eq? 'return (car statement)) (interpret_return statement environment)))))

;this is =
;if the variable on the left isn't in the environment, it throws and error
;if the right hand side of the = is a math statement, evaluate that statement and bind it to the variable
;otherwise bind it to the variable (redundant actually)			
(define interpret_assign
	(lambda (statement environment)
		(cond
			((null? statement) environment)
			((not (exists? (operand1 statement) environment)) (error 'Not_Declared_Error))
			((math_expression? (operand2 statement)) (bind (operand1 statement) (evaluate (operand2 statement) environment) environment))
			(else (bind (operand1 statement) (operand2 statement) environment)))))
			
;this is var
;if the lenght of the parse is 3 (var x (expression)), then evaluate that expression and assign it to x
;otherwise, assign x with a base value of 0
(define interpret_declare
	(lambda (statement environment)
		(cond
			((null? statement) environment)
			((= (length statement) 3) (bind (operand1 statement) (evaluate (operand2 statement) environment) environment))
			(else (bind (operand1 statement) 0 environment)))))
			
;this is if
;evaluate the cond and first statement
;if there is an else statement (if cond stmt2 else, so length is 4) evaluate that else statement
(define interpret_if
	(lambda (statement environment)
		(cond
			((null? statement) (environment))
			((evaluate (operand1 statement) environment) (interpret_statement (operand2 statement) environment))
			((= (length statement) 4) (interpret_statement (operand3 statement) environment))
			(else environment))))
			
;this is return
;bind 'return with the value of the evaluation of the return statement
(define interpret_return
	(lambda (statement environment)
		(cond
			((null? statement) (environment))
			(else (bind 'return (evaluate (operand1 statement) environment) environment)))))
			
;this returns true if it is a mathematical expression, otherwise false
(define math_expression?
	(lambda (expr)
		(cond
			((null? expr) #f)
			((number? expr) #t)
			((not (pair? expr)) #f)
			((null? (cdr expr)) (math_expression? (car expr)))
			((not (= (length expr) 3)) #f)
			((or (eq? '+ (operator expr)) (eq? '- (operator expr)) (eq? '* (operator expr))
				(eq? '/ (operator expr)) (eq? '% (operator expr)) 
					(eq? '== (operator expr)) (eq? '!= (operator expr)) (eq? '< (operator expr))
					(eq? '> (operator expr)) (eq? '<= (operator expr)) (eq? '>= (operator expr))
				(and (math_expression? (operand1 expr) (math_expression? (operand2 expr))))))
			(else #f))))
			
;this will evaluate boolean and mathematical expressions, including plain numbers and variables
(define evaluate
	(lambda (expr environment)
		(cond
			((number? expr) expr)
			((not (pair? expr)) (lookup expr environment))
			((not (math_expression? expr)) 0)
			((null? (cdr expr)) (evaluate (car expr)))
			((eq? '+ (operator expr)) (+ (evaluate (operand1 expr) environment) (evaluate (operand2 expr) environment)))
			((eq? '- (operator expr)) (- (evaluate (operand1 expr) environment) (evaluate (operand2 expr) environment)))
			((eq? '* (operator expr)) (* (evaluate (operand1 expr) environment) (evaluate (operand2 expr) environment)))
			((eq? '/ (operator expr)) (quotient (evaluate (operand1 expr) environment) (evaluate (operand2 expr) environment)))
			((eq? '% (operator expr)) (remainder (evaluate (operand1 expr) environment) (evaluate (operand2 expr) environment)))
			((eq? '== (operator expr)) (= (evaluate (operand1 expr) environment) (evaluate (operand2 expr) environment)))
			((eq? '!= (operator expr)) (not (= (evaluate (operand1 expr) environment) (evaluate (operand2 expr) environment))))
			((eq? '< (operator expr)) (< (evaluate (operand1 expr) environment) (evaluate (operand2 expr) environment)))
			((eq? '> (operator expr)) (> (evaluate (operand1 expr) environment) (evaluate (operand2 expr) environment)))
			((eq? '<= (operator expr)) (<= (evaluate (operand1 expr) environment) (evaluate (operand2 expr) environment)))
			((eq? '>= (operator expr)) (>= (evaluate (operand1 expr) environment) (evaluate (operand2 expr) environment)))
			(else -999))))
			
;helper method, gets the operator
(define operator
	(lambda (expr)
		(car expr)))

;helper method, gets operand 1
(define operand1
	(lambda (expr)
		(car (cdr expr))))

;helper method, gets operand 2
(define operand2
	(lambda (expr)
		(car (cdr (cdr expr)))))
		
;helper method, gets operand 3 (else statement for if statements)
(define operand3
	(lambda (expr)
		(car (cdr (cdr (cdr expr))))))
		
;helper method, checks if it is in environment
(define exists?
	(lambda (x environment)
		(cond
			((null? environment) #f)
			((eq? x (getVar environment)) #t)
			(else (exists? x (cdr environment))))))
			
;environment changing methods

;binds to the environment
(define bind
	(lambda (x1 x2 environment)
		(cond
			((null? x2) (environment))
			(else (cons (cons x1 (cons x2 '())) environment)))))

;looks up a variable in the environment
(define lookup
	(lambda (x environment)
		(cond
			((null? environment) (error 'Does_not_exist))
			((eq? (getVar environment) x) (getVal environment))
			(else (lookup x (cdr environment))))))

;helper, gets the variable name			
(define getVar
	(lambda (environment)
		(car (car environment))))

;helper, gets the variable value
(define getVal
	(lambda (environment)
		(car (cdr (car environment)))))
		
;makes the new environment
(define newenvironment
	(lambda ()
		'()))

;------------------------------------------------------------------------------------------------------