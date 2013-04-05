;Assignment 5
;Tic Tac Toe

;checks if argument is atomic
(define (atom? x)
  (not (or (pair? x) (null? x))))

;Tic Tac Toe Board combinations
(define board '((1 2 3) (4 5 6) (7 8 9)
                (1 4 7) (2 5 8) (3 6 9)
                (1 5 9) (3 5 7)
                ))

;substitutes old with new in the given list
(define subst 
  (lambda (new old l) 
    (cond ((null? l) (quote ())) 
          ((atom? (car l)) 
           (cond ((eq? (car l) old) 
                  (cons new (subst new old (cdr l)))) 
                 (else (cons (car l) (subst new old (cdr l)))))) 
          (else (cons (subst new old (car l)) 
                      (subst new old (cdr l)))))))


;checks if an element is part of list
(define member?
  (lambda (e board)
    (cond
      ((not (list? board)) #f)
      ((null? board) #f)
      ((null? e) #t)
      ((equal? (car board) e))
      (else (or (member? e (car board))
                (member? e (cdr board)))))))

;handles each players turn
(define (game turn)
  (cond ((= (modulo turn 2) 0) ;Player X's turn
         (display "Player X's turn:")
         (let ((input (read)))
           (if (not (member? input board)) ;check if invalid move
               "Invalid move. Please try again" ;display error if invalid move
               (set! board (subst 'X input board))) 
         (display board)
         (game (+ turn 1)))
        ((= (modulo turn 2) 1) ;Player O's turn
         (display "Player O's turn:")
         (set! board (subst 'O (read) board))
         (display board)
         (game (+ turn 1)))
        (else "error")))
         

;starts the game
(define (startGame)
  (display board)
  (display "\n")
  (game 0)
  )

