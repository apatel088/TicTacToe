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
  
  ;Player X's turn
  (cond ((= (modulo turn 2) 0) 
         (display "Player X's turn:")
         (let ((input (read)))
           (cond 
             ((not (member? input board)) ;check if invalid move
              (display "Invalid move. Please try again\n")
              (game turn)) ;restart at the same turn
             (else ;otherwise update and print board
              (set! board (subst 'X input board)) 
              (display board)
              (display "\n")
              (if (member? '(x x x) board) "Player X Wins!!"
                  (game (+ turn 1))))))) ;move to the next turn
        
        ;Player O's Turn
        ((= (modulo turn 2) 1)
         (display "Player O's turn:")
         (let ((input (read)))
           (cond 
             ((not (member? input board)) ;check if invalid move
              (display "Invalid move. Please try again\n")
              (game turn)) ;restart at the same turn
             (else ;otherwise update and print board
              (set! board (subst 'O input board)) 
              (display board)
              (display "\n")
              (if (member? '(o o o) board) "Player O Wins!!"
                  (game (+ turn 1))))))) ;move to the next turn
        
        ;if something goes wrong
        (else "error")))
         



;starts the game
(define (startGame)
  (display board)
  (display "\n")
  (game 0)
  )

