;Assignment 5
;Tic Tac Toe

;starts the game
(define (startGame)
  (display "Players must enter the value of the location they wish to place his/her marker upon\n")
  (display "type 'exit' to end the game.\n\n")
  (printBoard start)
  (display "\n")
  (game 0 start)
  )

;checks if argument is atomic
(define (atom? x)
  (not (or (pair? x) (null? x))))

;Tic Tac Toe Board combinations
(define start '((1 2 3) (4 5 6) (7 8 9)
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
(define (game turn board)
  (cond
    
    ;Tie Game
    ((= turn 9)
     (display "Tie Game!"))
    
    ;Player X's turn
    ((= (modulo turn 2) 0) 
         (display "Player X's turn:")
         (let ((input (read)))
           (cond 
             ((equal? input 'exit) ;user wants to exit
              (display "Game ended"))
             ((and (member? input board) (number? input)) ;check if valid move
              (printBoard (subst 'X input board)) 
              ;(display board)
              (display "\n")
              (if (member? '(x x x) (subst 'X input board)) "Player X Wins!!"
                  (game (+ turn 1) (subst 'X input board)))) ;move to the next turn
             (else ;invalid move
              (display "Invalid move. Please try again\n")
              (game turn board))))) ;restart at the same turn
    
    ;Player O's turn
    ((= (modulo turn 2) 1)
         (display "Player O's turn:")
         (let ((input (read)))
           (cond
             ((equal? input 'exit) ;user wants to exit
              (display "Game ended"))
             ((and (member? input board) (number? input)) ;check if valid move
              (printBoard (subst 'O input board)) 
              ;(display board)
              (display "\n")
              (if (member? '(o o o) (subst 'O input board)) "Player O Wins!!"
                  (game (+ turn 1) (subst 'O input board)))) ;move to the next turn
             (else ;invalid move
              (display "Invalid move. Please try again\n")
              (game turn board))))) ;restart at the same turn
        
        ;if something goes wrong
        (else "error")))

;pretty prints the board
(define (printBoard board)
  (display (caar board)) 
  (display " | ")
  (display (cadar board))
  (display " | ")
  (display (caddar board))
  (display "\n---------\n")
  (display (caadr board))
  (display " | ")
  (display (cadadr board))
  (display " | ")
  (display (caddar (cdr board)))
  (display "\n---------\n")
  (display (caar (cddr board)))
  (display " | ")
  (display (cadar (cddr board)))
  (display " | ")
  (display (caddar (cddr board)))
  (display "\n"))

;start the game as soon as the program is running
(startGame)