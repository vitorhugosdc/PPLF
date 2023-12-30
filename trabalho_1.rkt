#lang racket

(define labirinto
  '(("#" "#" "#" "#" "#" "#" "#")
    ("#" " " " " " " " " "#" "#")
    ("#" " " "#" "#" "#" " " "#")
    ("#" " " "#" " " " " " " "#")
    ("#" "#" "#" "#" "#" "#" "#")))

(define posição-inicial '(1 1))

(define (mostrar-labirinto labirinto posição)
  (for ([linha (in-range (length labirinto))])
    (for ([coluna (in-range (length (list-ref labirinto 0)))])
      (cond
        [(equal? posição (list linha coluna))
         (display "@")]
        [else
         (display (list-ref (list-ref labirinto linha) coluna))])
      (display " "))
    (newline)))

(define (mover labirinto posição direção)
  (let* ([linha (first posição)]
         [coluna (second posição)]
         [nova-posição
          (cond
            [(equal? direção 'norte) (list (- linha 1) coluna)]
            [(equal? direção 'sul)   (list (+ linha 1) coluna)]
            [(equal? direção 'leste) (list linha (+ coluna 1))]
            [(equal? direção 'oeste) (list linha (- coluna 1))])]
         [nova-linha (first nova-posição)]
         [nova-coluna (second nova-posição)])
    (if (and (>= nova-linha 0) (< nova-linha (length labirinto))
             (>= nova-coluna 0) (< nova-coluna (length (list-ref labirinto 0)))
             (not (equal? (list-ref (list-ref labirinto nova-linha) nova-coluna) "#")))
        nova-posição
        posição)))

(define (jogar)
  (define posição-atual posição-inicial)
  (display "Use 'norte', 'sul', 'leste', 'oeste' para mover. Digite 'sair' para sair.\n")
  (let loop ([posição posição-atual])
    (mostrar-labirinto labirinto posição)
    (let ([movimento (read)])
      (cond
        [(equal? movimento 'sair) (display "Jogo encerrado.\n")]
        [else
         (loop (mover labirinto posição movimento))]))))

(jogar)
