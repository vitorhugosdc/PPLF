#lang racket

(require racket/base)

(define labirinto
  '(("#" "#" "#" "#" "#" "#" "#")
    ("#" " " "D" " " " " "#" "#")
    ("#" " " "#" "#" "#" " " "#")
    ("#" " " "#" " " " " "S" "#")
    ("#" "#" "#" "#" "#" "#" "#")))

(define posição-inicial '(1 1))
(define posição-saída '(3 5))

; Desafio de programação simples
(define desafio-posição '(1 2))

(define desafio-resolvido #f)

(define (desafio-soma)
  (display "Desafio: Insira o corpo de uma função que some dois números.\n")
  (display "Por exemplo, você pode digitar (+ x y).\n")
  (let* ([resposta (read)]
         [contexto (make-base-namespace)]
         [função-soma (eval `(lambda (x y) ,resposta) contexto)]) ; Avalia no contexto adequado
    (let ([resultado (função-soma 2 3)]) ; Testa a função
      (if (= resultado 5)
          (begin
            (set! desafio-resolvido #t)
            (display "Desafio resolvido! Você pode continuar.\n"))
          (display "Resposta incorreta. Tente novamente.\n")))))

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
            [(equal? direção 'cima) (list (- linha 1) coluna)]
            [(equal? direção 'baixo)   (list (+ linha 1) coluna)]
            [(equal? direção 'direita) (list linha (+ coluna 1))]
            [(equal? direção 'esquerda) (list linha (- coluna 1))])]
         [nova-linha (first nova-posição)]
         [nova-coluna (second nova-posição)])
    (if (and (>= nova-linha 0) (< nova-linha (length labirinto))
             (>= nova-coluna 0) (< nova-coluna (length (list-ref labirinto 0)))
             (not (equal? (list-ref (list-ref labirinto nova-linha) nova-coluna) "#")))
        nova-posição
        posição)))

(define (jogar)
  (define posição-atual posição-inicial)
  (display "Bem-vindo ao Labirinto de Racket! Encontre a saída e resolva os desafios.\n")
  (let loop ([posição posição-atual])
    (cond
      [(equal? posição posição-saída)
       (if desafio-resolvido
           (display "Parabéns! Você encontrou a saída e resolveu os desafios!\n")
           (display "Você encontrou a saída, mas ainda há desafios a resolver.\n"))]
      [else
       (mostrar-labirinto labirinto posição)
       (when (and (equal? posição desafio-posição) (not desafio-resolvido))
         (desafio-soma))
       (let ([movimento (read)])
         (cond
           [(equal? movimento 'sair) (display "Jogo encerrado.\n")]
           [else
            (loop (mover labirinto posição movimento))]))])))

(jogar)
