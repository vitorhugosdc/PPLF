#lang racket

(require racket/base)

; Definição dos labirintos para cada fase
(define labirintos
  (list
    '(("#" "#" "#" "#" "#" "#" "#")
      ("#" " " "D" " " " " "#" "#")
      ("#" " " "#" "#" " " " " "#")
      ("#" " " "#" " " " " "S" "#")
      ("#" "#" "#" "#" "#" "#" "#"))
    '(("#" "#" "#" "#" "#" "#" "#")
      ("#" " " "D" " " "#" " " "#")
      ("#" "#" "#" " " "#" " " "#")
      ("#" " " " " " " "S" " " "#")
      ("#" "#" "#" "#" "#" "#" "#"))))

; Definição das posições iniciais e de saída para cada fase
(define posições-iniciais '((1 1) (1 1)))
(define posições-saída '((3 5) (3 5)))

; Desafio de programação simples (pode ser ajustado para cada fase)
(define desafio-posição '(1 2))
(define desafio-resolvido #f)

; Função para o desafio de programação
(define (desafio-soma)
  (display "Desafio: Insira o corpo de uma função que some dois números.\n")
  (display "Por exemplo, você pode digitar (+ x y).\n")
  (let* ([resposta (read)]
         [contexto (make-base-namespace)]
         [função-soma (eval `(lambda (x y) ,resposta) contexto)])
    (let ([resultado (função-soma 2 3)])
      (if (= resultado 5)
          (begin
            (set! desafio-resolvido #t)
            (display "Desafio resolvido! Você pode continuar.\n"))
          (display "Resposta incorreta. Tente novamente.\n")))))

; Função para mostrar o labirinto
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

; Função para mover o jogador no labirinto
(define (mover labirinto posição direção)
  (let* ([linha (first posição)]
         [coluna (second posição)]
         [nova-posição
          (cond
            [(equal? direção 'cima) (list (- linha 1) coluna)]
            [(equal? direção 'baixo) (list (+ linha 1) coluna)]
            [(equal? direção 'direita) (list linha (+ coluna 1))]
            [(equal? direção 'esquerda) (list linha (- coluna 1))])]
         [nova-linha (first nova-posição)]
         [nova-coluna (second nova-posição)])
    (if (and (>= nova-linha 0) (< nova-linha (length labirinto))
             (>= nova-coluna 0) (< nova-coluna (length (list-ref labirinto 0)))
             (not (equal? (list-ref (list-ref labirinto nova-linha) nova-coluna) "#")))
        nova-posição
        posição)))

; Função principal para jogar uma fase
(define (jogar-fase índice-fase)
  (define labirinto-atual (list-ref labirintos índice-fase))
  (define posição-inicial (list-ref posições-iniciais índice-fase))
  (define posição-saída (list-ref posições-saída índice-fase))
  (define posição-atual posição-inicial)

  (display "Bem-vindo ao Labirinto de Racket! Encontre a saída e resolva os desafios.\n")

  (define (loop posição)
    (cond
      [(equal? posição posição-saída)
       (if desafio-resolvido
           (if (< índice-fase (sub1 (length labirintos)))
               (begin
                 (set! desafio-resolvido #f)
                 (jogar-fase (add1 índice-fase)))
               (display "Parabéns! Você encontrou a saída e resolveu os desafios de todas as fases!\n"))
           (display "Você encontrou a saída, mas ainda há desafios a resolver.\n"))]
      [else
       (mostrar-labirinto labirinto-atual posição)
       (when (and (equal? posição desafio-posição) (not desafio-resolvido))
         (desafio-soma))
       (let ([movimento (read)])
         (cond
           [(equal? movimento 'sair) (display "Jogo encerrado.\n")]
           [else
            (loop (mover labirinto-atual posição movimento))]))]))

  (loop posição-inicial))

; Iniciar o jogo na primeira fase
(jogar-fase 0)
