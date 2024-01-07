#lang racket

(require racket/base)

; Definição dos labirintos para cada fase
(define labirintos
  (list
    '(("#" "#" "#" "#" "#" "#" "#")
      ("#" " " "D" " " "D" "#" "#")  ; Novo "D" adicionado aqui
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

; Desafio resolvido para cada fase
(define desafios-resolvidos (make-vector (length labirintos) #f))

; Função para o desafio de soma
(define (desafio-soma índice-fase)
  (display "Desafio: Insira o corpo de uma função que some dois números.\n")
  (display "Por exemplo, você pode digitar (+ x y).\n")
  (let* ([resposta (read)]
         [contexto (make-base-namespace)]
         [função-soma (eval `(lambda (x y) ,resposta) contexto)])
    (let ([resultado (função-soma 2 3)])
      (if (= resultado 5)
          (begin
            (vector-set! desafios-resolvidos índice-fase #t)
            (display "Desafio resolvido! Você pode continuar.\n"))
          (display "Resposta incorreta. Tente novamente.\n")))))

; Função para o desafio de multiplicação (segunda fase)
(define (desafio-multiplicação índice-fase)
  (display "Desafio 2: Insira o corpo de uma função que multiplique dois números.\n")
  (display "Por exemplo, você pode digitar (* x y).\n")
  (let* ([resposta (read)]
         [contexto (make-base-namespace)]
         [função-multiplicação (eval `(lambda (x y) ,resposta) contexto)])
    (let ([resultado (função-multiplicação 2 3)])
      (if (= resultado 6)
          (begin
            (vector-set! desafios-resolvidos índice-fase #t)
            (display "Desafio resolvido! Você pode continuar.\n"))
          (display "Resposta incorreta. Tente novamente.\n")))))

; Função para o desafio de somar elementos de uma lista
(define (desafio-soma-lista índice-fase)
  (display "Desafio 2: Escreva a definição completa de uma função que some todos os elementos de uma lista.\n")
  (display "Por favor, defina o cabeçalho da função como (define (soma-lista lst)... \n\n")
  (let* ([definição-função (read)]
         [contexto (make-base-namespace)])
    (eval definição-função contexto)  ; Avalia a definição da função
    (let ([função-soma-lista (eval 'soma-lista contexto)])  ; Recupera a função definida
      (let ([resultado (função-soma-lista (list 1 2 3 4 5))])  ; Testa a função
        (if (= resultado 15)
            (begin
              (vector-set! desafios-resolvidos índice-fase #t)
              (display "Desafio resolvido! Você pode continuar.\n"))
            (display "Resposta incorreta. Tente novamente.\n"))))))


; Lista de funções de desafio para cada fase

(define desafios (list desafio-soma desafio-soma-lista))


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
            [(equal? direção 'w) (list (- linha 1) coluna)]
            [(equal? direção 's) (list (+ linha 1) coluna)]
            [(equal? direção 'd) (list linha (+ coluna 1))]
            [(equal? direção 'a) (list linha (- coluna 1))])]
         [nova-linha (first nova-posição)]
         [nova-coluna (second nova-posição)])
    (if (and (>= nova-linha 0) (< nova-linha (length labirinto))
             (>= nova-coluna 0) (< nova-coluna (length (list-ref labirinto 0)))
             (not (equal? (list-ref (list-ref labirinto nova-linha) nova-coluna) "#")))
        nova-posição
        posição)))

; Associação de posições de desafios e suas funções correspondentes para cada fase; Associação de posições de desafios e suas funções correspondentes para cada fase
(define desafios-posições
  (list
    (list (cons '(1 2) desafio-soma) (cons '(1 4) desafio-multiplicação))  ; Fase 1
    (list (cons '(1 2) desafio-soma-lista))                                 ; Fase 2
  ))



; Função principal para jogar uma fase
(define (jogar-fase índice-fase)
  (define labirinto-atual (list-ref labirintos índice-fase))
  (define desafios-da-fase (list-ref desafios-posições índice-fase))
  (define posição-inicial (list-ref posições-iniciais índice-fase))
  (define posição-saída (list-ref posições-saída índice-fase))
  (define posição-atual posição-inicial)

  (display "Bem-vindo ao Labirinto de Racket! Encontre a saída e resolva os desafios.\n")

  (define (loop posição)
    (cond
      [(equal? posição posição-saída)
       (if (vector-ref desafios-resolvidos índice-fase)
           (if (< índice-fase (sub1 (length labirintos)))
               (begin
                 (vector-set! desafios-resolvidos índice-fase #f)
                 (jogar-fase (add1 índice-fase)))
               (display "Parabéns! Você encontrou a saída e resolveu os desafios de todas as fases!\n"))
           (display "Você encontrou a saída, mas ainda há desafios a resolver.\n"))]
      [else
       (mostrar-labirinto labirinto-atual posição)
       (for-each (lambda (par-desafio-posição)
                   (let ([pos-desafio (car par-desafio-posição)]
                         [função-desafio (cdr par-desafio-posição)])
                     (when (equal? posição pos-desafio)
                       (função-desafio índice-fase)
                       (vector-set! desafios-resolvidos índice-fase #t))))
                 desafios-da-fase)
       (let ([movimento (read)])
         (cond
           [(equal? movimento 'sair) (display "Jogo encerrado.\n")]
           [else
            (loop (mover labirinto-atual posição movimento))]))]))

  (loop posição-inicial))



; Iniciar o jogo na primeira fase
(jogar-fase 0)