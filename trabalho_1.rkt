#lang racket

(require racket/base)

; Definição dos labirintos para cada fase
(define labirintos
  (list
    '(("#" "#" "#" "#" "#" "#" "#") ;posição inicial é (0,0)
      ("#" " " "D" " " "D" "#" "#")
      ("#" "D" "#" "#" " " " " "#")
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
(define desafios-corretos (make-vector (length labirintos) 0))
(define desafios-pulados (make-vector (length labirintos) 0))


; Função para exibir o resumo do desempenho no final da fase
(define (exibir-resumo-desempenho índice-fase)
  (let ([acertos (vector-ref desafios-corretos índice-fase)]
        [pulados (vector-ref desafios-pulados índice-fase)])
    (display (format "Resumo da fase ~a: \nDesafios corretos: ~a \nDesafios pulados: ~a\n" índice-fase acertos pulados))))


; Função para o desafio de declaração de variáveis
(define (desafio-declaracao-variaveis índice-fase)
  (display "Desafio: Qual das seguintes opções é uma declaração de variável correta em Racket? Digite 'pular' para pular.\n")
  (display "1) (define var 10)\n2) (define (var 10))\n3) (define var, 10)\n4) define var 10\n")
  (display "Digite o número da sua resposta ou 'pular': ")
  (let ([resposta (read)])
    (cond
      [(equal? resposta 1)
       (begin
         (vector-set! desafios-corretos índice-fase (add1 (vector-ref desafios-corretos índice-fase)))
         (vector-set! desafios-resolvidos índice-fase #t)
         (display "Resposta correta! Você pode continuar.\n"))]
      [(string=? (format "~a" resposta) "pular")
       (begin
         (vector-set! desafios-pulados índice-fase (add1 (vector-ref desafios-pulados índice-fase)))
         (display "Desafio pulado.\n")
         (display "A declaração correta de uma variável em Racket é usando '(define var 10)'. Isso cria uma variável chamada 'var' com o valor 10.\n"))]
      [else
       (display "Resposta incorreta. Tente novamente.\n")
       (desafio-declaracao-variaveis índice-fase)])))

; Função para o desafio de soma com opção de dica
(define (desafio-soma índice-fase)
  (display "Desafio: Insira o corpo de uma função que some dois números ou digite '?' para uma dica. Digite 'pular' para pular.\n")
  (display "Por exemplo, você pode digitar (+ x y).\n")
  (let* ([resposta (read)])
    (cond
      [(equal? resposta "?")
       (begin
         (display "Dica: A função de soma em Racket é (+ x y), onde x e y são os números a serem somados.\n")
         (desafio-soma índice-fase))]
      [(string=? (format "~a" resposta) "pular")
       (begin
         (vector-set! desafios-pulados índice-fase (add1 (vector-ref desafios-pulados índice-fase)))
         (display "Desafio pulado.\n")
         (display "A função correta para somar dois números em Racket é '(+ x y)', onde 'x' e 'y' são os números a serem somados.\n"))]
      [else
       (let* ([contexto (make-base-namespace)]
              [função-soma (eval `(lambda (x y) ,resposta) contexto)])
         (let ([resultado (função-soma 2 3)])
           (if (= resultado 5)
               (begin
                 (vector-set! desafios-corretos índice-fase (add1 (vector-ref desafios-corretos índice-fase)))
                 (vector-set! desafios-resolvidos índice-fase #t)
                 (display "Desafio resolvido! Você pode continuar.\n"))
               (display "Resposta incorreta. Tente novamente.\n"))))])))

; Função para o desafio de multiplicação (primeira fase)
(define (desafio-multiplicação índice-fase)
  (display "Desafio 2: Insira o corpo de uma função que multiplique dois números ou digite '?' para uma dica. Digite 'pular' para pular.\n")
  (display "Por exemplo, você pode digitar (* x y).\n")
  (let* ([resposta (read)])
    (cond
      [(equal? resposta "?")
       (display "Dica: A função de multiplicação em Racket é (* x y), onde x e y são os números a serem multiplicados.\n")
       (desafio-multiplicação índice-fase)]  ; Chama novamente após mostrar a dica
      [(string=? (format "~a" resposta) "pular")
       (begin
         (vector-set! desafios-pulados índice-fase (add1 (vector-ref desafios-pulados índice-fase)))
         (display "Desafio pulado.\n")
         (display "A função correta para multiplicar dois números em Racket é '(* x y)', onde 'x' e 'y' são os números a serem multiplicados.\n"))]
      [else
       (let* ([contexto (make-base-namespace)]
              [função-multiplicação (eval `(lambda (x y) ,resposta) contexto)])
         (let ([resultado (função-multiplicação 2 3)])
           (if (= resultado 6)
               (begin
                 (vector-set! desafios-corretos índice-fase (add1 (vector-ref desafios-corretos índice-fase)))
                 (vector-set! desafios-resolvidos índice-fase #t)
                 (display "Desafio resolvido! Você pode continuar.\n"))
               (display "Resposta incorreta. Tente novamente.\n"))))])))


; Função para o desafio de somar elementos de uma lista
(define (desafio-soma-lista índice-fase)
  (display "Desafio 2: Escreva a definição completa de uma função que some todos os elementos de uma lista. Digite 'pular' para pular.\n")
  (display "Por favor, defina o cabeçalho da função como (define (soma-lista lst)... \n\n")
  (let* ([definição-função (read)])
    (cond
      [(string=? (format "~a" definição-função) "pular")
       (begin
         (vector-set! desafios-pulados índice-fase (add1 (vector-ref desafios-pulados índice-fase)))
         (display "Desafio pulado.\n")
         (display "Uma forma de somar todos os elementos de uma lista em Racket é definir uma função recursiva que soma o primeiro elemento (car lst) com a soma do restante da lista (cdr lst).\n"))]
      [else
       (let* ([contexto (make-base-namespace)])
         (eval definição-função contexto)  ; Avalia a definição da função
         (let ([função-soma-lista (eval 'soma-lista contexto)])  ; Recupera a função definida
           (let ([resultado (função-soma-lista (list 1 2 3 4 5))])  ; Testa a função
             (if (= resultado 15)
                 (begin
                   (vector-set! desafios-corretos índice-fase (add1 (vector-ref desafios-corretos índice-fase)))
                   (vector-set! desafios-resolvidos índice-fase #t)
                   (display "Desafio resolvido! Você pode continuar.\n"))
                 (display "Resposta incorreta. Tente novamente.\n")))))])))


; Lista de funções de desafio para cada fase

(define desafios (list desafio-soma desafio-multiplicação desafio-declaracao-variaveis desafio-soma-lista))


; Função para mostrar o labirinto com neblina de guerra
(define (mostrar-labirinto labirinto posição)
  (marcar-células-visitadas posição)
  (for ([linha (in-range (length labirinto))])
    (for ([coluna (in-range (length (list-ref labirinto 0)))])
      (cond
        [(equal? posição (list linha coluna))
         (display "@")]
        [(list-ref (list-ref células-visitadas linha) coluna)
         (display (list-ref (list-ref labirinto linha) coluna))]
        [else
         (display "?")])
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
            [(equal? direção 'a) (list linha (- coluna 1))]
            [else posição])]  ; Mantém a posição atual para entradas inválidas
         [nova-linha (first nova-posição)]
         [nova-coluna (second nova-posição)])
    (if (and (>= nova-linha 0) (< nova-linha (length labirinto))
             (>= nova-coluna 0) (< nova-coluna (length (list-ref labirinto 0)))
             (not (equal? (list-ref (list-ref labirinto nova-linha) nova-coluna) "#")))
        nova-posição
        posição)))


; Associação de posições de desafios e suas funções correspondentes para cada fase;
(define desafios-posições
  (list
    (list (cons '(1 2) desafio-soma) (cons '(1 4) desafio-multiplicação) (cons '(2 1) desafio-declaracao-variaveis)) ; Fase 1
    (list (cons '(1 2) desafio-soma-lista)); Fase 2
  ))

; Conteúdos educativos para cada fase
(define conteúdos-educativos
  (list
    (lambda ()
      (display "Bem-vindo à fase 1!\nNesta fase, você aprenderá sobre soma e multiplicação básica.\n\n")
      (display "A soma é uma operação que adiciona números. Por exemplo, 2 + 3 é igual a 5.\n")
      (display "A multiplicação é uma operação que multiplica números. Por exemplo, 2 * 3 é igual a 6.\n\n"))
    (lambda ()
      (display "Bem-vindo à fase 2!\nNesta fase, você aprenderá sobre outras operações matemáticas...\n\n"))

  ))

; Função para exibir conteúdo educativo
(define (exibir-conteúdo-educativo índice-fase)
  (let ([conteúdo (list-ref conteúdos-educativos índice-fase)])
    (conteúdo)))

; Estrutura para rastrear células visitadas
(define células-visitadas '())

; Função para inicializar células visitadas baseada no labirinto atual
(define (inicializar-células-visitadas labirinto)
  (set! células-visitadas (map (lambda (linha) (build-list (length linha) (lambda (_) #f))) labirinto)))

; Função auxiliar para atualizar uma lista em um índice específico
(define (atualizar-lista lst idx novo-valor)
  (if (= idx 0)
      (cons novo-valor (cdr lst))
      (cons (car lst) (atualizar-lista (cdr lst) (- idx 1) novo-valor))))

; Função para marcar células como visitadas
(define (marcar-células-visitadas posição)
  (let ([linha (first posição)]
        [coluna (second posição)])
    (for ([dl (in-range -1 2)])
      (for ([dc (in-range -1 2)])
        (let ([nova-linha (+ linha dl)]
              [nova-coluna (+ coluna dc)])
          (when (and (>= nova-linha 0) (< nova-linha (length células-visitadas))
                     (>= nova-coluna 0) (< nova-coluna (length (first células-visitadas))))
            (let ([linha-atualizada (atualizar-lista (list-ref células-visitadas nova-linha) nova-coluna #t)])
              (set! células-visitadas (atualizar-lista células-visitadas nova-linha linha-atualizada)))))))))


; Função principal para jogar uma fase
(define (jogar-fase índice-fase)
  ; Exibir conteúdo educativo antes de começar a fase
  (exibir-conteúdo-educativo índice-fase)

  (define labirinto-atual (list-ref labirintos índice-fase))
  (inicializar-células-visitadas labirinto-atual)
  (define desafios-da-fase (list-ref desafios-posições índice-fase))
  (define posição-inicial (list-ref posições-iniciais índice-fase))
  (define posição-saída (list-ref posições-saída índice-fase))
  (define posição-atual posição-inicial)

  (display "Bem-vindo ao Labirinto de Racket! Encontre a saída e resolva os desafios.\n\n")

  (define (loop posição)
    (cond
      [(equal? posição posição-saída)
       (if (vector-ref desafios-resolvidos índice-fase)
           (if (< índice-fase (sub1 (length labirintos)))
               (begin
                 (exibir-resumo-desempenho índice-fase)
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