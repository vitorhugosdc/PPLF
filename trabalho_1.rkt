#lang racket

(require racket/base)

; Definição dos labirintos para cada fase
(define labirintos
  (list
    '(("#" "#" "#" "#" "#" "#" "#") ;posição inicial da lista é (0,0)
      ("#" " " "D" " " "D" "#" "#")
      ("#" "D" "#" "#" " " " " "#")
      ("#" " " "#" " " " " "S" "#")
      ("#" "#" "#" "#" "#" "#" "#"))
    '(("#" "#" "#" "#" "#" "#" "#")
      ("#" " " "D" " " "#" " " "#")
      ("#" "#" "#" " " "#" " " "#")
      ("#" " " " " "D" "S" " " "#")
      ("#" "#" "#" "#" "#" "#" "#"))
    '(("#" "#" "#" "#" "#" "#" "#" "#" "#" "#")
     ("#" " " " " " " "D" " " " " " " "#" "#")
     ("#" " " "#" "#" "#" "#" "#" "#" " " "#")
     ("#" " " " " " " " " "#" " " " " "#" "#")
     ("#" "#" "#" " " "#" "#" " " "#" "#" "#")
     ("#" " " " " " " " " " " " " "S" "#" "#")
     ("#" " " "#" "#" "#" " " "#" "#" " " "#")
     ("#" " " " " "D" " " " " " " " " "#" "#")
     ("#" "#" "#" "#" "#" "#" "#" " " " " "#")
     ("#" "#" "#" "#" "#" "#" "#" "#" "#" "#"))
    '(("#" "#" "#" "#" "#" "#" "#" "#" "#" "#" "#" "#")
     ("#" " " " " " " "D" " " " " " " "#" "#" "#" "#")
     ("#" " " "#" "#" "#" "#" "#" "#" " " "#" "#" "#")
     ("#" " " " " " " " " "#" " " " " "#" "#" "#" "#")
     ("#" "#" "#" " " "#" "#" " " "#" "#" "#" "#" "#")
     ("#" " " " " " " " " " " " " "S" "#" "#" "#" "#")
     ("#" " " "#" "#" "#" " " "#" "#" " " "#" "#" "#")
     ("#" " " " " "D" " " " " " " " " "#" "#" "#" "#")
     ("#" "#" "#" "#" "#" "#" "#" " " " " "#" "#" "#")
     ("#" "#" "#" "#" "#" "#" "#" "#" "#" "#" "#" "#")
     ("#" "#" "#" "#" "#" "#" "#" "#" "#" "#" "#" "#")
     ("#" "#" "#" "#" "#" "#" "#" "#" "#" "#" "#" "#"))))


; Definição das posições iniciais e de saída para cada fase
(define posições-iniciais '((1 1) (1 1) (1 1) (1 1)))  ; Posição inicial para cada fase
(define posições-saída '((3 5) (3 4) (5 7) (5 7)))     ; Posição de saída de cada fase


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
      [(equal? resposta '?)
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
      [(equal? resposta '?)
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

(define (desafio-condicionais índice-fase)
  (display "Desafio: Defina uma função que verifica se um número é positivo, negativo ou zero. Digite '?' para uma dica ou 'pular' para pular.\n")
  (display "Por favor, defina o cabeçalho da função como (define (verificar-numero n)... \n\n")
  (let* ([definição-função (read)])
    (cond
      [(equal? definição-função '?)
       (begin
         (display "Dica: Use 'cond' para verificar se o número é maior, menor ou igual a zero.\n")
         (desafio-condicionais índice-fase))]
      [(string=? (format "~a" definição-função) "pular")
       (begin
         (vector-set! desafios-pulados índice-fase (add1 (vector-ref desafios-pulados índice-fase)))
         (display "Desafio pulado. Uma possível solução seria usar 'cond' para verificar as condições.\n"))]
      [else
       (let* ([contexto (make-base-namespace)])
         (eval definição-função contexto)
         (let ([função-verificar (eval 'verificar-numero contexto)])
           (cond
             [(and (equal? (função-verificar 5) 'positivo) 
                   (equal? (função-verificar -3) 'negativo) 
                   (equal? (função-verificar 0) 'zero))
              (begin
                (vector-set! desafios-corretos índice-fase (add1 (vector-ref desafios-corretos índice-fase)))
                (display "Resposta correta! Você pode continuar.\n"))]
             [else
              (display "Resposta incorreta. Tente novamente.\n")])))])))

; Função para o desafio de seleção de máximo
(define (desafio-maximo índice-fase)
  (display "Desafio: Escreva uma função que receba dois números e retorne o maior deles. Digite '?' para uma dica ou 'pular' para pular.\n")
  (display "Por favor, defina o cabeçalho da função como (define (maximo num1 num2)... \n\n")
  (let* ([definição-função (read)])
    (cond
      [(equal? definição-função '?)
       (begin
         (display "Dica: Use a expressão '(if (> num1 num2) num1 num2)' para retornar o maior dos dois números.\n")
         (desafio-maximo índice-fase))]
      [(string=? (format "~a" definição-função) "pular")
       (begin
         (vector-set! desafios-pulados índice-fase (add1 (vector-ref desafios-pulados índice-fase)))
         (display "Desafio pulado. Uma solução seria usar o if para comparar os dois números e retornar o maior.\n"))]
      [else
       (let* ([contexto (make-base-namespace)])
         (eval definição-função contexto)  ; Avalia a definição da função
         (let ([função-maximo (eval 'maximo contexto)])  ; Recupera a função definida
           (let ([resultado1 (função-maximo 3 5)]
                 [resultado2 (função-maximo -2 1)]
                 [resultado3 (função-maximo 7 7)])
             (if (and (= resultado1 5) (= resultado2 1) (= resultado3 7))
                 (begin
                   (vector-set! desafios-corretos índice-fase (add1 (vector-ref desafios-corretos índice-fase)))
                   (vector-set! desafios-resolvidos índice-fase #t)
                   (display "Desafio resolvido! Você pode continuar.\n"))
                   (display "Resposta incorreta. Tente novamente.\n")))))])))

(define (desafio-struct índice-fase)
  (display "Desafio: Estruturas (Structs) em Programação\n")
  (display "Considere uma struct chamada 'Carro', que representa informações sobre carros. A struct 'Carro' inclui 'marca', 'modelo', 'ano' e 'cor'. Como criar uma nova instância e acessar um de seus campos?\n\n")

  (display "1) (define meuCarro (make-Carro \"Ford\" \"Mustang\" 2021 \"Vermelho\")) (Carro-modelo meuCarro)\n")
  (display "2) (define meuCarro (Carro \"Ford\" \"Mustang\" 2021 \"Vermelho\")) (get-modelo meuCarro)\n")
  (display "3) (define meuCarro (new Carro(\"Ford\", \"Mustang\", 2021, \"Vermelho\"))) meuCarro.getModelo()\n")
  (display "4) (define meuCarro (struct Carro \"Ford\" \"Mustang\" 2021 \"Vermelho\")) (meuCarro 'modelo)\n")
  (display "Digite '?' para uma dica ou 'pular' para pular este desafio.\n")
  (display "Escolha a opção correta (1, 2, 3, 4, ?, pular): ")

  (let* ([resposta (read)])
    (cond
      [(equal? resposta 1)
       (begin
         (vector-set! desafios-corretos índice-fase (add1 (vector-ref desafios-corretos índice-fase)))
         (vector-set! desafios-resolvidos índice-fase #t)
         (display "Resposta correta! A opção 1 é a maneira correta de criar uma instância de 'Carro' e acessar um campo.\n"))]
      [(equal? resposta '?)
       (begin
         (display "Dica: Em Racket, structs são criadas usando 'make-StructName' e os campos são acessados com 'StructName-fieldName'.\n")
         (desafio-struct índice-fase))]
      [(string=? (format "~a" resposta) "pular")
       (begin
         (vector-set! desafios-pulados índice-fase (add1 (vector-ref desafios-pulados índice-fase)))
         (display "Desafio pulado. A resposta correta era a opção 1.\n"))]
      [else
       (begin
         (display "Resposta incorreta. Tente novamente.\n")
         (desafio-struct índice-fase))])))


; Função para o desafio de somar elementos de uma lista
(define (desafio-soma-lista índice-fase)
  (display "Desafio 2: Escreva a definição completa de uma função que some todos os elementos de uma lista. Digite '?' para uma dica ou 'pular' para pular.\n")
  (display "Por favor, defina o cabeçalho da função como (define (soma-lista lst)... \n\n")
  (let* ([definição-função (read)])
    (cond
      [(equal? definição-função '?)
       (begin
         (display "Dica: Você pode usar uma função recursiva para somar os elementos. Comece somando o primeiro elemento da lista (car lst) com o resultado da soma do restante (cdr lst).\n")
         (desafio-soma-lista índice-fase))]
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

(define (desafio-map índice-fase)
  (display "Desafio: Escreva uma função usando 'map' que some 1 a cada elemento de uma lista. Digite '?' para uma dica ou 'pular' para pular.\n")
  (display "Por exemplo, você pode escrever '(map add1 lst)'.\n")
  (display "Por favor, defina o cabeçalho da função como (define (mapeamento lst)... \n\n")
  (let* ([definição-função (read)])
    (cond
      [(equal? definição-função '?)
       (begin
         (display "Dica: A função 'map' aplica uma função a cada elemento de uma lista. Você pode usar 'map' com a função 'add1' para somar 1 a cada elemento.\n")
         (desafio-map índice-fase))]
      [(string=? (format "~a" definição-função) "pular")
       (begin
         (vector-set! desafios-pulados índice-fase (add1 (vector-ref desafios-pulados índice-fase)))
         (display "Desafio pulado. Uma solução seria usar a função 'map' para aplicar 'add1' a cada elemento da lista.\n"))]
      [else
       (let* ([contexto (make-base-namespace)])
         (eval definição-função contexto)
         (let ([função-map (eval 'mapeamento contexto)])
           (let ([resultado (função-map (list 1 2 3))])
             (if (equal? resultado (list 2 3 4))
                 (begin
                   (vector-set! desafios-corretos índice-fase (add1 (vector-ref desafios-corretos índice-fase)))
                   (display "Desafio resolvido! Você pode continuar.\n"))
                 (display "Resposta incorreta. Tente novamente.\n")))))])))

(define (desafio-filter índice-fase)
  (display "Desafio: Escreva uma função usando 'filter' que selecione apenas os números pares de uma lista. Digite '?' para uma dica ou 'pular' para pular.\n")
  (display "Por exemplo, você pode escrever '(filter even? lst)'.\n")
  (display "Por favor, defina o cabeçalho da função como (define (filtragem lst)... \n\n")
  (let* ([definição-função (read)])
    (cond
      [(equal? definição-função '?)
       (begin
         (display "Dica: A função 'filter' seleciona elementos de uma lista que satisfazem um predicado. Use 'even?' como predicado para selecionar números pares.\n")
         (desafio-filter índice-fase))]
      [(string=? (format "~a" definição-função) "pular")
       (begin
         (vector-set! desafios-pulados índice-fase (add1 (vector-ref desafios-pulados índice-fase)))
         (display "Desafio pulado. Uma solução seria usar a função 'filter' para selecionar apenas os números pares da lista.\n"))]
      [else
       (let* ([contexto (make-base-namespace)])
         (eval definição-função contexto)
         (let ([função-filter (eval 'filtragem contexto)])
           (let ([resultado (função-filter (list 1 2 3 4 5))])
             (if (equal? resultado (list 2 4))
                 (begin
                   (vector-set! desafios-corretos índice-fase (add1 (vector-ref desafios-corretos índice-fase)))
                   (display "Desafio resolvido! Você pode continuar.\n"))
                 (display "Resposta incorreta. Tente novamente.\n")))))])))


; Lista de funções de desafio para cada fase

(define desafios (list desafio-soma desafio-multiplicação desafio-declaracao-variaveis desafio-soma-lista desafio-condicionais desafio-maximo desafio-map desafio-filter desafio-struct))


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


; Definição das posições de desafios e suas funções correspondentes para cada fase
(define desafios-posições
  (list
     ; Fase 1
    (list (cons '(1 2) desafio-soma)
          (cons '(1 4) desafio-multiplicação)
          (cons '(2 1) desafio-declaracao-variaveis))
     ; Fase 2
    (list (cons '(1 2) desafio-condicionais)
          (cons '(3 3) desafio-maximo))
    ; Fase 3
    (list (cons '(1 4) desafio-soma-lista)
          (cons '(7 3) desafio-struct))
    ; Fase 4
    (list (cons '(7 3) desafio-map)
          (cons '(5 6) desafio-filter))
  ))

; Conteúdos educativos para cada fase
(define conteúdos-educativos
  (list
    (lambda ()
      (display "Bem-vindo à Fase 1: Fundamentos em Racket!\n\n")
      (display "Nesta fase, você vai explorar os conceitos básicos de programação em Racket, começando com variáveis e operações matemáticas simples.\n\n")
      (display "1. Variáveis em Racket:\n")
      (display "   Em Racket, uma variável é um nome que você atribui a um valor. Você pode criar uma variável usando a forma '(define nome valor)'.\n")
      (display "   Por exemplo, '(define x 10)' cria uma variável chamada 'x' com o valor 10.\n\n")
      (display "2. Soma:\n")
      (display "   A soma é uma operação que adiciona dois ou mais números. Em Racket, você usa '(+ num1 num2 ...)' para somar números.\n")
      (display "   Por exemplo, '(+ 2 3)' resulta em 5, que é a soma de 2 e 3.\n\n")
      (display "3. Multiplicação:\n")
      (display "   A multiplicação combina números multiplicando-os. Em Racket, você faz isso com '(* num1 num2 ...)' .\n")
      (display "   Por exemplo, '(* 2 3)' resulta em 6, que é o produto de 2 e 3.\n\n")
      (display "Pronto para colocar esses conceitos em prática? Avance pelo labirinto e enfrente os desafios!\n"))
    (lambda ()
      (display "Bem-vindo à Fase 2: Estruturas de Controle em Racket!\n\n")
      (display "Nesta fase, você irá explorar as estruturas de controle fundamentais em programação, focando em condicionais e loops.\n\n")
      (display "1. Condicionais (if, cond):\n")
      (display "   Condicionais são usadas para tomar decisões com base em condições. Em Racket, usamos 'if' e 'cond' para representar essas estruturas.\n")
      (display "   Exemplo: '(if (> x 5) 'grande 'pequeno)' retorna 'grande' se x for maior que 5, e 'pequeno' caso contrário.\n\n")
      (display "2. Loops (for, while):\n")
      (display "   Loops são usados para executar um bloco de código repetidamente. Em Racket, você pode usar 'for' ou estruturas similares para criar loops.\n")
      (display "   Exemplo: '(for ([i (in-range 0 5)]) (display i))' irá imprimir números de 0 a 4.\n\n")
      (display "Está pronto para mergulhar nessas estruturas de controle e desvendar os desafios? Avance e boa sorte!\n"))
    (lambda ()
     (display "Bem-vindo à Fase 3: Coleções em Racket!\n\n")
      (display "Nesta fase, você irá explorar o poderoso conceito de coleções em Racket, com ênfase em listas, e as funções 'map' e 'filter'.\n\n")
      (display "1. Listas:\n")
      (display "   As listas são uma forma fundamental de armazenar coleções de elementos em Racket. Você pode criar listas usando a notação '(elemento1 elemento2 ...)', e elas podem ser manipuladas de várias maneiras.\n\n")
      (display "2. Função Map:\n")
      (display "   A função 'map' é usada para aplicar uma função a cada elemento de uma lista, retornando uma nova lista com os resultados. É uma forma eficiente de transformar coleções.\n")
      (display "   Exemplo: '(map add1 (list 1 2 3))' retorna '(2 3 4)'.\n\n")
      (display "3. Função Filter:\n")
      (display "   A função 'filter' permite filtrar elementos de uma lista com base em um critério definido. Ela retorna uma nova lista contendo apenas os elementos que satisfazem a condição.\n")
      (display "   Exemplo: '(filter odd? (list 1 2 3 4 5))' retorna '(1 3 5)', que são os números ímpares da lista.\n\n")
      (display "Pronto para explorar e manipular coleções em Racket? Avance pelo labirinto e enfrente os desafios!\n"))
    (lambda ()
     (display "Bem-vindo à Fase 4: Coleções em Racket!\n\n")
      (display "Nesta fase, você irá explorar o poderoso conceito de coleções em Racket, com ênfase em listas, e as funções 'map' e 'filter'.\n\n")
      (display "1. Listas:\n")
      (display "   As listas são uma forma fundamental de armazenar coleções de elementos em Racket. Você pode criar listas usando a notação '(elemento1 elemento2 ...)', e elas podem ser manipuladas de várias maneiras.\n\n")
      (display "2. Função Map:\n")
      (display "   A função 'map' é usada para aplicar uma função a cada elemento de uma lista, retornando uma nova lista com os resultados. É uma forma eficiente de transformar coleções.\n")
      (display "   Exemplo: '(map add1 (list 1 2 3))' retorna '(2 3 4)'.\n\n")
      (display "3. Função Filter:\n")
      (display "   A função 'filter' permite filtrar elementos de uma lista com base em um critério definido. Ela retorna uma nova lista contendo apenas os elementos que satisfazem a condição.\n")
      (display "   Exemplo: '(filter odd? (list 1 2 3 4 5))' retorna '(1 3 5)', que são os números ímpares da lista.\n\n")
      (display "Pronto para explorar e manipular coleções em Racket? Avance pelo labirinto e enfrente os desafios!\n"))))

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