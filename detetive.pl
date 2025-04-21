% lembrar de escrever "swipl" pra iniciar o interpretador do prolog
% e "[detetiveV2]." pra entrar no arquivo
% daí é só chamar os predicados normalmente.

:- use_module(library(random)).
:- dynamic culpado/1.
:- dynamic arma_usada/1.
:- dynamic local_crime/1.
:- dynamic jogador/1.
:- dynamic cartas_jogador/2.
:- dynamic historico_palpites/1.



% ---------------------------------------
% Cartas
personagem(mordomo).
personagem(jardineiro).
personagem(motorista).
personagem(cozinheira).
personagem(empregada).
personagem(esposa).

arma(faca).
arma(revolver).
arma(cano).
arma(chave_inglesa).
arma(abajour).
arma(corda).

comodo(cozinha).
comodo(sala).
comodo(biblioteca).
comodo(jardim).
comodo(varanda).
comodo(escritorio).


% ---------------------------------------
% Jogadores
inicializa_jogadores :-
    retractall(jogador(_)),
    retractall(cartas_jogador(_, _)),
    retractall(historico_palpites(_)),
    assertz(historico_palpites([])),
    assertz(jogador(player)),
    assertz(jogador(jogador_1)),
    assertz(jogador(jogador_2)),
    assertz(jogador(jogador_3)),
    assertz(jogador(jogador_4)).


% ---------------------------------------
% Sorteio da cena do crime
sorteia_suspeito(S) :- findall(X, personagem(X), L), random_member(S, L).
sorteia_arma(A) :-     findall(X, arma(X), L), random_member(A, L).
sorteia_comodo(C) :-   findall(X, comodo(X), L), random_member(C, L).

escolher_cena_de_crime(S, A, C) :-
    retractall(culpado(_)),
    retractall(arma_usada(_)),
    retractall(local_crime(_)),
    sorteia_suspeito(S), assertz(culpado(S)),
    sorteia_arma(A), assertz(arma_usada(A)),
    sorteia_comodo(C), assertz(local_crime(C)).


% ---------------------------------------
% Embaralha e distribui as cartas entre os jogadores
embaralha_cartas(PersonagensBaralhados, ArmasBaralhadas, ComodosBaralhados, S, A, C, Distribuicao) :-
    findall(P, personagem(P), Personagens),
    findall(Ar, arma(Ar), Armas),
    findall(Co, comodo(Co), Comodos),
    findall(J, jogador(J), Jogadores),

    escolher_cena_de_crime(S, A, C),

    delete(Personagens, S, PersonagensSemCulpado),
    delete(Armas, A, ArmasSemCulpado),
    delete(Comodos, C, ComodosSemCulpado),

    random_permutation(PersonagensSemCulpado, PersonagensBaralhados),
    random_permutation(ArmasSemCulpado, ArmasBaralhadas),
    random_permutation(ComodosSemCulpado, ComodosBaralhados),

    distribuir_cartas(Jogadores, PersonagensBaralhados, ArmasBaralhadas, ComodosBaralhados, Distribuicao).



% ---------------------------------------
% Distribuição ordenada: 1 de cada tipo por jogador
distribuir_cartas([], [], [], [], []).
distribuir_cartas([J|Js], [P|Ps], [A|As], [C|Cs], [jogador(J, P, A, C)|Distribuicao]) :-
    distribuir_cartas(Js, Ps, As, Cs, Distribuicao).



% ---------------------------------------
% Salva cartas na base de dados
salvar_distribuicao([]).
salvar_distribuicao([jogador(J, P, A, C)|T]) :-
    assertz(cartas_jogador(J, [P, A, C])),
    salvar_distribuicao(T).



% ---------------------------------------
% Caso o palpite esteja errado, verifica qual jogador possui a carta do
% palpite
verifica_cartas_com_outros(S, A, C, Encontradas) :-
    findall(Jogador, (jogador(Jogador), Jogador \= player), OutrosJogadores),
    findall(Carta, (
        member(J, OutrosJogadores),
        cartas_jogador(J, Cartas),
        (member(S, Cartas); member(A, Cartas); member(C, Cartas)),
        (
            member(S, Cartas) -> Carta = S ;
            member(A, Cartas) -> Carta = A ;
            member(C, Cartas) -> Carta = C
        )
    ), Lista),
    list_to_set(Lista, Encontradas),
    mostrar_dono_cartas(Encontradas).



% ---------------------------------------
% Vai registrar as cartas encontradas com os outro jogaadores
registrar_cartas_encontradas([], _, _, _) :-
    writeln("Nenhuma das cartas foi revelada. Elas podem fazer parte do crime!").
registrar_cartas_encontradas(Cartas, S, A, C) :-
    historico_palpites(Hist),
    (member(S, Cartas) -> S1 = S ; S1 = '-'),
    (member(A, Cartas) -> A1 = A ; A1 = '-'),
    (member(C, Cartas) -> C1 = C ; C1 = '-'),
    append(Hist, [(S1, A1, C1)], NovoHist),
    retractall(historico_palpites(_)),
    assertz(historico_palpites(NovoHist)).


% ---------------------------------------
% Exibe o dono da carta
mostrar_dono_cartas([]).
mostrar_dono_cartas([Carta|Resto]) :-
    jogador(J),
    J \= player,
    cartas_jogador(J, Cartas),
    member(Carta, Cartas),
    format("A carta ~w está com o jogador: ~w~n", [Carta, J]),
    mostrar_dono_cartas(Resto).



% ---------------------------------------
% Mostra o palpite
mostrar_lista_palpites([]).
mostrar_lista_palpites([(S, A, C)|T]) :-
    format("Suspeito: ~w | Arma: ~w | Cômodo: ~w~n", [S, A, C]),
    mostrar_lista_palpites(T).



% ---------------------------------------
% Vai exibir as crtas do usuário quando iniciar o jogo
mostrar_cartas_player :-
    cartas_jogador(player, Cartas),
    format("~nSuas cartas são:~n"),
    mostrar_lista_cartas(Cartas).



% ---------------------------------------
% Vai exibir no menu todas as possibilidades de cartas
mostrar_cartas_possiveis :-
    cartas_jogador(player, MinhasCartas),

    findall(P, personagem(P), TodosPersonagens),
    findall(A, arma(A), TodasArmas),
    findall(C, comodo(C), TodosComodos),

    subtract(TodosPersonagens, MinhasCartas, PersonagensPossiveis),
    subtract(TodasArmas, MinhasCartas, ArmasPossiveis),
    subtract(TodosComodos, MinhasCartas, ComodosPossiveis),

    writeln("\nCartas possíveis para palpite:"),
    format("Personagens disponíveis:~n   ~w~n", [PersonagensPossiveis]),
    format("Armas disponíveis:~n   ~w~n", [ArmasPossiveis]),
    format("Cômodos disponíveis:~n   ~w~n~n", [ComodosPossiveis]).



% ---------------------------------------
% Exibe no histórico de partidas as cartas que estão com outros
% jogadores
mostrar_lista_cartas([]).
mostrar_lista_cartas([C|R]) :-
    format("- ~w~n", [C]),
    mostrar_lista_cartas(R).





% ----------------------------------------------------------------------------
% Interface mixuruca do jogo Inicializa o jogo embralhando e
% distribuindo as cartas.
iniciar_jogo :-
    inicializa_jogadores,
    embaralha_cartas(_, _, _, _, _, _, Distribuicao),
    salvar_distribuicao(Distribuicao),
    writeln('Jogo iniciado! As cartas foram distribuídas.'),
    mostrar_cartas_possiveis,
    mostrar_cartas_player,
    writeln("\nDigite 'palpite(Suspeito, Arma, Comodo)' paara dar o seu palpite.").

% Recolhe e processa o palpite do usuÃ¡rio
palpite(Suspeito, Arma, Comodo) :-
    format("SEU PALPITE:~n"),
    format("suspeito: ~w~n", [Suspeito]),
    format("arma: ~w~n", [Arma]),
    format("cômodo: ~w~n~n", [Comodo]),

    (culpado(Suspeito), arma_usada(Arma), local_crime(Comodo) ->
        writeln("Parabéns, você desvendou o crime!");
        (
            writeln("Algum palpite está incorreto. Vamos investigar..."),
            verifica_cartas_com_outros(Suspeito, Arma, Comodo, Encontradas),
            registrar_cartas_encontradas(Encontradas, Suspeito, Arma, Comodo),
            mostrar_historico_palpites,
            mostrar_cartas_possiveis,
            mostrar_cartas_player

        )
    ).

% Printa uma tabela com os palpites ja feitos
mostrar_historico_palpites :-
    historico_palpites(Hist),
    format("~n===== Histórico de Palpites =====~n"),
    mostrar_lista_palpites(Hist),
    format("=================================~n").



