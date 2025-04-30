:- use_module(library(random)).
:- dynamic jogador/1.
:- dynamic cartas_jogador/2.
:- dynamic cena_crime/3.
:- dynamic cartas_confirmadas/1.
:- dynamic historico_palpites/1.
:- dynamic culpado/1.
:- dynamic arma_usada/1.
:- dynamic local_crime/1.
:- dynamic sus/1.
:- dynamic loc/1.
:- dynamic arm/1.



cartas_confirmadas([]).
historico_palpites([]).

% --- Cartas ---
personagem(mordomo).  personagem(jardineiro).  personagem(motorista).
personagem(cozinheira).  personagem(empregada).  personagem(esposa).

arma(faca).  arma(revolver).  arma(cano).
arma(chave_inglesa).  arma(abajour).  arma(corda).

comodo(cozinha).  comodo(sala).  comodo(biblioteca).
comodo(jardim).  comodo(varanda).  comodo(escritorio).

% --- Inicializa jogadores ---
inicializa_jogadores :-
    retractall(jogador(_)),
    retractall(cartas_jogador(_, _)),
    forall(member(J, [player, jogador1, jogador2, jogador3, jogador4]), assertz(jogador(J))).

% --- Sorteio da cena do crime ---
sortear_cena_do_crime :-
    retractall(cena_crime(_, _, _)),
    retractall(culpado(_)), retractall(arma_usada(_)), retractall(local_crime(_)),
    findall(P, personagem(P), Ps), random_member(S, Ps),
    findall(A, arma(A), As), random_member(W, As),
    findall(C, comodo(C), Cs), random_member(L, Cs),

    assertz(culpado(S)),assertz(arma_usada(W)),assertz(local_crime(L)),


    assertz(cena_crime(S, W, L)).



% --- Embaralhar e distribuir cartas ---
distribuir_cartas :-
    findall(P, personagem(P), Ps), findall(A, arma(A), As), findall(C, comodo(C), Cs),
    cena_crime(S, W, L),
    delete(Ps, S, Pss), delete(As, W, Ass), delete(Cs, L, Css),
    random_permutation(Pss,SuspeitosEmbaralhados),
    random_permutation(Ass,ArmasEmbaralhadas),
    random_permutation(Css,ComodosEmbaralhados),

    %append([Pss, Ass, Css], Todas), random_permutation(Todas, Embaralhadas),
    findall(J, jogador(J), Jogadores), distribuir_cartas_aux(Jogadores, SuspeitosEmbaralhados, ArmasEmbaralhadas, ComodosEmbaralhados).


distribuir_cartas_aux([], _,_,_).
distribuir_cartas_aux([J|Js], Sus, Arm, Com) :-
    %length(Mao, 3), append(Mao, Resto, Cartas),
    random_member(S, Sus),
    random_member(A, Arm),
    random_member(C, Com),
    Mao = [S, A, C],
    delete(Sus, S, Su),
    delete(Arm, A, Ar),
    delete(Com, C, Co),

    assertz(cartas_jogador(J, Mao)),
    distribuir_cartas_aux(Js, Su, Ar, Co).

% --- Palpite ---
palpite(S, A, C) :-
    format("\n===== PALPITE FEITO =====~nSuspeito: ~w~nArma: ~w~nCômodo: ~w~n~n", [S, A, C]),
    cena_crime(SC, AC, LC),
    (S == SC, A == AC, C == LC ->
        writeln("Parabéns, você desvendou o crime!"), processa_opcao(0)
    ;
        verificar_cartas([S, A, C], Confirmadas),
        registrar_cartas_confirmadas(Confirmadas),
        registrar_palpite(S, A, C),
        mostrar_historico_palpites).

% --- Verifica quem tem cartas do palpite ---
verificar_cartas(Cartas, Confirmadas) :-
    findall(J, (jogador(J), J \= player), Js),
    findall(C, (
        member(J, Js), cartas_jogador(J, Mao), member(C, Cartas), member(C, Mao)
    ), Lista),
    list_to_set(Lista, Confirmadas).

registrar_cartas_confirmadas([]).
registrar_cartas_confirmadas([C|R]) :-
    cartas_confirmadas(Lista),
    (member(C, Lista) -> true ; (
        retractall(cartas_confirmadas(_)),
        assertz(cartas_confirmadas([C|Lista])))),
    registrar_cartas_confirmadas(R).

registrar_palpite(S, A, C) :-
    cartas_confirmadas(Confirmadas),
    (member(S, Confirmadas) -> ES = confirmado ; ES = desconhecido),
    (member(A, Confirmadas) -> EA = confirmado ; EA = desconhecido),
    (member(C, Confirmadas) -> EC = confirmado ; EC = desconhecido),
    historico_palpites(Hist),
    append(Hist, [(S-ES, A-EA, C-EC)], NovoHist),
    retractall(historico_palpites(_)),
    assertz(historico_palpites(NovoHist)).






    % --- Iniciar jogo ---
iniciar_jogo :-
    retractall(culpado(_)), retractall(arma_usada(_)), retractall(local_crime(_)),
    retractall(carta_confirmada(_)),
    %retractall(cena_crime(_, _, _)),
    retractall(cartas_confirmadas(_)), assertz(cartas_confirmadas([])),
    retractall(historico_palpites(_)), assertz(historico_palpites([])),

    format("------------------------------------------"),
    format("~n                 DETETIVE ~n"),
    format("------------------------------------------"),

    inicializa_jogadores,
    sortear_cena_do_crime,
    distribuir_cartas,

    format('~nJogo iniciado! As cartas foram distribuídas.~n'),

    mostrar_cartas_player,
    menu_principal.


menu_principal :-
    writeln("\n===== MENU ====="),
    writeln("1 - Fazer um palpite"),
    writeln("2 - Exibir cartas das mãos"),
    writeln("3 - Listar histórico"),
    writeln("4 - Mostrar cartas do crime"),
    writeln("0 - Sair do jogo"),
    write("Escolha: "),
    read(Opcao),
    processa_opcao(Opcao).


ler_suspeito(Sus):-
    read_line_to_string(user_input, Suspeito),
    string_lower(Suspeito, SuspeitoMin),
    atom_string(SusTratado, SuspeitoMin),

    (   personagem(SusTratado) ->
        Sus = SusTratado
    ;
        format("Entrada inválida! Verifique os personagens disponíveis~n"),
        ler_suspeito(Sus)
    ).

ler_arma(Arm) :-
    read_line_to_string(user_input, Arma),
    string_lower(Arma, ArmaMin),
    atom_string(ArmaTratado, ArmaMin),

    (   arma(ArmaTratado) ->
        Arm = ArmaTratado
    ;
        format("Entrada inválida! Verifique as armas disponíveis~n"),
        ler_arma(Arm)
    ).


ler_comodo(Com) :-
    read_line_to_string(user_input, Comodo),
    string_lower(Comodo, ComodoMin),
    atom_string(ComodoTratado, ComodoMin),

    (   arma(ComodoTratado) ->
        Com = ComodoTratado
    ;
        format("Entrada inválida! Verifique as armas disponíveis~n"),
        ler_comodo(Com)
    ).



% ----------------------------------------------------------------------------
% Predicados para processar a decisão do usuário
processa_opcao(1) :-
    mostrar_cartas_possiveis,
    format("~nFaça o seu palpite ~n"),

    format("Quem você acha que foi o culpado? ~n"),
    ler_suspeito(Sus),

    format("Qual arma ele utilizou no assassinato? ~n"),
    ler_arma(Arma),

    format("Em que cômodo ocorreu o crime? ~n"),
    ler_comodo(Comodo),

    palpite(Sus, Arma, Comodo),
    menu_principal.   % Depois de palpitar, volta ao menu

processa_opcao(2) :-
    mostrar_cartas_player,
    menu_principal.
processa_opcao(3) :-
    mostrar_historico_palpites,
    menu_principal.
processa_opcao(4) :-
    exibir_cena_crime,
    menu_principal.
processa_opcao(5) :-
    exibir_cartas_jogador,
    menu_principal.

processa_opcao(0) :-
    writeln("\nJogo encerrado. Até a próxima!"),
    mostrar_historico_palpites.
processa_opcao(_) :-
    writeln("\nOpção inválida, tente novamente."),
    menu_principal.  % Volta pro menu se digitar errado


% --- Exibe o hist\xF3rico ---
mostrar_historico_palpites :-
    historico_palpites(Hist),
    format("\n===== Hist\xF3rico de Palpites =====~n"),
    forall(member((S-ES, A-EA, C-EC), Hist), (
        format("Suspeito: ~w (~w) | Arma: ~w (~w) | C\xF4modo: ~w (~w)~n", [S, ES, A, EA, C, EC])
    )),
    format("=================================~n").

% --- Mostrar cartas do player ---
mostrar_cartas_player :-
    cartas_jogador(player, Cartas),
    format("\nSuas cartas:\n"), forall(member(C, Cartas), format("- ~w~n", [C])).


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

    writeln("\n-------------------------------"),
    writeln("        CARTAS POSSÍVEIS"),
    writeln("--------------------------------"),
    format("Personagens:~n - ~w~n", [PersonagensPossiveis]),
    format("Armas:~n - ~w~n", [ArmasPossiveis]),
    format("Cômodos:~n - ~w~n~n", [ComodosPossiveis]).


% -----------------------------------------------------------------------
% Predicado utilizado para exibir as cartas do crime
exibir_cena_crime :-
    findall(S, culpado(S), Culpado),
    findall(A, arma_usada(A), Arma),
    findall(C, local_crime(C), Comodo),

    format("Culpado: ~w~n", Culpado),
    format("Arma Usada: ~w~n", Arma),
    format("Local do Crime: ~w~n", Comodo).


% EXCLUIRR
exibir_cartas_jogador :-
    writeln('--- Cartas dos jogadores ---'),
    forall(cartas_jogador(Jogador, Cartas), (
        format('~w possui as cartas: ~w~n', [Jogador, Cartas])
    )).

