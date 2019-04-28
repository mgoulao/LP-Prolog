:- consult(codigo_comum).


aplica_R1_triplo([X, Y, Z], N_Triplo) :- var(X), var(Y), !, N_Triplo = [X, Y, Z].
aplica_R1_triplo([X, Y, Z], N_Triplo) :- var(X), var(Z), !, N_Triplo = [X, Y, Z].
aplica_R1_triplo([X, Y, Z], N_Triplo) :- var(Y), var(Z), !, N_Triplo = [X, Y, Z].

aplica_R1_triplo([X, Y, Z], N_Triplo) :- var(X), Y=\=Z, !, X1 = X, N_Triplo = [X1, Y, Z].
aplica_R1_triplo([X, Y, Z], N_Triplo) :- var(X), X1=0, X1=\=Y,
    X1=\=Z, !,
    N_Triplo = [X1, Y, Z].
aplica_R1_triplo([X, Y, Z], N_Triplo) :- var(X), X1=1, X1=\=Y,
    X1=\=Z, !,
    N_Triplo = [X1, Y, Z].

aplica_R1_triplo([X, Y, Z], N_Triplo) :- var(Y), X=\=Z, !, Y1 = Y, N_Triplo = [X, Y1, Z].
aplica_R1_triplo([X, Y, Z], N_Triplo) :- var(Y), Y1=0, Y1=\=X,
    Y1=\=Z, !,
    N_Triplo = [X, Y1, Z].
aplica_R1_triplo([X, Y, Z], N_Triplo) :- var(Y), Y1=1, Y1=\=X,
    Y1=\=Z, !,
    N_Triplo = [X, Y1, Z].

aplica_R1_triplo([X, Y, Z], N_Triplo) :- var(Z), X=\=Y, !, Z1 = Z, N_Triplo = [X, Y, Z1].
aplica_R1_triplo([X, Y, Z], N_Triplo) :- var(Z), Z1=0, Z1=\=X,
    Z1=\=Y, !,
    N_Triplo = [X, Y, Z1].
aplica_R1_triplo([X, Y, Z], N_Triplo) :- var(Z), Z1=1, Z1=\=X,
    Z1=\=Y, !,
    N_Triplo = [X, Y, Z1].

aplica_R1_triplo([X, Y, Z], N_Triplo) :- X==Y, Y==Z, fail, N_Triplo = [].
aplica_R1_triplo([X, Y, Z], N_Triplo) :- X==Y, Y=\=Z, !, N_Triplo = [X, Y, Z].
aplica_R1_triplo([X, Y, Z], N_Triplo) :- X=\=Y, Y==Z, !, N_Triplo = [X, Y, Z].
aplica_R1_triplo([X, Y, Z], N_Triplo) :- X=\=Y, X==Z, !, N_Triplo = [X, Y, Z].


%===========================
%
%===========================

aplica_R1_fila_aux([X, Y], [X, Y]).

aplica_R1_fila_aux([X, Y, Z | R], N_Fila) :- 
    append([Y, Z], R, Temp_Fila),
    aplica_R1_fila_aux(Temp_Fila, [X1, Y1 | R1]),
    aplica_R1_triplo([X, X1, Y1], Triplo),
    append(Triplo, R1, N_Fila).

%===========================
%
%===========================

aplica_R1_fila(Fila, N_Fila) :- aplica_R1_fila_aux(Fila, N_Fila),
    write(Fila), write(N_Fila),
    Fila == N_Fila, !.

aplica_R1_fila(Fila, N_Fila) :- aplica_R1_fila_aux(Fila, N_Fila_Aux),
write(Fila), write(N_Fila),
    \+ Fila == N_Fila, aplica_R1_fila(N_Fila_Aux, N_Fila).

%===========================
%
%===========================

aplica_R2_fila_conta([], 0, 0).
aplica_R2_fila_conta([ X | R ], Zeros, Uns) :- var(X), !,
    aplica_R2_fila_conta(R, Zeros, Uns).
aplica_R2_fila_conta([ X | R ], Zeros, N_Uns) :- X==1, !,
    aplica_R2_fila_conta(R, Zeros, Uns),
    Um = 1,
    N_Uns is Uns + Um.

aplica_R2_fila_conta([ X | R ], N_Zeros, Uns) :- X==0, !,
    aplica_R2_fila_conta(R, Zeros, Uns),
    Um = 1,
    N_Zeros is Zeros + Um.

%===========================
%
%===========================

aplica_R2_fila_preenche([], [], _).
aplica_R2_fila_preenche([ X | R ], N_Fila, Valor) :- var(X), !,
    aplica_R2_fila_preenche(R, Fila_Antiga, Valor),
    append([Valor], Fila_Antiga, N_Fila).

aplica_R2_fila_preenche([ X | R ], N_Fila, Valor) :-
    aplica_R2_fila_preenche(R, Fila_Antiga, Valor),
    append([X], Fila_Antiga, N_Fila).


%===========================
%
%===========================

aplica_R2_fila(Fila, N_Fila) :- aplica_R2_fila_conta(Fila, Zeros, _),
    length(Fila, L),
    N is div(L, 2),
    write("aplica_R2_fila 1 "),
    Zeros > N, !,
    fail, N_Fila.

aplica_R2_fila(Fila, N_Fila) :- aplica_R2_fila_conta(Fila, _, Uns),
    length(Fila, L),
    N is div(L, 2),
    write("aplica_R2_fila 2 "),
    Uns > N, !,
    fail, N_Fila.

aplica_R2_fila(Fila, N_Fila) :- aplica_R2_fila_conta(Fila, Zeros, Uns),
    length(Fila, L),
    write("aplica_R2_fila 3 "),
    N is div(L, 2),
    Zeros < N, Uns < N, !,
    N_Fila = Fila.

aplica_R2_fila(Fila, N_Fila) :- aplica_R2_fila_conta(Fila, Zeros, _),
    length(Fila, L),
    write("aplica_R2_fila 4 "),
    N is div(L, 2),
    Zeros == N, !,
    aplica_R2_fila_preenche(Fila, N_Fila, 1).

aplica_R2_fila(Fila, N_Fila) :- aplica_R2_fila_conta(Fila, _, Uns),
    length(Fila, L),
    write("aplica_R2_fila 5 "),
    N is div(L, 2),
    Uns =< N, !,
    aplica_R2_fila_preenche(Fila, N_Fila, 0).

%===========================
%
%===========================

aplica_R1_R2_fila(Fila, N_Fila) :- aplica_R1_fila(Fila, Fila_R1),
    aplica_R2_fila(Fila_R1, N_Fila).

%===========================
%
%===========================
aplica_R1_R2_linhas([], []).
aplica_R1_R2_linhas([ X | R ], N_Puz) :- 
    aplica_R1_R2_linhas(R, Temp_Puz),
    aplica_R1_R2_fila(X, N_Linha),
    N_Puz = [N_Linha|Temp_Puz].

aplica_R1_R2_puzzle(Puz, N_Puz) :- 
    aplica_R1_R2_linhas(Puz, N_Puz_Linha),
    mat_transposta(N_Puz_Linha, Puz_Trans),
    aplica_R1_R2_linhas(Puz_Trans, N_Puz_Coluna_Trans),
    mat_transposta(N_Puz_Coluna_Trans, N_Puz).

%===========================
%
%===========================

inicializa(Puz, N_Puz) :-
    aplica_R1_R2_puzzle(Puz, N_Puz),
    Puz == N_Puz, !.

inicializa(Puz, N_Puz) :-
    aplica_R1_R2_puzzle(Puz, N_Puz_R1_R2),
    inicializa(N_Puz_R1_R2, N_Puz).


%===========================
%
%===========================

verifica_R3_linha(_, []).

verifica_R3_linha(Linha1, [ Linha2 | _ ]) :-
    Linha1 == Linha2, !, fail.

verifica_R3_linha(Linha1, [ _ | R ]) :- !,
    verifica_R3_linha(Linha1, R).

verifica_R3_todas_linhas([]).

verifica_R3_todas_linhas([ X | R ]) :- 
    verifica_R3_linha(X, R), !,
    verifica_R3_todas_linhas(R).

verifica_R3(Puz) :-
    verifica_R3_todas_linhas(Puz),
    mat_transposta(Puz, Puz_Trans),
    verifica_R3_todas_linhas(Puz_Trans).Puz = [[0,1,0,1],
    [0,1,0,1],
    [1,_,_,_],
    [1,0,_,_]], verifica_R3(Puz).
    