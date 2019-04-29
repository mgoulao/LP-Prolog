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
    Fila == N_Fila, !.

aplica_R1_fila(Fila, N_Fila) :- aplica_R1_fila_aux(Fila, N_Fila_Aux),
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
    Zeros > N, !,
    fail, N_Fila.

aplica_R2_fila(Fila, N_Fila) :- aplica_R2_fila_conta(Fila, _, Uns),
    length(Fila, L),
    N is div(L, 2),
    Uns > N, !,
    fail, N_Fila.

aplica_R2_fila(Fila, N_Fila) :- aplica_R2_fila_conta(Fila, Zeros, Uns),
    length(Fila, L),
    N is div(L, 2),
    Zeros < N, Uns < N, !,
    N_Fila = Fila.

aplica_R2_fila(Fila, N_Fila) :- aplica_R2_fila_conta(Fila, Zeros, _),
    length(Fila, L),
    N is div(L, 2),
    Zeros == N, !,
    aplica_R2_fila_preenche(Fila, N_Fila, 1).

aplica_R2_fila(Fila, N_Fila) :- aplica_R2_fila_conta(Fila, _, Uns),
    length(Fila, L),
    N is div(L, 2),
    Uns =< N, !,
    aplica_R2_fila_preenche(Fila, N_Fila, 0).

%===========================
%
%===========================

aplica_R1_R2_fila(Fila, N_Fila) :- 
    aplica_R1_fila(Fila, Fila_R1),
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
    verifica_R3_todas_linhas(Puz_Trans).
    

%===========================
% 
%===========================

propaga_dif_linha(_, _, _, N_Coluna, Tamanho, []) :-
    N_Coluna > Tamanho, !.
propaga_dif_linha(Matriz_Atual, Matriz_Antiga, N_Linha, N_Coluna, Tamanho, Diff_Colunas) :-
    Prox_Coluna is N_Coluna + 1,
    propaga_dif_linha(Matriz_Atual, Matriz_Antiga, N_Linha, Prox_Coluna, Tamanho, Diff_Rec_Coluna),
    mat_ref(Matriz_Atual, (N_Linha, N_Coluna), Atual),
    mat_ref(Matriz_Antiga, (N_Linha, N_Coluna), Antigo),
    var(Atual), var(Antigo), !,
    Diff_Colunas = Diff_Rec_Coluna.
propaga_dif_linha(Matriz_Atual, Matriz_Antiga, N_Linha, N_Coluna, Tamanho, Diff_Colunas) :-
    Prox_Coluna is N_Coluna + 1,
    propaga_dif_linha(Matriz_Atual, Matriz_Antiga, N_Linha, Prox_Coluna, Tamanho, Diff_Rec_Coluna),
    mat_ref(Matriz_Atual, (N_Linha, N_Coluna), Atual),
    mat_ref(Matriz_Antiga, (N_Linha, N_Coluna), Antigo),
    \+ var(Atual), var(Antigo), !,
    append([N_Coluna], Diff_Rec_Coluna, Diff_Colunas).
propaga_dif_linha(Matriz_Atual, Matriz_Antiga, N_Linha, N_Coluna, Tamanho, Diff_Colunas) :-
    Prox_Coluna is N_Coluna + 1,
    propaga_dif_linha(Matriz_Atual, Matriz_Antiga, N_Linha, Prox_Coluna, Tamanho, Diff_Rec_Coluna),
    Diff_Colunas = Diff_Rec_Coluna.


propaga_aplica_R1_R2_linha(Matriz_Atual, Matriz_Resultado, N_Linha) :-
    mat_transposta(Matriz_Atual, Mat_Transp),
    mat_elementos_coluna(Mat_Transp, N_Linha, Linha),
    aplica_R1_R2_fila(Linha, Nova_Linha),
    mat_muda_linha(Matriz_Atual, N_Linha, Nova_Linha, Matriz_Resultado).

propaga_aplica_R1_R2_coluna(Matriz_Atual, Matriz_Resultado, N_Coluna) :-
    mat_elementos_coluna(Matriz_Atual, N_Coluna, Coluna),
    aplica_R1_R2_fila(Coluna, Nova_Coluna),
    mat_muda_coluna(Matriz_Atual, N_Coluna, Nova_Coluna, Matriz_Resultado).

propaga_constroi_lista_posicao_coluna([], _ , []).
propaga_constroi_lista_posicao_coluna([ X | R ], N_Coluna, Novas_Posicoes) :-
    propaga_constroi_lista_posicao_coluna(R, N_Coluna, Novas_Posicoes_Ant),
    append([(X, N_Coluna)], Novas_Posicoes_Ant, Novas_Posicoes).

propaga_constroi_lista_posicao_linha([], _ , []).
propaga_constroi_lista_posicao_linha([ X | R ], N_Linha, Novas_Posicoes) :-
    propaga_constroi_lista_posicao_linha(R, N_Linha, Novas_Posicoes_Ant),
    append([(N_Linha, X)], Novas_Posicoes_Ant, Novas_Posicoes).

propaga_posicoes([], Puz, N_Puz) :-
    N_Puz = Puz.

propaga_posicoes([ (L,C) | R ], Puz, N_Puz) :-
    mat_dimensoes(Puz, N, _),
    %Linha
    propaga_aplica_R1_R2_linha(Puz, Puz_Res_L, L),
    propaga_dif_linha(Puz_Res_L, Puz, L, 1, N, Diff_Colunas),
    propaga_constroi_lista_posicao_linha(Diff_Colunas, L, Novas_Posicoes_Colunas),
    %Coluna
    propaga_aplica_R1_R2_coluna(Puz_Res_L, Puz_Res_C, C),
    mat_transposta(Puz, Puz_Transposto),
    mat_transposta(Puz_Res_C, Puz_Res_Transposto),
    propaga_dif_linha(Puz_Res_Transposto, Puz_Transposto, C, 1, N, Diff_Linhas),
    
    propaga_constroi_lista_posicao_coluna(Diff_Linhas, C, Novas_Posicoes_Linhas),
    
    append(Novas_Posicoes_Colunas, Novas_Posicoes_Linhas, Todas_Novas_Posicoes),
    append(Todas_Novas_Posicoes, R, Todas_Posicoes),

    verifica_R3(Puz_Res_C),
    propaga_posicoes(Todas_Posicoes, Puz_Res_C, N_Puz).
     

resolve_preenche(Puz, (L, C), Puz_Res) :-
    %Zero
    mat_muda_posicao(Puz, (L, C), 0, Puz_Zero),
    propaga_posicoes([(L,C)], Puz_Zero, Puz_Res), !.

resolve_preenche(Puz, (L, C), Puz_Res) :-
    %Um
    mat_muda_posicao(Puz, (L, C), 1, Puz_Zero),
    propaga_posicoes([(L,C)], Puz_Zero, Puz_Res), !.

resolve_aux(Puz, (L, C), N_Puz) :-
    mat_dimensoes(Puz, N, _),
    C > N,
    L1 is L + 1,
    C1 = 1,
    resolve_aux(Puz,(L1, C1), N_Puz).

resolve_aux(Puz, (L, _), N_Puz) :-
    mat_dimensoes(Puz, N, _),
    L > N, !,
    N_Puz = Puz.

resolve_aux(Puz, (L, C), N_Puz) :-
    mat_ref(Puz, (L, C), X),
    var(X),
    resolve_preenche(Puz, (L, C), Puz_Res),
    C1 is C + 1,
    resolve_aux(Puz_Res,(L, C1), N_Puz), !.

resolve_aux(Puz, (L, C), N_Puz) :-
    C1 is C + 1,
    resolve_aux(Puz,(L, C1), N_Puz).

percorre_posicoes(Puz, Sol) :-
    L=1,
    C=1,
    resolve_aux(Puz, (L, C), Sol),
    percorre_posicoes(Sol).

resolve(Puz, Sol) :-
    L=1,
    C=1,
    resolve_aux(Puz, (L, C), Sol),
    escreve_Puzzle(Sol).