/*
ROBÔ DE LIMPEZA

A sala é um tabuleiro com origem no canto superior esquerdo.
Os numeros de posicao iniciam em 1, o canto superior esquerdo é pos(1,1)).
*/

/*
==============================
configuracoes problema
------------------------------
*/
% tamanhoSala(+IdSala, +Largura, +Altura)
tamanhoSala(1, 5, 5).

% obstaculo(+IdSala, +PosObstaculo)
obstaculo(1, pos(2,2)).

% sujeira(+IdSala, +PosSujeira)
sujeira(1, pos(3,3)).

%posicaoEntrada(+IdSala, +Posicao)
posicaoEntrada(1, pos(1, 1)).

% posicaoSaida(+IdSala, +Posicao)
posicaoSaida(1, pos(4, 4)).

% resolveProblema(-Solucao, -Custo)
resolveProblema(Solucao, Custo) :-
	use(sala, IdSala),
	posicaoEntrada(IdSala, PosEntrada),
	limparSala(PosEntrada, Solucao, Custo).

% limparSala(+PosInicial, +PosFinal, -Solucao, -Custo)
limparSala(PosInicial, Solucao, Custo) :-
	use(busca, B),
	% use(concatena, C),
	% use(ordena, O),
	% use(sala, IdSala),
	% mapaSala(IdSala, S),
	busca(B, PosInicial, Solucao, Custo).

% ==============================
% definicoes
% ------------------------------
% escolhe o algoritmo de busca a ser usado
%use(busca, hillclimb).
%use(busca, bestFirst).
%use(busca, branchAndBound).
use(busca, aEstrela).

% escolher o algoritmo de concatenacao a ser usado
use(concatena, builtin).
%use(concatena, customizado).

% escolher o algoritmo de ordenacao a ser usado
use(ordena, builtin).
%use(ordena, customizado).

% escolher o calculo de distancia a ser usado
use(distancia, manhattan).
%use(distancia, diagHorVert).
%use(distancia, euclidiana).

% escolher o id da sala para resolver o problema
% use(sala, +IdSala)
use(sala, 1).

% escolhe o caractere que representa cada posicao
tile(entrada, 'E').
tile(saida, 'S').
tile(vazio, ' ').
tile(sujeira, '-').
tile(obstaculo, '■').

% ==============================
% buscas
% ------------------------------
busca(hillClimb, PosInicial, Solucao, Custo) :-
	hillClimb([[0,PosInicial]], Solucao, Custo).
busca(bestFirst, PosInicial, Solucao, Custo) :-
	bestFirst([[0,PosInicial]], Solucao, Custo).
busca(branchAndBound, PosInicial, Solucao, Custo) :-
	branchAndBound([[0,PosInicial]], Solucao, Custo).
busca(aEstrela,  PosInicial, Solucao, Custo) :-
	aEstrela([[0,0,0,PosInicial]], Solucao, Custo).

/*
LOGICA:
verifica > estende (avaliacao) > ordena (extensao) > concatena (prepende extensao)> repete

REQUER:
estendeH/2
concatena/3
ordenaF/2
objetivo/1
*/
hillClimb([[_, No|Caminho]|_], Solucao, '-') :-
	objetivo(No),
	reverse([No|Caminho], Solucao).
hillClimb([Caminho|Caminhos], Solucao, Custo) :-
	estendeH(Caminho, NovosCaminhos),
	ordena(NovosCaminhos, CaminhosOrd),
	concatena(CaminhosOrd, Caminhos, CaminhosTotal),
	hillClimb(CaminhosTotal, Solucao, Custo).
% ----------
/*
LOGICA:
verifica > estende (avaliacao) > concatena > ordena > repete

REQUER:
estendeH/2
concatena/3
ordenaF/2
objetivo/1
*/
bestFirst([[_, No|Caminho]|_], Solucao, '-'):-
	objetivo(No),
	reverse([No|Caminho], Solucao).
bestFirst([Caminho|Caminhos], Solucao, Custo):-
	estendeH(Caminho, NovosCaminhos),
	concatena(NovosCaminhos, Caminhos, CaminhosTotal),
	ordena(CaminhosTotal, CaminhosOrd),
	bestFirst(CaminhosOrd, Solucao, Custo).
% ----------
/*
LOGICA:
verifica > estende (custo) > concatena > ordena > repete

REQUER:
estendeH/2
concatena/3
ordenaF/2
objetivo/1
*/
branchAndBound([[G, No|Caminho]|_], Solucao, G):-
	objetivo(No),
	reverse([No|Caminho], Solucao).
branchAndBound([Caminho|Caminhos], Solucao, G):-
	estendeG(Caminho, NovosCaminhos),
	concatena(Caminhos, NovosCaminhos, CaminhosTotal),
	ordena(CaminhosTotal, CaminhosTotOrd),
	branchAndBound(CaminhosTotOrd, Solucao, G).
% ----------
/*
LOGICA:
verifica > estende (custo + avaliacao) > concatena > ordena > repete

REQUER:
estendeF/2
concatena/3
ordenaF/2
objetivo/1
*/
aEstrela([[G, _, _, No|Caminho]|_], Solucao, G) :-
	objetivo([No|Caminho]),
	reverse([No|Caminho], Solucao).
aEstrela([Caminho|Caminhos], Solucao, G) :-
	estendeF(Caminho, NovosCaminhos),
	concatena(Caminhos, NovosCaminhos, CaminhosTotal),
	ordena(CaminhosTotal, CaminhosTotOrd),
	aEstrela(CaminhosTotOrd, Solucao, G).

% ==============================
%
% ------------------------------
objetivo([PosAtual|PosPassadas]):-
	use(sala, IdSala),
	findall(Pos, sujeira(IdSala, Pos), Sujeiras),
	subconjunto(Sujeiras, PosPassadas),
	posicaoSaida(IdSala, PosAtual).

subconjunto([], [_|_]).
subconjunto([H1|T1], L2) :-
	memberchk(H1, L2),
	subconjunto(T1, L2).

% ==============================
% extensores
% ------------------------------
%estendeF([_, GC, _, No|Caminho], NovosCaminhos):-
%	findall([FNovo, GNovo, HNovo, NovoNo, No|Caminho],
%		(
%			sF(GN, HN, _, No, NovoNo),
%			not(member(NovoNo, [No|Caminho])),
%			GNovo is GC + GN,
%			HNovo is HN,
%			FNovo is GNovo + HNovo
%		),
%		NovosCaminhos).

%estendeG([G, No|Caminho], NovosCaminhos) :-
%	findall([GNovo, NovoNo, No|Caminho],
%		(
%			sG(GN, No, NovoNo),
%			not(member(NovoNo, [No|Caminho])),
%			GNovo is GN + G),
%		NovosCaminhos).

%estendeH([_, No|Caminho], NovosCaminhos) :-
%	findall([HNovo, NovoNo, No|Caminho],
%		(
%			sH(HN, No, NovoNo),
%			not(member(NovoNo, [No|Caminho])),
%			HNovo is HN),
%		NovosCaminhos).

estendeF([_, G, _, No|Caminho], NovosCaminhos) :-
	findall([FNovo, GNovo, HNovo, NovoNo, No|Caminho],
		(
			heuristica(No, [No|Caminho], NovoNo, DeltaG, HNovo),
			% calcula custo
			GNovo is G + DeltaG,
			% estimativa total
			FNovo is GNovo + HNovo
		),
		NovosCaminhos).

estendeG([G, No|Caminho], NovosCaminhos) :-
	findall([GNovo, NovoNo, No|Caminho],
		(
			heuristica(No, [No|Caminho], NovoNo, DeltaG, _),
			% calcula custo
			GNovo is G + DeltaG
		),
		NovosCaminhos).

estendeH([_, No|Caminho], NovosCaminhos) :-
	findall([HNovo, NovoNo, No|Caminho],
		(
			heuristica(No, [No|Caminho], NovoNo, _, HNovo)
		),
		NovosCaminhos).
% ----------

concatena(L1, L2, L3) :-
	append(L1, L2, L3).

ordena(Original, Ordenado) :-
	sort(Original, Ordenado).

% ----------
/*
concatena([], L, L).
concatena([], L, L) :-
	use(concatena, customizado).
	concatena(L1, L, L2).

ordena(Caminhos, CaminhosOrd) :-
	use(ordena, customizado),
	quicksortF(Caminhos, CaminhosOrd).

particionarF(_, [], [], []).
particionarF(X, [Y|Cauda], [Y|Menor], Maior) :-
	maiorF(X, Y),
	!,
	particionarF(X, Cauda, Menor, Maior).
particionarF(X, [Y|Cauda], Menor, [Y|Maior]) :-
	particionarF(X, Cauda, Menor, Maior).

quicksortF([], []).
quicksortF([X|Cauda], ListaOrd):-
	particionarF(X, Cauda, Menor, Maior),
	quicksortF(Menor, MenorOrd),
	quicksortF(Maior, MaiorOrd),
	concatena(MenorOrd, [X|MaiorOrd], ListaOrd).

maiorF([F1|_], [F2|_]):-
	F1 > F2.
*/

% ==============================
%
% ------------------------------
% heuristica
heuristica(PosAtual, Caminho, PosNova, DeltaG, H) :-
	proximoQuadrado(PosAtual, PosNova),
	maximoOcorrencias(PosNova, Caminho, 2),
	custoDeslocamento(PosAtual, PosNova, DeltaG),
	avaliaRestante(PosAtual, PosNova, H).

% proximoQuadrado(+PosAtual, +Caminho, -PosNova)
proximoQuadrado(PosAtual, PosNova) :-
	% posicao ao lado da atual
	posicaoVizinha(PosAtual, PosNova),
	% nova posicao é valida
	dentroSala(PosNova),
	% nova posicao não tem obstáculo
% REVIEW - \+obstaculo; \+(obstaculo); \+ obstaculo;
% REVIEW - todos passaram a funcionar depois de executar not(obstaculo)
	\+obstaculo(PosNova).
	% % nova posicao tem sujeira
	% sujeira(PosNova),
	% verifica que a PosNova foi visitada no maximo uma vez
	%maxOcorrencias(PosNova, Caminho, Oc),
	% visitado no maximo 1 vez
	%2 >= Oc.

% posicaoVizinha(+PosAtual, -PosVizinha)
% acima
posicaoVizinha(pos(X,Y), pos(X, NovoY)) :-
	NovoY is Y-1.
% acima direita
posicaoVizinha(pos(X,Y), pos(NovoX, NovoY)) :-
	NovoY is Y-1,
	NovoX is X+1.
% direita
posicaoVizinha(pos(X,Y), pos(NovoX, Y)):-
	NovoX is X+1.
% abaixo direita
posicaoVizinha(pos(X,Y), pos(NovoX, NovoY)) :-
	NovoY is Y+1,
	NovoX is X+1.
% abaixo
posicaoVizinha(pos(X,Y), pos(X, NovoY)) :-
	NovoY is Y+1.
% abaixo esquerda
posicaoVizinha(pos(X,Y), pos(NovoX, NovoY)) :-
	NovoY is Y+1,
	NovoX is X-1.
% esquerda
posicaoVizinha(pos(X,Y), pos(NovoX, Y)) :-
	NovoX is X-1.
% acima esquerda
posicaoVizinha(pos(X,Y), pos(NovoX, NovoY)) :-
	NovoY is Y-1,
	NovoX is X-1.

% dentroSala(+PosicaoQualquer)
dentroSala(pos(X,Y)) :-
	use(sala, IdSala),
	tamanhoSala(IdSala, Larg, Alt),
	0 < X,
	X =< Larg,
	0 < Y,
	Y =< Alt.

% obstaculo(+PosicaoQualquer)
obstaculo(Pos) :-
	use(sala, IdSala),
	obstaculo(IdSala, Pos).

%% ocorrencias(+Elemento, +Lista, -NumOcorrencias)
%ocorrencias(_, [], 0) :- !.
%ocorrencias(Elem, [Elem| Cauda], N) :-
%	!,
%	ocorrencias(Elem, Cauda, _n),
%	N is _n+1.
%ocorrencias(Elem, [NotElem| Cauda], N) :-
%	Elem \== NotElem,
%	!,
%	ocorrencias(Elem, Cauda, N).

maximoOcorrencias(Elemento, Lista, Maximo) :-
	maximoOcorrencias(Elemento, Lista, Maximo, _).
maximoOcorrencias(_, [], _, 0) :-
	!.
maximoOcorrencias(Elem, [NotElem| Cauda], Max, N) :-
	Elem \== NotElem,
	maximoOcorrencias(Elem, Cauda, Max, N),
	!.
maximoOcorrencias(Elem, [Elem| Cauda], Max, N) :-
	maximoOcorrencias(Elem, Cauda, Max, _n),
	N is _n+1,
	N =< Max,
	!.

% custoDeslocamento(+PosAtual, +PosNova, -Custo) :-
custoDeslocamento(PosAtual, PosNova, Custo) :-
	use(distancia, D),
	distancia(D, PosAtual, PosNova, Custo).

% avaliaRestante(+PosAtual, +PosNova, -Avaliacao) :-
avaliaRestante(_, PosNova, Avaliacao) :-
	use(sala, IdSala),
	use(distancia, D),
	posicaoSaida(IdSala, PosSaida),
	distancia(D, PosNova, PosSaida, Avaliacao).

% calcula a distancia de alguma maneira
distancia(manhattan, PosA, PosB, Distancia) :-
	distManhattan(PosA, PosB, Distancia).
distancia(diagHorVert, PosA, PosB, Distancia) :-
	dist45DiagHorVert(PosA, PosB, Distancia).
distancia(euclidiana, PosA, PosB, Distancia) :-
	distEuclidiana(PosA, PosB, Distancia).

%custoPasso(PosAtual, PosVizinho, Custo) :-
%	distManhattan(PosAtual, PosVizinho, Custo).

%avaliacao(PosAtual, PosVizinho, Custo) :-
%	distManhattan(PosAtual, PosVizinho, Custo).

% ==============================
% calculos de distância
% ------------------------------
% distancia considerando movimentos horizontais e verticais
distManhattan(pos(Ax, Ay), pos(Bx, By), Dist) :-
	_dx is abs(Bx-Ax),
	_dy is abs(By-Ay),
	Dist is _dx+_dy.

% distancia considerando movimentos diagonais de 45, horizontais e verticais
dist45DiagHorVert(pos(Ax, Ay),pos(Bx, By), Dist) :-
	_dx is abs(Bx-Ax),
	_dy is abs(By-Ay),
	Dist is max(_dx, _dy).

% distancia em linha reta
distEuclidiana(pos(Ax, Ay),pos(Bx, By), Dist) :-
	_dx is abs(Bx-Ax),
	_dy is abs(By-Ay),
	Dist is sqrt(_dx**2+_dy**2).

/*
==============================

------------------------------
*/
montarSala(Sala) :-
	montarLinhas(1, Sala),
	!.

montarLinhas(NumLinha, [New|Old]) :-
	use(sala, IdSala),
	tamanhoSala(IdSala, _, MaxLinhas),
	NumLinha =< MaxLinhas,
	ProximaLinha is NumLinha+1,
	montarLinhas(ProximaLinha, Old),
	montarColunas(NumLinha, 1, New).

montarLinhas(NumLinha, []) :-
	use(sala, IdSala),
	tamanhoSala(IdSala, _, MaxLinhas),
	NumLinha > MaxLinhas.

montarColunas(NumLinha, NumColuna, [New|Old]) :-
	use(sala, IdSala),
	tamanhoSala(IdSala, MaxColunas, _),
	NumColuna =< MaxColunas,
	ProximaColuna is NumColuna+1,
	montarColunas(NumLinha, ProximaColuna, Old),
	representacaoTile(IdSala, NumLinha, NumColuna, New).

montarColunas(_, NumColuna, []) :-
	use(sala, IdSala),
	tamanhoSala(IdSala, MaxColunas, _),
	NumColuna > MaxColunas.

representacaoTile(IdSala, X, Y, Repr) :-
	posicaoEntrada(IdSala, pos(X, Y)),
	tile(entrada, Repr),
	!.
representacaoTile(IdSala, X, Y, Repr) :-
	posicaoSaida(IdSala, pos(X, Y)),
	tile(saida, Repr),
	!.
representacaoTile(IdSala, X, Y, Repr) :-
	sujeira(IdSala, pos(X, Y)),
	tile(sujeira, Repr),
	!.
representacaoTile(IdSala, X, Y, Repr) :-
	obstaculo(IdSala, pos(X, Y)),
	tile(obstaculo, Repr),
	!.
representacaoTile(_, _, _, Repr) :-
	tile(vazio, Repr),
	!.

:- use_module(library(dcg/high_order)).
mostrarSalaHTML(Sala) :-
	html(table(
		[class([table, 'table-striped']),style('width:auto; border: solid black;')],
		[\foreach(member(Linha, Sala),
		html(tr([
			\foreach(member(Coluna, Linha),
				html(td(style('text-align:center; vertical-align:center; padding:0; border: 1px solid black; margin:0; width: 20px; height:20px;'), Coluna)))
			]))
		)]
	)).

/** <examples>
?- resolveProblema(Solucao, Custo).
?- montarSala(Ls).
?- montarSala(Sala), mostrarSalaHTML(Sala), projection([]).
*/