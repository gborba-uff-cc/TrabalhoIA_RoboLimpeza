% escolhe o algoritmo de busca a ser usado
use(busca, hillclimb).
%use(busca, bestFirst).
%use(busca, branchAndBound).
%use(busca, aEstrela).

% escolher o algoritmo de concatenacao a ser usado
use(concatena, builtin).
%use(concatena, customizado).

% escolher o algoritmo de ordenacao a ser usado
use(ordena, builtin).
%use(ordena, customizado).

% escolher o id da sala para resolver o problema
% use(sala, +IdSala)
use(sala, 1).

% tamanhoSala(+IdSala, +Largura, +Altura)
tamanhoSala(1, 5, 5).

% obstaculo(+IdSala, +PosObstaculo)
obstaculo(1, pos()).

% sujeira(+IdSala, +PosSujeira)
sujeira(1, pos()).

%posicaoEntrada(+IdSala, +Posicao)
posicaoEntrada(1, pos(0, 0)).

% posicaoSaida(+IdSala, +Posicao)
posicaoSaida(1, pos(4, 4)).

resolveProblema(Solucao) :-
	use(sala, IdSala),
	posicaoEntrada(IdSala, PosEntrada),
	posicaoSaida(IdSala, PosSaida),
	limparSala(PosEntrada, PosSaida, Solucao).

limparSala(PosInicial, PosFinal, Solucao) :-
	use(busca, B),
	% use(concatena, C),
	% use(ordena, O),
	% use(sala, IdSala),
	% mapaSala(IdSala, S),
	busca(B, PosInicial, PosFinal, Solucao).

busca(hillClimb, PosInicial, PosFinal, Custo) :-
	hillClimb([[PosInicial]], Solucao, Custo).
busca(bestFirst, PosInicial, PosFinal, Custo) :-
	bestFirst([[PosInicial]], Solucao, Custo).
busca(branchAndBound, PosInicial, PosFinal, Custo) :-
	branchAndBound([[PosInicial]], Solucao, Custo).
busca(aEstrela,  PosInicial, PosFinal, Custo) :-
	aEstrela([[PosInicial]], Solucao, Custo).

% ==============================
% heuristica custo e avaliacao
% ------------------------------
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
aEstrela([[G, _, _, No|Caminho]|_], Solucao, G):-
	objetivo(No),
	reverse([No|Caminho], Solucao).
aEstrela([Caminho|Caminhos], Solucao, G) :-
	estendeF(Caminho, NovosCaminhos),
	concatena(Caminhos, NovosCaminhos, CaminhosTotal),
	ordena(CaminhosTotal, CaminhosTotOrd),
	aEstrela(CaminhosTotOrd, Solucao, G).

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
estendeF([_, GC, _, No|Caminho], NovosCaminhos):-
	findall([FNovo, GNovo, HNovo, NovoNo, No|Caminho],
		(
			proximoQuadrado(PosAtual, [No|Caminho], PosNova),
			% calcula custo corrente
			GNovo is GC + GN,
			% estima custo restante
			HNovo is HN,
			%
			FNovo is GNovo + HNovo
		),
		NovosCaminhos).

estendeG([G, No|Caminho], NovosCaminhos) :-
	findall([GNovo, NovoNo, No|Caminho],
		(
			sG(GN, No, NovoNo),
			not(member(NovoNo, [No|Caminho])),
			GNovo is GN + G),
		NovosCaminhos).

estendeH([_, No|Caminho], NovosCaminhos) :-
	findall([HNovo, NovoNo, No|Caminho],
		(
			sH(HN, No, NovoNo),
			not(member(NovoNo, [No|Caminho])),
			HNovo is HN),
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
% proximoQuadrado(+PosAtual, +Caminho, -PosNova)
proximoQuadrado(PosAtual, Caminho, PosNova) :-
	% posicao ao lado da atual
	posicaoVizinha(PosAtual, PosNova),
	% nova posicao é valida
	dentroSala(PosNova),
	% nova posicao não tem obstáculo
	\obstaculo(PosNova),
	% % nova posicao tem sujeira
	% sujeira(PosNova),
	% visitado no maximo 1 vez
	ocorrencias(PosNova, Caminho, Oc),
	2 >= Oc.

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
posicaoVizinha(pos(X,Y), pos(NovoX, NovoY)) :-
	NovoX is X-1.
% acima esquerda
posicaoVizinha(pos(X,Y), pos(NovoX, NovoY)) :-
	NovoY is Y-1,
	NovoX is X-1.

% dentroSala(+PosicaoQualquer)
dentroSala(pos(X,Y)) :-
	tamanhoSala(Larg, Alt),
	0 < X,
	X >= Larg,
	0 < Y,
	Y >= Alt.

% ocorrencias(+Elemento, +Lista, -NumOcorrencias)
ocorrencias(_, [], 0) :- !.
ocorrencias(Elem, [Elem| Cauda], N) :-
	!,
	ocorrencias(Elem, Cauda, _n),
	N is _n+1.
ocorrencias(Elem, [NotElem| Cauda], N) :-
	Elem \== NotElem,
	!,
	ocorrencias(Elem, Cauda, N).

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
