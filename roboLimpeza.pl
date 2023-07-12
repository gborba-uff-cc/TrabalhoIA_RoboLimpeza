/*
ROBÔ DE LIMPEZA

A sala é um tabuleiro com origem no canto superior esquerdo.
Os numeros de posicao iniciam em 1, o canto superior esquerdo é pos(1,1)).
*/

/*
================================================================================
	PROBLEMAS
--------------------------------------------------------------------------------
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
	use(limiteSolucoes, L),
	busca(B, L, PosInicial, Solucao, Custo).

/*
================================================================================
	CONFIGURACOES
--------------------------------------------------------------------------------
*/
% escolhe o limite de solucoes retornados ao procurar a solucao
use(limiteSolucoes, 1).

% escolhe o algoritmo de busca a ser usado
%use(busca, hillClimb).
%use(busca, bestFirst).
%use(busca, branchAndBound).
use(busca, aEstrela).

% escolher o algoritmo de concatenacao a ser usado
use(concatena, builtin).
%use(concatena, customizado).

% escolher o algoritmo de ordenacao a ser usado
use(ordena, builtin).
%use(ordena, customizado).

% permite o movimento do robo para as diagonais
use(move, diagonais).

% escolher o calculo de distancia a ser usado
use(distancia, manhattan).
%use(distancia, diagHorVert).
%use(distancia, euclidiana).

% escolher o id da sala para resolver o problema
% use(sala, +IdSala)
use(sala, 1).

% escolhe o tamanho do quadrado que representa uma posicao na sala
use(tileSize, "20px").

% escolhe o caractere que representa cada posicao
use(representacaoEntrada, 'E').
use(representacaoSaida, 'S').
use(representacaoVazio, ' ').
use(representacaoSujeira, '-').
use(representacaoObstaculo, '■').

/*
================================================================================
	ABSTRACOES BUSCAS
--------------------------------------------------------------------------------
*/
busca(hillClimb, LimiteSolucoes, PosInicial, Solucao, Custo) :-
	limit(LimiteSolucoes, hillClimb([[0,PosInicial]], Solucao, Custo)).
busca(bestFirst, LimiteSolucoes, PosInicial, Solucao, Custo) :-
	limit(LimiteSolucoes, bestFirst([[0,PosInicial]], Solucao, Custo)).
busca(branchAndBound, LimiteSolucoes, PosInicial, Solucao, Custo) :-
	limit(LimiteSolucoes, branchAndBound([[0,PosInicial]], Solucao, Custo)).
busca(aEstrela, LimiteSolucoes, PosInicial, Solucao, Custo) :-
	limit(LimiteSolucoes, aEstrela([[0,0,0,PosInicial]], Solucao, Custo)).

/*
================================================================================
	ALGORITMOS BUSCAS
--------------------------------------------------------------------------------
*/
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
	objetivo([No|Caminho]),
	reverse([No|Caminho], Solucao).
hillClimb([Caminho|Caminhos], Solucao, Custo) :-
	estendeH(Caminho, NovosCaminhos),
	ordena(NovosCaminhos, CaminhosOrd),
	concatena(CaminhosOrd, Caminhos, CaminhosTotal),
	hillClimb(CaminhosTotal, Solucao, Custo).

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
	objetivo([No|Caminho]),
	reverse([No|Caminho], Solucao).
bestFirst([Caminho|Caminhos], Solucao, Custo):-
	estendeH(Caminho, NovosCaminhos),
	concatena(NovosCaminhos, Caminhos, CaminhosTotal),
	ordena(CaminhosTotal, CaminhosOrd),
	bestFirst(CaminhosOrd, Solucao, Custo).

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
	objetivo([No|Caminho]),
	reverse([No|Caminho], Solucao).
branchAndBound([Caminho|Caminhos], Solucao, G):-
	estendeG(Caminho, NovosCaminhos),
	concatena(Caminhos, NovosCaminhos, CaminhosTotal),
	ordena(CaminhosTotal, CaminhosTotOrd),
	branchAndBound(CaminhosTotOrd, Solucao, G).

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

/*
================================================================================
	OBJETIVO E EXTENSORES
--------------------------------------------------------------------------------
*/
% verifica se o Caminho satisfaz é uma solucao para o problema
% objetivo(+Caminho)
objetivo([PosAtual|PosPassadas]):-
	use(sala, IdSala),
	findall(Pos, sujeira(IdSala, Pos), Sujeiras),
	subconjunto(Sujeiras, PosPassadas),
	posicaoSaida(IdSala, PosAtual).

% verifica se todos os elementos em Lista1 estao em Lista2
% subconjunto(+Lista1, +Lista2)
subconjunto([], [_|_]).
subconjunto([H1|T1], L2) :-
	memberchk(H1, L2),
	subconjunto(T1, L2).

% gera todos os caminhos ao mover uma posicao a partir da posicao atual
% estendeF(+FGHCaminho, -NovosCaminhos)
estendeF([_, G, _, No|Caminho], NovosCaminhos) :-
	findall([FNovo, GNovo, HNovo, NovoNo, No|Caminho],
		(
			heuristica([No|Caminho], NovoNo, DeltaG, HNovo),
			% calcula custo
			GNovo is G + DeltaG,
			% estimativa total
			FNovo is GNovo + HNovo
		),
		NovosCaminhos).

% gera todos os caminhos ao mover uma posicao a partir da posicao atual
% estendeG(+GCaminho, -NovosCaminhos)
estendeG([G, No|Caminho], NovosCaminhos) :-
	findall([GNovo, NovoNo, No|Caminho],
		(
			heuristica([No|Caminho], NovoNo, DeltaG, _),
			% calcula custo
			GNovo is G + DeltaG
		),
		NovosCaminhos).

% gera todos os caminhos ao mover uma posicao a partir da posicao atual
% estendeG(+HCaminho, -NovosCaminhos)
estendeH([_, No|Caminho], NovosCaminhos) :-
	findall([HNovo, NovoNo, No|Caminho],
		(
			heuristica([No|Caminho], NovoNo, _, HNovo)
		),
		NovosCaminhos).

% abstracao da funcao de concatenacao
concatena(L1, L2, L3) :-
	use(concatena, C),
	concatena(C, L1, L2, L3).
% concatena usando a funcao interna
concatena(builtin, L1, L2, L3) :-
	append(L1, L2, L3).
% concatena usando a funcao fornecida
concatena(customizado, [], L, L).
concatena(customizado, [X|L1],L,[X|L2]) :-
	concatena(customizado, L1, L, L2).

% abstracao da funcao de ordenacao
ordena(Original, Ordenado) :-
	use(ordena, O),
	ordena(O, Original, Ordenado).
% ordena usando a funcao interna
ordena(builtin, Original, Ordenado) :-
	% sort remove duplicatas (atomos com mesmo endereco de memoria?)
	sort(Original, Ordenado).
% ordena usando quicksort
ordena(customizado, Caminhos, CaminhosOrd) :-
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

/*
================================================================================
	HEURISTICAS
--------------------------------------------------------------------------------
*/
% gera nova posicao a partir da posicao atual, custo do movimento e avalizaçao
% heuristica(+Caminho, -PosNova, -DeltaG, -H)
heuristica([PosAtual|PosPassadas], PosNova, DeltaG, H) :-
	% passou pela saida no maximo 1 vez
	use(sala, IdSala),
	posicaoSaida(IdSala, PosSaida),
	maximoOcorrencias(PosSaida, [PosAtual|PosPassadas], 1),
	proximoQuadrado(PosAtual, PosNova),
	% ocorrencia anterior da nova posicao no maximo 1 vez (para entrar e sair)
	% REVIEW - limita o número de corredores que compartilham um mesmo quadrado
	maximoOcorrencias(PosNova, [PosAtual|PosPassadas], 1),
	custoDeslocamento(PosAtual, PosNova, DeltaG),
	estimaRestante([PosAtual|PosPassadas], PosNova, H).

% gera PosNova vizinha a PosAtual que esta dentro da sala e nao tenha obstaculo
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

% gera PosVizinha vizinha a PosAtual
% posicaoVizinha(+PosAtual, -PosVizinha)
% acima
posicaoVizinha(pos(X,Y), pos(X, NovoY)) :-
	NovoY is Y-1.
% acima direita
posicaoVizinha(pos(X,Y), pos(NovoX, NovoY)) :-
	use(move, diagonais),
	NovoY is Y-1,
	NovoX is X+1.
% direita
posicaoVizinha(pos(X,Y), pos(NovoX, Y)):-
	NovoX is X+1.
% abaixo direita
posicaoVizinha(pos(X,Y), pos(NovoX, NovoY)) :-
	use(move, diagonais),
	NovoY is Y+1,
	NovoX is X+1.
% abaixo
posicaoVizinha(pos(X,Y), pos(X, NovoY)) :-
	NovoY is Y+1.
% abaixo esquerda
posicaoVizinha(pos(X,Y), pos(NovoX, NovoY)) :-
	use(move, diagonais),
	NovoY is Y+1,
	NovoX is X-1.
% esquerda
posicaoVizinha(pos(X,Y), pos(NovoX, Y)) :-
	NovoX is X-1.
% acima esquerda
posicaoVizinha(pos(X,Y), pos(NovoX, NovoY)) :-
	use(move, diagonais),
	NovoY is Y-1,
	NovoX is X-1.

% verifica se posicao esta dentro dos limites da sala
% dentroSala(+PosicaoQualquer)
dentroSala(pos(X,Y)) :-
	use(sala, IdSala),
	tamanhoSala(IdSala, Larg, Alt),
	0 < X,
	X =< Larg,
	0 < Y,
	Y =< Alt.

% verifica se posicao na sala tem obstaculo
% obstaculo(+PosicaoQualquer)
obstaculo(Pos) :-
	use(sala, IdSala),
	obstaculo(IdSala, Pos).

% falha caso Elemento tenha aparecido mais vezes que Maximo em Lista
% maximoOcorrencias(+Elemento, +Lista, +Maximo)
maximoOcorrencias(Elemento, Lista, Maximo) :-
	maximoOcorrencias(Elemento, Lista, Maximo, _).
% maximoOcorrencias(+Elemento, +Lista, +Maximo, -Contagem)
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

% calcula o custo de deslocamento como a distancia entre as duas posicoes
% custoDeslocamento(+PosAtual, +PosNova, -Custo) :-
custoDeslocamento(PosAtual, PosNova, Custo) :-
	use(distancia, D),
	distancia(D, PosAtual, PosNova, Custo).

% avalia o quao perto da solucao o caminho atual esta
% estimaRestante(+Caminho, +PosNova, -Avaliacao) :-
estimaRestante(_, PosNova, Avaliacao) :-
	use(sala, IdSala),
	use(distancia, D),
	posicaoSaida(IdSala, PosSaida),
	distancia(D, PosNova, PosSaida, Avaliacao).

% calcula a distancia de usando alguma formula
% distancia(+IdDistancia, +PosA, +PosB, -Distancia)
distancia(manhattan, PosA, PosB, Distancia) :-
	distManhattan(PosA, PosB, Distancia).
distancia(diagHorVert, PosA, PosB, Distancia) :-
	dist45DiagHorVert(PosA, PosB, Distancia).
distancia(euclidiana, PosA, PosB, Distancia) :-
	distEuclidiana(PosA, PosB, Distancia).

/*
================================================================================
	FORMULAS DISTANCIAS
--------------------------------------------------------------------------------
*/
% distancia considerando movimentos horizontais e verticais
% distanciaManhattan(+PosicaoA, +PosicaoB, -Distancia)
distManhattan(pos(Ax, Ay), pos(Bx, By), Dist) :-
	_dx is abs(Bx-Ax),
	_dy is abs(By-Ay),
	Dist is _dx+_dy.

% distancia considerando movimentos diagonais de 45, horizontais e verticais
% dist45DiagHorVert(+PosicaoA, +PosicaoB, -Distancia)
dist45DiagHorVert(pos(Ax, Ay),pos(Bx, By), Dist) :-
	_dx is abs(Bx-Ax),
	_dy is abs(By-Ay),
	Dist is max(_dx, _dy).

% distancia em linha reta
% distanciaEuclidiana(+PosicaoA, +PosicaoB, -Distancia)
distEuclidiana(pos(Ax, Ay),pos(Bx, By), Dist) :-
	_dx is abs(Bx-Ax),
	_dy is abs(By-Ay),
	Dist is sqrt(_dx**2+_dy**2).

/*
================================================================================
	REPRESENTACOES
--------------------------------------------------------------------------------
*/
% representa a sala como matriz
% montarSala(-Sala)
montarSala(Sala) :-
	montarLinhas(1, Sala),
	!.

% agrupa as linhas da matriz que representa a sala
% montarLinhas(+NumLinha, -Matriz)
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

% lista as colunas da linha da matriz que representa a sala
% montrarColunas(+NumLinha, +NumColuna, -Lista)
montarColunas(NumLinha, NumColuna, [New|Old]) :-
	use(sala, IdSala),
	tamanhoSala(IdSala, MaxColunas, _),
	NumColuna =< MaxColunas,
	ProximaColuna is NumColuna+1,
	montarColunas(NumLinha, ProximaColuna, Old),
	representacaoTile(IdSala, pos(NumColuna, NumLinha), New).
montarColunas(_, NumColuna, []) :-
	use(sala, IdSala),
	tamanhoSala(IdSala, MaxColunas, _),
	NumColuna > MaxColunas.

% recupera a representacao para os diferentes tipos de quadrados da sala
% representacaoTile(+IdSala, +Pos, -Repr)
representacaoTile(IdSala, Pos, Repr) :-
	posicaoEntrada(IdSala, Pos),
	use(representacaoEntrada, Repr),
	!.
representacaoTile(IdSala, Pos, Repr) :-
	posicaoSaida(IdSala, Pos),
	use(representacaoSaida, Repr),
	!.
representacaoTile(IdSala, Pos, Repr) :-
	sujeira(IdSala, Pos),
	use(representacaoSujeira, Repr),
	!.
representacaoTile(IdSala, Pos, Repr) :-
	obstaculo(IdSala, Pos),
	use(representacaoObstaculo, Repr),
	!.
representacaoTile(_, _, Repr) :-
	use(representacaoVazio, Repr),
	!.

:- use_module(library(dcg/high_order)).
% exibe a matriz Sala como uma tabela html no swish.swi-prolog
% mostrarSalaHTML(+Sala)
mostrarSalaHTML(Sala) :-
	use(tileSize, TileSize),
	html(table(
		[class([table, 'table-striped']),style('width:auto; border: solid black;')],
		[\foreach(member(Linha, Sala),
		html(tr([
			\foreach(member(Coluna, Linha),
				html(td(style(['text-align:center; vertical-align:center; padding:0; border: 1px solid black; margin:0; width: ',TileSize,'; height: ',TileSize,';']), Coluna)))
			]))
		)]
	)).

% exibe a solucao sobre a matriz da sala no swish.swi-prolog
% mostrarSolucaoHTML(+Solucao)
mostrarSolucaoHTML(Solucao) :-
	use(tileSize, TileSize),
	use(sala, IdSala),
	tamanhoSala(IdSala, Largura, Altura),
	html(table(
		[class(table), style('width:auto; border: solid black;')],
		[\foreach(between(1, Altura, Y),
			html(tr([
				\foreach((between(1, Largura, X), Pos = pos(X, Y), findall(N, encontre(Pos, Solucao, N), NumVisita)),
					html(td(style(['text-align:center; vertical-align:center; padding:0; border: 1px solid black; margin:0; width: ',TileSize,'; height: ',TileSize,';']), NumVisita))
				)
			]))
		)]
	)).

% encontra a posicao de um elemento na lista (logica de membro e soma)
% encontre(+Elemento, +Lista, -Posicao)
encontre(Elemento, [Elemento|_], 1).
encontre(Elemento, [_|T], N) :-
	encontre(Elemento, T, N1),
	N is N1+1.

/** <examples>
% exibe sala na forma matricial
?-
montarSala(SalaSuja).

% exibe sala na forma matricial e lista a solucao
?-
montarSala(SalaSuja),
resolveProblema(Solucao, Custo),
use(distancia, FormulaDistancia).

% lista a solucao
?-
resolveProblema(Solucao, Custo),
use(distancia, FormulaDistancia).

% exibe sala como elemento html
?-
montarSala(Sala),
mostrarSalaHTML(Sala),
projection([]).

% exibe sala e solucao como elemento html
?-
montarSala(SalaSuja),
mostrarSalaHTML(SalaSuja),
resolveProblema(Solucao, Custo),
mostrarSolucaoHTML(Solucao),
use(distancia, FormulaDistancia),
projection([Custo, FormulaDistancia]).
*/
