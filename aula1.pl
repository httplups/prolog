% tamanho de uma lista tam(+,-)
tam(L,N) :- tam(L,N,0).
% tam(+,-,+)
tam([], N, Acc) :- N = Acc.
tam([_|R],N,Acc) :- Nacc is Acc+1, tam(R,N, Nacc).

% somar quantos elementos numa lista, cont(+,-)
% somar([1,2,3,4,5],M).
% caso baso Acc = 0, dps faz 5+0, 4 + 5, 3 + 9, 2 +12, 1 + 14 e devolve 15
somar(L, N) :- somar(L, N, 0).
% cont(+,-,+)
somar([], N, Acc) :- N = Acc.
somar([X|R],N,Acc) :- Nacc is Acc + X, somar(R,N,Nacc).

% trocar uma ocorrencia de um elemento X na lista
% trocar1(+,+,+,_)
trocar1([],_,_,[]).
% verifica se X unifica com Velho
trocar1([X | R], N, V, LN) :- X = V, LN = [N | R].
% senão,
trocar1([X | R], N, V, LN) :- trocar1(R,N,V,RR), LN = [X | RR].

% troca todas as ocorrencias
% trocar(+,+,+,_)
trocarAll([],_,_,[]).
% verifica se X unifica com Velho
trocarAll([X | R], N, V, LN) :- X = V, trocarAll(R,N,V,RR),LN = [N | RR].
% senão,
trocarAll([X | R], N,V, LN) :- trocarAll(R,N,V,RR), LN = [X | RR].

% maior elemento de uma lista
maior([X | R], Max) :- maior([X | R], Max, X).
maior([],Max,Acc) :- Max = Acc.
maior([X | R], Max, Acc) :- X > Acc, maior(R, Max, X).
maior([X | R], Max, Acc) :- maior(R, Max, Acc).


append([],A,A).
append([X|R],A,AA) :- append(R,A,RR), AA = [X|RR].

remove([],_,_,[]).
remove(L, _, 0, LN) :- LN = L.
remove([X | R], It, N, LN) :- X = It, NN is N-1, remove(R, It, NN, RR), LN = RR.
remove([X | R], It, N, LN) :- remove(R, It, N, RR), LN = [X | RR].

maximo([X],X).
maximo([X|R],M) :- maximo(R,MM), (MM > X, M = MM) ; M = X.

mx([X],X).
mx([X|R],M) :- mx(R,MM), MM > X, M = MM.
mx([X|R], M) :- M = X.


% (if) -> then ; else
maximoo([X], X).
maximoo([X | R], M) :- maximoo(R, MM), (MM > X) -> M = MM; M = X.

tamanhoo([],0).
tamanhoo([X|R],T) :- tamanhoo(R,TT), T is TT+1.

% conta dos números pares de uma lista somap(+LISTA,-SOMA)
contapares([],0).
contapares([X | R],S) :- contapares(R,SS), ((0 is mod(X,2)) -> S is SS+1; S is SS).

% existe elem na lista
elem(X,[X|_]) :- true.
elem(IT,[_ | R]) :- elem(IT,R).

% posição do item na lista: 1 se é o primeiro, falha se nao esta na lista pos(+IT,+LISTA,-POS)
pos(IT,L,P) :- pos(IT,L,P,1).
pos(IT,[X|_],P,N) :- X=IT,P=N.
pos(IT, [_|R],P,N) :- NN is N+1, pos(IT,R,P,NN).

pos1(IT,L,P) :- pos(IT,L,P,1).
% aqui fica pos1(+,+,-,+).
pos1(IT,[X|R],P,N) :- X=IT -> P=N ; NN is N+1, pos1(IT,R,P,NN).

% quantas vezes it aparece na lista
conta(_,[],0).
conta(I, [X | R], C) :- conta(I,R,CC), (X = I -> C is CC+1; C is CC).

conta1(_,[],0).
conta1(I,[X|R],C) :- conta1(I,R,CC), (X = I, C is CC+1; C is CC).

% somapares 
somapares([],0).
somapares([X | R],S) :- somapares(R,SS), ((0 is mod(X,2)) -> S is SS+X; S is SS).

% reverte uma lista
rev([X|R],B) :- rev(R,B,[X]).
rev([],A,A).
rev([X|R],B,A) :- rev(R,B,[X|A]).


pred(0,[]).
pred(1, [1]).
pred(N,[N | T]) :- (N1 is N-1), pred(N1, T).

map2(_,[],[]).
map2(P,[X|RX],[Y|RY]) :- G =.. [P,X,Y], call(G),
                         map2(P,RX,RY).

%  filter(+Teste, +Lin, -Lout)
filter(_,[],[]).
filter(T,[X|R],Lout) :- G =.. [T,X],
                        ( call(G) 
                        -> Lout = [X| RR]
                        ;  Lout = RR
                        ), filter(T,R,RR).

%banco de dados
pai(a,b).
pai(a,c).
pai(b,e).
pai(c,f).

ant(A,B) :- pai(A,B).
ant(A,B) :- pai(A,C),ant(C,B).

% findall(X,pai(a,X),L). --> significa: (o que eu quero setar em L, predicado, L que vai ser retornadoh)

% +++-
trocar2([],_,_,[]).
trocar2([X | R], V, N, L) :- trocar2(R, V, N, RR), (V = X -> L = [N | RR]; L = [X | RR]).

% gera(N, L) :- gera1(N, L , []).
% gera1(0,[],_).
% gera1(1,[ 1 | Acc],Acc). 
% gera1(N,RR, Acc) :- append([N],Acc,Acc1), N1 is N-1, gera1(N1,RR, Acc1).