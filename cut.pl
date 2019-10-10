nota(N,L) :- N>9,L=a.
nota(N,L) :- N>7,L=b.
nota(N,L) :- N>5,L=c.
nota(_,d).

% Abb : 1 - nó é MAIOR que todos os elementos de AE
%       2 - nó é MENOR que todos os elementos de AD

% arv teste: Tree = arv(7, arv(1, vazia, vazia), arv(6,arv(4,vazia,vazia), arv(7, vazia,vazia))).
% elem(7,arv(7, arv(1, vazia, vazia), arv(6,arv(4,vazia,vazia), arv(7, vazia,vazia)))). 
elem(IT,arv(IT,_,_)).
elem(IT,arv(X,AE,AD)) :- IT < X 
                         -> elem(IT,AE)
                         ;  elem(IT,AD).

% insere numa Abb
adicionaAbb(IT, vazia, arv(IT, vazia, vazia)).
adicionaAbb(IT, arv(X, AE, AD), New) :- IT =< X
                                         -> (adicionaAbb(IT, AE, AE2), New = arv(X, AE2, AD))
                                          ; (adicionaAbb(IT, AD, AD2)), New = arv(X, AE, AD2).

% remove um item de uma abb
removeBST(vazia,_,vazia).
removeBST(arv(N,AE,vazia), N, AE).
removeBST(arv(N,vazia,AD), N, AD).
removeBST(arv(N,AE,AD),N,A) :- menorBST(AD,Men,NAD),A = arv(Men,AE,NAD).
removeBST(arv(N,AE,AD), X, A) :- 
         (X < N -> 
          removeBST(AE,X,NAE), A=arv(N,NAE,AD)
         ; removeBST(AD,X,NAD), A=arv(N,AE,NAD)
         ).

% menor item de uma BST, removendo o menor da arvore
menorBST(arv(N,vazia,AD), N, AD).
menorBST(arv(N,AE,AD), Men, A) :- menorBST(AE,Men,NAE),A=arv(N, NAE, AD).


% menor(arv(N,vazia,AD), N, AD).
% menor(arv(N,AE,AD), Men, A) :- menor(AE,Men, NAE),A=arv(N, NAE, AD).

maiorNo(vazia, vazia).
maiorNo(arv(N, _ , vazia), N).
maiorNo(arv(N, _, AD), Maior) :- maiorNo(AD, MM), (MM > N -> Maior = MM; Maior = N).

menorNo(vazia, vazia).
menorNo(arv(N, vazia , vazia), N).
menorNo(arv(N, _, AD), Menor) :- menorNo(AD, M1), menorNo(AE, M2),(MM < N -> Menor = MM; Menor = N).


acesse(CH,[dic(CH,V)|_],V).
acesse(CH,[_|DIC],V) :- acesse(CH,DIC,V).

insereDic(CH, V, [], dic(CH, V)).
insereDic(CH, V, [dic(X,Y) | R], DIC2) :- X = CH
                                  -> DIC2 = [dic(X,V) | R]
                                  ; DIC2 = [dic(X,V) | R], ret(CH, R, dic(W,Z)).

ret(CH,[dic(CH,V)|_],dic(CH,V)).
ret(CH,[_|DIC],DIC2) :- ret(CH,DIC,DIC2).

% arv(CH, V, AE, AD)
soma1(vazia, CH, arv(CH, 1, vazia, vazia)).
soma1(arv(CH,V, AE, AD), CH, arv(CH, VV, AE, AD)) :- VV is V+1.
soma1(arv(CH, V, AE, AD),IT, arv(CH, V, AE, AD2)) :- IT > CH, soma1(AD, IT, AD2).
soma1(arv(CH, V, AE, AD), IT, arv(CH, V, AE2, AD)) :- soma1(AE, IT, AE2).