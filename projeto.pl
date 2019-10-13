% RA: 201705 - Luana Felipe de Barros
% RA: ****** - Guilherme Tetsuya Inuy

% % Casos de Teste: 
% Funciona: [quad(a,8,2,2), quad(d,7,3,1)].
% Não funciona: 

% testar caso: uma figura dentro da outra

topo:- 
  read(Lista),  % le a entrada
  write_ln(Lista), % printa a entrada
  get_size(Lista, T), % seta tamanho da lista em T
  % write_ln(T), % printa o tamanho
  for1(Lista).
  % getInter(Lista, NewList), write_ln(NewList).
  
  get_size([], 0).
  get_size([_ | R], T) :- get_size(R, TT), T is TT + 1.

  getInter([_, B | R]) :- write_ln([B | R]).

  % percorre a lista fazendo a combinação de cada figura em pares
  for1([]).
  for1([A|R]) :- for2(A, R), for1(R).
  for2(_,[]).
  for2(A,[B|R]) :- write(A), write(B), nl, inter(A,B), for2(A, R). %chama função inter que testa a intersecção das figuras

  

  inter(A,B) :- inter_circ_circ(A,B).
  inter(A,B) :- inter_quad_quad(A,B).
  inter(A,B) :- inter_circ_quad(A,B).
  
  % por enquanto retorna true apenas para dar certo e o for prosseguir
  inter_circ_quad(_,_) :- true.

  inter_circ_circ(circ(_,X1,_,R1), circ(_,X2,_,R2)) :- ((X2 - R2) - (X1 + R1)) < 0
                                                          -> write_ln("Yes")
                                                          ; write_ln("No").

  inter_circ_circ(circ(_,_,Y1,R1), circ(_,_,Y2,R2)) :- ((Y2 - R2) - (Y1 + R1)) < 0 
                                                          -> write_ln("Yes")
                                                          ; write_ln("No").

  inter_quad_quad(A, B) :- set_coord(A, CooA), set_coord(B, CooB), is_inter_quad(CooB, CooA).

  set_coord(quad(N, X, Y, L), coord(N,A,B,C,D)) :- A = [AX,AY], AX is X - (L/2), AY is Y - (L/2),
                                                  B = [BX,AY], BX is X + (L/2),
                                                  C = [BX,CY], CY is Y + (L/2),
                                                  D = [AX,CY].
                                                  % write_ln(A),write_ln(B),write_ln(C),write_ln(D).

  is_inter_quad(coord(Q1,A1,B1,_,D1), coord(Q2,A2,B2,_,D2)) :-  get_elem_pos(A2,0,A2X), % verifica na horizontal
                                                                get_elem_pos(B2,0,B2X),
                                                                get_elem_pos(A1,0,A1X),
                                                                get_elem_pos(B1,0,B1X),
                                                                verifica_x(A2X,B2X,A1X,B1X, Cond1),
                                                                write_ln(Cond1),

                                                                % write_ln(A2X), write_ln(B2X), write_ln(A1X), write_ln(B1X),
                                                                Cond1 = True,
                                                                get_elem_pos(D2,1,D2Y),
                                                                get_elem_pos(A2,1,A2Y),
                                                                get_elem_pos(A1,1,A1Y),
                                                                get_elem_pos(D1,1,D1Y),
                                                                verifica_y(D2Y,A2Y,D1Y,A1Y, Cond2),
                                                              %  write_ln(D2Y), write_ln(A2Y), write_ln(D1Y), write_ln(A1Y),
                                                              %  write_ln(Cond1),
                                                               write_ln(Cond2),
                                                                ((Cond1 = true, Cond2 = true) 
                                                                -> write("Eh Interseccao"), write(" "), write(Q2),write(" "), write_ln(Q1)
                                                                ; write("Nao Eh Interseccao"), write(" "), write(Q2),write(" "), write_ln(Q1)).



  % retorna os elementos do par ordenado do plano cartesiano
  get_elem_pos([],_, _) :- fail, !.
  get_elem_pos(_, N, _) :- N > 1, !,fail.
  get_elem_pos([X, _], 0, X).
  get_elem_pos([_, Y], 1, Y).

  %  verifica se os pontos A2X ou B2X estão entre os pontos A1X e B1X.
  verifica_x(A2X,_,A1X,B1X, true) :- (A2X >= A1X, A2X =< B1X), write_ln("Entrou Aqui 1").
  verifica_x(_,B2X,A1X,B1X, true) :- (B2X >= A1X, B2X =< B1X),  write_ln("Entrou Aqui 2").
  verifica_x(_,_,_,_, fail).

  verifica_y(D2Y,_,D1Y,A1Y, true) :- (D2Y =< D1Y, D2Y >= A1Y). 
  verifica_y(_,A2Y,D1Y,A1Y, true) :- (A2Y =< D1Y, A2Y >= A1Y). 
  verifica_y(_,_,_,_, fail).
