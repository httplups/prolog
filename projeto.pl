% RA: 201705 - Luana Felipe de Barros
% RA: ****** - Guilherme Tetsuya Inuy

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

% testloop2(0).
% testloop2(N) :- N>0, write("YYY : "), write(N), nl, M is N-1, testloop2(M).

% testloop(0).
% testloop(N) :- N>0, write("XXX : "), write_ln(N), M is N-1, testloop2(M), testloop(M).
% inter_circ_circ(A,B)
  for2(_,[]).
  for2(A,[B|R]) :- write(A), write(B), nl, inter(A,B), for2(A, R).

  for1([]).
  for1([A|R]) :- for2(A, R), for1(R).

  inter(A, B) :- inter_circ_circ(A,B).
  inter(A,B) :- inter_circ_quad(A,B).
  inter(A,B) :- inter_quad_quad(A,B).

  inter_circ_quad(A,B) :- true.
  inter_quad_quad(A,B) :- true.
% inter_circ_circ(circ(_,X1,_,R1), circ(_,X2,_,R2)) :- true, !.
  inter_circ_circ(circ(_,X1,_,R1), circ(_,X2,_,R2)) :- ((X2 - R2) - (X1 + R1)) < 0
                                                          -> write_ln("Yes")
                                                          ; write_ln("No").

  inter_circ_circ(circ(_,_,Y1,R1), circ(_,_,Y2,R2)) :- ((Y2 - R2) - (Y1 + R1)) < 0 
                                                          -> write_ln("Yes")
                                                          ; write_ln("No").