findlist([X|_],X) :- !.
findlist([Y|L],X) :- findlist(Y,X) ; findlist(L,X). 


pack([], []).
pack([X|XS], [Z|ZS]) :- pack_helper(X, XS, YS, Z), pack(YS, ZS).

% third arg is the rest of the list, fourth is the list of the adiacent occurences of X
pack_helper(X, [], [], [X]).
pack_helper(X, [Y|YS], [Y|YS], [X]) :- X \= Y.
pack_helper(X, [X|XS], YS, [X|ZS]) :- pack_helper(X, XS, YS, ZS).


encode(X, Y) :- pack(X, Z), encode_helper(Z, Y).

encode_helper([], []).
encode_helper([[X|XS]|YS], [[X,Y]|ZS]) :- length([X|XS], Y), encode_helper(YS, ZS). 


decode([], []).
decode([[X,C]|Y], [X|Z]) :-  C > 0, !, NewC is C - 1, decode([[X,NewC]|Y], Z).
decode([_|Y], Z) :- decode(Y, Z).


tree_singleton(N,R) :- R = tree(N, empty, empty).


tree_insert(N, empty, tree(N,empty,empty)).
tree_insert(N, tree(N, L, R), tree(N, L, R)).
tree_insert(N, tree(Cur, L, R), tree(Cur, L, R1)) :- N > Cur, tree_insert(N, R, R1).
tree_insert(N, tree(Cur, L, R), tree(Cur, L1, R)) :- N < Cur, tree_insert(N, L, L1).


tree_elem(E, tree(E, _, _)).
tree_elem(E, tree(N, L, _)) :- N > E, tree_elem(E, L).  
tree_elem(E, tree(N, _, R)) :- N < E, tree_elem(E, R).  


tree_sum(empty, 0).
tree_sum(tree(N, L, R), Sum) :- tree_sum(L, SumLeft), tree_sum(R, SumRight), Sum is SumLeft + SumRight + N.


tree_values(empty, []).
tree_values(tree(N, L, R), [N|Res]) :- tree_values(L, N1), tree_values(R, N2), append(N1, N2, Res).