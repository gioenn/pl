% a recap from the lessons

% atoms: my_atom atom1 atomOne 'Vincent' ' ' , ; :- 
% numbers: 1 -100
% strings: "string"
% variables: X Y Var VAR _var
% terms: male(john)

% clauses or sentences

%     head            body
% find([X|Y], Z) :- find(X,Y).

zero(0) :- true.

find([X|_], X).
find([_|Y], X) :- find(Y,X).

% ?- find([1,2], 5).
% find([_|[2]], 5) :- find([2], 5).
% find([_|[]], 5) :- find([], 5).
% false

% ?- find([1,2,3], 2).
% find([_|[2,3]], 2) :- find([2,3], 2).
% find([2|_], 2).
% true ;

% find([_|[3]], 2) :- find([3],2).
% find([_|[]], 2) :- find([], 2).
% false

concatenate([], Y, Y).
concatenate([X|XS], Y, [X|Z]) :- concatenate(XS, Y, Z).

% ?- concatenate([1,2,3], [5,5], N).
% concatenate([1|[2,3]], [5,5], [1|Z]) :- concatenate([2,3], [5,5], Z).
% concatenate([2|[3]], [5,5], [2|Z]) :- concatenate([3], [5,5], Z).
% concatenate([3|[]], [5,5], [3|Z]) :- concatenate([], [5,5], Z).
% concatenate([], [5,5], [5,5]).
% N = [1,2,3,5,5]

% ?- concatenate([1,2,3], N, [1,2,3,4]).
% concatenate([1|[2,3]], Y, [1|[2,3,4]]) :- concatenate([2,3], Y, [2,3,4]).
% concatenate([2|[3]], Y, [2|[3,4]]) :- concatenate([3], Y, [3,4]).
% concatenate([3|[]], Y, [3|[4]]) :- concatenate([], Y, [4]).
% concatenate([], [4], [4]).
% N = [4]



% Genealogy
%
%                              James I
%                                 |
%                                 |
%                +----------------+-----------------+
%                |                                  |
%             Charles I                          Elizabeth
%                |                                  |
%                |                                  |
%     +----------+------------+                     |
%     |          |            |                     |
% Catherine   Charles II   James II               Sophia
%                                                   |
%                                                   |
%                                                   |
%                                                George I


male(james1).
male(charles1).
male(charles2).
male(james2).
male(george1).
female(catherine).
female(elizabeth).
female(sophia).

parent(james1, charles1).
parent(james1, elizabeth).
parent(charles1, charles2).
parent(charles1, catherine).
parent(charles1, james2).
parent(elizabeth, sophia).
parent(sophia, george1).

father(F, C) :- male(F), parent(F, C).
mother(M, C) :- female(M), parent(M, C).
grandfather(G, C) :- father(G, X), parent(X, C).
grandmother(G, C) :- mother(G, X), parent(X, C).
son(S, P) :- male(S), parent(P, S).
daughter(D, P) :- female(D), parent(P, D).
siblings(A, B) :- parent(P, A), parent(P, B), A \= B.
uncle(U, N) :- male(U), siblings(U, X), parent(X, N).
aunt(A, N) :- female(A), siblings(A, X), parent(X, N).

descendant(D, A) :- parent(A,D) ; parent(X, D), descendant(X, A).

ancestor(A, D) :- descendant(D, A).


% Reverse

rev([], []).
rev([H|T], Rev) :- rev(T, RT), append(RT, [H], Rev).


rev2(L, Rev) :- rev2_acc(L, [], Rev).

rev2_acc([], Acc, Acc).
rev2_acc([H|T], Acc, Rev) :- rev2_acc(T, [H|Acc], Rev).


% Takeout

takeout(X, [X|R], R).
takeout(X, [F|R], [F|S]) :- takeout(X, R, S).

% Permutation

perm([], []).
perm([X|Y], Z) :- perm(Y, W), takeout(X, Z, W).

% ?- perm([1,2,3], N).
% perm([1|[2,3]], Z) :- perm([2,3], W), takeout(1, Z, W). % step1
% perm([2|[3]], Z) :- perm([3], W), takeout(2, Z, W). % step2
% perm([3|[]], Z) :- perm([], W), takeout(3, Z, W). % step3

% step3: W=[], takeout(3, Z, []), Z=[3]
% step2: W=[3], takeout(2, Z, [3]), Z=[2,3] ; [3,2]
% step1a: W=[2,3], takeout(1, Z, [2,3]), Z=[1,2,3] ; [2,1,3] ; [2,3,1]
% step1b: W=[3,2], takeout(1, Z, [3,2]), Z=[1,3,2] ; [3,1,2] ; [3,2,1]
% N = [1,2,3] ; [2,1,3] ; [2,3,1] ; [1,3,2] ; [3,1,2] ; [3,2,1]








