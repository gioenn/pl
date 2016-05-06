% Family Tree
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

uncle(U, G) :- male(U), siblings(U, X), parent(X, G).
aunt(A, G) :- female(A), siblings(A, X), parent(X, G).

descendant(D, A) :- parent(A,D) ; parent(X, D), descendant(X, A).

ancestor(A, D) :- descendant(D, A).


% Reverse

rev([], []).
rev([H|T], Rev) :- rev(T, RT), append(RT, [H], Rev).

% ?- reverse([1,2,3], Z).
% reverse([1|[2,3]], Z) :- reverse([2,3], [3,2]), append([3,2], [1], Z).
% reverse([2|[3]], RT1) :- reverse([3], [3]), append([3], [2], [3,2]).
% reverse([3|[]], RT2) :- reverse([], []), append([], [3], [3]).
% reverse([], []).

% RT2 = [3], RT1 = [3,2], Z = [3,2,1]


rev2(L, Rev) :- rev2_acc(L, [], Rev).

rev2_acc([], Acc, Acc).
rev2_acc([H|T], Acc, Rev) :- rev2_acc(T, [H|Acc], Rev).

% ?- rev2_acc([1,2,3], [], Z).
% rev2_acc([1|[2,3]], [], Z) :- rev2_acc([2,3], [1], Z).
% rev2_acc([2|[3]], [1], Z) :- rev2_acc([3], [2,1], Z).
% rev2_acc([3|[]], [2,1], Z) :- rev2_acc([], [3,2,1], Z).
% rev2_acc([], [3,2,1], [3,2,1])
% Z=[3,2,1]


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


% use is instead of = to assign the value of a numerical expression to a var
len([], 0).
len([_|T], N) :- len(T, N1), N is N1 + 1.

% map

map(_, [], []).
map(F, [X|XS], [Y|YS]) :- call(F, X, Y), map(F, XS, YS).

double(N, R) :- R is N * N.

% filter version 1

filter(_, [], []).
filter(P, [X|XS], [X|YS]) :- call(P, X), filter(P, XS, YS).
filter(P, [X|XS], YS) :- \+ call(P, X), filter(P, XS, YS).

even(N) :- 0 is N mod 2.
odd(N) :- \+ even(N).

% better version of filter using the cut operator (!)
filter2(_, [], []).
filter2(P, [X|XS], [X|YS]) :- call(P, X), !, filter2(P, XS, YS).
filter2(P, [_|XS], YS) :- filter2(P, XS, YS).

% tozero returns all the numbers from a positive number to zero
tozero(0, 0) :- !.
tozero(Y, Z) :- Z = Y ; Y1 is Y-1, tozero(Y1, Z).

% find with cut
find([X|_], X) :- !.
find([_|Y], X) :- find(Y, X).

% find2 is equivalent to find, keep attention to the use of = instead of == (try to query find2 with an undefined second parameter e.g., find2([1,2,3], X))
find2([Y|YS], X) :- Y=X, ! ; find2(YS, X).

% fold left

foldl(_, [], Acc, Acc).
foldl(F, [X|XS], Acc, R) :- call(F, X, Acc, Y), foldl(F, XS, Y, R).

+(A, B, C) :- C is A + B.
