% a recap from the lessons

% atoms: my_atom atom1 atomOne 'Vincent' ' ' , ; :-
% numbers: 1 -100
% strings: "string"
% variables: X Y Var VAR _var
% terms: male(john)

% clauses or sentences

%     head            body
% find([X|Y], Z) :- find(X,Y).

% closed word assumption
zero(0) :- true.
% zero(0).

% zero(1) > false

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
% find([_|[3]], 2) :- find([3], 2).
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
