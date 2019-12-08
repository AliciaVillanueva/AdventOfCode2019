:- use_module(library(clpfd)).
:- dynamic p/1.

day4_part1(Min,Max,Solutions) :-
    setof(Password,(solve(Min,Max,Password)),Passwords), 
    length(Passwords,Solutions).
    
solve(Min,Max,Password) :-
    length(Password,6), 
    Password ins 0..9,
    labeling([enum],Password),
    toInt(Password,Number),
    Number >= Min, Number =< Max,
    twoAreSame(Password),
    sort(0,@=<,Password,Password).

toInt([X1,X2,X3,X4,X5,X6],Number) :- 
    Number #= X6 + 10*X5+ 100*X4 + 1000*X3 + 10000*X2 + 100000*X1.
    
twoAreSame([X,X,_,_,_,_]).
twoAreSame([_,X,X,_,_,_]).
twoAreSame([_,_,X,X,_,_]).
twoAreSame([_,_,_,X,X,_]).
twoAreSame([_,_,_,_,X,X]).


day4_part2(Min,Max,Solutions) :-
    setof(Password,(solve_just_two(Min,Max,Password)),Passwords), 
    length(Passwords,Solutions).
    
solve_just_two(Min,Max,Password) :-
    length(Password,6), 
    Password ins 0..9,
    labeling([enum],Password),
    toInt(Password,Number),
    Number >= Min, Number =< Max,
    onlyTwoAreSame(Password),
    sort(0,@=<,Password,Password).

onlyTwoAreSame([X,X,Y,_,_,_]) :- X\==Y.
onlyTwoAreSame([Y,X,X,Z,_,_]) :- X\==Y, X\==Z.
onlyTwoAreSame([_,Y,X,X,Z,_]) :- X\==Y, X\==Z.
onlyTwoAreSame([_,_,Y,X,X,Z]) :- X\==Y, X\==Z.
onlyTwoAreSame([_,_,_,Y,X,X]) :- X\==Y.

