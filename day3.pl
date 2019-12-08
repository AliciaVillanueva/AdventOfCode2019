
% run with swipl -G100g -T20g -L2g

day3_part1(File, Distance) :-
    retractall(p(_,_,_)),
    retractall(moves(_)),	
    open(File, read, Stream),
    repeat,
    read_line_to_codes(Stream, Codes),
    (   Codes \= end_of_file
    ->  ( 
          parseCable(Codes,LMoves),
          assertz(moves(LMoves)), fail )
    ; close(Stream), !, 
    findall(R,moves(R),LCables), movesToPoints(1,LCables), 
    findClosest(0,_,(0,0),(X,Y)), distance(X,Y,Distance)
    ).

distance(X,Y,Res) :- abs(X,Xabs), abs(Y,Yabs), Res is Xabs + Yabs.
    
movesToPoints(_,[]).
movesToPoints(N,[Cable|RestCables]) :-
    movesToPointsCable(N,(0,0),(_FX,_FY),Cable),
    NN is N + 1, movesToPoints(NN,RestCables).

movesToPointsCable(_N,(CX,CY),(CX,CY),[]).
movesToPointsCable(N,(CX,CY),(FX,FY),[Move|RMoves]) :-
    fillLine(N,(CX,CY),Move,(NX,NY)),
    % write(Move), write(' '), writeln(fillLine),
    movesToPointsCable(N,(NX,NY),(FX,FY),RMoves).

fillLine(_N,(CX,CY),(_,0),(CX,CY)).
fillLine(N,(CX,CY),(r,Dist),(FX,FY)) :- !,
    NX is CX + 1, assertz(p(N,NX,CY)),
    NDist is Dist - 1, fillLine(N,(NX,CY),(r,NDist),(FX,FY)).
fillLine(N,(CX,CY),(l,Dist),(FX,FY)) :- !,
    NX is CX - 1, assertz(p(N,NX,CY)),
    NDist is Dist - 1, fillLine(N,(NX,CY),(l,NDist),(FX,FY)).
fillLine(N,(CX,CY),(u,Dist),(FX,FY)) :- !,
    NY is CY + 1, assertz(p(N,CX,NY)),
    NDist is Dist - 1, fillLine(N,(CX,NY),(u,NDist),(FX,FY)).
fillLine(N,(CX,CY),(d,Dist),(FX,FY)) :- !,
    NY is CY - 1, assertz(p(N,CX,NY)),
    NDist is Dist - 1, fillLine(N,(CX,NY),(d,NDist),(FX,FY)).
       
findClosest(_,_,(X,Y),(X,Y)) :-
    p(C1,X,Y), p(C2,X,Y), C1 \== C2, !, writeln(found).
findClosest(Dim,FDim,(X,Y),(FX,FY)) :-
    addOne(Dim,NDim,(X,Y),Next), writeln(addOne(Dim,NDim,(X,Y),Next)),
    findClosest(NDim,FDim,Next,(FX,FY)).

addOne(0,1,(0,0),(1,1)).
addOne(Dim,Dim,(Dim,Dim),(NX,Dim)) :- NX is Dim - 1, !.
addOne(Dim,Dim,(DimNeg,Dim),(DimNeg,NY)) :- abs(DimNeg,Dim), Dim \== DimNeg, !, NY is Dim - 1.
addOne(Dim,Dim,(DimNeg,DimNeg),(NX,DimNeg)) :- abs(DimNeg,Dim), Dim \== DimNeg, !, NX is DimNeg+1.
addOne(Dim,Dim,(Dim,DimNeg),(Dim,NY)) :- abs(DimNeg,Dim), Dim \== DimNeg, !, NY is DimNeg+1.    
addOne(Dim,NDim,(Dim,Y),(NDim,NDim)) :- Y is Dim - 1, !, NDim is Dim + 1.
addOne(Dim,Dim,(X,Dim),(NX,Dim)) :- NX is X - 1, !.
addOne(Dim,Dim,(DimNeg,Y),(DimNeg,NY)) :- abs(DimNeg,Dim), Dim \== DimNeg, NY is Y - 1, !.
addOne(Dim,Dim,(X,DimNeg),(NX,DimNeg)) :- abs(DimNeg,Dim), Dim \== DimNeg, NX is X + 1, !.
addOne(Dim,Dim,(Dim,Y),(Dim,NY)) :- NY is Y + 1, !.



day3_part2(File, Steps) :-
    retractall(p(_,_,_)),
    retractall(moves(_)),	
    open(File, read, Stream),
    repeat,
    read_line_to_codes(Stream, Codes),
    (   Codes \= end_of_file
    ->  ( 
          parseCable(Codes,LMoves),
          assertz(moves(LMoves)), fail )
    ; close(Stream), !, 
    findall(R,moves(R),LCables), movesToPoints(1,LCables),
    findIntersections(0,_,(0,0),_,LIntersections), writeln(LIntersections),
    stepsIntersections(LIntersections,[FS|LSteps]), writeln(LSteps),
    minSteps(FS,LSteps,Steps)
    ).

maxPoint(Max) :-
    findall(X,p(_,X,_),LX), max_list(LX,Max).
    
minSteps(Acc,[],Acc).
minSteps(Acc,[X|XS],Min) :- X < Acc, !, minSteps(X,XS,Min).
minSteps(Acc,[_|XS],Min) :- minSteps(Acc,XS,Min).


findIntersections(Dim,FDim,(X,Y),(FX,FY),[(X,Y)|Rest]) :- Dim =< 3000,
    p(C1,X,Y), p(C2,X,Y), C1 \== C2, !, addOne(Dim,NDim,(X,Y),Next), 
    findIntersections(NDim,FDim,Next,(FX,FY),Rest).
findIntersections(Dim,FDim,(X,Y),(FX,FY),Rest) :- Dim =< 3000, !,
    addOne(Dim,NDim,(X,Y),Next), 
    findIntersections(NDim,FDim,Next,(FX,FY),Rest).
findIntersections(Dim,_,_,_,[]) :- Dim > 3000.

stepsIntersections([],[]).
stepsIntersections([Point|Rest],[NSteps|RSteps]) :-
    computeSteps(Point,(0,0),NSteps), stepsIntersections(Rest,RSteps).

computeSteps((X,Y),(CX,CY),NSteps) :-
    findall(Moves,moves(Moves),[C1,C2]), 
    computeStepsCable((X,Y),0,(CX,CY),C1,N1), % writeln(N1), 
    computeStepsCable((X,Y),0,(CX,CY),C2,N2), % writeln(N2),
    NSteps is N1 + N2.

distanceD(N1,N2,Dist) :-
    N1 > 0, N2 > 0, !, Dist is abs(N1-N2).
distanceD(N1,N2,Dist) :-
    N1 < 0, N2 < 0, !, Dist is abs(N1-N2).
distanceD(N1,N2,Dist) :-
    D1 is abs(N1), D2 is abs(N2), Dist is D1 + D2.
    
computeStepsCable((X,Y),Acc,(CX,Y),[(r,S)|_RMoves],Steps) :-
    FX is CX + S, X>=CX, X =< FX, !, % INTERSECT
    distanceD(X,CX,Dist), Steps is Acc + Dist.
computeStepsCable((X,Y),Acc,(CX,Y),[(r,S)|RMoves],Steps) :-
    writeln((r,S)),    
    !, % SAME LINE NOT INTERSECT
    NAcc is Acc + S, NX is CX + S,
    computeStepsCable((X,Y),NAcc,(NX,Y),RMoves,Steps).
computeStepsCable((X,Y),Acc,(CX,Y),[(l,S)|_RMoves],Steps) :-
    FX is CX - S, X=<CX, X >= FX, !, % INTERSECT
    distanceD(X,CX,Dist), Steps is Acc + Dist.
computeStepsCable((X,Y),Acc,(CX,Y),[(l,S)|RMoves],Steps) :-
    !, % SAME LINE NOT INTERSECT
    NAcc is Acc + S, NX is CX - S,
    computeStepsCable((X,Y),NAcc,(NX,Y),RMoves,Steps).
%%%% ROWS    
computeStepsCable((X,Y),Acc,(X,CY),[(u,S)|_RMoves],Steps) :-
    FY is CY + S, Y>= CY, Y =< FY, !, % INTERSECT
    distanceD(Y,CY,Dist), Steps is Acc + Dist.
computeStepsCable((X,Y),Acc,(X,CY),[(u,S)|RMoves],Steps) :-
    !, % SAME LINE NOT INTERSECT
    NAcc is Acc + S, NY is CY + S,
    computeStepsCable((X,Y),NAcc,(X,NY),RMoves,Steps).
computeStepsCable((X,Y),Acc,(X,CY),[(d,S)|_RMoves],Steps) :-
    FY is CY - S, Y=<CY, Y >= FY, !, % INTERSECT
    distanceD(Y,CY,Dist), Steps is Acc + Dist.
computeStepsCable((X,Y),Acc,(X,CY),[(d,S)|RMoves],Steps) :-
    !, % SAME LINE NOT INTERSECT
    NAcc is Acc + S, NY is CY - S,
    computeStepsCable((X,Y),NAcc,(X,NY),RMoves,Steps).
computeStepsCable((X,Y),Acc,(CX,CY),[(r,S)|RMoves],Steps) :-
    !, 
    NAcc is Acc + S, NX is CX + S,
    computeStepsCable((X,Y),NAcc,(NX,CY),RMoves,Steps).
computeStepsCable((X,Y),Acc,(CX,CY),[(l,S)|RMoves],Steps) :-
    !, 
    NAcc is Acc + S, NX is CX - S,
    computeStepsCable((X,Y),NAcc,(NX,CY),RMoves,Steps).
computeStepsCable((X,Y),Acc,(CX,CY),[(u,S)|RMoves],Steps) :-
    !, 
    NAcc is Acc + S, NY is CY + S,
    computeStepsCable((X,Y),NAcc,(CX,NY),RMoves,Steps).
computeStepsCable((X,Y),Acc,(CX,CY),[(d,S)|RMoves],Steps) :-
    !, 
    NAcc is Acc + S, NY is CY - S,
    computeStepsCable((X,Y),NAcc,(CX,NY),RMoves,Steps).
    
% extract from first argument a number in third argument. fourth argument is the remaining list of codes

number([],Acc,N,[]) :- !, atom_codes(N,Acc).
number([X|XS],Acc,N,Rest) :- X >= 48, X =< 57, !, append(Acc,[X],Acc1), number(XS,Acc1,N,Rest).
number([X|XS],Acc,N,[X|XS]) :- atom_codes(N,Acc), !.


parseCable([],[]) :- !.
% removes blanks
parseCable([32|LCodes],LMoves) :- !, parseCable(LCodes,LMoves).
% removes tabs
parseCable([9|LCodes],LMoves) :- !, parseCable(LCodes,LMoves).
% remove ,
parseCable([44|LCodes],LMoves) :- !, parseCable(LCodes,LMoves).
% deal with direction R
parseCable([82|LCodes],[(r,Dist)|RestMoves]) :- !, 
    number(LCodes,[],Num,RestCodes), atom_number(Num,Dist),
    parseCable(RestCodes,RestMoves).
% deal with direction L
parseCable([76|LCodes],[(l,Dist)|RestMoves]) :- !, 
    number(LCodes,[],Num,RestCodes), atom_number(Num,Dist),
    parseCable(RestCodes,RestMoves).
% deal with direction U
parseCable([85|LCodes],[(u,Dist)|RestMoves]) :- !, 
    number(LCodes,[],Num,RestCodes), atom_number(Num,Dist),
    parseCable(RestCodes,RestMoves).
% deal with direction D
parseCable([68|LCodes],[(d,Dist)|RestMoves]) :- !, 
    number(LCodes,[],Num,RestCodes), atom_number(Num,Dist),
    parseCable(RestCodes,RestMoves).
