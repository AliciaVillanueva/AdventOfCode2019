:- use_module(library(clpfd)).
:- use_module(library(lists)).

%%% decided to extend assert-retract version since I expect long programs from
%%% future puzzles and it will be faster this way (I think long executions/programs are
%%% more probable than need for symbolic execution/inversibility

day11(Program, FValue) :-
	retractall(painted(_,_,_)),
	retractall(firstOut),
	retractall(direction),
	retractall(robot(_,_,_)),
	retractall(paintedCells(_)),
	retractall(input(_)),
    Program = [3,8,1005,8,352,1106,0,11,0,0,0,104,1,104,0,3,8,102,-1,8,10,101,1,10,10,4,10,108,1,8,10,4,10,102,1,8,28,1,1003,20,10,2,106,11,10,2,1107,1,10,1,1001,14,10,3,8,1002,8,-1,10,1001,10,1,10,4,10,1008,8,0,10,4,10,1002,8,1,67,2,1009,7,10,3,8,1002,8,-1,10,1001,10,1,10,4,10,108,0,8,10,4,10,101,0,8,92,1,105,9,10,1006,0,89,1,108,9,10,3,8,1002,8,-1,10,1001,10,1,10,4,10,1008,8,1,10,4,10,1002,8,1,126,1,1101,14,10,1,1005,3,10,1006,0,29,1006,0,91,3,8,102,-1,8,10,101,1,10,10,4,10,108,1,8,10,4,10,1002,8,1,161,1,1,6,10,1006,0,65,2,106,13,10,1006,0,36,3,8,1002,8,-1,10,1001,10,1,10,4,10,1008,8,1,10,4,10,102,1,8,198,1,105,15,10,1,1004,0,10,3,8,1002,8,-1,10,1001,10,1,10,4,10,1008,8,0,10,4,10,101,0,8,228,2,1006,8,10,2,1001,16,10,3,8,102,-1,8,10,1001,10,1,10,4,10,108,0,8,10,4,10,1001,8,0,257,1006,0,19,2,6,10,10,2,4,13,10,2,1002,4,10,3,8,102,-1,8,10,1001,10,1,10,4,10,1008,8,1,10,4,10,1002,8,1,295,3,8,1002,8,-1,10,101,1,10,10,4,10,108,0,8,10,4,10,101,0,8,316,2,101,6,10,1006,0,84,2,1004,13,10,1,1109,3,10,101,1,9,9,1007,9,1046,10,1005,10,15,99,109,674,104,0,104,1,21101,387365315340,0,1,21102,369,1,0,1105,1,473,21101,666685514536,0,1,21102,380,1,0,1106,0,473,3,10,104,0,104,1,3,10,104,0,104,0,3,10,104,0,104,1,3,10,104,0,104,1,3,10,104,0,104,0,3,10,104,0,104,1,21102,1,46266346536,1,21102,427,1,0,1105,1,473,21101,235152829659,0,1,21101,438,0,0,1105,1,473,3,10,104,0,104,0,3,10,104,0,104,0,21102,838337188620,1,1,21101,461,0,0,1105,1,473,21102,988753429268,1,1,21102,1,472,0,1106,0,473,99,109,2,22101,0,-1,1,21101,40,0,2,21101,504,0,3,21102,494,1,0,1106,0,537,109,-2,2105,1,0,0,1,0,0,1,109,2,3,10,204,-1,1001,499,500,515,4,0,1001,499,1,499,108,4,499,10,1006,10,531,1101,0,0,499,109,-2,2106,0,0,0,109,4,2101,0,-1,536,1207,-3,0,10,1006,10,554,21102,1,0,-3,21202,-3,1,1,21201,-2,0,2,21102,1,1,3,21101,573,0,0,1105,1,578,109,-4,2105,1,0,109,5,1207,-3,1,10,1006,10,601,2207,-4,-2,10,1006,10,601,21201,-4,0,-4,1105,1,669,22101,0,-4,1,21201,-3,-1,2,21202,-2,2,3,21101,620,0,0,1106,0,578,22102,1,1,-4,21101,0,1,-1,2207,-4,-2,10,1006,10,639,21101,0,0,-1,22202,-2,-1,-2,2107,0,-3,10,1006,10,661,22101,0,-1,1,21102,661,1,0,106,0,536,21202,-2,-1,-2,22201,-4,-2,-4,109,-5,2106,0,0],
    retractall(pos(_,_)),
    retractall(offset(_)),
    parseProgram(0,Program),
    assertz(offset(0)),
    assertz(robot(0,0,u)),
%    runRobot1(FValue), % part 1
    runRobot2(FValue). % part 2


runRobot1(FValue) :-
	assertz(paintedCells([])),
	assertz(robot(0,0)),
	assertz(painted(0,0,0)),
	assertz(firstOut),
	run(0,_FValue),
	paintedCells(PCells),
	length(PCells,FValue).

runRobot2(FValue) :-
	assertz(paintedCells([])),
	assertz(robot(0,0)),
	assertz(painted(0,0,1)),
	assertz(firstOut),
	run(0,_FValue),
	findall((X,Y,C),painted(X,Y,C),LPoints),
	pprint(LPoints).

pprint(LPoints) :-
	nonNeg(LPoints,LPointsPos),
	toMatrix(LPointsPos,Matrix),
	reverse(Matrix,MatrixR),			
	toScreen(MatrixR).

%% this is ugly... but fast
nonNeg([],[]).
nonNeg([(X,Y,C)|RPoints],[(X,YY,C)|RRPoints]) :- YY is Y + 5,
	nonNeg(RPoints,RRPoints).

toMatrix([(X,Y,C)],Matrix) :-
	nth0(Y,Matrix,Row), nth0(X,Row,C).
toMatrix([(X,Y,C)|RPoints],Matrix) :-
	nth0(Y,Matrix,Row), nth0(X,Row,C), toMatrix(RPoints,Matrix).

toScreen([]).
toScreen([[]|Rows]) :-
	writeln(' '),
	toScreen(Rows).
toScreen([[0|RX]|RRows]) :-
	write(' '), toScreen([RX|RRows]).
toScreen([[1|RX]|RRows]) :-
	write('X'), toScreen([RX|RRows]).
toScreen([[_|RX]|RRows]) :-
	write(' '), toScreen([RX|RRows]).

whichColor(X,Y,C) :-
	painted(X,Y,C), !.
whichColor(X,Y,C) :-
	\+ painted(X,Y,_), !,
	assertz(painted(X,Y,0)), C = 0.

paint(C) :-
	robot(X,Y,_D), 
	whichColor(X,Y,C),!,
	retract(firstOut), assertz(direction). 

paint(C) :- !, 
	robot(X,Y,_D), paintedCells(PCells),
	whichColor(X,Y,CColor),
	retractall(painted(X,Y,CColor)),
	assertz(painted(X,Y,C)),  
	sort([(X,Y)|PCells],NPCells),
	retractall(paintedCells(PCells)),
	assertz(paintedCells(NPCells)), retract(firstOut), assertz(direction).


move(0) :- robot(X,Y,D), 
	( (D == u, !, retractall(robot(X,Y,u)), XX is X - 1, assertz(robot(XX,Y,l)));
	  (D == l, !, retractall(robot(X,Y,l)), YY is Y - 1, assertz(robot(X,YY,d)));
	  (D == d, !, retractall(robot(X,Y,d)), XX is X + 1, assertz(robot(XX,Y,r)));
	  (D == r, !, retractall(robot(X,Y,r)), YY is Y + 1, assertz(robot(X,YY,u)))),
	retract(direction), assertz(firstOut). 

move(1) :- robot(X,Y,D),
	( (D == u, !, retractall(robot(X,Y,u)), XX is X + 1, assertz(robot(XX,Y,r)));
	  (D == r, !, retractall(robot(X,Y,r)), YY is Y - 1, assertz(robot(X,YY,d)));
	  (D == d, !, retractall(robot(X,Y,d)), XX is X - 1, assertz(robot(XX,Y,l)));
	  (D == l, !, retractall(robot(X,Y,l)), YY is Y + 1, assertz(robot(X,YY,u)))),
	retract(direction), assertz(firstOut). 

%%%% PART 1
parseProgram(_Pos,[]) :- !.
parseProgram(Pos,[Value|RProg]) :- assertz(pos(Pos,Value)), !,
	NPos is Pos + 1, parseProgram(NPos,RProg).

%% END
run(Pos,FValue) :- pos(Pos,Code), getOp(Code,99), !, 
        pos(0,FValue), 
	writeln(' End!') .
%% ADD
run(Pos,FValue) :- pos(Pos,Code), getOp(Code,01), !, 
	getModes(Code,[M1,M2,M3]), 
	Desp1 is Pos + 1, Desp2 is Pos + 2, 
	readMem(Desp1,M1,V1), 
	readMem(Desp2,M2,V2), 
	NValue is V1 + V2,
	Desp3 is Pos + 3, writeMem(Desp3,M3,NValue), 
	NPos is Pos + 4, run(NPos,FValue).
%% MULT
run(Pos,FValue) :- pos(Pos,Code), getOp(Code,02), !, 
	getModes(Code,[M1,M2,M3]),
	Desp1 is Pos + 1, Desp2 is Pos + 2,
	readMem(Desp1,M1,V1), readMem(Desp2,M2,V2), 
	NValue is V1 * V2,
	Desp3 is Pos + 3, writeMem(Desp3,M3,NValue),
	NPos is Pos + 4, run(NPos,FValue).
%% IN
run(Pos,FValue) :- pos(Pos,Code), getOp(Code,03), !, 
	getModes(Code,[M1,_M2,_M3]), robot(X,Y,_D), whichColor(X,Y,Input),
	!, 
	Desp1 is Pos + 1, writeMem(Desp1,M1,Input),
	NPos is Pos + 2, run(NPos,FValue).
%% OUT
run(Pos,FValue) :- pos(Pos,Code), getOp(Code,04), firstOut, !, 
	getModes(Code,[M1,_M2,_M3]),
	Desp1 is Pos + 1, readMem(Desp1,M1,Output),
   	paint(Output),
	NPos is Pos + 2, run(NPos,FValue).
run(Pos,FValue) :- pos(Pos,Code), getOp(Code,04), direction, !, 
	getModes(Code,[M1,_M2,_M3]),
	Desp1 is Pos + 1, readMem(Desp1,M1,Output), 
   	move(Output),
	NPos is Pos + 2, run(NPos,FValue).


%%% PART 2
%% !=0
run(Pos,FValue) :- pos(Pos,Code), getOp(Code,05), !, 
	getModes(Code,[M1,M2,_M3]),
	Desp1 is Pos + 1, readMem(Desp1,M1,V1), 
	(V1 = 0 -> (NPos is Pos + 3, run(NPos,FValue)) ; 
		   (Desp2 is Pos + 2, readMem(Desp2,M2,V2), run(V2,FValue))).
%% ==0
run(Pos,FValue) :- pos(Pos,Code), getOp(Code,06), !, 
	getModes(Code,[M1,M2,_M3]),
	Desp1 is Pos + 1, readMem(Desp1,M1,V1), 
	(V1 = 0 -> (Desp2 is Pos + 2, readMem(Desp2,M2,V2), run(V2,FValue));
		   (NPos is Pos + 3, run(NPos,FValue))).
%% <
run(Pos,FValue) :- pos(Pos,Code), getOp(Code,07), !, 
	getModes(Code,[M1,M2,M3]),
	Desp1 is Pos + 1, readMem(Desp1,M1,V1), 
	Desp2 is Pos + 2, readMem(Desp2,M2,V2), 
	Desp3 is Pos + 3,
	(V1 < V2 -> writeMem(Desp3,M3,1);
		    writeMem(Desp3,M3,0)),
	NPos is Pos + 4, run(NPos,FValue).
%% ==
run(Pos,FValue) :- pos(Pos,Code), getOp(Code,08), !, 
	getModes(Code,[M1,M2,M3]),
	Desp1 is Pos + 1, readMem(Desp1,M1,V1), 
	Desp2 is Pos + 2, readMem(Desp2,M2,V2), 
	Desp3 is Pos + 3,
	(V1 == V2 -> writeMem(Desp3,M3,1);
		     writeMem(Desp3,M3,0)),
	NPos is Pos + 4, run(NPos,FValue).
%%% relative base offset
run(Pos,FValue) :- pos(Pos,Code), getOp(Code,09), !, 
	getModes(Code,[M1,_M2,_M3]),
	Desp1 is Pos + 1, readMem(Desp1,M1,V1),
	offset(O), NO is O + V1, retract(offset(O)), assertz(offset(NO)),
	NPos is Pos + 2, run(NPos,FValue).

run(_Pos,_FValue) :- writeln('SOMETHING WENT WRONT'), !.

getOp(Code,Op) :- Op is mod(Code,100).
getModes(Op,[M1,M2,M3]) :- 
	M1_ is div(Op,100), M1 is mod(M1_,10), 
	M2_ is div(Op,1000), M2 is mod(M2_,10),
	M3_ is div(Op,10000), M3 is mod(M3_,10).

%% position mode
readMem(Pointer,0,Value) :- Pointer >=0, pos(Pointer,Pos), readMem(Pos,1,Value).
%% direct mode
readMem(Pos,1,Value) :- Pos >=0, pos(Pos,Value).
%% relative mode
readMem(Pointer,2,Value) :-  Pointer >=0, offset(O), pos(Pointer,Pos), RPos is Pos + O, RPos>=0, pos(RPos,Value).
readMem(Pointer,_M,0) :- Pointer >=0, \+ pos(Pointer,_), assertz(pos(Pointer,0)).

%% position mode
writeMem(Pointer,0,Value) :- Pointer>=0, pos(Pointer,Pos), writeMem(Pos,1,Value). 
%% direct mode
writeMem(Pos,1,Value) :- Pos >= 0, retractall(pos(Pos,_)), assertz(pos(Pos,Value)).
%% relative mode
writeMem(Pointer,2,Value) :- Pointer >=0, offset(O), pos(Pointer,Pos), RPos is Pos + O,  RPos>=0, retractall(pos(RPos,_)), assertz(pos(RPos,Value)).
writeMem(Pointer,_M,Value) :- Pointer >=0, \+ pos(Pointer,_), assertz(pos(Pointer,Value)).


% extract from first argument a number in third argument. fourth argument is the remaining list of codes

number([],Acc,N,[]) :- !, atom_codes(N,Acc).
number([X|XS],Acc,N,Rest) :- X >= 48, X =< 57, !, append(Acc,[X],Acc1), number(XS,Acc1,N,Rest).
number([X|XS],Acc,N,[X|XS]) :- atom_codes(N,Acc), !.


parse([],[]) :- !.
% removes blanks
parse([32|LCodes],Num) :- !, parse(LCodes,Num).
% removes tabs
parse([9|LCodes],Num) :- !, parse(LCodes,Num).
% remove +
parse([43|LCodes],Num) :- !, parse(LCodes,Num).
% deal with -
parse([45|LCodes],Num) :- !, parse(LCodes,N), Num is 0-N.
parse(LCodes,Num) :- !, number_codes(Num,LCodes).
%parse(LCodes,[N|LRest]) :- number(LCodes,[],Num,Rest), atom_number(Num,N), !, parse(Rest,LRest).
