:- use_module(library(clpfd)).
:- use_module(library(lists)).


%%% decided to extend assert-retract version since I expect long programs from
%%% future puzzles and it will be faster this way (I think long executions/programs are
%%% more probable than need for symbolic execution/inversibility

day5(Program, FValue) :-
    retractall(pos(_,_)),	
    parseProgram(0,Program),
    run(0,FValue).


%%%% PART 1
parseProgram(_Pos,[]) :- !.
parseProgram(Pos,[Value|RProg]) :- assertz(pos(Pos,Value)), !,
	NPos is Pos + 1, parseProgram(NPos,RProg).

run(Pos,FValue) :- pos(Pos,Code), getOp(Code,99), !, 
        pos(0,FValue), %writeln(FValue), 
	writeln(' End!') .
run(Pos,FValue) :- pos(Pos,Code), getOp(Code,01), !, getModes(Code,[M1,M2,M3]), 
	Desp1 is Pos + 1, Desp2 is Pos + 2, readMem(Desp1,M1,V1), readMem(Desp2,M2,V2),
	NValue is V1 + V2,
	Desp3 is Pos + 3, writeMem(Desp3,M3,NValue), 
	NPos is Pos + 4, run(NPos,FValue).
run(Pos,FValue) :- pos(Pos,Code), getOp(Code,02), !, getModes(Code,[M1,M2,M3]),
	Desp1 is Pos + 1, Desp2 is Pos + 2,
	readMem(Desp1,M1,V1), readMem(Desp2,M2,V2), 
	NValue is V1 * V2,
	Desp3 is Pos + 3, writeMem(Desp3,M3,NValue),
	NPos is Pos + 4, run(NPos,FValue).
run(Pos,FValue) :- pos(Pos,Code), getOp(Code,03), !, getModes(Code,[M1,_M2,_M3]),
	read(Input),
	Desp1 is Pos + 1, writeMem(Desp1,M1,Input),
	NPos is Pos + 2, run(NPos,FValue).
run(Pos,FValue) :- pos(Pos,Code), getOp(Code,04), !, getModes(Code,[M1,_M2,_M3]),
	Desp1 is Pos + 1, readMem(Desp1,M1,Output),
   	writeln(Output),
	NPos is Pos + 2, run(NPos,FValue).
%%% PART 2
run(Pos,FValue) :- pos(Pos,Code), getOp(Code,05), !, getModes(Code,[M1,M2,_M3]),
	Desp1 is Pos + 1, readMem(Desp1,M1,V1), 
	(V1 = 0 -> (NPos is Pos + 3, run(NPos,FValue)) ; 
		   (Desp2 is Pos + 2, readMem(Desp2,M2,V2), run(V2,FValue))).
run(Pos,FValue) :- pos(Pos,Code), getOp(Code,06), !, getModes(Code,[M1,M2,_M3]),
	Desp1 is Pos + 1, readMem(Desp1,M1,V1), 
	(V1 = 0 -> (Desp2 is Pos + 2, readMem(Desp2,M2,V2), run(V2,FValue));
		   (NPos is Pos + 3, run(NPos,FValue))).
run(Pos,FValue) :- pos(Pos,Code), getOp(Code,07), !, getModes(Code,[M1,M2,M3]),
	Desp1 is Pos + 1, readMem(Desp1,M1,V1), 
	Desp2 is Pos + 2, readMem(Desp2,M2,V2), 
	Desp3 is Pos + 3,
	(V1 < V2 -> writeMem(Desp3,M3,1);
		    writeMem(Desp3,M3,0)),
	NPos is Pos + 4, run(NPos,FValue).
run(Pos,FValue) :- pos(Pos,Code), getOp(Code,08), !, getModes(Code,[M1,M2,M3]),
	Desp1 is Pos + 1, readMem(Desp1,M1,V1), 
	Desp2 is Pos + 2, readMem(Desp2,M2,V2), 
	Desp3 is Pos + 3,
	(V1 == V2 -> writeMem(Desp3,M3,1);
		     writeMem(Desp3,M3,0)),
	NPos is Pos + 4, run(NPos,FValue).

run(_Pos,_FValue) :- writeln('SOMETHING WENT WRONT'), !.

getOp(Code,Op) :- Op is mod(Code,100).
getModes(Op,[M1,M2,M3]) :- 
	M1_ is div(Op,100), M1 is mod(M1_,2),
	M2_ is div(Op,1000), M2 is mod(M2_,2),
	M3_ is div(Op,10000), M3 is mod(M3_,2).

readMem(Pointer,0,Value) :- pos(Pointer,Pos), pos(Pos,Value).
readMem(Pos,1,Value) :- pos(Pos,Value).

writeMem(Pointer,0,Value) :- pos(Pointer,Pos), retractall(pos(Pos,_)), assertz(pos(Pos,Value)).
writeMem(Pos,1,Value) :- retractall(pos(Pos,_)), assertz(pos(Pos,Value)).


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
