:- use_module(library(clpfd)).
:- use_module(library(lists)).

day7_part1(Program, Input, Output) :-
    retractall(pos(_,_,_)),	
    parsePrograms(Program),
    findall(O,meta_run(Input,O),Outputs),
    max_list(Outputs,Output).

meta_run(Input,Output) :-
	set_phases([P1,P2,P3,P4,P5]),
	writeln([P1,P2,P3,P4,P5]),
	run(1,0,Input,P1,O1),
	run(2,0,O1,P2,O2),
	run(3,0,O2,P3,O3),
	run(4,0,O3,P4,O4),
	run(5,0,O4,P5,Output).

set_phases([P1,P2,P3,P4,P5]) :-
	P1 in 0..4,
	P2 in 0..4,
	P3 in 0..4,
	P4 in 0..4,
	P5 in 0..4,
	labeling([enum],[P1,P2,P3,P4,P5]),
	all_distinct([P1,P2,P3,P4,P5]).



%%%% PART 1
parsePrograms(Prog) :-
	parseProgram(1,0,Prog),
	parseProgram(2,0,Prog),
	parseProgram(3,0,Prog),
	parseProgram(4,0,Prog),
	parseProgram(5,0,Prog).

parseProgram(_Id,_Pos,[]) :- !.
parseProgram(Id,Pos,[Value|RProg]) :- assertz(pos(Id,Pos,Value)), !,
	NPos is Pos + 1, parseProgram(Id,NPos,RProg).

run(Id,Pos,_Input,_Phase,FValue) :- pos(Id,Pos,Code), getOp(Code,99), !, 
        pos(0,FValue), %writeln(FValue), 
	writeln(' End!') .
run(Id,Pos,Input,Phase,FValue) :- pos(Id,Pos,Code), getOp(Code,01), !, getModes(Code,[M1,M2,M3]), 
	Desp1 is Pos + 1, Desp2 is Pos + 2, readMem(Id,Desp1,M1,V1), readMem(Id,Desp2,M2,V2),
	NValue is V1 + V2,
	Desp3 is Pos + 3, writeMem(Id,Desp3,M3,NValue), 
	NPos is Pos + 4, run(Id,NPos,Input,Phase,FValue).
run(Id,Pos,Input,Phase,FValue) :- pos(Id,Pos,Code), getOp(Code,02), !, getModes(Code,[M1,M2,M3]),
	Desp1 is Pos + 1, Desp2 is Pos + 2,
	readMem(Id,Desp1,M1,V1), readMem(Id,Desp2,M2,V2), 
	NValue is V1 * V2,
	Desp3 is Pos + 3, writeMem(Id,Desp3,M3,NValue),
	NPos is Pos + 4, run(Id,NPos,Input,Phase,FValue).
run(Id,Pos,Input,-1,FValue) :- pos(Id,Pos,Code), getOp(Code,03), !, getModes(Code,[M1,_M2,_M3]),
%	read(Input),
	Desp1 is Pos + 1, writeMem(Id,Desp1,M1,Input),
	NPos is Pos + 2, run(Id,NPos,Input,-2,FValue).
run(Id,Pos,_Input,-2,FValue) :- pos(Id,Pos,Code), getOp(Code,03), !, getModes(Code,[M1,_M2,_M3]),
	read(Input),
	Desp1 is Pos + 1, writeMem(Id,Desp1,M1,Input),
	NPos is Pos + 2, run(Id,NPos,_,-2,FValue).
run(Id,Pos,Input,Phase,FValue) :- pos(Id,Pos,Code), getOp(Code,03), !, getModes(Code,[M1,_M2,_M3]),
%	read(Input),
	Desp1 is Pos + 1, writeMem(Id,Desp1,M1,Phase),
	NPos is Pos + 2, run(Id,NPos,Input,-1,FValue).
run(Id,Pos,Input,Phase,FValue) :- pos(Id,Pos,Code), getOp(Code,04), !, getModes(Code,[M1,_M2,_M3]),
	Desp1 is Pos + 1, readMem(Id,Desp1,M1,FValue).
%   	writeln(FValue).
%	NPos is Pos + 2, run(Id,NPos,Input,Phase,FValue).


%%% PART 2
run(Id,Pos,Input,Phase,FValue) :- pos(Id,Pos,Code), getOp(Code,05), !, getModes(Code,[M1,M2,_M3]),
	Desp1 is Pos + 1, readMem(Id,Desp1,M1,V1), 
	(V1 = 0 -> (NPos is Pos + 3, run(Id,NPos,Input,Phase,FValue)) ; 
		   (Desp2 is Pos + 2, readMem(Id,Desp2,M2,V2), run(Id,V2,Input,Phase,FValue))).
run(Id,Pos,Input,Phase,FValue) :- pos(Id,Pos,Code), getOp(Code,06), !, getModes(Code,[M1,M2,_M3]),
	Desp1 is Pos + 1, readMem(Id,Desp1,M1,V1), 
	(V1 = 0 -> (Desp2 is Pos + 2, readMem(Id,Desp2,M2,V2), run(Id,V2,Input,Phase,FValue));
		   (NPos is Pos + 3, run(Id,NPos,Input,Phase,FValue))).
run(Id,Pos,Input,Phase,FValue) :- pos(Id,Pos,Code), getOp(Code,07), !, getModes(Code,[M1,M2,M3]),
	Desp1 is Pos + 1, readMem(Id,Desp1,M1,V1), 
	Desp2 is Pos + 2, readMem(Id,Desp2,M2,V2), 
	Desp3 is Pos + 3,
	(V1 < V2 -> writeMem(Id,Desp3,M3,1);
		    writeMem(Id,Desp3,M3,0)),
	NPos is Pos + 4, run(Id,NPos,Input,Phase,FValue).
run(Id,Pos,Input,Phase,FValue) :- pos(Id,Pos,Code), getOp(Code,08), !, getModes(Code,[M1,M2,M3]),
	Desp1 is Pos + 1, readMem(Id,Desp1,M1,V1), 
	Desp2 is Pos + 2, readMem(Id,Desp2,M2,V2), 
	Desp3 is Pos + 3,
	(V1 == V2 -> writeMem(Id,Desp3,M3,1);
		     writeMem(Id,Desp3,M3,0)),
	NPos is Pos + 4, run(Id,NPos,Input,Phase,FValue).

run(_Pos,_Input,_Phase,_FValue) :- writeln('SOMETHING WENT WRONT'), !.

getOp(Code,Op) :- Op is mod(Code,100).
getModes(Op,[M1,M2,M3]) :- 
	M1_ is div(Op,100), M1 is mod(M1_,2),
	M2_ is div(Op,1000), M2 is mod(M2_,2),
	M3_ is div(Op,10000), M3 is mod(M3_,2).

readMem(Id,Pointer,0,Value) :- pos(Id,Pointer,Pos), pos(Id,Pos,Value).
readMem(Id,Pos,1,Value) :- pos(Id,Pos,Value).

writeMem(Id,Pointer,0,Value) :- pos(Id,Pointer,Pos), retractall(pos(Id,Pos,_)), assertz(pos(Id,Pos,Value)).
writeMem(Id,Pos,1,Value) :- retractall(pos(Id,Pos,_)), assertz(pos(Id,Pos,Value)).


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

%%%% PART 2 is EASIER THAN THIS... I made the mistake of not restarting amplifiers AFTER each
%%%% phase combination and this lead to me to create a complicated virtual machine... probably
%%%% the first simpler version was correct... No time to come back in time and have the simple version :(


%%% PART 2
day7_part2(Program, Input, Output) :-
    retractall(pos(_,_,_)),	
    findall(O,meta_run2(Program,Input,O),Outputs),
    max_list(Outputs,Output).
%    ).

%% Tag used to read phase, then signals

meta_run2(Program,Input,Output) :-
	set_phases2([P1,P2,P3,P4,P5]),
	writeln([P1,P2,P3,P4,P5]),
	retractall(pos(_,_,_)),
	parsePrograms(Program),
	runAnd(1,[0,0,0,0,0],FPos1,[[Input],[],[],[],[]],P1,Queue1,_,1), 
	runAnd(2,FPos1,FPos2,Queue1,P2,Queue2,_,1),
	runAnd(3,FPos2,FPos3,Queue2,P3,Queue3,_,1),
	runAnd(4,FPos3,FPos4,Queue3,P4,Queue4,_,1),
	runAnd(5,FPos4,FPos5,Queue4,P5,Queue5,_,1),
	run_loop([P1,P2,P3,P4,P5],FPos5,Queue5,_,Output).


run_loop(LPhases,LPos,QueueI,QueueF,Output) :-
	oneIte(LPhases,LPos,NPos,QueueI,QueueO,Out),
	(number(Out) -> (!, Output=Out) ; 
	run_loop(LPhases,NPos,QueueO,QueueF,Output)).

oneIte([P1,P2,P3,P4,P5],LPos,FPos,QueueI,QueueF,Output) :-
	runAnd(1,LPos,FPos1,QueueI,P1,Queue1,_,0),
	runAnd(2,FPos1,FPos2,Queue1,P2,Queue2,_,0),
	runAnd(3,FPos2,FPos3,Queue2,P3,Queue3,_,0),
	runAnd(4,FPos3,FPos4,Queue3,P4,Queue4,_,0),
	runAnd(5,FPos4,FPos,Queue4,P5,QueueF,Output,0). 

addId(5,1) :- !.
addId(Id,NId) :- NId is Id + 1.

set_phases2([P1,P2,P3,P4,P5]) :-
	P1 in 5..9,
	P2 in 5..9,
	P3 in 5..9,
	P4 in 5..9,
	P5 in 5..9,
	labeling([enum],[P1,P2,P3,P4,P5]),
	all_distinct([P1,P2,P3,P4,P5]).

updateOutputs(1,Value,[_O|RO],[Value|RO]) :- !.
updateOutputs(N,Value,[O|RO],[O|ROUpdated]) :- NN is N - 1, updateOutputs(NN,Value,RO,ROUpdated).

findOutput([[X]|_R],X) :- !.
findOutput([[]|R],X) :- findOutput(R,X).

runAnd(Id,LPos,UPos,QueueI,_Phase,QueueI,Output,_) :-
	nth1(Id,LPos,Pos),
	pos(Id,Pos,Code), getOp(Code,99), !,
	findOutput(QueueI,Output),
	NPos is Pos + 1, updatePos(Id,NPos,LPos,UPos).
%%% 1 sum
runAnd(Id,LPos,FPos,QueueI,Phase,QueueO,Output,Tag) :-
	nth1(Id,LPos,Pos),
	pos(Id,Pos,Code), getOp(Code,01), !, getModes(Code,[M1,M2,M3]),
	Desp1 is Pos + 1, Desp2 is Pos + 2, readMem(Id,Desp1,M1,V1), readMem(Id,Desp2,M2,V2),
	NValue is V1 + V2,
	Desp3 is Pos + 3, writeMem(Id,Desp3,M3,NValue), 
	NPos is Pos + 4, updatePos(Id,NPos,LPos,UPos),	
	runAnd(Id,UPos,FPos,QueueI,Phase,QueueO,Output,Tag).
%%% 2 multiply
runAnd(Id,LPos,FPos,QueueI,Phase,QueueO,Output,Tag) :-
	nth1(Id,LPos,Pos),
	pos(Id,Pos,Code), getOp(Code,02), !, getModes(Code,[M1,M2,M3]),
	Desp1 is Pos + 1, Desp2 is Pos + 2,
	readMem(Id,Desp1,M1,V1), readMem(Id,Desp2,M2,V2), 
	NValue is V1 * V2,
	Desp3 is Pos + 3, writeMem(Id,Desp3,M3,NValue),
	NPos is Pos + 4, updatePos(Id,NPos,LPos,UPos),	
	runAnd(Id,UPos,FPos,QueueI,Phase,QueueO,Output,Tag).
%%% INPUT

runAnd(Id,LPos,FPos,QueueI,Phase,QueueF,Output,0) :-
	nth1(Id,LPos,Pos), nth1(Id,QueueI,[]), 
	pos(Id,Pos,Code), getOp(Code,03), !, addId(Id,NId),
	runAnd(NId,LPos,FPos,QueueI,Phase,QueueF,Output,0).


runAnd(Id,LPos,FPos,QueueI,Phase,QueueF,Output,0) :-
	nth1(Id,LPos,Pos), nth1(Id,QueueI,[IValue|RInputs]), 
	pos(Id,Pos,Code), getOp(Code,03), !, updateOutputs(Id,RInputs,QueueI,QueueO), getModes(Code,[M1,_M2,_M3]),
%	write('03b '),writeln(QueueI),
	Desp1 is Pos + 1, writeMem(Id,Desp1,M1,IValue),
	NPos is Pos + 2, updatePos(Id,NPos,LPos,UPos),
       %writeln(updatePos(Id,NPos,LPos,UPos)),
	runAnd(Id,UPos,FPos,QueueO,Phase,QueueF,Output,0).
runAnd(Id,LPos,FPos,QueueI,Phase,QueueO,Output,1) :-
	nth1(Id,LPos,Pos),
	pos(Id,Pos,Code), getOp(Code,03), !, getModes(Code,[M1,_M2,_M3]),
%	write('03c '),writeln(QueueI),
%	read(Input),
	Desp1 is Pos + 1, writeMem(Id,Desp1,M1,Phase),
	NPos is Pos + 2, updatePos(Id,NPos,LPos,UPos),
	%writeln(updatePos(Id,NPos,LPos,UPos)),
	runAnd(Id,UPos,FPos,QueueI,Phase,QueueO,Output,0).

%%% OUTPUT
runAnd(Id,LPos,UPos,QueueI,_Phase,QueueO,_Output,_Tag) :-
	nth1(Id,LPos,Pos),
	pos(Id,Pos,Code), getOp(Code,04), !, getModes(Code,[M1,_M2,_M3]),
%	write('04 '),writeln(QueueI),
	Desp1 is Pos + 1, readMem(Id,Desp1,M1,FValue),
	addId(Id,NId), nth1(NId,QueueI,CInput), append(CInput,[FValue],NInput),
	updateOutputs(NId,NInput,QueueI,QueueO),
	%write('output '), writeln(FValue),
	NPos is Pos + 2, updatePos(Id,NPos,LPos,UPos).   

%%% JUMP-IF-TRUE
runAnd(Id,LPos,FPos,QueueI,Phase,QueueO,Output,Tag) :-
	nth1(Id,LPos,Pos),
	pos(Id,Pos,Code), 
	getOp(Code,05), !, getModes(Code,[M1,M2,_M3]),
%	write('05 '),writeln(QueueI),
	Desp1 is Pos + 1, readMem(Id,Desp1,M1,V1), 
	(V1 == 0 -> (NPos is Pos + 3, updatePos(Id,NPos,LPos,UPos), 	
		    runAnd(Id,UPos,FPos,QueueI,Phase,QueueO,Output,Tag)) ; 
		   (Desp2 is Pos + 2, readMem(Id,Desp2,M2,V2), updatePos(Id,V2,LPos,UPos), 
		    runAnd(Id,UPos,FPos,QueueI,Phase,QueueO,Output,Tag))).
%%%% JUMP-IF-FALSE
runAnd(Id,LPos,FPos,QueueI,Phase,QueueO,Output,Tag) :-
	nth1(Id,LPos,Pos),
	pos(Id,Pos,Code), getOp(Code,06), !, getModes(Code,[M1,M2,_M3]),
%	write('06 '),writeln(QueueI),
	Desp1 is Pos + 1, readMem(Id,Desp1,M1,V1), 
	(V1 == 0 -> (Desp2 is Pos + 2, readMem(Id,Desp2,M2,V2), updatePos(Id,V2,LPos,UPos), 	
		    runAnd(Id,UPos,FPos,QueueI,Phase,QueueO,Output,Tag));
		   (NPos is Pos + 3, updatePos(Id,NPos,LPos,UPos), 
		    runAnd(Id,LPos,FPos,QueueI,Phase,QueueO,Output,Tag))).
%%%% LESS THAN
runAnd(Id,LPos,FPos,QueueI,Phase,QueueO,Output,Tag) :-
	nth1(Id,LPos,Pos),
	pos(Id,Pos,Code), getOp(Code,07), !, getModes(Code,[M1,M2,M3]),
%	write('07 '),writeln(QueueI),
	Desp1 is Pos + 1, readMem(Id,Desp1,M1,V1), 
	Desp2 is Pos + 2, readMem(Id,Desp2,M2,V2), 
	Desp3 is Pos + 3,
	(V1 < V2 -> writeMem(Id,Desp3,M3,1);
		    writeMem(Id,Desp3,M3,0)),
	NPos is Pos + 4, updatePos(Id,NPos,LPos,UPos),	
	runAnd(Id,UPos,FPos,QueueI,Phase,QueueO,Output,Tag).
%%%% EQUALS
runAnd(Id,LPos,FPos,QueueI,Phase,QueueO,Output,Tag) :-
	nth1(Id,LPos,Pos),
	pos(Id,Pos,Code), getOp(Code,08), !, getModes(Code,[M1,M2,M3]),
%	write('08 '),writeln(QueueI),
	Desp1 is Pos + 1, readMem(Id,Desp1,M1,V1), 
	Desp2 is Pos + 2, readMem(Id,Desp2,M2,V2), 
	Desp3 is Pos + 3,
	(V1 == V2 -> writeMem(Id,Desp3,M3,1);
		     writeMem(Id,Desp3,M3,0)),
	NPos is Pos + 4, updatePos(Id,NPos,LPos,UPos), 	
	runAnd(Id,UPos,FPos,QueueI,Phase,QueueO,Output,Tag).

runAnd(Id,Pos,FPos,QueueI,Phase,QueueO,Output,Tag) :- writeln('SOMETHING WENT WRONG'), writeln(runAnd(Id,Pos,FPos,QueueI,Phase,QueueO,Output,Tag)),!.

updatePos(1,NPos,[_P1|RP],[NPos|RP]) :- !.
updatePos(Id,NPos,[P1|RP],[P1|RPUpdated]) :- IId is Id - 1, updatePos(IId,NPos,RP,RPUpdated).