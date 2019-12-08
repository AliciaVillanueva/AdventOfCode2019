:- use_module(library(clpfd)).
:- use_module(library(lists)).

%% with assert-retract

day2_part1(Program, FValue) :-
    retractall(pos(_,_)),	
    parseProgram(0,Program),
    run(0,FValue).

%% with declarative arithmetic (clpfd) and lists instead of assert-retract for inversibility
day2_part2(Program, FValue) :- 
    retractall(pos(_,_)),	
    runWithList(0,Program,_FinalProgram,FValue). 

%%%% PART 1
parseProgram(_Pos,[]) :- !.
parseProgram(Pos,[Value|RProg]) :- assertz(pos(Pos,Value)), !,
	NPos is Pos + 1, parseProgram(NPos,RProg).

run(Pos,FValue) :- pos(Pos,99), !,  pos(0,FValue), writel(FValue), writeln(' End!') .
run(Pos,FValue) :- pos(Pos,1), !,
	Desp1 is Pos + 1, Desp2 is Pos + 2,
	pos(Desp1,P1), pos(Desp2,P2), pos(P1,V1), pos(P2,V2),
	NValue is V1 + V2,
	Desp3 is Pos + 3, pos(Desp3,P3), retract(pos(P3,_)), assertz(pos(P3,NValue)),
	NPos is Pos + 4, run(NPos,FValue).

run(Pos,FValue) :- pos(Pos,2), !,
	Desp1 is Pos + 1, Desp2 is Pos + 2,
	pos(Desp1,P1), pos(Desp2,P2), pos(P1,V1), pos(P2,V2),
	NValue is V1 * V2,
	Desp3 is Pos + 3, pos(Desp3,P3), retract(pos(P3,_)), assertz(pos(P3,NValue)),
	NPos is Pos + 4, run(NPos,FValue).
run(_Pos,_FValue) :- writeln('SOMETHING WENT WRONT'), !.

%%%% PART 2
	
runWithList(Pos,Program,Program,FValue) :- nth0(Pos,Program,99), !,
	 nth0(0,Program,FValue), write(FValue), writeln(' End!') .
runWithList(Pos,Program,FinalProgram,FValue) :- nth0(Pos,Program,1), !,
	Desp1 #= Pos + 1, Desp2 #= Pos + 2,
	nth0(Desp1,Program,P1), nth0(Desp2,Program,P2),
	nth0(P1,Program,V1), nth0(P2,Program,V2),
	NValue #= V1 + V2,
	Desp3 #= Pos + 3, nth0(Desp3,Program,P3),
	updateProgram(Program,P3,NValue,NextProgram),
	NPos #= Pos + 4,
	runWithList(NPos,NextProgram,FinalProgram,FValue).

runWithList(Pos,Program,FinalProgram,FValue) :- nth0(Pos,Program,2), !,
	Desp1 #= Pos + 1, Desp2 #= Pos + 2,
	nth0(Desp1,Program,P1), nth0(Desp2,Program,P2),
	nth0(P1,Program,V1), nth0(P2,Program,V2),
	NValue #= V1 * V2,
	Desp3 #= Pos + 3, nth0(Desp3,Program,P3),
	updateProgram(Program,P3,NValue,NextProgram),
	NPos #= Pos + 4,
	runWithList(NPos,NextProgram,FinalProgram,FValue).

updateProgram(Program,Pos,Value,UpdatedProgram) :- !,
	takeList(Pos,Program,Init),
	ToRemove is Pos + 1,
	dropList(ToRemove,Program,Rest),
	append(Init,[Value|Rest],UpdatedProgram).

takeList(_,[],[]) :- !.
takeList(0,_R,[]) :- !.
takeList(N,[E|ES],[E|Rest]) :- NN #= N-1, takeList(NN,ES,Rest).

dropList(_,[],[]) :- !.
dropList(0,L,L) :- !.
dropList(N,[_E|ES],Rest) :- NN #= N-1, dropList(NN,ES,Rest).

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
