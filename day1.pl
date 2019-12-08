day1_part1(File, Fuel) :-
    retractall(row(_)),	
    open(File, read, Stream),
    repeat,
    read_line_to_codes(Stream, Codes),
    (   Codes \= end_of_file
    ->  ( 
          parse(Codes,Num),
          assertz(row(Num)), fail )
    ; close(Stream), !, 
    findall(R,row(R),LNums), writeln(LNums), computeFuel(LNums,Fuel)
    ).

day1_part2(File, Fuel) :-
    retractall(row(_)),
    retractall(lfreq(_)),
    retractall(f(_)),
    open(File, read, Stream),
    repeat,
    read_line_to_codes(Stream, Codes),
    (   Codes \= end_of_file
    ->  ( 
          parse(Codes,Num),
          assertz(row(Num)), fail )
    ; close(Stream), !, 
    findall(R,row(R),LNums), 
    computeAddFuel(LNums,Fuel)
    ).


%% FIRST PART
computeFuel([],0).
computeFuel([One|LNums],Fuel) :- One =< 8, !, computeFuel(LNums,Fuel).
computeFuel([One|LNums],TotalFuel) :- Fuel is (div(One,3)-2),
	   computeFuel(LNums,Rec), TotalFuel is Fuel+Rec.

%% SECOND PART

computeSingleFuel(N,0) :- N =< 8, !.
computeSingleFuel(N,Fuel) :- Fuel is (div(N,3)-2).

computeAddSingle(N,0) :- N =< 8, !.
computeAddSingle(N,Fuel) :- computeSingleFuel(N,FuelOne), computeAddSingle(FuelOne,FuelRec), Fuel is FuelOne + FuelRec.
	
computeAddFuel([],0).
computeAddFuel([One|LNums],Fuel) :- 
	computeAddSingle(One,FuelOneRec),
	computeAddFuel(LNums,FuelRec),
	Fuel is FuelOneRec + FuelRec.


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
