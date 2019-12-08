:- use_module(library(lists)).

day8_part1(File, Size, CheckSum) :-
    open(File, read, Stream),
    repeat,
    read_line_to_codes(Stream, Codes),
          parseDigits(Codes,LNums),
          writeln(LNums),
    close(Stream), !, 
    computeCheckSum(LNums,Size,CheckSum).

computeCheckSum(LNums,Size,CheckSum) :-
	splitLayers(LNums,Size,Layers),
	processLayers(Layers,WithCounters),
	findCheckSum(WithCounters,150,0,CheckSum).

splitLayers([],_,[]) :- !.
splitLayers(LNums,Size,[OneLayer|RestLayers]) :-
	take(Size,LNums,OneLayer),
	append(OneLayer,RLNums,LNums),
	splitLayers(RLNums,Size,RestLayers).

take(0,_,[]) :- !.
take(N,[L|RList],[L|RTaked]) :- NN is N - 1, take(NN,RList,RTaked).

processLayers([],[]).
processLayers([Layer|RLayers],[(Num0,Num1,Num2)|RProcessed]) :-
	processOne(Layer,Num0,Num1,Num2),
	processLayers(RLayers,RProcessed).

processOne([],0,0,0).
processOne([0|RDigits],Num0,Num1,Num2) :- !, processOne(RDigits,N0,Num1,Num2), Num0 is N0 + 1.
processOne([1|RDigits],Num0,Num1,Num2) :- !, processOne(RDigits,Num0,N1,Num2), Num1 is N1 + 1.
processOne([2|RDigits],Num0,Num1,Num2) :- !, processOne(RDigits,Num0,Num1,N2), Num2 is N2 + 1.
processOne([_|RDigits],Num0,Num1,Num2) :- processOne(RDigits,Num0,Num1,Num2).
findCheckSum([],_N,C,C) :- !.
findCheckSum([(N0,N1,N2)|RLayers],Acc0,_AccC,CheckSum) :-
	N0 < Acc0, !, NCheckSum is N1*N2, findCheckSum(RLayers,N0,NCheckSum,CheckSum).
findCheckSum([(N0,_N1,_N2)|RLayers],Acc0,AccC,CheckSum) :-
	N0 >= Acc0, !, findCheckSum(RLayers,Acc0,AccC,CheckSum).

parseDigits([],[]).
parseDigits([Code|RCodes],[Digit|RDigits]) :- Digit is Code - 48, %writeln(Digit),
	parseDigits(RCodes,RDigits).

day8_part2(File, Size, Image) :-
    open(File, read, Stream),
    repeat,
    read_line_to_codes(Stream, Codes),
          parseDigits(Codes,LNums),
    close(Stream), !, 
    computeImage(LNums,Size,Image), writeImage(1,Image).

writeImage(_,[]).
writeImage(25,[X|R]) :- writeln(X), writeImage(1,R).
writeImage(N,[X|R]) :- write(X), NN is N + 1, writeImage(NN,R).

computeImage(LNums,Size,Image) :-
	splitLayers(LNums,Size,Layers),
	mytranspose(Layers,Pixels),
	writeln(Pixels),
	filterColor(Pixels,Image).

%% transpose from swiprolog does not work for rectangular matrixes, just squares

mytranspose(Matrix, Transp) :- mytranspose(Matrix,[],[],Transp).

mytranspose([],_Acc1,Empties,[]) :- empties(Empties), !.
mytranspose([],Acc1,AccRest,[Acc1|RestTransp]) :-  mytranspose(AccRest,[],[],RestTransp).
mytranspose([[]|Rest], Acc1, AccRest, [Acc1|RestTransp]) :- append(AccRest,[Rest],Next), mytranspose(Next,[],[],RestTransp).
mytranspose([[X|R]|Rest],Acc1,AccRest,RestTransp) :- append(AccRest,[R],Next), append(Acc1,[X],NAcc1), mytranspose(Rest,NAcc1,Next,RestTransp).

empties([[]]).
empties([[]|R]) :- empties(R).

filterColor([],[]).
filterColor([Pixel1|Rest],[Color1|RColors]) :-
	findColor(Pixel1,Color1),
	filterColor(Rest,RColors).

findColor([1|_],1) :- !.
findColor([0|_],0) :- !.
findColor([2|R],Color) :- findColor(R,Color).