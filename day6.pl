day6_part1(File, N) :-
    retractall(orbits(_,_)),	
    open(File, read, Stream),
    repeat,
    read_line_to_codes(Stream, Codes),
    (   Codes \= end_of_file
    ->  ( 
          parseOrbit(Codes,[],Orbit),
          assertz(Orbit), fail )
    ; close(Stream), !, 
    findall((O,IO),orbits(O,IO),LDirectOrbits),
    objects(LDirectOrbits,[],LObjects),
    computeOrbits(LObjects,N)
    ).

objects([],LO,LO).
objects([(O,IO)|Rest],AccO,RObjects) :- member(O,AccO), member(IO,AccO), !,
	objects(Rest,AccO,RObjects).
objects([(O,IO)|Rest],AccO,RObjects) :- member(O,AccO), \+ member(IO,AccO), !,
	objects(Rest,[IO|AccO],RObjects).
objects([(O,IO)|Rest],AccO,RObjects) :- \+ member(O,AccO), member(IO,AccO), !,
	objects(Rest,[O|AccO],RObjects).
objects([(O,IO)|Rest],AccO,RObjects) :- \+ member(O,AccO), \+ member(IO,AccO), !,
	objects(Rest,[O,IO|AccO],RObjects).

computeOrbits([],0).
computeOrbits([O|RObjects],N) :-
	findall(IO,inOrbitOf(O,IO),LinOrbits), length(LinOrbits,N1),
	computeOrbits(RObjects,RN), N is RN + N1.
	
inOrbitOf(O,IO) :- orbits(O,IO).
inOrbitOf(O,IO) :- orbits(O,OO), inOrbitOf(OO,IO).

parseOrbiting([],Center,Orbiting,orbits(Orbiting_,Center_)) :- !, atom_codes(Orbiting_,Orbiting), atom_codes(Center_,Center).
parseOrbiting([C|LCodes],Center,AccO,Res) :-
	append(AccO,[C],NAccO), parseOrbiting(LCodes,Center,NAccO,Res).
parseOrbit([41|LCodes],AccC,Res) :- !, parseOrbiting(LCodes,AccC,[],Res).
parseOrbit([C|LCodes],AccC,Res) :-
	append(AccC,[C],NAccC), parseOrbit(LCodes,NAccC,Res).


%% since it's a tree, the strategy is to find the path to root and then find the closest intersection

day6_part2(File, Steps) :-
    retractall(orbits(_,_)),
    open(File, read, Stream),
    repeat,
    read_line_to_codes(Stream, Codes),
    (   Codes \= end_of_file
    ->  ( 
          parseOrbit(Codes,[],Orbit),
          assertz(Orbit), fail )
    ; close(Stream), !,
    findCommon2('YOU','SAN',Steps)
    ).
pathToCom('COM',[]).
pathToCom(Origin,[Dest|RPath]) :- orbits(Origin,Dest), pathToCom(Dest,RPath).

findCommon2(Origin1,Origin2,Steps) :-
	pathToCom(Origin1,Path1), pathToCom(Origin2,Path2),
	common(Path1,Path1,Path2,Path2,Steps). 

common([X1|R1],O1,P2,O2,Steps) :- \+ member(X1,P2), !, common(R1,O1,P2,O2,Steps).
common([X1|_R1],O1,P2,O2,Steps) :- member(X1,P2), !, 
	append(Pref1,[X1],Aux1), append(Aux1,_Suff1,O1), 
	append(Pref2,[X1],Aux2), append(Aux2,_Suff2,O2), 
	length(Pref1,N1),length(Pref2,N2), Steps is N1 + N2.
	
