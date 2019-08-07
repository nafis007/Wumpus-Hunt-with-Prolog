:- module(wumpus,[initialState/5, guess/3, updateState/4]).


adjacentPosition(Direction,X0,Y0,X1,Y1) :- 
		(Direction = east, X1 is X0+1, Y1 is Y0);
		(Direction = west, X1 is X0-1, Y1 is Y0);
		(Direction = north, X1 is X0 , Y1 is Y0-1);
		(Direction = south, X1 is X0, Y1 is Y0+1).


reverseDirection(A,B) :-
  (A = east, B = west);
  (A = west, B = east);
  (A = north, B = south);
  (A = south, B = north).

  
generateNRandomMoves(_,0,_,_,[]).

generateNRandomMoves(InitialDirection,N,X,Y,L) :- 
  N > 0, 
  AllPossibleDirections = [north,south,east,west],
  reverseDirection(InitialDirection,RevDir),
  delete(AllPossibleDirections,RevDir,NewAllowedDirections),
  random_member(Move,NewAllowedDirections),
  isNewMoveValid(Move,X,Y),
  adjacentPosition(Move,X,Y,X1,Y1),
  NewN is N-1,
  generateNRandomMoves(Move,NewN,X1,Y1,NewL), 
  append([Move],NewL,L).
    
generateNRandomMoves(_,_,_,_,[]).
  

getMazeDimenstion(Direction,Dimension) :- 
  ((Direction = north; Direction = south), mazeDimension(Dimension,_));
  ((Direction = east; Direction = west), mazeDimension(_,Dimension)).


isSafe(X,Y) :- 
  maze(X,Y,'.'); 
  maze(X,Y,"smell"); 
  maze(X,Y,"stench");
  maze(X,Y,"damp").


isNewMoveValid(D,X0,Y0) :- 
  adjacentPosition(D,X0,Y0,X,Y), 
  mazeDimension(NR,NC), 
  X > 0, Y > 0, X =< NC, Y =< NR.
  

isPositionValid(X,Y) :- 
  mazeDimension(NR,NC), 
  X > 0, Y > 0, 
  X =< NC, Y =< NR.

isDiscoverable(X,Y, FromX, FromY) :- 
  maze(X,Y,"unknown"), 
  isSafe(FromX,FromY), 
  adjacentPosition(_,FromX,FromY,X,Y).
  

isShootable(X0,Y0,X1,Y1,Direction) :- 
  (Direction = east, Y1 is Y0, X1 > X0, not(wallInBetween(X0,Y0,X1,Y1,east)));
  (Direction = west, Y1 is Y0, X1 < X0, not(wallInBetween(X0,Y0,X1,Y1,west)));
  (Direction = north, X1 is X0, Y1 < Y0, not(wallInBetween(X0,Y0,X1,Y1,north)));
  (Direction = south, X1 is X0, Y1 > Y0, not(wallInBetween(X0,Y0,X1,Y1,south))).

  
wallInBetween(X0,Y0,X1,Y1,Direction) :-
  (Direction = east, (maze(X_Wall,Y0,'#'), X_Wall > X0, X_Wall < X1));
  (Direction = west, (maze(X_Wall,Y0,'#'), X_Wall < X0, X_Wall > X1));
  (Direction = north, (maze(X0,Y_Wall,'#'), Y_Wall < Y0, Y_Wall > Y1));
  (Direction = south, (maze(X0,Y_Wall,'#'), Y_Wall > Y0, Y_Wall < Y1)).


initialState(NR, NC, XS, YS, state(XS,YS)) :- 
	assert(mazeDimension(NR,NC)),
	Infinity is 100000000,
	foreach(between(1,NC,X), 
	  foreach(between(1,NR,Y), 
	   (assert(maze(X,Y,"unknown")),
		assert(cellDistance(X,Y,north,Infinity)),
		assert(cellDistance(X,Y,south,Infinity)),
		assert(cellDistance(X,Y,west,Infinity)),
		assert(cellDistance(X,Y,east,Infinity))))),
	assert(maze(XS,YS,'.')),
	assert(cellDistance(XS,YS,north,0)).


guess(state(X,Y), state(X,Y), Guess) :- 
  getInstructionList(X,Y,Guess).


updateState(state(X,Y), Guess, Feedback, state(X,Y)) :- 
  useFeedback(1,Guess,Feedback,X,Y).
  
 
useFeedback(_,_, [], _, _).
useFeedback(Dist, [FirstGuess | RemainingGuesses], 
  [FirstFeedback | RemainingFeedbacks], X, Y) :- 
    updateMazeInfo(Dist,FirstGuess,FirstFeedback,X,Y,NextX,NextY), 
	NewDist is (Dist+1), 
	useFeedback(NewDist,RemainingGuesses,RemainingFeedbacks,NextX,NextY).


updateMazeInfo(Dist,Instruction,Feedback,X0,Y0,X1,Y1) :-

  (Instruction = shoot, Feedback = hit, X1 is X0, Y1 is Y0);

  (Instruction = shoot, Feedback = miss, X1 is X0, Y1 is Y0);

  (Instruction = Move, Feedback = stench,
   adjacentPosition(Move,X0,Y0,X1,Y1), 
   markAs(X1,Y1,"stench"), 
   updateDistance(X1,Y1,Move,Dist));
   
  (Instruction = Move, Feedback = smell,
   adjacentPosition(Move,X0,Y0,X1,Y1), 
   markAs(X1,Y1,"smell"), 
   updateDistance(X1,Y1,Move,Dist));
  
  (Instruction = Move, Feedback = wumpus,
   X1 is X0, Y1 is Y0,
   adjacentPosition(Move,X0,Y0,X2,Y2), markAs(X2,Y2,'W'));
  
  (Instruction = Move, Feedback = damp,
   adjacentPosition(Move,X0,Y0,X1,Y1), 
   markAs(X1,Y1,"damp"), 
   updateDistance(X1,Y1,Move,Dist));
   
   
  (Instruction = Move, Feedback = pit,
   X1 is X0, Y1 is Y0,
   adjacentPosition(Move,X0,Y0,X2,Y2), markAs(X2,Y2,'P'));
  
  (Instruction = Move, Feedback = wall,
   X1 is X0, Y1 is Y0,
   adjacentPosition(Move,X0,Y0,X2,Y2), markAs(X2,Y2,'#'));
   
  (Instruction = Move, Feedback = empty,
   adjacentPosition(Move,X0,Y0,X1,Y1), 
   markAs(X1,Y1,'.'), 
   updateDistance(X1,Y1,Move,Dist)). 


getInstructionList(XS,YS, Guess) :- 
  getDiscoverShootInstructionList(XS,YS,D,L), 
  length(L,N), N1 is 100-N-4, N1 > 0, 
  getRandomMovesList(N1,D,XS,YS,L2), 
  append(L,L2,Guess).  
getInstructionList(XS,YS, Guess) :- 
  getShootInstructionList(XS,YS,D,L), 
  length(L,N), N1 is 100-N-4, N1 > 0, 
  getRandomMovesList(N1,D,XS,YS,L2), 
  append(L,L2,Guess).   

  
getDiscoverShootInstructionList(XS,YS, D, L) :- 
  findall(position(ToX,ToY,FromX,FromY),
  isDiscoverable(ToX,ToY,FromX,FromY),DiscoverableList),
  random_member(position(X,Y,NX,NY),DiscoverableList),
  findPath(1,XS,YS,NX,NY,L1),
  adjacentPosition(D,NX,NY,X,Y),
  append(L1,[D, shoot],L).
	

getShootInstructionList(XS,YS, D, L1) :- 
  wumpusPosition(X,Y),
  maze(XP,YP,_),
  isSafe(XP,YP),
  isShootable(XP,YP,X,Y,D),
  findPath(1,XS,YS,XP,YP,L),
  append(L,[shoot],L1).

 
getRandomMovesList(0,_,_,_,[]).

getRandomMovesList(Energy,Direction,XS,YS,L) :- 
  
  (Energy > 0, Energy < 6, 
  getRandomDirection(Direction,RandomDirection), 
  getAssumedValidPath(RandomDirection,XS,YS,Energy,RemainingEnergy,L1),
  getRandomMovesList(RemainingEnergy,RandomDirection,XS,YS,NewL), 
  append(L1,NewL,L));
  
  (Energy >= 6, EnergyAfterShoot is (Energy-6), 
  getRandomDirection(Direction,RandomDirection), 
  getAssumedValidPath(RandomDirection,XS,YS,EnergyAfterShoot,
                     RemainingEnergy,L1),  
  getRandomMovesList(RemainingEnergy,RandomDirection,XS,YS,NewL),
  append([RandomDirection,shoot],L1,L2),
  append(L2,NewL,L)).


  
getRandomDirection(CurrentDirection, NewRandomDirection) :- 
  AllPossibleDirections = [north,south,east,west], 
  delete(AllPossibleDirections,CurrentDirection,NewDirectionList), 
  random_member(NewRandomDirection,NewDirectionList).
  

getAssumedValidPath(Direction,X,Y,Energy,RemainingEnergy,MoveList) :- 
  getMazeDimenstion(Direction,Dimension), 
  Depth is min(Energy,Dimension),
  generateNRandomMoves(Direction,Depth,X,Y,MoveList), 
  length(MoveList,Len), RemainingEnergy is (Energy - Len).

   
updateDistance(X,Y,Direction,NewDistance) :- 
  cellDistance(X,Y,Direction,OldDistance), OldDistance > NewDistance, 
  retract(cellDistance(X,Y,Direction,OldDistance)), 
  assert(cellDistance(X,Y,Direction,NewDistance)).
updateDistance(_,_,_,_).

markAs(X,Y,Mark) :- retract(maze(X,Y,_)), assert(maze(X,Y,Mark)).
markAs(_,_,'#').


wumpusPosition(X,Y) :- maze(X,Y,'W').

wumpusPosition(X,Y) :- maze(X,Y,"unknown"), checkSmell(X,Y), checkStench(X,Y).


checkList([],_,_).
checkList([L|LS],E1,E2) :- 
  member(L,[E1,E2]),checkList(LS,E1,E2).

  
checkSmell(X,Y) :- 
  findall(Distance,(maze(XS,YS,"smell"), 
  getManhattanDistance(X,Y,XS,YS,Distance)),L),
  length(L,Len),
  ((Len == 0, fail);
  checkList(L,2,3)).
  

  
checkStench(X,Y) :- 
  findall(Distance,(maze(XS,YS,"stench"), 
  getManhattanDistance(X,Y,XS,YS,Distance)),L),
  length(L,Len),
  ((Len == 0, fail);
  checkList(L,1,1)).

getManhattanDistance(X0,Y0,X1,Y1,Distance) :- 
  Distance is (abs(X1-X0) + abs(Y1-Y0)).


findPath(_,XS,YS,XS,YS,[]).
findPath(Distance,XS,YS,X,Y,L) :- 
  adjacentPosition(Direction,XS,YS,XTemp,YTemp), 
  isSafe(XTemp,YTemp), 
  cellDistance(XTemp,YTemp,Direction,OldDistance), 
  Distance =< OldDistance, 
  updateDistance(XTemp,YTemp,Direction,Distance), 
  NextDistance is Distance+1, 
  findPath(NextDistance,XTemp,YTemp,X,Y,L1), 
  L = [Direction | L1].