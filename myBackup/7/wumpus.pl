:- module(wumpus,[initialState/5, guess/3, updateState/4]).


adjacentPosition(Direction,X0,Y0,X1,Y1) :- 
		(Direction = east, X1 is X0+1, Y1 is Y0);
		(Direction = west, X1 is X0-1, Y1 is Y0);
		(Direction = north, X1 is X0 , Y1 is Y0-1);
		(Direction = south, X1 is X0, Y1 is Y0+1).


findRemainingMazeDimension(D,X,Y,S) :- 
  (D = east, mazeDimension(_,C), S is C-X);
  (D = west, mazeDimension(_,_), S is X-1);
  (D = north, mazeDimension(_,_), S is Y-1);
  (D = south, mazeDimension(R,_), S is R-Y).
  

reverseDirection(east,RevDir) :- RevDir = west.
reverseDirection(west,RevDir) :- RevDir = east.
reverseDirection(north,RevDir) :- RevDir = south.
reverseDirection(south,RevDir) :- RevDir = north.

  
generateNRandomMoves(_,0,[]).
generateNRandomMoves(InitialDirection,N,L) :- 
  N > 0, 
  AllPossibleDirections = [north,south,east,west],
  reverseDirection(InitialDirection,RevDir),
  delete(AllPossibleDirections,RevDir,NewAllowedDirections),
  random_member(Move,NewAllowedDirections),
  NewN is N-1,
  generateNRandomMoves(InitialDirection,NewN,NewL), 
  append([Move],NewL,L).


isSafe(X,Y) :- 
  maze(X,Y,'.'); 
  maze(X,Y,"smell"); 
  maze(X,Y,"stench");
  maze(X,Y,"damp").


isDiscoverable(X,Y, FromX, FromY) :- 
  maze(X,Y,"unknown"), 
  isSafe(FromX,FromY), 
  adjacentPosition(_,X,Y,FromX,FromY).
  

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
useFeedback(N, [GH | GT], [FH | FT], X, Y) :- 
  updateMazeInfo(N,GH,FH,X,Y,DX,DY), N1 is N+1, useFeedback(N1,GT,FT,DX,DY).


updateMazeInfo(Dist,Instruction,Feedback,X0,Y0,X1,Y1) :-

  (Instruction = shoot, Feedback = hit, X1 is X0, Y1 is Y0);

  (Instruction = shoot, Feedback = miss, X1 is X0, Y1 is Y0);

  (Instruction = Move, Feedback = stench,
   adjacentPosition(Move,X0,Y0,X1,Y1), 
   markAsStench(X1,Y1), 
   updateDistance(X1,Y1,Move,Dist));
   
  (Instruction = Move, Feedback = smell,
   adjacentPosition(Move,X0,Y0,X1,Y1), 
   markAsSmell(X1,Y1), 
   updateDistance(X1,Y1,Move,Dist));
  
  (Instruction = Move, Feedback = wumpus,
   X1 is X0, Y1 is Y0,
   adjacentPosition(Move,X0,Y0,X2,Y2), markAsWumpus(X2,Y2));
  
  (Instruction = Move, Feedback = damp,
   adjacentPosition(Move,X0,Y0,X1,Y1), 
   markAsDamp(X1,Y1), 
   updateDistance(X1,Y1,Move,Dist));
   
   
  (Instruction = Move, Feedback = pit,
   X1 is X0, Y1 is Y0,
   adjacentPosition(Move,X0,Y0,X2,Y2), markAsPit(X2,Y2));
  
  (Instruction = Move, Feedback = wall,
   X1 is X0, Y1 is Y0,
   adjacentPosition(Move,X0,Y0,X2,Y2), markAsWall(X2,Y2));
   
  (Instruction = Move, Feedback = empty,
   adjacentPosition(Move,X0,Y0,X1,Y1), 
   markAsEmpty(X1,Y1), 
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

  
getDiscoverShootInstructionList(XS,YS, D, L1) :- 
  findall(cell(CX,CY,DX,DY),isDiscoverable(CX,CY,DX,DY),L3),
  random_member(cell(X,Y,NX,NY),L3),
  findPath(1,XS,YS,NX,NY,L),
  adjacentPosition(D,NX,NY,X,Y),
  append(L,[D, shoot],L1).
	

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
  findRemainingMazeDimension(Direction,X,Y,RemainingDimension), 
  Depth is min(Energy,RemainingDimension),  
  generateNRandomMoves(Direction,Depth,MoveList), 
  length(MoveList,Len), RemainingEnergy is (Energy - Len).

   
updateDistance(X,Y,Direction,NewDistance) :- 
  cellDistance(X,Y,Direction,OldDistance), OldDistance > NewDistance, 
  retract(cellDistance(X,Y,Direction,OldDistance)), 
  assert(cellDistance(X,Y,Direction,NewDistance)).
updateDistance(_,_,_,_).


markAsWall(X,Y) :- retract(maze(X,Y,_)), assert(maze(X,Y,'#')).
markAsWall(_,_).


markAsWumpus(X,Y) :- retract(maze(X,Y,_)), assert(maze(X,Y,'W')).


markAsPit(X,Y) :- retract(maze(X,Y,_)), assert(maze(X,Y,'P')).


markAsSmell(X,Y) :- retract(maze(X,Y,_)), assert(maze(X,Y,"smell")).


markAsStench(X,Y) :- retract(maze(X,Y,_)), assert(maze(X,Y,"stench")).


markAsDamp(X,Y) :- retract(maze(X,Y,_)), assert(maze(X,Y,"damp")).


markAsEmpty(X,Y) :- retract(maze(X,Y,_)), assert(maze(X,Y,'.')).


wumpusPosition(X,Y) :- maze(X,Y,'W').

wumpusPosition(X,Y) :- maze(X,Y,"unknown"), checkSmell(X,Y), checkStench(X,Y).


checkList([],_,_).
checkList([L|LS],E1,E2) :- 
  member(L,[E1,E2]),checkList(LS,E1,E2).

  
checkSmell(X,Y) :- 
  findall(Distance,(maze(XS,YS,"smell"), 
  getManhattanDistance(X,Y,XS,YS,Distance)),L),
  checkList(L,2,3).

  
checkStench(X,Y) :- 
  findall(Distance,(maze(XS,YS,"stench"), 
  getManhattanDistance(X,Y,XS,YS,Distance)),L), 
  checkList(L,1,1).


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
  findPath(NextDistance,XTemp,YTemp,X,Y,L1), L = [Direction | L1].