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
  

generateSameElementList(_,0,[]).
generateSameElementList(Element,Count,L) :- 
  Count > 0, NewCount is Count-1, 
  generateSameElementList(Element,NewCount,L1), 
  append([Element],L1,L).


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
		assert(dist(X,Y,north,Infinity)),
		assert(dist(X,Y,south,Infinity)),
		assert(dist(X,Y,west,Infinity)),
		assert(dist(X,Y,east,Infinity))))),
	assert(maze(XS,YS,'.')),
	assert(dist(XS,YS,north,0)).


guess(state(X,Y), state(X,Y), Guess) :- 
  getInstructionList(X,Y,Guess).


updateState(state(X,Y), Guess, Feedback, state(X,Y)) :- 
  useFeedback(1,Guess,Feedback,X,Y).


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

  
getRandomMovesList(0,_,_,_,[]).

getRandomMovesList(N,D,XS,YS,L) :- 
  N > 0, N < 6, getRandomDirection(D,X), 
  getValidPathLength(X,XS,YS,N,N1,L1),
  getRandomMovesList(N1,X,XS,YS,LL), 
  append(L1,LL,L).

getRandomMovesList(N,D,XS,YS,L) :- 
  N >= 6, N1 is N-6, 
  getRandomDirection(D,X), 
  getValidPathLength(X,XS,YS,N1,N2,L1),  
  getRandomMovesList(N2,X,XS,YS,LL), 
  L2 = [X, shoot | L1],
  append(L2,LL,L).

  
getRandomDirection(D, X) :- 
 L = [north,south,east,west], delete(L,D,L2), random_member(X,L2).
  

getValidPathLength(D,X,Y,N,NR,LR) :- 
  findRemainingMazeDimension(D,X,Y,S), 
  MX is min(N,S),
  random_between(0,MX,Depth),  
  generateSameElementList(D,Depth,LR), 
  length(LR,Len), NR is N-Len.


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
   
   
updateDistance(X,Y,D,N) :- 
  dist(X,Y,D,DN), DN > N, 
  retract(dist(X,Y,D,DN)), 
  assert(dist(X,Y,D,N)).
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
  member(L,[E1,E2]), checkList(LS,E1,E2).

  
checkSmell(X,Y) :- 
  findall(Dst, (maze(XS,YS,"smell"), 
  getManhattanDistance(X,Y,XS,YS,Dst)) ,L),
  checkList(L,2,3).

  
checkStench(X,Y) :- 
  findall(Dst, (maze(XS,YS,"stench"), 
  getManhattanDistance(X,Y,XS,YS,Dst)), L), 
  checkList(L,1,1).


getManhattanDistance(X0,Y0,X1,Y1,Distance) :- 
  Distance is (abs(X0-X1) + abs(Y0-Y1)).


findPath(_,XS,YS,XS,YS,[]).
findPath(N,XS,YS,X,Y,L) :- 
  N1 is N+1, 
  adjacentPosition(D,XS,YS,DX,DY), 
  isSafe(DX,DY), 
  dist(DX,DY,D,DN), 
  DN >= N, 
  updateDistance(DX,DY,D,N), 
  findPath(N1,DX,DY,X,Y,L1), L = [D | L1].