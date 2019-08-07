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


% generateSameElementList(+V,+N,-L)
% Generate a list with the element V inserted N times.
generateSameElementList(_,0,[]).
generateSameElementList(Element,Count,L) :- 
  Count > 0, NewCount is Count-1, 
  generateSameElementList(Element,NewCount,L1), 
  append([Element],L1,L).

% isSafe(+X,+Y)
% Check if the point(X,Y) can be transversed without dying
isSafe(X,Y) :- 
  maze(X,Y,'.'); 
  maze(X,Y,"smell"); 
  maze(X,Y,"stench");
  maze(X,Y,"damp").


% isDiscoverable(+X,+Y,-NX,-NY)
% Check if the point (X,Y) needs to be discovered and return the walkable point (NX,NY) from where
% it can be reached.
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

%initialState(+NR, +NC, +XS, +YS, -State0)
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

%guess(+State0, -State, -Guess)
guess(state(X,Y), state(X,Y), Guess) :- generate_plan(X,Y,Guess).

% updateState(+State0, +Guess, +Feedback, -State)
updateState(state(X,Y), Guess, Feedback, state(X,Y)) :- processFeedback(1,Guess,Feedback,X,Y).

% generate_discover_shoot_plan (+XS,+YS,-endingDir,-Instructions)
% Create a plan to move from point (XS,YS) to a hidden cell (chosen randomly) that is next to a cleared one and then shoot after moving
% to the hidden cell
generate_discover_shoot_plan(XS,YS, D, L1) :- findall(cell(CX,CY,DX,DY),isDiscoverable(CX,CY,DX,DY),L3)
 	,random_member(cell(X,Y,NX,NY),L3)
	,findPath(1,XS,YS,NX,NY,L)
	,adjacentPosition(D,NX,NY,X,Y)
	,append(L,[D, shoot],L1).

	
% generate_shoot_plan(+XS,+YS,-endingDir,-Instructions)
% Create a plan to shoot at a possible wumpus position (chosen randomly) from a safe cell
generate_shoot_plan(XS,YS, D, L1) :- wumpusPosition(X,Y)
	,maze(XP,YP,_)
	,isSafe(XP,YP)
    ,isShootable(XP,YP,X,Y,D)
	,findPath(1,XS,YS,XP,YP,L)
	,append(L,[shoot],L1).

% generate_plan (+XS,+YS, -Guess)
% Create a list of instructions for the robot to move, the posibles are shoot&discover, discover&shoot, shoot and random combinations of move&shoot&foward
generate_plan(XS,YS, Guess) :- generate_discover_shoot_plan(XS,YS,D,L), length(L,N), N1 is 100-N-4, N1 > 0, getRandomMovesList(N1,XS,YS,D,L2), append(L,L2,Guess).
generate_plan(XS,YS, Guess) :- generate_shoot_plan(XS,YS,D,L), length(L,N), N1 is 100-N-4, N1 > 0, getRandomMovesList(N1,XS,YS,D,L2), append(L,L2,Guess).
% generate_plan(_,_, Guess) :- getRandomMovesList(100,_,_,_,Guess).

getRandomMovesList(0,_,_,_,[]) :- !.
getRandomMovesList(N,XS,YS,D,L) :- N > 0, N < 6, getRandomDirection(X), getValidPathLength(X,XS,YS,N,N1,L1),getRandomMovesList(N1,XS,YS,X,LL), append(L1,LL,L).
getRandomMovesList(N,XS,YS,D,L) :- N >= 6, N1 is N-6, getRandomDirection(X), getValidPathLength(X,XS,YS,N1,N2,L1),  getRandomMovesList(N2,XS,YS,X,LL), L2 = [X, shoot | L1],append(L2,LL,L).

% getRandomDirection(D, X) :- 
% L = [north,south,east,west], delete(L,D,L2), random_member(X,L2).
  
getRandomDirection(X) :- 
  L = [north,south,east,west], random_member(X,L).

getValidPathLength(D,X,Y,N,NR,LR) :- 
  findRemainingMazeDimension(D,X,Y,S), 
  Depth is min(N,S), 
  generateSameElementList(D,Depth,LR), 
  length(LR,Len), NR is N-Len.

% processFeedback (+N,+Guess,+Feedback, +X,+Y)
% Process the guess and feedback list starting from position (X,Y).
processFeedback(_,_, [], _, _).
processFeedback(N, [GH | GT], [FH | FT], X, Y) :- updateMazeInfo(N,GH,FH,X,Y,DX,DY), N1 is N+1, processFeedback(N1,GT,FT,DX,DY).

% updateMazeInfo(+N,+Instruction,+Feedback,+X0,+Y0,-X1,-Y1)
% Do the respective action depending on the feedback. 
% (X0,Y0) is the start position before the action
% (X1,Y1) is the final position after the action


updateMazeInfo(Dist,Instruction,Feedback,X0,Y0,X1,Y1) :-

  (Instruction = shoot, Feedback = hit, X1 is X0, Y1 is Y0);
  
  (Instruction = shoot, Feedback = miss, X1 is X0, Y1 is Y0);
  
  (Instruction = Move, Feedback = smell,
   adjacentPosition(Move,X0,Y0,X1,Y1), 
   markAsSmell(X1,Y1), 
   updateDistance(X1,Y1,Move,Dist));
   
  (Instruction = Move, Feedback = stench,
   adjacentPosition(Move,X0,Y0,X1,Y1), 
   markAsSmell(X1,Y1), 
   updateDistance(X1,Y1,Move,Dist));
   
  (Instruction = Move, Feedback = wumpus,
   adjacentPosition(Move,X0,Y0,X2,Y2), markAsWumpus(X2,Y2),
   X1 is X0, Y1 is Y0);

  (Instruction = Move, Feedback = damp,
   adjacentPosition(Move,X0,Y0,X1,Y1), 
   markAsDamp(X1,Y1), 
   updateDistance(X1,Y1,Move,Dist));

  (Instruction = Move, Feedback = pit,
   adjacentPosition(Move,X0,Y0,X2,Y2), markAsPit(X2,Y2),
   X1 is X0, Y1 is Y0);

  (Instruction = Move, Feedback = wall,
   adjacentPosition(Move,X0,Y0,X2,Y2), markAsWall(X2,Y2),
   X1 is X0, Y1 is Y0);
   
  (Instruction = Move, Feedback = empty,
   adjacentPosition(Move,X0,Y0,X1,Y1), 
   markAsEmpty(X1,Y1), 
   updateDistance(X1,Y1,Move,Dist)).

     
   




  

% updateDistance(+X,+Y,+D,+N)
% Update the distance to the pos (X,Y) facing in direction D if the distance (N) is better than the current saved one
updateDistance(X,Y,D,N) :- 
  dist(X,Y,D,DN), DN > N, 
  retract(dist(X,Y,D,DN)), 
  assert(dist(X,Y,D,N)).
updateDistance(_,_,_,_).

% markAsWall(+X,+Y)
markAsWall(X,Y) :- retract(maze(X,Y,_)), assert(maze(X,Y,'#')).
markAsWall(_,_).

% markAsWumpus(+X,+Y)
markAsWall(X,Y) :- retract(maze(X,Y,_)), assert(maze(X,Y,'W')).
markAsWumpus(_,_).

% markAsPit(+X,+Y)
markAsPit(X,Y) :- retract(maze(X,Y,_)), assert(maze(X,Y,'P')).
markAsPit(_,_).

% markAsSmell(+X,+Y)
markAsSmell(X,Y) :- retract(maze(X,Y,_)), assert(maze(X,Y,"smell")).
markAsSmell(_,_).

% markAsStench(+X,+Y)
markAsStench(X,Y) :- retract(maze(X,Y,_)), assert(maze(X,Y,"stench")).
markAsStench(_,_).

% markAsDamp(+X,+Y)
markAsDamp(X,Y) :- retract(maze(X,Y,_)), assert(maze(X,Y,"damp")).
markAsDamp(_,_).

% markAsEmpty(+X,+Y)
markAsEmpty(X,Y) :- retract(maze(X,Y,_)), assert(maze(X,Y,'.')).
markAsEmpty(_,_).


% wumpusPosition (+X,+Y)
% Check if in position (X,Y) there is a wumpus 
wumpusPosition(X,Y) :- maze(X,Y,'W').
wumpusPosition(X,Y) :- maze(X,Y,"unknown"), checkSmell(X,Y), checkStench(X,Y).

% checkSmell(+X,+Y)
% Check if all the found smell positions are consistent with a wumpus at position (X,Y)
checkSmell(X,Y) :- 
  findall(sm(XS,YS,Dst), (maze(XS,YS,"smell"), 
  getManhattanDistance(X,Y,XS,YS,Dst)) ,L),
  delete(L,sm(_,_,2),L2), delete(L2,sm(_,_,3),L3), length(L3,Len), Len =:= 0.

% checkStench(+X,+Y)
% Check if all the found stench positions are consistent with a wumpus at position (X,Y)
checkStench(X,Y) :- 
  findall(Dst, (maze(XS,YS,"stench"), 
  getManhattanDistance(X,Y,XS,YS,Dst)), L), 
  delete(L,1,L2), length(L2,Len), Len =:= 0.


% getManhattanDistance(+X0,+Y0,+X1,+Y1,-D)
% Manhattan Distance between 2 points D = |X0-X1| + |Y0-Y1|
getManhattanDistance(X0,Y0,X1,Y1,Distance) :- 
  Distance is (abs(X0-X1) + abs(Y0-Y1)).

% findPath (+CurrentDistance,+XS,+YS,+X,+Y,-Path)
% Find a shortest path from (XS,YS) to (X,Y) using only the discovered walkable cells.
findPath(_,XS,YS,XS,YS,[]).
findPath(N,XS,YS,X,Y,L) :- N1 is N+1, adjacentPosition(D,XS,YS,DX,DY), isSafe(DX,DY), dist(DX,DY,D,DN), DN >= N, updateDistance(DX,DY,D,N), findPath(N1,DX,DY,X,Y,L1), L = [D | L1].
