:- module(wumpus,[initialState/5, guess/3, updateState/4]).


adjacentPosition(Direction,X0,Y0,X1,Y1) :- 
		(Direction = east, X1 is X0+1, Y1 is Y0);
		(Direction = west, X1 is X0-1, Y1 is Y0);
		(Direction = north, X1 is X0 , Y1 is Y0-1);
		(Direction = south, X1 is X0, Y1 is Y0+1).

% findPartialDimension(+D,-S)
% Get the associated (sub)maze dimension to a particular direction
findPartialDimension(Direction,PartialDimension) :- 
  ((Direction = north; Direction = south),mazeDimension(PartialDimension,_));
  (mazeDimension(_,PartialDimension)).


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
  maze(X,Y,"stench").


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
	
	retract(maze(XS,YS,"unknown")),assert(maze(XS,YS,'.')),
	retract(dist(XS,YS,north,Infinity)),assert(dist(XS,YS,north,0)).

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
% ,findPathDir(1,XS,YS,north,XP,YP,D,L)
	,findPath(1,XS,YS,XP,YP,L)
	,append(L,[shoot],L1).

% generate_plan (+XS,+YS, -Guess)
% Create a list of instructions for the robot to move, the posibles are shoot&discover, discover&shoot, shoot and random combinations of move&shoot&foward
generate_plan(XS,YS, Guess) :- generate_discover_shoot_plan(XS,YS,D,L), length(L,N), N1 is 100-N-4, N1 > 0, generate_random_moves(N1,D,L2), append(L,L2,Guess).
generate_plan(XS,YS, Guess) :- generate_shoot_plan(XS,YS,D,L), length(L,N), N1 is 100-N-4, N1 > 0, generate_random_moves(N1,D,L2), append(L,L2,Guess).
generate_plan(_,_, Guess) :- generate_random_moves(100,_,Guess).

generate_random_moves(0,_,[]) :- !.
generate_random_moves(N,D,L) :- N > 0, N < 6, generate_direction(D,X), generate_depth(X,N,N1,L1),generate_random_moves(N1,X,LL), append(L1,LL,L).
generate_random_moves(N,D,L) :- N >= 6, N1 is N-6, generate_direction(D,X), generate_depth(X,N1,N2,L1),  generate_random_moves(N2,X,LL), L2 = [X, shoot | L1],append(L2,LL,L).

generate_direction(D, X) :- L = [north,south,east,west], delete(L,D,L2), random_member(X,L2).
generate_depth(D,N,NR,LR) :- findPartialDimension(D,S), MX is min(N,S), random_between(1,MX,Depth), generateSameElementList(D,Depth,LR), length(LR,Len), NR is N-Len.

% processFeedback (+N,+Guess,+Feedback, +X,+Y)
% Process the guess and feedback list starting from position (X,Y).
processFeedback(_,_, [], _, _).
processFeedback(N, [GH | GT], [FH | FT], X, Y) :- processMove(N,GH,FH,X,Y,DX,DY), N1 is N+1, processFeedback(N1,GT,FT,DX,DY).

% processMove(+N,+Instruction,+Feedback,+X0,+Y0,-X1,-Y1)
% Do the respective action depending on the feedback. 
% (X0,Y0) is the start position before the action
% (X1,Y1) is the final position after the action
processMove(_,shoot, miss, X0,Y0, X0,Y0).
processMove(_,shoot, hit, X0,Y0, X0,Y0).
processMove(N,Move, wall, X0,Y0,X0,Y0) :-  adjacentPosition(Move,X0,Y0,X1,Y1), updateWall(X1,Y1), updateDist(X1,Y1,Move,N).
processMove(N,Move, empty, X0,Y0,X1,Y1) :-  adjacentPosition(Move,X0,Y0,X1,Y1), retract(maze(X1,Y1,_)), assert(maze(X1,Y1,'.')), updateDist(X1,Y1,Move,N).
processMove(N,Move, wumpus, X0,Y0,X0,Y0) :-  adjacentPosition(Move,X0,Y0,X1,Y1), retract(maze(X1,Y1,_)), assert(maze(X1,Y1,'W')), updateDist(X1,Y1,Move,N).
processMove(N,Move, pit, X0,Y0,X0,Y0) :-  adjacentPosition(Move,X0,Y0,X1,Y1), retract(maze(X1,Y1,_)), assert(maze(X1,Y1,'P')), updateDist(X1,Y1,Move,N). 
processMove(N,Move, stench, X0,Y0,X1,Y1) :-  adjacentPosition(Move,X0,Y0,X1,Y1), retract(maze(X1,Y1,_)), assert(maze(X1,Y1, "stench")), updateDist(X1,Y1,Move,N).
processMove(N,Move, smell, X0,Y0,X1,Y1) :-  adjacentPosition(Move,X0,Y0,X1,Y1), retract(maze(X1,Y1,_)), assert(maze(X1,Y1, "smell")), updateDist(X1,Y1,Move,N).
processMove(N,Move, damp, X0,Y0,X1,Y1) :-  adjacentPosition(Move,X0,Y0,X1,Y1), retract(maze(X1,Y1,_)), assert(maze(X1,Y1, '.')), updateDist(X1,Y1,Move,N). 

% updateDist(+X,+Y,+D,+N)
% Update the distance to the pos (X,Y) facing in direction D if the distance (N) is better than the current saved one
updateDist(X,Y,D,N) :- dist(X,Y,D,DN), DN > N, retract(dist(X,Y,D,DN)), assert(dist(X,Y,D,N)).
updateDist(_,_,_,_).

% updateWall(+X,+Y)
% Change cell (X,Y) to a wall
updateWall(X,Y) :- retract(maze(X,Y,_)), assert(maze(X,Y,'#')).
updateWall(_,_).

% wumpusPosition (+X,+Y)
% Check if in position (X,Y) there is a wumpus 
wumpusPosition(X,Y) :- maze(X,Y,'W').
wumpusPosition(X,Y) :- maze(X,Y,"unknown"), allSmellValid(X,Y), allStenchValid(X,Y).

% allSmellValid(+X,+Y)
% Check if all the found smell positions are consistent with a wumpus at position (X,Y)
allSmellValid(X,Y) :- findall(sm(XS,YS,Dst), (maze(XS,YS,"smell"), getManhattanDistance(X,Y,XS,YS,Dst)) ,L), delete(L,sm(_,_,2),L2), delete(L2,sm(_,_,3),L3), length(L3,Len), Len =:= 0.

% allStenchValid(+X,+Y)
% Check if all the found stench positions are consistent with a wumpus at position (X,Y)
allStenchValid(X,Y) :- findall(Dst, (maze(XS,YS,"stench"), getManhattanDistance(X,Y,XS,YS,Dst)), L), delete(L,1,L2), length(L2,Len), Len =:= 0.


% getManhattanDistance(+X0,+Y0,+X1,+Y1,-D)
% Manhattan Distance between 2 points D = |X0-X1| + |Y0-Y1|
getManhattanDistance(X0,Y0,X1,Y1,Distance) :- 
  Distance is (abs(X0-X1) + abs(Y0-Y1)).

% findPath (+CurrentDistance,+XS,+YS,+X,+Y,-Path)
% Find a shortest path from (XS,YS) to (X,Y) using only the discovered walkable cells.
findPath(_,XS,YS,XS,YS,[]).
findPath(N,XS,YS,X,Y,L) :- N1 is N+1, adjacentPosition(D,XS,YS,DX,DY), isSafe(DX,DY), dist(DX,DY,D,DN), DN >= N, updateDist(DX,DY,D,N), findPath(N1,DX,DY,X,Y,L1), L = [D | L1].

% findPath (+CurrentDistance,+XS,+YS,+currentDirection,+X,+Y,+endingDirection,-Path)
% Find a shortest path from (XS,YS) to (X,Y) ending facing in endingDirection using only the discovered walkable cells.
findPathDir(_,XS,YS,D,XS,YS,D,[]).
findPathDir(N,X0,Y0,_,X1,Y1,DD,L) :- N1 is N+1, adjacentPosition(D,X0,Y0,DX,DY), dist(DX,DY,D,DN), DN >= N, updateDist(DX,DY,D,N), isSafe(DX,DY), findPathDir(N1,DX,DY,D,X1,Y1,DD,L1), L = [D | L1].
