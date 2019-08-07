:- module(wumpus,[initialState/5, guess/3, updateState/4]).

:- dynamic visited/2.

% Store with intructions are move
isMove(north).
isMove(east).
isMove(south).
isMove(west).

% Store the opposite direction of D
oppositeDir(north,south).
oppositeDir(south,north).
oppositeDir(west,east).
oppositeDir(east,west).

% nextPosition(+D,+X0,+Y0,-X0,-X1)
% Get next position moving from (X0,Y0) in direction D
nextPosition(north,X0,Y0,X0,Y1) :- Y1 is Y0-1.
nextPosition(south,X0,Y0,X0,Y1) :- Y1 is Y0+1.
nextPosition(west,X0,Y0,X1,Y0) :- X1 is X0-1.
nextPosition(east,X0,Y0,X1,Y0) :- X1 is X0+1.

% getDimension(+D,-S)
% Get the associated maze dimension to a particular direction
getDimension(north,S) :- dim(S,_).
getDimension(south,S) :- dim(S,_).
getDimension(_,S) :- dim(_,S).

% insertXtimes(+V,+N,-L)
% Generate a list with the element V inserted N times.
insertXtimes(_,0,[]).
insertXtimes(V,N,L) :- N > 0, N1 is N-1, insertXtimes(V,N1,L1), L = [V | L1].

% isWalkable(+X,+Y)
% Check if the point(X,Y) can be transversed without dying
isWalkable(X,Y) :- isPositionValid(X,Y), (map(X,Y,'.') ; map(X,Y,'*') ; map(X,Y,'+')).

% isPositionValid(+X,+Y)
% Check if the point (X,Y) is inside the maze limits
isPositionValid(X,Y) :- dim(NR,NC), X > 0, Y > 0, X =< NC, Y =< NR.

% isExplorablePosition(+X,+Y,-NX,-NY)
% Check if the point (X,Y) needs to be discovered and return the walkable point (NX,NY) from where
% it can be reached.
isExplorablePosition(X,Y, NX, NY) :- map(X,Y,'?'), nextPosition(_,X,Y,NX,NY), isWalkable(NX,NY).

% isAdyacent(+D,+X0,+Y0,+X1,+Y1)
% Check if the point (X0,Y0) is adyacent to point (X1,Y1)
isAdyacent(D,X0,Y0,X1,Y1) :- nextPosition(D,X0,Y0,X1,Y1).

% inLineOfSight(+X0,+Y0,+X1,+Y1,D)
% Check if point (X1,Y1) is in the line of sight from point (X0,Y0) when facing to the direction D.
inLineOfSight(X0,Y0,X0,Y1,north) :- Y1 < Y0, not((map(X0,YW,'#'), YW < Y0, YW > Y1)).
inLineOfSight(X0,Y0,X0,Y1,south) :- Y1 > Y0, not((map(X0,YW,'#'), YW > Y0, YW < Y1)).
inLineOfSight(X0,Y0,X1,Y0,east) :- X1 > X0, not((map(XW,Y0,'#'), XW > X0, XW < X1)).
inLineOfSight(X0,Y0,X1,Y0,west) :- X1 < X0, not((map(XW,Y0,'#'), XW < X0, XW > X1)).

%initialState(+NR, +NC, +XS, +YS, -State0)
initialState(NR, NC, XS, YS, state(pos(XS,YS))) :- 
	retractall(dim(_,_)), assert(dim(NR,NC)),
	retractall(map(_,_,_)),
	retractall(dist(_,_,_,_)),
	INF is NR*NC+1,
	foreach(between(1,NC,X), foreach(between(1,NR,Y), (assert(map(X,Y,'?')),assert(dist(X,Y,north,INF)),assert(dist(X,Y,south,INF)),assert(dist(X,Y,west,INF)),assert(dist(X,Y,east,INF))  ))),
	retract(map(XS,YS,'?')),assert(map(XS,YS,'.')),
	retract(dist(XS,YS,north,INF)),assert(dist(XS,YS,north,0)).

%guess(+State0, -State, -Guess)
guess(state(pos(X,Y)), state(pos(X,Y)), Guess) :- generate_plan(X,Y,Guess).

%updateState(+State0, +Guess, +Feedback, -State)
updateState(state(pos(X,Y)), Guess, Feedback, state(pos(X,Y))) :- processFeedback(1,Guess,Feedback,X,Y).

% generate_discover_shoot_plan (+XS,+YS,-endingDir,-Instructions)
% Create a plan to move from point (XS,YS) to a hidden cell (chosen randomly) that is next to a cleared one and then shoot after moving
% to the hidden cell
generate_discover_shoot_plan(XS,YS, D, L1) :- findall(cell(CX,CY,DX,DY),isExplorablePosition(CX,CY,DX,DY),L3)
	,random_member(cell(X,Y,NX,NY),L3)
	,retractall(visited(_,_))
	,findPath(1,XS,YS,NX,NY,L)
	,nextPosition(D,NX,NY,X,Y)
	,append(L,[D, shoot],L1).

% generate_shoot_discover_plan (+XS,+YS,-endingDir, -Instructions)
% Create a plan to move from point (XS,YS) to a hidden cell (chosen randomly) that is next to a cleared one and shoot before
% moving to the hidden cell.
generate_shoot_discover_plan(XS,YS,D,L1) :- findall(cell(CX,CY,DX,DY),isExplorablePosition(CX,CY,DX,DY),L3)
	,random_member(cell(X,Y,NX1,NY1),L3)
	,retractall(visited(_,_))
	,nextPosition(D,NX1,NY1,X,Y)
	,oppositeDir(D,DO)
	,isAdyacent(DO,NX1,NY1,NX2,NY2)
	,isWalkable(NX2,NY2)
	,findPath(1,XS,YS,NX2,NY2,L)
	,append(L,[D, shoot, D],L1).

% generate_shoot_plan(+XS,+YS,-endingDir,-Instructions)
% Create a plan to shoot at a possible wumpus position (chosen randomly) from a safe cell
generate_shoot_plan(XS,YS, D, L1) :- (map(X,Y,'?') ; map(X,Y,'W')), wumpusPosition(X,Y)
	,isMove(D)
	,map(XP,YP,_)
	,isWalkable(XP,YP)
	,inLineOfSight(XP,YP,X,Y,D)
	,findPathDir(1,XS,YS,north,XP,YP,D,L)
	,append(L,[shoot],L1).

% generate_plan (+XS,+YS, -Guess)
% Create a list of instructions for the robot to move, the posibles are shoot&discover, discover&shoot, shoot and random combinations of move&shoot&foward
generate_plan(XS,YS, Guess) :- generate_shoot_discover_plan(XS,YS,D,L), length(L,N), N1 is 100-N-4, N1 > 0, generate_random_moves(N1,D,L2), append(L,L2,Guess).
generate_plan(XS,YS, Guess) :- generate_discover_shoot_plan(XS,YS,D,L), length(L,N), N1 is 100-N-4, N1 > 0, generate_random_moves(N1,D,L2), append(L,L2,Guess).
generate_plan(XS,YS, Guess) :- generate_shoot_plan(XS,YS,D,L), length(L,N), N1 is 100-N-4, N1 > 0, generate_random_moves(N1,D,L2), append(L,L2,Guess).
generate_plan(_,_, Guess) :- generate_random_moves(100,nothing,Guess).

generate_random_moves(0,_,[]) :- !.
generate_random_moves(N,D,L) :- N > 0, N < 6, generate_direction(D,X), generate_depth(X,N,N1,L1),generate_random_moves(N1,X,LL), append(L1,LL,L).
generate_random_moves(N,D,L) :- N >= 6, N1 is N-6, generate_direction(D,X), generate_depth(X,N1,N2,L1),  generate_random_moves(N2,X,LL), L2 = [X, shoot | L1],append(L2,LL,L).

generate_direction(D, X) :- L = [north,south,east,west], delete(L,D,L2), random_member(X,L2).
generate_depth(D,N,NR,LR) :- getDimension(D,S), MX is min(N,S), random_between(1,MX,Depth), insertXtimes(D,Depth,LR), length(LR,Len), NR is N-Len.

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
processMove(N,Move, wall, X0,Y0,X0,Y0) :- isMove(Move), nextPosition(Move,X0,Y0,X1,Y1), updateWall(X1,Y1), updateDist(X1,Y1,Move,N).
processMove(N,Move, empty, X0,Y0,X1,Y1) :- isMove(Move), nextPosition(Move,X0,Y0,X1,Y1), retract(map(X1,Y1,_)), assert(map(X1,Y1,'.')), updateDist(X1,Y1,Move,N).
processMove(N,Move, wumpus, X0,Y0,X0,X0) :- isMove(Move), nextPosition(Move,X0,Y0,X1,Y1), retract(map(X1,Y1,_)), assert(map(X1,Y1,'W')), updateDist(X1,Y1,Move,N).
processMove(N,Move, pit, X0,Y0,X1,Y1) :- isMove(Move), nextPosition(Move,X0,Y0,X1,Y1), retract(map(X1,Y1,_)), assert(map(X1,Y1,'P')), updateDist(X1,Y1,Move,N). 
processMove(N,Move, stench, X0,Y0,X1,Y1) :- isMove(Move), nextPosition(Move,X0,Y0,X1,Y1), retract(map(X1,Y1,_)), assert(map(X1,Y1, '*')), updateDist(X1,Y1,Move,N).
processMove(N,Move, smell, X0,Y0,X1,Y1) :- isMove(Move), nextPosition(Move,X0,Y0,X1,Y1), retract(map(X1,Y1,_)), assert(map(X1,Y1, '+')), updateDist(X1,Y1,Move,N).
processMove(N,Move, damp, X0,Y0,X1,Y1) :- isMove(Move), nextPosition(Move,X0,Y0,X1,Y1), retract(map(X1,Y1,_)), assert(map(X1,Y1, '.')), updateDist(X1,Y1,Move,N). 

% updateDist(+X,+Y,+D,+N)
% Update the distance to the pos (X,Y) facing in direction D if the distance (N) is better than the current saved one
updateDist(X,Y,D,N) :- dist(X,Y,D,DN), DN > N, retract(dist(X,Y,D,DN)), assert(dist(X,Y,D,N)).
updateDist(_,_,_,_).

% updateWall(+X,+Y)
% Change cell (X,Y) to a wall
updateWall(X,Y) :- isPositionValid(X,Y), retract(map(X,Y,_)), assert(map(X,Y,'#')).
updateWall(_,_).

% wumpusPosition (+X,+Y)
% Check if in position (X,Y) there is a wumpus 
wumpusPosition(X,Y) :- map(X,Y,'W').
wumpusPosition(X,Y) :- map(X,Y,'?'), allSmellValid(X,Y), allStenchValid(X,Y), allEmptyValid(X,Y).

% allSmellValid(+X,+Y)
% Check if all the found smell positions are consistent with a wumpus at position (X,Y)
allSmellValid(X,Y) :- findall(sm(XS,YS,Dst), (map(XS,YS,'+'), dst_manhattan(X,Y,XS,YS,Dst)) ,L), delete(L,sm(_,_,2),L2), delete(L2,sm(_,_,3),L3), length(L3,Len), Len =:= 0.

% allStenchValid(+X,+Y)
% Check if all the found stench positions are consistent with a wumpus at position (X,Y)
allStenchValid(X,Y) :- findall(Dst, (map(XS,YS,'*'), dst_manhattan(X,Y,XS,YS,Dst)), L), delete(L,1,L2), length(L2,Len), Len =:= 0.

% allEmptyValid(+X,+Y)
% Check if the found empty positions are consistent with a wumpus at position (X,Y)
allEmptyValid(X,Y) :- map(XS,YS,'.'), dst_manhattan(X,Y,XS,YS,Dst), Dst =< 3, !, fail.
allEmptyValid(_,_).

% dst_manhattan(+X0,+Y0,+X1,+Y1,-D)
% Manhattan Distance between 2 points D = |X0-X1| + |Y0-Y1|
dst_manhattan(X0,Y0,X1,Y1,D) :- D is (abs(X0-X1) + abs(Y0-Y1)).

% findPath (+CurrentDistance,+XS,+YS,+X,+Y,-Path)
% Find a shortest path from (XS,YS) to (X,Y) using only the discovered walkable cells.
findPath(_,XS,YS,XS,YS,[]).
findPath(N,XS,YS,X,Y,L) :- N1 is N+1, nextPosition(D,XS,YS,DX,DY), isWalkable(DX,DY), dist(DX,DY,D,DN), DN >= N, updateDist(DX,DY,D,N), findPath(N1,DX,DY,X,Y,L1), L = [D | L1].

% findPath (+CurrentDistance,+XS,+YS,+currentDirection,+X,+Y,+endingDirection,-Path)
% Find a shortest path from (XS,YS) to (X,Y) ending facing in endingDirection using only the discovered walkable cells.
findPathDir(_,XS,YS,D,XS,YS,D,[]).
findPathDir(N,X0,Y0,_,X1,Y1,DD,L) :- N1 is N+1, nextPosition(D,X0,Y0,DX,DY), dist(DX,DY,D,DN), DN >= N, updateDist(DX,DY,D,N), isWalkable(DX,DY), findPathDir(N1,DX,DY,D,X1,Y1,DD,L1), L = [D | L1].

% printMap()
% Print the current state of the map seen so far by the robots
printMap() :- dim(NR,NC), foreach(between(1,NR,Y), printRow(Y,NC)),write("\n").
printRow(Y,NC) :- findall(cell(X,V), (map(X,Y,V), X > 0, X =< NC ), L1), msort(L1,L), foreach(member(cell(_,CV),L), write(CV)),write("\n").