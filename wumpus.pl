% File    : wumpus.pl
% Author  : Mohammad Nafis Ul Islam <LMS Login Id : mislam3>
% Origin  : Thursday May 24 02:41:32 2018
% Purpose : Implementing the guessing and planning part of 
%           the Wumpus game and coming up with a 
%           plan(Instruction Sequence) to kill the wumpus.
%
% Wumpus Game Description:
% 
% In this game, the player will have to find and kill a wumpus, which
% is hiding in an unknown maze. For that, the player sends a series of
% disposable robots with some instructions into the maze. The robot follows
% the instructions until it is destroyed, or has killed the wumpus, or
% the instructions' list is empty. After that, the player gets a feedback
% on what the robot has sensed by following the instructions. By using those
% feedbacks, the player will send the next robot with another set of 
% instructions. The main goal of this game is to find and kill the wumpus 
% with minimum number of attempts (number of robots sent).
%
% The maze is a rectangular grid with 4 kinds of cells. A cell can have a
% pit(marked by 'P') or an empty space(marked by '.')
% or a wall(marked by '#') 
% or a wumpus(there can be only one wumpus, marked by 'W').
%
% An instruction for each robot can be either a move or "shoot", where;
% a move can be north, south, east or west and "shoot" instruction shoots
% in the last moved direction.
%
% The feedbacks can be of the following types:
% pit     - when the robot falls into the pit and is destroyed;
% wall    - when the robot hits a wall or tries to go beyond the maze,
%           and stays in the same position;
% wumpus  - when the robot moves into the same cell as the wumpus and is 
%           destroyed.
% stench  - when the robot moves to any directly adjacent cell to the wumpus;
% smell   - when the robot moves to a cell within 3 manhattan distance, but
%           not directly adjacent to the wumpus;
% damp    - when the robot is further than 3 manhattan distance from the 
%           wumpus and directly adjacent to one or more pits;
% empty   - when the robot is further than 3 manhattan distance from the
%           wumpus and not directly adjacent to any pit. 
%
% One other thing is the energy. Each move instruction consumes 1 energy
% and each shoot instruction consumes 5 energy. A secondary optimization of
% this game is to use minimum total energy.
%
% File Summary:
%
% We have implemented the guessing and planning part of this game in this
% single file. There are 3 major predicates in this file: "initialState",
% "guess" and "updateState", along with many other necessary predicates
% and rules. Their explanations are given along with their definitions. 


:- module(wumpus,[initialState/5, guess/3, updateState/4]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% adjacentPosition(+Direction,+X0,+Y0,-X1,-Y1)
% gets the adjacent position from (X0,Y0) in given Direction.
% Here in (X,Y), X denotes column position and Y denotes row position and
% the indexing starts from top left corner of the maze.

adjacentPosition(Direction,X0,Y0,X1,Y1) :- 
		(Direction = east, X1 is X0+1, Y1 is Y0);
		(Direction = west, X1 is X0-1, Y1 is Y0);
		(Direction = north, X1 is X0 , Y1 is Y0-1);
		(Direction = south, X1 is X0, Y1 is Y0+1).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% reverseDirection(+A,-B)
% gets the reverse direction of A.
		
reverseDirection(A,B) :-
  (A = east, B = west);
  (A = west, B = east);
  (A = north, B = south);
  (A = south, B = north).

  
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% getMazeDimenstion(+Direction,-Dimension)
% gets the partial dimension of the maze in the given Direction.
% If the Direction is north or south, it will be the number of rows,
% otherwise it will be the number of columns of the maze.
% It will be needed to determine how far a robot can go in the given
% Direction.
  
getMazeDimenstion(Direction,Dimension) :- 
  ((Direction = north; Direction = south), mazeDimension(Dimension,_));
  ((Direction = east; Direction = west), mazeDimension(_,Dimension)).

  
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% isSafe(+X,+Y)
% checks if the position (X,Y) is safe for the robot to move into and
% it does not get destroyed right away by stepping there.

isSafe(X,Y) :- 
  maze(X,Y,'.'); 
  maze(X,Y,"smell"); 
  maze(X,Y,"stench");
  maze(X,Y,"damp").
   

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% initialState(+NR, +NC, +XS, +YS, -state(XS,YS))  
% takes number of rows NR and number of columns NC of the maze and
% the starting position (XS,YS) for the robot and returns the 
% initial state. Our state here is the starting position of robot 
% in the maze. Based on this starting position, we always update information 
% about the maze later on. 
% For this "initialState" predicate, we initially create a 2-D array like
% interprataion of the maze in prolog. 
% This is done by defining the "maze" facts and the "cellDistance" facts.
% The "maze" facts have the positions (X,Y) and their 
% third arguments are initialized to "unknown" in the beginning. 
% The "cellDistance" facts have the positions (X,Y) for each valid
% direction and the distances are initialized to Infinity in the 
% beginning.
% Then in the end, for the starting position, 
% a maze fact will have empty('.') as the third argument and 
% a cellDistance fact will have north as the initial direction 
% and 0 as the initial distance.

initialState(NR, NC, XS, YS, state(XS,YS)) :- 
	assert(mazeDimension(NR,NC)),
	Infinity is 10000000000,
	foreach(between(1,NC,X), 
	  foreach(between(1,NR,Y), 
	   (assert(maze(X,Y,"unknown")),
	    assert(cellDistance(X,Y,east,Infinity)),
	    assert(cellDistance(X,Y,west,Infinity)),
		assert(cellDistance(X,Y,north,Infinity)),
		assert(cellDistance(X,Y,south,Infinity))))),
	assert(maze(XS,YS,'.')),
	assert(cellDistance(XS,YS,north,0)).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%	
% guess(+state(X,Y), -state(X,Y), -Guess)
% returns the same current state as the new state, because we actually update
% the facts (maze information) each time from the starting position.
% It also returns a new Guess, which is a list of instructions. This Guess 
% list is obtained by using the predicate "getGuessList".
% This "guess" predicate is called continuously by the testing system, until
% the wumpus is killed.

guess(state(X,Y), state(X,Y), Guess) :- 
  getGuessList(X,Y,Guess).

  
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% updateState(+state(X,Y), +Guess, +Feedback, -state(X,Y))
% returns the same current state as the new updated state, which is the 
% initial position (X,Y) because of the same reason stated above. 
% Here, we use the "useFeedback" predicate to update the maze information, 
% based on our guesses and received feedbacks. The initial distance is
% obviously 1 for that, as we are starting from the starting position (X,Y).
% This predicate is also called continuously by the testing system, until
% the wumpus is killed.

updateState(state(X,Y), Guess, Feedback, state(X,Y)) :- 
  useFeedback(1,Guess,Feedback,X,Y).
  
 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% useFeedback(+Distance, +GuessList, +FeedbackList, +X, +Y)
% uses each feedback for the corresponding guess to update the maze 
% information. The next position (NextX,NextY) is obtained from the
% "updateMazeInfo" predicate. The Distance is incremented by 1 at each
% time as the robot only moves using single steps.
% The GuessList and FeedbackList is written as [Head|Tail] format below, for
% implementation convenience.

useFeedback(_,_, [], _, _).
useFeedback(Distance, [FirstGuess | RemainingGuesses], 
  [FirstFeedback | RemainingFeedbacks], X, Y) :- 
    updateMazeInfo(Distance,FirstGuess,FirstFeedback,X,Y,NextX,NextY), 
	NewDistance is (Distance+1), 
	useFeedback(NewDistance,RemainingGuesses,RemainingFeedbacks,NextX,NextY).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%	
% updateMazeInfo(+Dist,+Instruction,+Feedback,+X0,+Y0,-X1,-Y1)
% updates the "maze" facts and "cellDistance" facts based on the Instruction, 
% Feedback and distance Dist (for updating distance).
% Position (X0,Y0) is the previous position before executing the instruction.
% Position (X1,Y1) is returned as the final position after executing the 
% instruction, for which the robot gets the feedback.
% We update the facts based on those feedbacks.
% We also do not update any distance for the cells where, 
% the robot cannot go (wall) or which are unsafe (pit and wumpus), 
% since these cells will never be in the finally planned path.

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


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%   
% getGuessList(+XS,+YS, -Guess)
% generates a list of Guess using the full 100 Energy.
% It either takes a shoot plan after moving to a safe cell, or 
% attempts to kill the wumpus from a safe cell in the beginning.
% Then it generates some random moves with random direction and shoot
% instruction to discover new information about the maze utilizing
% the rest of the Energy.
% In the initial list L, there will be one shoot instruction. As shoot
% consumes 5 Energy, the remaining energy is 100-EnergyUsed-4(not 5),
% because shoot itself is included in the list length of L.
 
getGuessList(XS,YS, Guess) :- 
  (getMoveThenShootPlan(XS,YS,D,L);
  getAttempKillPlan(XS,YS,D,L)),  
  length(L,EnergyUsed), 
  RemainingEnergy is 100-EnergyUsed-4, RemainingEnergy > 0, 
  getRandomMovesList(D,RemainingEnergy,NewL), 
  append(L,NewL,Guess).  
  
  
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%  
% getMoveThenShootPlan(+XS,+YS, -Direction, -Plan)
% gets a plan to move from position (XS,YS) to a new discoverable 
% position and then shoot from that newly discovered safe cell, to gain
% new information about the maze.
% For that, we take any random discoverable cell after finding all possible
% ones and get the shortest path to that cell from (XS,YS). Then we
% get the Direction which was needed to get to that newly discovered safe
% cell. Then we shoot after adding that Direction to the Path list and 
% complete our Plan.

getMoveThenShootPlan(XS,YS, Direction, Plan) :- 
  findall(position(FromX,FromY,ToX,ToY),
  isDiscoverable(FromX,FromY,ToX,ToY),DiscoverableList),
  random_member(position(X0,Y0,X1,Y1),DiscoverableList),
  getShortestPath(1,XS,YS,X0,Y0,Path),
  adjacentPosition(Direction,X0,Y0,X1,Y1),
  append(Path,[Direction, shoot],Plan).
  

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%  
% isDiscoverable(+FromX,+FromY,-X,-Y)
% returns the position (X,Y) if it(initially unknown) can be  
% discovered from an adjacent position (FromX,FromY).

isDiscoverable(FromX, FromY, X, Y) :- 
  maze(X,Y,"unknown"), 
  isSafe(FromX,FromY), 
  adjacentPosition(_,FromX,FromY,X,Y).  
	

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% getAttempKillPlan(+XS,+YS, -Direction, -Plan)	
% gets the plan to shoot the wumpus at position (X,Y)(possible assumption) 
% from a safe position (XShoot,YShoot). For that, we get the shortest path
% from the position (XS,YS) to the shooting position (XShoot,YShoot) and
% add shoot to Plan. Ofcourse, the assumed wumpus position (X,Y) needs to
% be shootable from (XShoot,YShoot) and the Direction for that can be 
% obtained by checking with the "isShootable" predicate. We do not need
% to add any direction for shooting in the plan, as it will just shoot in the 
% last moved(to (XShoot,YShoot)) direction.

getAttempKillPlan(XS,YS, Direction, Plan) :- 
  wumpusPosition(X,Y),
  maze(XShoot,YShoot,_),
  isSafe(XShoot,YShoot),
  isShootable(XShoot,YShoot,X,Y,Direction),
  getShortestPath(1,XS,YS,XShoot,YShoot,Path),
  append(Path,[shoot],Plan).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% isShootable(+X0,+Y0,+X1,+Y1,+Direction)
% checks if the position (X1,Y1) is in the line of sight from position
% (X0,Y0), while facing in the given Direction and there is no wall 
% between them. If so, it will be shootable from position (X0,Y0).

isShootable(X0,Y0,X1,Y1,Direction) :- 
  (Direction = east,Y1 is Y0,X1 > X0,not(wallInBetween(X0,Y0,X1,Y1,east)));
  (Direction = west,Y1 is Y0,X1 < X0,not(wallInBetween(X0,Y0,X1,Y1,west)));
  (Direction = north,X1 is X0,Y1 < Y0,not(wallInBetween(X0,Y0,X1,Y1,north)));
  (Direction = south,X1 is X0,Y1 > Y0,not(wallInBetween(X0,Y0,X1,Y1,south))).

  
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% wallInBetween(+X0,+Y0,+X1,+Y1,+Direction)
% checks if there is any wall('#') between position (X0,Y0) and (X1,Y1),
% while facing in the given Direction. It is used to check the shootable 
% position.
  
wallInBetween(X0,Y0,X1,Y1,Direction) :-
  (Direction = east, (maze(X_Wall,Y0,'#'), X_Wall > X0, X_Wall < X1));
  (Direction = west, (maze(X_Wall,Y0,'#'), X_Wall < X0, X_Wall > X1));
  (Direction = north, (maze(X0,Y_Wall,'#'), Y_Wall < Y0, Y_Wall > Y1));
  (Direction = south, (maze(X0,Y_Wall,'#'), Y_Wall > Y0, Y_Wall < Y1)).  
  
  
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 
% generateNRandomMoves(+InitialDirection,+N,-L)
% generates N random moves into the list L based on the initial direction.
% The initial direction is used to ensure that a new random move is
% any direction other than the opposite of the initial direction. So, it 
% only moves forward to discover new information about the maze.

generateNRandomMoves(_,0,[]).
generateNRandomMoves(InitialDirection,N,L) :- 
  N > 0, 
  AllPossibleDirections = [north,south,east,west],
  reverseDirection(InitialDirection,RevDir),
  delete(AllPossibleDirections,RevDir,NewAllowedDirections),
  random_member(Move,NewAllowedDirections),
  NewN is N-1,
  generateNRandomMoves(Move,NewN,NewL), 
  append([Move],NewL,L).
 
 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 
% getRandomDirection(+CurrentDirection, -NewRandomDirection)
% gets a random direction other than the current direction.

getRandomDirection(CurrentDirection, NewRandomDirection) :- 
  AllPossibleDirections = [north,south,east,west], 
  delete(AllPossibleDirections,CurrentDirection,NewDirectionList), 
  random_member(NewRandomDirection,NewDirectionList). 
 
 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 
% getRandomMovesList(+Direction,+Energy,-L)
% gets a random list of moves based on the given Direction and Energy.
% If, the energy is between 1 and 5 (inclusive), then we can just give
% directions. If it is more than 5, we give directions with a shoot 
% instruction embedded into the list. We call the predicate recursively
% until the full energy is used.

getRandomMovesList(_,0,[]).
getRandomMovesList(Direction,Energy,L) :- 
  
  (Energy > 0, Energy < 6, 
  getRandomDirection(Direction,RandomDirection), 
  getMazeDimenstion(RandomDirection,Dimension), 
  N is min(Energy,Dimension),
  generateNRandomMoves(RandomDirection,N,L1),
  RemainingEnergy is (Energy - N),
  getRandomMovesList(RandomDirection,RemainingEnergy,NewL), 
  append(L1,NewL,L));
  
  (Energy >= 6, EnergyAfterShoot is (Energy-6), 
  getRandomDirection(Direction,RandomDirection),
  getMazeDimenstion(RandomDirection,Dimension), 
  N is min(EnergyAfterShoot,Dimension),
  generateNRandomMoves(RandomDirection,N,L1),
  RemainingEnergy is (EnergyAfterShoot - N),			 
  getRandomMovesList(RandomDirection,RemainingEnergy,NewL),
  append([RandomDirection,shoot],L1,L2),
  append(L2,NewL,L)).

  
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 
% updateDistance(+X,+Y,+Direction,+NewDistance)
% updates the "cellDistance" fact for position (X,Y) in the given
% Direction when we find a smaller new distance. 
 
updateDistance(X,Y,Direction,NewDistance) :- 
  cellDistance(X,Y,Direction,OldDistance), OldDistance > NewDistance, 
  retract(cellDistance(X,Y,Direction,OldDistance)), 
  assert(cellDistance(X,Y,Direction,NewDistance)).
updateDistance(_,_,_,_).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% markAs(+X,+Y,+Mark)
% updates the "maze" fact by retracting the old argument and asserting the
% new Mark as the third argument.

markAs(X,Y,Mark) :- retract(maze(X,Y,_)), assert(maze(X,Y,Mark)).

% This is done to handle the case of the border of the maze, which
% does not have any explicit '#' mark but is treated as wall according
% to the feedbacks.

markAs(_,_,'#').


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% wumpusPosition(+X,+Y)
% checks if the position (X,Y) can have the wumpus or not.

wumpusPosition(X,Y) :- maze(X,Y,'W').
wumpusPosition(X,Y) :- maze(X,Y,"unknown"),checkSmell(X,Y),checkStench(X,Y).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% checkList(+List,+E1,+E2)
% checks whether all the elements in the List are either E1 or E2 and
% nothing else. List is written as [Head|Tail] format below for 
% implementation convenience.

checkList([],_,_).
checkList([L|LS],E1,E2) :- 
  member(L,[E1,E2]), checkList(LS,E1,E2).

  
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% checkSmell(+X,+Y)
% checks if we can find all the valid smell positions for assuming a 
% wumpus to be at position (X,Y).
% For that, we find all the positions (XTemp,YTemp) which have 
% smell and take the distances of (XTemp,YTemp) positions from assumed 
% wumpus position (X,Y) and save them in a list L.
% This distance list L must only have twos or threes to be consistent with
% a wumpus at position (X,Y), because the smell can only be sensed from 2 or 
% 3 manhattan distance positions of the wumpus but not adjacent ones. 
% If we do not find any smell position, then we cannot assume that (X,Y) has 
% a wumpus based on checking the smell. So, we cut there. 
% Otherwise, we check the list for only twos or threes as its elements.
% If we find any distance other than 2 or 3 in the list L, then the (X,Y) 
% position cannot be validly assumed to contain a wumpus based on 
% smell checking.
  
checkSmell(X,Y) :- 
  findall(Distance,(maze(XTemp,YTemp,"smell"), 
  getManhattanDistance(X,Y,XTemp,YTemp,Distance)),L),
  length(L,Len),
  ( Len =:= 0 -> !
  ; checkList(L,2,3) 
  ).
  

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%  
% checkStench(+X,+Y)
% checks if we can find all the valid stench positions for assuming a 
% wumpus to be at position (X,Y).
% For that, we find all the positions (XTemp,YTemp) which have 
% stench and take the distances of (XTemp,YTemp) positions from assumed 
% wumpus position (X,Y) and save them in a list L.
% This distance list L must only have ones to be consistent with
% a wumpus at position (X,Y), because the stench can only be sensed from the 
% adjacent position of the wumpus. If we do not find any stench position, 
% then we cannot assume that (X,Y) has a wumpus based on checking the stench. 
% So, we cut there. Otherwise, we check the list for only ones as its 
% elements. If we find any distance other than 1 in the list L, then 
% the (X,Y) position cannot be validly assumed to contain a wumpus based on 
% stench checking.

checkStench(X,Y) :- 
  findall(Distance,(maze(XTemp,YTemp,"stench"), 
  getManhattanDistance(X,Y,XTemp,YTemp,Distance)),L),
  length(L,Len),
  ( Len =:= 0 -> !
  ; checkList(L,1,1)
  ).

  
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%  
% getManhattanDistance(+X0,+Y0,+X1,+Y1,-Distance)
% just returns the Manhattan Distance between the positions (X0,Y0) and 
% (X1,Y1) in the maze.

getManhattanDistance(X0,Y0,X1,Y1,Distance) :- 
  Distance is (abs(X1-X0) + abs(Y1-Y0)).

  
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% getShortestPath(+Distance,+XS,+YS,+X,+Y,-L)
% gets the shortest path from the starting position (XS,YS) to the ending
% position (X,Y) using only the safe cells discovered till now. The path
% is given in the list L. The Distance is used to update the "cellDistance"
% facts where necessary. So, the initial Distance is 1 when this predicate
% is called the first time. Then we recursively get the shortest path by 
% incrementing the Distance by 1 and taking the new starting 
% position (XTemp,YTemp).

getShortestPath(_,XS,YS,XS,YS,[]).
getShortestPath(Distance,XS,YS,X,Y,L) :- 
  adjacentPosition(Direction,XS,YS,XTemp,YTemp), 
  isSafe(XTemp,YTemp), 
  cellDistance(XTemp,YTemp,Direction,OldDistance), 
  Distance =< OldDistance, 
  updateDistance(XTemp,YTemp,Direction,Distance), 
  NextDistance is Distance+1, 
  getShortestPath(NextDistance,XTemp,YTemp,X,Y,L1), 
  append([Direction],L1,L).