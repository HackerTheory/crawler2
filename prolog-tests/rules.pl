% This prolog file builds a knowledge base about this map (origin Lower Left):
% j are junctions.
% d are doors.
% ###########
% #...###...#
% #...###...#
% #...###...#
% #d#######j#
% #.#######.#
% #....####.#
% ####.####.#
% ####..###.#
% #####.....#
% ###########

% axion's original rules.
% 1) A door can only be added to a cell with a junction
% 2) A floor can only be added to a cell with a wall, and must remove
% the wall in the process.
% 3) A wall can only be added to a cell with a floor, and must remove
% the floor in the process
% 4) A door can only be added to a floor cell
% 5) A trap can only be added to a floor cell (even if it is triggered
% by inspecting a wall in that space)
% 6) A door must not be walkable if it is closed
% (that is modify the _floor_ sub-feature)
% 7) a floor's sub-features can only be 1 of corridor or room. if one is
% true, the other must be set to nil

% ((:wall . (:walkable nil :material (:stone)))
% (:floor . (:walkable t :corridor t :room nil :entry-distance -1 :region 1 :material (:stone :moss)))
% (:entry . (:direction :up :method :staircase))
% (:junction . (:regions (1 2)))
% (:door . (:closed t :locked t :secret nil :orientation :vertical :material (:wood)))
% (:trap . (:trigger :door-open :type :poison-dart)))


% Rewritten rules more suitable for prolog. (NOT DONE).

% A cell has a 2d position.
% All cells must have space or not have space.
% The topology of a cell is either room or cell.

% The offset of a cell is another defined cell located at (dx, dy) from it.
% Offset shortcuts of e, ne, n, nw, e, sw, s, se are available.

% A vjunct cell is a corridor cell whose:
% 	north neighbor cell is a room cell and 
%	the south neighbor cell is a corridor cell
% OR
% 	north neighbor cell is a corridor cell and 
%	the south neighbor cell is a room cell

% An hjunct cell is a corridor cell whose:
% 	east neighbor cell is a room cell and 
%	the west neighbor cell is a corridor cell
% OR
% 	west neighbor cell is a corridor cell and 
%	the east neighbor cell is a room cell

% A junction cell is a cell that is an hjunct OR vjunct cell.

% A cell may be a door if it is also a junction.
% A cell may be a trap if it is also a floor


% ---------------------------------------------------------------------------

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Define each cell itself. Only those which are defined are actually on the map.
% cell(rowindex, colindex).
% There is no ':- dynamic cell/2.' since we can't add/delete any new cells.
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
cell(0, 0). cell(0, 1). cell(0, 2). cell(0, 3). cell(0, 4). cell(0, 5).
cell(0, 6). cell(0, 7). cell(0, 8). cell(0, 9). cell(0, 10).

cell(1, 0). cell(1, 1). cell(1, 2). cell(1, 3). cell(1, 4). cell(1, 5).
cell(1, 6). cell(1, 7). cell(1, 8). cell(1, 9). cell(1, 10).

cell(2, 0). cell(2, 1). cell(2, 2). cell(2, 3). cell(2, 4). cell(2, 5).
cell(2, 6). cell(2, 7). cell(2, 8). cell(2, 9). cell(2, 10).

cell(3, 0). cell(3, 1). cell(3, 2). cell(3, 3). cell(3, 4). cell(3, 5).
cell(3, 6). cell(3, 7). cell(3, 8). cell(3, 9). cell(3, 10).

cell(4, 0). cell(4, 1). cell(4, 2). cell(4, 3). cell(4, 4). cell(4, 5).
cell(4, 6). cell(4, 7). cell(4, 8). cell(4, 9). cell(4, 10).

cell(5, 0). cell(5, 1). cell(5, 2). cell(5, 3). cell(5, 4). cell(5, 5).
cell(5, 6). cell(5, 7). cell(5, 8). cell(5, 9). cell(5, 10).

cell(6, 0). cell(6, 1). cell(6, 2). cell(6, 3). cell(6, 4). cell(6, 5).
cell(6, 6). cell(6, 7). cell(6, 8). cell(6, 9). cell(6, 10).

cell(7, 0). cell(7, 1). cell(7, 2). cell(7, 3). cell(7, 4). cell(7, 5).
cell(7, 6). cell(7, 7). cell(7, 8). cell(7, 9). cell(7, 10).

cell(8, 0). cell(8, 1). cell(8, 2). cell(8, 3). cell(8, 4). cell(8, 5).
cell(8, 6). cell(8, 7). cell(8, 8). cell(8, 9). cell(8, 10).

cell(9, 0). cell(9, 1). cell(9, 2). cell(9, 3). cell(9, 4). cell(9, 5).
cell(9, 6). cell(9, 7). cell(9, 8). cell(9, 9). cell(9, 10).

cell(10, 0). cell(10, 1). cell(10, 2). cell(10, 3). cell(10, 4). cell(10, 5).
cell(10, 6). cell(10, 7). cell(10, 8). cell(10, 9). cell(10, 10).

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% offset/4
% Bind NeighborCell to the (OFF_X,OFF_Y) offset of the specified cell.
% It will bind NeighborCell to false if the cell it computes is not in the KB,
% Otherwise NeighborCell will be bound to the computed cell.
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
offset(cell(X,Y), OFF_X, OFF_Y, OffsetCell) :-
	number(OFF_X), number(OFF_Y),
	NX is X + OFF_X,
	NY is Y + OFF_Y,
	% enforce that the probed cell location is actually valid.
	cell(NX, NY),
	OffsetCell = cell(NX, NY).

% convenience direction rules
e(cell(X,Y), Cell) :- offset(cell(X, Y), 1, 0, Cell).
ne(cell(X,Y), Cell) :- offset(cell(X, Y), 1, 1, Cell).
n(cell(X,Y), Cell) :- offset(cell(X, Y), 0, 1, Cell).
nw(cell(X,Y), Cell) :- offset(cell(X, Y), -1, 1, Cell).
w(cell(X,Y), Cell) :- offset(cell(X, Y), -1, 0, Cell).
sw(cell(X,Y), Cell) :- offset(cell(X, Y), -1, -1, Cell).
s(cell(X,Y), Cell) :- offset(cell(X, Y), 0, -1, Cell).
se(cell(X,Y), Cell) :- offset(cell(X, Y), 1, -1, Cell).

% Now, we define all of the relations that cells can be a part of.
:- dynamic has_space/1.
:- dynamic corridor/1.
:- dynamic room/1.
:- dynamic junction/1.


% Scanning from left to right, and bottom to top, define the carved
% cells and what kind of cell they are.
has_space(cell(1,5)).
has_space(cell(1,6)).
has_space(cell(1,7)).
has_space(cell(1,8)).
has_space(cell(1,9)).
has_space(cell(2,4)).
has_space(cell(2,5)).
has_space(cell(2,9)).
has_space(cell(3,4)).
has_space(cell(3,9)).
has_space(cell(4,1)).
has_space(cell(4,2)).
has_space(cell(4,3)).
has_space(cell(4,4)).
has_space(cell(4,9)).
has_space(cell(5,1)).
has_space(cell(5,9)).
has_space(cell(6,1)).
has_space(cell(6,9)).
has_space(cell(7,1)).
has_space(cell(7,2)).
has_space(cell(7,3)).
has_space(cell(7,7)).
has_space(cell(7,8)).
has_space(cell(7,9)).
has_space(cell(8,1)).
has_space(cell(8,2)).
has_space(cell(8,3)).
has_space(cell(8,7)).
has_space(cell(8,8)).
has_space(cell(8,9)).
has_space(cell(9,1)).
has_space(cell(9,2)).
has_space(cell(9,3)).
has_space(cell(9,7)).
has_space(cell(9,8)).
has_space(cell(9,9)).

corridor(cell(1,5)).
corridor(cell(1,6)).
corridor(cell(1,7)).
corridor(cell(1,8)).
corridor(cell(1,9)).
corridor(cell(2,4)).
corridor(cell(2,5)).
corridor(cell(2,9)).
corridor(cell(3,4)).
corridor(cell(3,9)).
corridor(cell(4,1)).
corridor(cell(4,2)).
corridor(cell(4,3)).
corridor(cell(4,4)).
corridor(cell(4,9)).
corridor(cell(5,1)).
corridor(cell(5,9)).
corridor(cell(6,1)).
corridor(cell(6,9)).

room(cell(7,1)).
room(cell(7,2)).
room(cell(7,3)).
room(cell(7,7)).
room(cell(7,8)).
room(cell(7,9)).
room(cell(8,1)).
room(cell(8,2)).
room(cell(8,3)).
room(cell(8,7)).
room(cell(8,8)).
room(cell(8,9)).
room(cell(9,1)).
room(cell(9,2)).
room(cell(9,3)).
room(cell(9,7)).
room(cell(9,8)).
room(cell(9,9)).

junction(cell(6,1)).
junction(cell(6,9)).

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Terrain types
% Can only add terrain to carved cells.
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- dynamic terrain/2.
add_terrain(Cell, TerrainType) :- 
	has_space(Cell),
	\+(terrain(Cell, TerrainType)), 
	assert(terrain(Cell, TerrainType)).

% This is a test of how to write a rule such that we can find out why it
% failed. I deeply suspect this is in no way idiomatic prolog.
add_terrain2(Cell, TerrainType, Status) :- 
	(has_space(Cell) ->  
		(\+(terrain(Cell, TerrainType)) -> 
			(assert(terrain(Cell, TerrainType)) ->
				Status = ok ; 
				Status = not_ok:cant_assert_terrain) ;
			Status = not_ok:already_has_terrain)
	; Status = not_ok:no_space).


remove_terrain(Cell, TerrainType) :- 
	has_space(Cell),
	terrain(Cell, TerrainType),
	retract(terrain(Cell, TerrainType)).

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Keys
% Dynamic means here we can allow/disable other objects to be keys.
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- dynamic key/1.
key(k1).

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Doors
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- dynamic door/1.
:- dynamic door_location/2.
:- dynamic place_door/2.
:- dynamic closed/1. % open by default.
:- dynamic locked/1. % unlocked by default.
:- dynamic locked_by/2. % not locked_by by default.

% Define a door.
door(d1).

% It is currently closed.
closed(d1).

% The door is locked with key k1.
locked_by(d1, k1).

% This is how we place a door into the maze. It must go at a junction.
place_door(Door, Cell) :- 
	has_space(Cell),
	junction(Cell), 
	assert(door_location(Door, Cell)).

% Place the door we just defined.
:- place_door(d1, cell(6,1)).

% Basic check to see if a door is locked.
locked(Door) :- locked_by(Door, Key), key(Key), closed(Door).

% Specify the rules to unlock and lock a door.
unlock_door_with_key(Door, Key) :- 
	key(Key),
	door(Door), closed(Door), locked_by(Door, Key), 
	retract(locked_by(Door, Key)).

lock_door_with_key(Door, Key) :-
	key(Key),
	door(Door), \+(locked_by(Door, Key)), closed(Door),
	assert(locked_by(Door, Key)).




