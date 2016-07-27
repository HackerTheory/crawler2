% 1) A door can only be added to a cell with a junction
% 2) A floor can only be added to a cell with a wall, and must remove
% the wall in the process
% 3) A wall can only be added to a cell with a floor, and must remove
% the floor in the process
% 4) A door can only be added to a floor cell
% 5) A trap can only be added to a floor cell (even if it is triggered
% by inspecting a wall in that space)
% 6) A door must not be walkable if it is closed
% (that is modify the _floor_ sub-feature)
% 7) a floor's sub-features can only be 1 of corridor or room. if one is
% true, the other must be set to nil


% how about this map as a test, Lower Left corner is (0,0).
% d are doors.
% ###########
% #...###...#
% #...###...#
% #...###...#
% #d#######d#
% #.#######.#
% #....####.#
% ####.####.#
% ####..###.#
% #####.....#
% ###########

% Define the cells themselves. 
% cell(rowindex, colindex).
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

% Define (only) the floor cells. a non-floor cell is a wall cell.
% scanning from bottom to top, left to right
floor(cell(1,5)).
floor(cell(1,6)).
floor(cell(1,7)).
floor(cell(1,8)).
floor(cell(1,9)).
floor(cell(2,4)).
floor(cell(2,5)).
floor(cell(2,9)).
floor(cell(3,4)).
floor(cell(3,9)).
floor(cell(4,1)).
floor(cell(4,2)).
floor(cell(4,3)).
floor(cell(4,4)).
floor(cell(4,9)).
floor(cell(5,1)).
floor(cell(5,9)).
floor(cell(6,1)). % will be a junction and door too
floor(cell(6,9)). % will be a junction and door too
floor(cell(7,1)).
floor(cell(7,2)).
floor(cell(7,3)).
floor(cell(7,7)).
floor(cell(7,8)).
floor(cell(7,9)).
floor(cell(8,1)).
floor(cell(8,2)).
floor(cell(8,3)).
floor(cell(8,7)).
floor(cell(8,8)).
floor(cell(8,9)).
floor(cell(9,1)).
floor(cell(9,2)).
floor(cell(9,3)).
floor(cell(9,7)).
floor(cell(9,8)).
floor(cell(9,9)).

% a wall is a non-floor cell (\+ means true if it can't prove the goal).
wall(Cell) :- \+ floor(Cell).

% Some cells are part of rooms.
% TODO 

% Some cells are part of corridors.
% TODO 

% Define the junctions.
junction(cell(6,1)).
junction(cell(6,9)).

% Define the doors (which are a floor AND a junction). Currently, this means
% that ALL of those locations are doors.
door(Door) :- floor(Door), junction(Door).

% Define the floor neighbors to each floor cell.
% TODO





