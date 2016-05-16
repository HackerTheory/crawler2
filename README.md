# Crawler

Crawler is an experimental map generation library. The goal is to be able to
procedurally generate a map for use in a rogue-like, or other games.

## Installation

Crawler is not included in Quicklisp yet. If you would still like to try it out,
you can simply clone this repository to your Quicklisp local-projects directory.

## Usage

If you would like to try out the library visually, the provided examples may be
of use. Jump to the Examples section below.

If you just want to generate the map data without rendering it:

```lisp
(ql:quickload :crawler)
(crawler:make-stage 'labyrinth)
```
`CRAWLER:MAKE-STAGE` takes 1 required argument - the type of stage to generate.
Currently, the only available type is 'LABYRINTH. In addition to the stage type,
there are optional arguments to control the generation of the chosen type.

The optional parameters are specified as keyword/values:

For the LABYRINTH stage type, the optional arguments to MAKE-STAGE are:

* `:WIDTH`: Specifies the width of the stage in cells.

* `:HEIGHT`: Specifies the height of the stage in cells.

* `:CORRIDOR-WINDINESS`: Specifies how windy corridors will be, as a range from
`0.0` to `1.0`. This has minimal effect for densely packed rooms - see
`:ROOM-DENSITY` below to control this. Default value is `0.0`.

* `:ROOM-DENSITY`: Specifies how densely rooms should be packed in the stage, as
a range from `0.1` (10%) to `1.0` (100%). Note: This is only an estimation.
Default value is `0.65`.

* `:ROOM-SIZE-MIN`: Specifies the minimum size in cells the width or height of
a room is allowed to be, within the range `3` to `99`. Note: This should be
supplied as an odd integer value, else it will be decremented by 1. Default
value is `3`.

* `:ROOM-SIZE-MAX`: Specifies the maximum size in cells the width or height of
a room is allowed to be, within the range `ROOM-SIZE-MIN` to `101`. Note: This
should be supplied as an odd integer value, else it will be decremented by 1.
Default value is `11`.

* `:DOOR-RATE`: Specified the percentage of junctions that should be doors,
as a range from `0.0` to `1.0`. A junction is an intersection between rooms and
corridors. Default value is `0.5`.

All of the data you need to render a map is located in the `GRID` slot of the
returned map data object. This is an array of `CELL` instances, each having the
following slots:

* `X`: The X location in the map.
* `Y`: The Y location in the map.
* `CARVEDP`: Predicate determining whether this cell is ground that can be
walked upon.
* `REGION-ID`: An integer representing the region. A region is a group of
adjacent cells. Each room's cells are of the same `REGION-ID`. Likewise, a
corridor between two rooms (or more, in the case of branching) is of the same
`REGION-ID`. You can think of a `REGION-ID` as belonging to a set of cells as
if it was flood-filled, stopping at junctions (what crawler calls doors or
whatever your game may define them as).
* `FEATURES`: A list of symbols identifying a special cell property, if any.
Currently, this can be one or more of the following:
 * `:JUNCTION`: The cell joins 2 unique regions.
 * `:DOOR`: A junction has a chance of becoming a door.
 * `:STAIRS-UP`: The cell with an entrance staircase.
 * `:STAIRS-DOWN`: The cell with an exit staircase.
 * `:ROOM`: The cell is part of a room.
 * `:CORRIDOR`: The cell is part of a corridor.
 * `:WALL`: The cell is part of a wall.

## Examples

If you want to try out the example map generator graphically, load the
example system and generate a map with:

```lisp
(ql:quickload :crawler-examples)
(crawler-examples:run 'crawler2:labyrinth)
```

The examples require Sketch, which is part of Quicklisp. However, it depends on
SDL2, so you will need the library for your operating system installed.

The examples use the following colors for different cells of the stage:

* White: Walkable floor.
* Gray: Un-walkable wall.
* Blue: Door.
* Red (circle): The start of the stage. A game can use this to place a staircase leading
up, etc.
* Blue (circle): The end of the stage. A game can use this to place a staircase
leading down, etc.

Within the example window the following input is accepted:

* Left mouse-click: Generate and display a new stage of the same size.
