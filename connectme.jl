# A constraint based solver for the "Connect Me - Logic Puzzle"
# Android app developed by Viktor Bohush.


using Printf
using StaticArrays
using Test


mutable struct Cell
  row::Int 
  column::Int
  candidates    # ::Vector{Candidate}

  function Cell(row::Int, column::Int)
    return new(row, column, Vector{Candidate}())
  end
end

count_candidates(cell::Cell) = length(cell.candidates)

function Base.show(io::IO, c::Cell)
  @printf(io, "Cell(%d, %d [%d candidates])",
          c.row, c.column, count_candidates(c))
end


# Each edge of each Tile has an associated LinkCount.  To solve the
# puzzle, the LinkCounts of the common edge of two adjacent Tiles must
# match.
@enum LinkCount O=1 I=1<<1 II=1<<2 III=1<<3 IIII=1<<4

struct LinkCountSet
  bits::UInt8

  LinkCountSet(bits::UInt8) = new(bits)
  LinkCountSet() = LinkCountSet(0x0)
  LinkCountSet(lc::LinkCount) = LinkCountSet(UInt8(lc))
end

isempty(lcs::LinkCountSet) = lcs.bits == 0

Base.:|(lc1::LinkCount, lc2::LinkCount) = LinkCountSet(UInt8(lc1) | UInt8(lc2))

Base.:|(lc::LinkCount, lcs::LinkCountSet) = LinkCountSet(UInt8(lc) | lcs.bits)

Base.:|(lcs::LinkCountSet, lc::LinkCount) = lc | lcs

Base.:|(lcs1::LinkCountSet, lcs2::LinkCountSet) = LinkCountSet(lcs1.bits | lcs2.bits)

Base.:&(lc1::LinkCount, lc2::LinkCount) = LinkCountSet(UInt8(lc1) & UInt8(lc2))

Base.:&(lc::LinkCount, lcs::LinkCountSet) = LinkCountSet(UInt8(lc) & lcs.bits)

Base.:&(lcs::LinkCountSet, lc::LinkCount) = lc & lcs

Base.:&(lcs1::LinkCountSet, lcs2::LinkCountSet) = LinkCountSet(lcs1.bits & lcs2.bits)

function Base.show(io::IO, lcs::LinkCountSet)
  local any = false
  for lc in [O, I, II, III, IIII]
    if (lcs & lc) != LinkCountSet()
      if any
        print(io, "|")
      end
      show(io, lc)
      any = true
    end
  end
  if !any
    print("LinkCountSet()")
  end
end


# Each Tile has four sides identified by their directions.
# Directions start at 1 because they are used as the index into
# the link_count field of Tile.
@enum Direction UP=1 RIGHT=2 DOWN=3 LEFT=4

function opposite(d::Direction)
  if d == RIGHT return LEFT end
  if c == LEFT return RIGHT end
  if d == UP return DOWN end
  if d == DOWN return UP end
end

# ????? Why is no constructor defined for this?
# primitive type Rotation <: Integer 8 end
Rotation = Integer

Base.:+(direction::Direction, rotation::Rotation) = Direction(mod1(Int(direction) - Int(rotation), 4))

@test DOWN + 0 == DOWN
@test DOWN + 2 == UP


""" Some cells in the grid have Tiles in them.  There are restrictions
about where a given tile can be placed and whether it can be
rotated."""
struct Tile
  # restricted_to_row specifies that the tile can only be placed in a
  # cell of the specified row.
  restricted_to_row::Union{Int, Nothing}
  # restricted_to_column specifies that the tile can only be placed in a
  # cell of the specified column.
  restricted_to_column::Union{Int, Nothing}
  rotates::Bool
  # The number of links that the tile presents in a given direction.
  link_counts::Vector{LinkCount}     # indexed by Direction, length 4.

  function Tile(link_counts::Vararg{LinkCount, 4}; row=nothing, col=nothing, rotates=true)
    return new(row, col, rotates,
               [lc for lc in link_counts])
  end
end

function Base.show(io::IO, tile::Tile)
  print(io, "Tile(")
  local first = true
  for lc in tile.link_counts
    if !first
      print(io, ", ")
    end
    first = false
    show(io, lc)
  end
  if tile.rotates == false
    print(io, ", rotates=false")
  end
  if tile.restricted_to_row != nothing
    @printf(io, ", row=%d", tile.restricted_to_row)
  end
  if tile.restricted_to_column != nothing
    @printf(io, ", col=%d", tile.restricted_to_column)
  end
  print(io, ")")
end

function equivalent(tile1::Tile, tile2::Tile)::Bool
  if tile1.restricted_to_row != tile2.restricted_to_row return false end
  if tile1.restricted_to_column != tile2.restricted_to_column return false end
  if tile1.rotates != tile2.rotates return false end
  if tile1.rotates
    # The Tiles could be rotated relative to one another
    # Is there a rotation of tile2 that h as the same link_counts as tile1?
    function match(rotation, direction)
      return link_count(tile1, direction, 0) ==
             link_count(tile2, direction, rotation)
    end
    if !any(rotation -> all(direction -> match(rotation, direction),
                            instances(Direction)),
            0:3)
       return false
     end
  else
    if tile1.link_counts != tile2.link_counts return false end
  end
  return true
end

function link_count(tile::Tile, direction::Direction, rotation::Rotation)
  if !tile.rotates
    @assert rotation == 0
  end
  return tile.link_counts[Int(direction + rotation)]
end

@test link_count(Tile(IIII, III, I, O), RIGHT, 0) == III


"""Create all possible Candidates for the Cell and add them."""
function add_candidates(cell::Cell, tile::Tile)
  if !(tile.restricted_to_row == nothing ||
       tile.restricted_to_row == cell.row)
    return
  end
  if !(tile.restricted_to_column == nothing ||
       tile.restricted_to_column == cell.column)
    return
  end
  local rotations =
    if tile.rotates
      if link_count(tile, UP, 0) == link_count(tile, DOWN, 0) &&
         link_count(tile, UP, 0) == link_count(tile, DOWN, 0)
         # 180 degree symetry
         if link_count(tile, UP, 0) == link_count(tile, RIGHT, 0)
           # full rotational symetry
           [0]
         else
           [0, 2]
         end
      else
        [0, 1, 2, 3]
      end
    else
      [0]
    end
  for rotation in rotations
    push!(cell.candidates, Candidate(tile, rotation))
  end
end


# It's convenient for has_candidate to work on the the absense of a
# Tile, as can be returned by neighbor.
function has_candidate(cell::Nothing, tile::Tile)::Bool
  return false
end

function has_candidate(cell::Cell, tile::Tile)::Bool
  for c in cell.candidates
    if c.tile == tile
      return true
    end
  end
  return false
end


"""Candidate represents a possible orientation of a Tile within a Cell.
Candidates are added to Cells when the puzzle is created.  Candidates are
removed during constraint propagation."""
struct Candidate
  tile::Tile
  rotation::Rotation
end

function link_count(candidate::Candidate, direction::Direction)
  return link_count(candidate.tile, direction, candidate.rotation)
end


"""A Connect Me puzzle sonsists of a square grid.  There are also
tiles which can be placed in the grid."""
struct Puzzle
  tiles::Vector{Tile}
  grid::Array{Cell, 2}
end

rows(p::Puzzle) = size(p.grid, 1)
columns(p::Puzzle) = size(p.grid, 2)

function Puzzle(width::Int, height::Int, tiles::Vector{Tile})
  # Create Cells
  local grid = Array{Cell}(undef, height, width)
  for row = 1:height
    for column = 1:width
      local cell = Cell(row, column)
      grid[row, column] = cell
      for tile in tiles
        add_candidates(cell, tile)
      end
    end
  end
  return Puzzle(tiles, grid)
end

function solved(puzzle::Puzzle)::Bool
  return length(puzzle.tiles) == count_candidates(puzzle)
end

function count_candidates(puzzle::Puzzle)::Int
  count = 0
  for c in puzzle.grid
    count += count_candidates(c)
  end
  return count
end

function cell(puzzle::Puzzle, row::Int, column::Int)::Union{Cell, Nothing}
  if row < 1 return nothing end
  if row > rows(puzzle) return nothing end
  if column < 1 return nothing end
  if column > columns(puzzle) return nothing end
  return puzzle.grid[row, column]
end
  
function show_candidates(puzzle::Puzzle)
  for c in puzzle.grid
    show(c)
    print("\n")
    for candidate in c.candidates
      @printf("  %s\n", candidate)
    end
  end
end


function neighbor(puzzle::Puzzle, c::Cell,
                  direction::Direction)::Union{Cell, Nothing}
  local neighbor_row = c.row
  local neighbor_column = c.column
  if direction == UP
    neighbor_row -= 1
  elseif direction == DOWN
    neighbor_row += 1
  elseif direction == LEFT
    neighbor_column -= 1
  elseif direction == RIGHT
    neighbor_column += 1
  else
    error("unknown direction")
  end
  return cell(puzzle, neighbor_row, neighbor_column)
end


begin
  @test equivalent(Tile(I, II, III, IIII),
                   Tile(I, II, III, IIII)) == true
  @test equivalent(Tile(I, II, III, IIII),
                   Tile(I, III, II, IIII)) == false
  @test equivalent(Tile(I, II, III, IIII, rotates=false),
                   Tile(I, II, III, IIII)) == false
  @test equivalent(Tile(IIII, I, II, III),
                   Tile(I, II, III, IIII)) == true
end

begin
  local tiles = [
    Tile(I, I, I, I),                   # fully symetric, 1 rotation.
    Tile(O, II, III, O, row=1, col=1, rotates=false),
    Tile(III, II, I, IIII, row=3),
    Tile(IIII, II, IIII, II, col=2),    # 180 symetry, 2 rotations.
    Tile(IIII, III, II, I)              # 4 rotations
  ]
  local puzzle = Puzzle(4, 4, tiles)
  @test neighbor(puzzle, cell(puzzle, 1, 1), UP) == nothing
  @test neighbor(puzzle, cell(puzzle, 1, 1), LEFT) == nothing
  @test neighbor(puzzle, cell(puzzle, 4, 4), DOWN) == nothing
  @test neighbor(puzzle, cell(puzzle, 4, 4), RIGHT) == nothing
  @test neighbor(puzzle, cell(puzzle, 2, 2), UP) === cell(puzzle, 1, 2)
  @test neighbor(puzzle, cell(puzzle, 2, 2), DOWN) === cell(puzzle, 3, 2)
  @test neighbor(puzzle, cell(puzzle, 2, 2), LEFT) === cell(puzzle, 2, 1)
  @test neighbor(puzzle, cell(puzzle, 2, 2), RIGHT) === cell(puzzle, 2, 3)
  @test length(puzzle.grid[1, 3].candidates) == 5
  @test length(puzzle.grid[1, 1].candidates) == 6
  @test length(puzzle.grid[3, 2].candidates) == 11
end


# A ssample puzzle
ADVANCED_112 = Puzzle(5, 5, [
  Tile(I, II, O, O),
  Tile(O, IIII, II, O, row=1),
  Tile(IIII, O, O, III, rotates=false),
  Tile(II, O, IIII, III, col=5),
  Tile(II, O, I, II, col=2),
  Tile(II, I, III, O, row=3, col=3),
  Tile(O, II, III, I, rotates=false),
  Tile(O, I, O, O, col=1, rotates=false),
  Tile(IIII, O, II, II, rotates=false),
  Tile(III, III, O, II, row=4),
  Tile(III, O, O, O, col=4),
  Tile(III, IIII, III, II),
  Tile(II, O, II, I),
  Tile(O, II, II, II, rotates=false),
  Tile(III, III, I, I, col=3),
  Tile(III, II, II, IIII)
  ])


"""Edges provides an iterator over the edges of a Puzzle.
An "edge" is a pair of adjacent cells.
Each iteration produces a tuple (c1ll1, direction1, direction2, cell2)
such that
  neighbor(puzzle, cell1, direction1) == cell2
and
  neighbor(puzzle, cell2, direction2) == cell2
"""
struct Edges
  puzzle::Puzzle
end

rows(e::Edges) = rows(e.puzzle)
columns(e::Edges) = columns(e.puzzle)
cell(e::Edges, row::Int, col::Int) = cell(e.puzzle, row, col)

function Base.iterate(e::Edges)
  # We use half indices to indicate the "edge" between two rows or columns.
  # The state we return refers to that last pair of cells returned.
  return ((nothing, DOWN, UP, cell(e, 1, 1)), (e, 0.5, 1))
end

function Base.iterate(e::Edges, state)
  (e, row, col) = state
  if isa(row, AbstractFloat)
    # We're on a horizontal edge, with cells that are UP/DOWN with
    # respect to each other.
    # Advance to the next column.
    col += 1
    if col > columns(e)
      col = 1
      row += 1
    end
    if row < rows(e) + 1
      local row1 = floor(Int, row)
      local row2 = ceil(Int, row)
      return ((cell(e, row1, col), DOWN, UP, cell(e, row2,col)),
              (e, row, col))
    else
      # We're done with all horizontal edges.  Return the first pair
      # in the first column
      return ((nothing, RIGHT, LEFT, cell(e, 1, 1)), (e, 1, 0.5))
    end
  end
  row::Int
  col::AbstractFloat
  row += 1
  if row > rows(e)
    col += 1
    row = 1
    if col > columns(e) + 1
      return nothing
    end
  end
  col1 = floor(Int, col)
  col2 = ceil(Int, col)
  return ((cell(e, row, col1), RIGHT, LEFT, cell(e, row, col2)),
          (e, row, col))
end


begin
  local puzzle = Puzzle(4, 4, Vector{Tile}())
  local count = 0
  for i in Edges(puzzle)
     count += 1
    local cell1, d1, d2, cell2 = i
    # show(i)
    # print("\n")
    if cell1 != nothing
      @test neighbor(puzzle, cell1, d1) == cell2
    end
    if cell2 != nothing
      @test neighbor(puzzle, cell2, d2) == cell1
    end
  end
  # Make sure we hit the right number of edge pairs.
  @test count == (rows(puzzle) + 1) * columns(puzzle) + rows(puzzle) * (columns(puzzle) + 1)
end


LinkCountSet(c::Candidate, direction::Direction) =
  LinkCountSet(link_count(c, direction))

# This is a convenience for dealing with the bounds of the grid.
# The bounding edges of the cell grid have LinkCount O, as distinct
# from the empty LinkCountSet.
LinkCountSet(::Nothing, ::Direction) = LinkCountSet(O)

"""The LinkCountSet of a Cell is the union of the LinklCountSets of its
current candidates."""
function LinkCountSet(c::Cell, direction::Direction)::LinkCountSet
  # An empty Cell presents LinkCount O in alldirections.
  if length(c.candidates) == 0
    return LinkCountSet(O)
  end
  local result = LinkCountSet()
  for candidate in c.candidates
    result |= LinkCountSet(candidate, direction)
  end
  return result
end

begin
  # Test LinkCountSet on Candidate.
  local puzzle = Puzzle(1, 1, [
    Tile(I, II, III, IIII, rotates=false)
  ])
  local c1 = cell(puzzle, 1, 1)
  @test LinkCountSet(c1.candidates[1], UP) == LinkCountSet(I)
  @test LinkCountSet(c1.candidates[1], RIGHT) == LinkCountSet(II)
  @test LinkCountSet(c1.candidates[1], DOWN) == LinkCountSet(III)
  @test LinkCountSet(c1.candidates[1], LEFT) == LinkCountSet(IIII)
  @test LinkCountSet(c1, UP) == LinkCountSet(I)
  @test LinkCountSet(c1, RIGHT) == LinkCountSet(II)
  @test LinkCountSet(c1, DOWN) == LinkCountSet(III)
  @test LinkCountSet(c1, LEFT) == LinkCountSet(IIII)
end

begin
  # Thest LinkCountSet on Cell.
  local puzzle = Puzzle(1, 1, [
    Tile(I, II, II, I),
    Tile(I, I, I, O, rotates=false)
  ])
  local c1 = cell(puzzle, 1, 1)
  @test LinkCountSet(c1, UP) == (I | II)
  @test LinkCountSet(c1, RIGHT) == (I | II)
  @test LinkCountSet(c1, DOWN) == (I | II)
  @test LinkCountSet(c1, LEFT) == (O | I | II)
end

begin
  local tiles = [
    Tile(I, I, I, I),
    Tile(II, II, II, II),
    Tile(III, IIII, III, IIII, rotates=false, col=3)
  ]
  local puzzle = Puzzle(4, 4, tiles)
  @test LinkCountSet(cell(puzzle, 1, 1), RIGHT) == I | II
  @test LinkCountSet(cell(puzzle, 1, 3), RIGHT) == I | II | IIII
  @test LinkCountSet(cell(puzzle, 1, 3), DOWN) == I | II | III
end


struct Effect
  cell::Cell
  removed::Vector{Candidate}
end

count_candidates(effect::Effect) = length(effect.removed)

count_candidates(effects::Vector{Effect}) = reduce(+, map(count_candidates, effects))

do_it(effects::Vector{Effect}) = map(do_it, effects)

function do_it(effect::Effect)
  setdiff!(effect.cell.candidates, effect.removed)
end

struct LogEntry
  rule::Function
  # Total number of candidates across all Cells before and after rule is
  # applied:
  before::Int
  after::Int
  effects::Vector{Effect}
end

count_candidates(le::LogEntry) = count_candidates(le.effects)

Log = Vector{LogEntry}


# function the_only_candidate(puzzle::Puzzle)::Vector{Effect}
#   # If a tile is the only candidate for some Cell then it can't be
#   # anywhere else.
#   # AM I CONVINCED THIS IS AN APPROPRIATE RULE?  A Cell could be empty.
#   local effects = Vector{Effect}()
#   for c1 in puzzle.grid
#     if length(c1.candidates) != 1
#       for c2 in puzzle.grid
#         if c2 === c1 continue end
#         push!(effects, Effect(c2, [c1.candidates[1]]))
#       end
#   end
#   return effects
# end


function the_only_place(puzzle::Puzzle)::Vector{Effect}
  # If a tile can only be in one Cell then no other Tile can be in that Cell.
  local effects = Vector{Effect}()
  for tile in puzzle.tiles
    local in_cell = nothing
    for c in puzzle.grid
      if has_candidate(c, tile)
        if in_cell == nothing
          in_cell = c
        else
          # tileis present in more than one cell.  Give up on it.
          in_cell = nothing
          break
        end
      end
    end
    if in_cell != nothing
      # Thou shalt have no other Tiles before me.
      local remove = filter(candidate -> candidate.tile != tile,
                            in_cell.candidates)
      if length(remove) > 0
        push!(effects, Effect(in_cell, remove))
      end
    end
  end
  return effects
end

begin
  # Test the_only_place.
  local puzzle = Puzzle(2, 2, [
    Tile(O, I, II, O, row=1, col=1),
    Tile(IIII, IIII, IIII, IIII, rotates=false)
  ])
  @test count_candidates(cell(puzzle, 1, 1)) == 5
  @test count_candidates(cell(puzzle, 1, 2)) == 1
  @test count_candidates(puzzle) == 8
  do_it(the_only_place(puzzle))
  @test count_candidates(cell(puzzle, 1, 1)) == 4
  @test count_candidates(cell(puzzle, 1, 2)) == 1
  @test count_candidates(puzzle) == 7
end


function common_edge_constrains_link_counts(puzzle::Puzzle)::Vector{Effect}
  local effects = Vector{Effect}()
  for e in Edges(puzzle)
    local cell1, d1, d2, cell2 = e
    local common = LinkCountSet(cell1, d1) & LinkCountSet(cell2, d2)
    local remove1 = Vector{Candidate}()
    local remove2 = Vector{Candidate}()
    function removal(cell, direction, rm)
      if cell == nothing return end
      for candidate in cell.candidates
        # Empty intersection?
        if isempty(common & LinkCountSet(candidate, direction))
          # @printf("Remove %s %s %s %s %s\n", cell, direction, common, LinkCountSet(candidate, direction), candidate)
          push!(rm, candidate)
        end
      end
    end
    removal(cell1, d1, remove1)
    removal(cell2, d2, remove2)
    if cell1 != nothing && length(remove1) > 0
      push!(effects, Effect(cell1, remove1))
    end
    if cell2 != nothing && length(remove2) > 0
      push!(effects, Effect(cell2, remove2))
    end
  end
  return effects
end

begin
  local puzzle = Puzzle(2, 2, [
    Tile(II, O, O, O, row=1, col=1),
    Tile(O, O, O, II, row=1, col=2, rotates=false)
  ])
  @test count_candidates(puzzle) == 5
  do_it(common_edge_constrains_link_counts(puzzle))
  @test count_candidates(puzzle) == 2
end

begin
  local puzzle = Puzzle(2, 2, [
    Tile(I, O, O, O,row=1,col=1),
    Tile(I, O, O, O, rotates=false)
  ])
  @test count_candidates(puzzle) == 8
  do_it(the_only_place(puzzle))
  @test count_candidates(puzzle) == 7
  do_it(common_edge_constrains_link_counts(puzzle))
  @test count_candidates(puzzle) == 2
end


global constraints = [
  common_edge_constrains_link_counts,
  the_only_place
]

function do_constraints(puzzle::Puzzle, log=Log)::Log
  local count = count_candidates(puzzle)
  while true
    for constraint in constraints
      effects = constraint(puzzle)
      local before = count_candidates(puzzle)
      do_it(effects)
      local after = count_candidates(puzzle)
      local log_entry = LogEntry(constraint, before, after, effects)
      push!(log, log_entry)
      @printf("  %s removes %d candidates\n", constraint, count_candidates(log_entry))
    end
    local after_count = count_candidates(puzzle)
    if count == after_count break end
    count = after_count
  end
  return log
end


begin
  local puzzle = Puzzle(3, 3, [
    Tile(O, O, II, I, row=2, col=1),   # 1 Cell, 4 rotations
    Tile(I, O, O, O, rotates=false),   # 9 Cells, 1 rotation
    Tile(II, O, O, O)                  # 9 Cells, 4 rotations
  ])
  @test count_candidates(puzzle) == 4 + 9 + 4 * 9
  local log = Log()
  do_constraints(puzzle, log)
  show_candidates(puzzle)
  if solved(puzzle)
    print("SOLVED!\n")
  end
end


# # I've not found a way to filter the methods by return type.
# function constraints()
#   local found = Vector{Any}()
#   for m in methodswith(Puzzle)
#     if length(m.sig) == 2
#       push!(found, m)
#     end
#   end
#   return found
# end


begin
  print("\n\n\nADVANCED_112\n")
  puzzle = ADVANCED_112
  local log = Log()
  do_constraints(puzzle, log)
  show_candidates(puzzle)
  if solved(puzzle)
    print("SOLVED!\n")
  end
end

