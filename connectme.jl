# A constraint based solver for the "Connect Me - Logic Puzzle"
# Android app developed by Viktor Bohush.


using DataStructures
using InteractiveUtils
using Printf
using StaticArrays
using Test


function test()
  test_neighbor_and_puzzle_construction()
  test_edges_iteration()
  test_LinkCountSet()
  test_TheOnlyPlace()
  test_cell_LinkCountSet()
  test_solve_1()
  test_solve_2()
  test_CommonEdgeConstrainsLinkCounts()
  test_solve_advanced_112()
  test_solve_advances_125()
end


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
  # If optional is false then the Tile must be present in the solved Puzzle.
  optional::Bool
  # The number of links that the tile presents in a given direction.
  link_counts::Vector{LinkCount}     # indexed by Direction, length 4.

  function Tile(link_counts::Vararg{LinkCount, 4}; row=nothing, col=nothing,
                rotates=true, optional=false)
    return new(row, col, rotates, optional, 
               [lc for lc in link_counts])
  end
end

"""Return a Tile that models an empty Cell."""
Tile(row::Int, col::Int) = Tile(O, O, O, O, rotates=false, row=row, col=col,
                                optional=true)

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
    @printf(io, ", rotates=%s", tile.rotates)
  end
  if tile.restricted_to_row != nothing
    @printf(io, ", row=%d", tile.restricted_to_row)
  end
  if tile.restricted_to_column != nothing
    @printf(io, ", col=%d", tile.restricted_to_column)
  end
  if tile.optional
    @printf(io, ", optional=%s", tile.optional)
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

"""Ruturn the set of distinct rotations of a tile, taking symetry into account."""
function rotations(tile::Tile)
  if tile.rotates
    if link_count(tile, UP, 0) == link_count(tile, DOWN, 0) &&
       link_count(tile, LEFT, 0) == link_count(tile, RIGHT, 0)
       # 180 degree symetry
       if link_count(tile, UP, 0) == link_count(tile, RIGHT, 0)
         # full rotational symetry
         return [0]
       else
         return [0, 2]
       end
    else
      return [0, 1, 2, 3]
    end
  else
    return [0]
  end
end

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
  for rotation in rotations(tile)
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

"""Return true iff the two candidates present the same link counts."""
function equivalent(c1::Candidate, c2::Candidate)
  for d in instances(Direction)
    if LinkCountSet(c1, d) != LinkCountSet(c2, d)
      return false
    end
  end
  return true
end


"""A Connect Me puzzle sonsists of a square grid.  There are also
tiles which can be placed in the grid."""
struct Puzzle
  tiles::Vector{Tile}
  grid::Array{Cell, 2}
  log                    # ::Log
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
      # A Cell can be empty -- having no Tile.
      # For simplicity we add a Candidate which uses a tile
      # that models an empty cell.
      add_candidates(cell, Tile(row, column))
      # Add candidates for the specified Tiles:
      for tile in tiles
        add_candidates(cell, tile)
      end
    end
    
  end
  return Puzzle(tiles, grid, Log())
end

function estimate_initial_candidates(puzzle::Puzzle)::Int
  local count = rows(puzzle) * columns(puzzle)   # empty cell surrogate tiles.
  for tile in puzzle.tiles
    rot = length(rotations(tile))
    r = tile.restricted_to_row == nothing ?
        rows(puzzle) : 1
    c = tile.restricted_to_column == nothing ?
        columns(puzzle) : 1
    count += rot * r * c
  end
  return count
end

function solved(puzzle::Puzzle)::Bool
  if !all(c -> count_candidates(c) == 1,
          puzzle.grid)
    return false
  end
  # Each tile is in exactly ione Cell.
  for tile in puzzle.tiles
    if length(cells_with_candidate(puzzle, tile)) != 1
      return false
    end
  end
  # Edges are correct
  for e in Edges(puzzle)
    (cell1, d1, d2, cell2) = e
    if LinkCountSet(cell1, d1) != LinkCountSet(cell2, d2)
      return false
    end
  end
  return true
end

function cells_with_candidate(puzzle::Puzzle, tile::Tile)
  return filter(cell -> has_candidate(cell, tile),
                puzzle.grid)
end

"""Find Cells of Puzzle that have a candidate that presents the
same link counts as the specified Candidate."""
function cells_with_candidate(puzzle::Puzzle, candidate::Candidate)
  return filter(c -> any(can -> equivalent(can, candidate),
                         c.candidates),
                puzzle.grid)
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

function test_neighbor_and_puzzle_construction()
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
  @test length(puzzle.grid[1, 3].candidates) == 6
  @test length(puzzle.grid[1, 1].candidates) == 7
  @test length(puzzle.grid[3, 2].candidates) == 12
end


# A ssample puzzle from the game
function advanced_112()
  return Puzzle(5, 5, [
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
end


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

# Convenience trampolene methods:
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


function test_edges_iteration()
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

"""The LinkCountSet of a Cell is the union of the LinkCountSets of its
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

function test_LinkCountSet()
  # Test LinkCountSet on Candidate.
  local puzzle = Puzzle(1, 1, [
    Tile(I, II, III, IIII, rotates=false)
  ])
  local c1 = cell(puzzle, 1, 1)
  @test LinkCountSet(c1.candidates[2], UP) == LinkCountSet(I)
  @test LinkCountSet(c1.candidates[2], RIGHT) == LinkCountSet(II)
  @test LinkCountSet(c1.candidates[2], DOWN) == LinkCountSet(III)
  @test LinkCountSet(c1.candidates[2], LEFT) == LinkCountSet(IIII)
  @test LinkCountSet(c1, UP) == (O | I)
  @test LinkCountSet(c1, RIGHT) == (O | II)
  @test LinkCountSet(c1, DOWN) == (O | III)
  @test LinkCountSet(c1, LEFT) == (O | IIII)
end

function test_cell_LinkCountSet()
  # Test LinkCountSet on Cell.
  local puzzle1 = Puzzle(1, 1, [
    Tile(I, II, II, I),
    Tile(I, I, I, O, rotates=false)
  ])
  local c1 = cell(puzzle1, 1, 1)
  @test LinkCountSet(c1, UP) == O | I | II
  @test LinkCountSet(c1, RIGHT) == O | I | II
  @test LinkCountSet(c1, DOWN) == O | I | II
  @test LinkCountSet(c1, LEFT) == O | I | II
  local puzzle2 = Puzzle(4, 4, [
    Tile(I, I, I, I),
    Tile(II, II, II, II),
    Tile(III, IIII, III, IIII, rotates=false, col=3)
  ])
  @test LinkCountSet(cell(puzzle2, 1, 1), RIGHT) == O | I | II
  @test LinkCountSet(cell(puzzle2, 1, 3), RIGHT) == O | I | II | IIII
  @test LinkCountSet(cell(puzzle2, 1, 3), DOWN) == O | I | II | III
end


"""To facilitate finding our constraint rule functions, we define
them as subtypes of Constraint."""
abstract type Constraint; end


struct Effect
  cell::Cell
  removed::Vector{Candidate}
end

count_candidates(effect::Effect) = length(effect.removed)

count_candidates(effects::Vector{Effect}) = reduce(+, map(count_candidates, effects))

do_it(effects::Vector{Effect}) = map(do_it, effects)

function do_it(effect::Effect)
  setdiff!(effect.cell.candidates, effect.removed)
  if length(effect.cell.candidates) < 1
    error("No candidates remaining after", effect)
  end
end

struct LogEntry
  rule::Constraint
  # Total number of candidates across all Cells before and after rule is
  # applied:
  before::Int
  after::Int
  effects::Vector{Effect}
end

function report(log_entry::LogEntry)
  @printf("%40s %4d %4d %4d\n",
          log_entry.rule,
          log_entry.before,
          count_candidates(log_entry.effects),
          log_entry.after)
end

count_candidates(le::LogEntry) = count_candidates(le.effects)

Log = Vector{LogEntry}

function report(log::Log)
  map(report, log)
  return
end


# function the_only_candidate(puzzle::Puzzle)::Vector{Effect}
#   # If a tile is the only candidate for some Cell then it can't be
#   # anywhere else.
#   # AM I CONVINCED THIS IS AN APPROPRIATE RULE?  A Cell could be empty.
#   local effects = Vector{Effect}()
#   for c1 in puzzle.grid
      # This isn't the right test.  We want to test that there's only
      # one Tile, not only one Candidate.
#     if length(c1.candidates) != 1
#       for c2 in puzzle.grid
#         if c2 === c1 continue end
#         push!(effects, Effect(c2, [c1.candidates[1]]))
#       end
#   end
#   return effects
# end


struct TheOnlyPlace <: Constraint; end
function (r::TheOnlyPlace)(puzzle::Puzzle)::Vector{Effect}
  # If a tile can only be in one Cell then no other Tile can be in that Cell.
  local effects = Vector{Effect}()
  for tile in puzzle.tiles
    if tile.optional
      continue
    end
    local in_cell = nothing
    for c in puzzle.grid
      if has_candidate(c, tile)
        if in_cell == nothing
          in_cell = c
        else
          # Tile is present in more than one cell.  Give up on it.
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

function test_TheOnlyPlace()
  local puzzle = Puzzle(2, 2, [
    Tile(O, I, II, O, row=1, col=1),
    Tile(IIII, IIII, IIII, IIII, rotates=false)
  ])
  @test count_candidates(cell(puzzle, 1, 1)) == 6
  @test count_candidates(cell(puzzle, 1, 2)) == 2
  @test count_candidates(puzzle) == 12
  do_it(TheOnlyPlace()(puzzle))
  @test count_candidates(cell(puzzle, 1, 1)) == 4
  @test count_candidates(cell(puzzle, 1, 2)) == 2
  @test count_candidates(puzzle) == 10
end


struct TheOnlyTile <: Constraint; end
function (r::TheOnlyTile)(puzzle::Puzzle)::Vector{Effect}
  # If a tile is the only candidate for a cell, then that tile can be
  # nowhere else.
  local effects = Vector{Effect}()
  for c in puzzle.grid
    @assert length(c.candidates) > 0
    local is_tile = can -> can.tile == c.candidates[1].tile
    if all(is_tile, c.candidates)
      # All candidates are for the same Tile.  That Tile can be nowhere else.
      for c1 in puzzle.grid
        if c1 == c continue end
        local remove = filter(is_tile, c1.candidates)
        if length(remove) > 0
          push!(effects, Effect(c1, remove))
        end
      end
    end
  end
  return effects
end


struct CommonEdgeConstrainsLinkCounts <: Constraint; end
function (r::CommonEdgeConstrainsLinkCounts)(puzzle::Puzzle)::Vector{Effect}
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

function test_CommonEdgeConstrainsLinkCounts()
  # Do we find the correct rotation of the first Tile to match the seconmd.
  local puzzle1 = Puzzle(2, 2, [
    Tile(II, O, O, O, row=1, col=1),
    Tile(O, O, O, II, row=1, col=2, rotates=false)
  ])
  @test count_candidates(puzzle1) == 9
  do_it(CommonEdgeConstrainsLinkCounts()(puzzle1))
  @test count_candidates(puzzle1) == 6
  #
  local puzzle2 = Puzzle(2, 2, [
    Tile(I, O, O, O,row=1,col=1),
    Tile(I, O, O, O, rotates=false)
  ])
  @test count_candidates(puzzle2) == 12
  do_it(TheOnlyPlace()(puzzle2))
  @test count_candidates(puzzle2) == 10
  # We need to run CommonEdgeConstrainsLinkCounts twice
  do_it(CommonEdgeConstrainsLinkCounts()(puzzle2))
  do_it(CommonEdgeConstrainsLinkCounts()(puzzle2))
  @test count_candidates(puzzle2) == 4
  @test solved(puzzle2)
end


function do_constraints(puzzle::Puzzle)
  local count = count_candidates(puzzle)
  try
    while true
      for C in subtypes(Constraint)
        local constraint = C()
        effects = constraint(puzzle)
        local before = count_candidates(puzzle)
        try  
          do_it(effects)
        finally
          local after = count_candidates(puzzle)
          local log_entry = LogEntry(constraint, before, after, effects)
          push!(puzzle.log, log_entry)
          # @printf("  %s removes %d candidates\n", constraint, count_candidates(log_entry))
        end
      end
      local after_count = count_candidates(puzzle)
      if count == after_count break end
      count = after_count
    end
  catch e
    show(e)
  end
  return solved(puzzle)
end


function test_solve_1()
  local puzzle = Puzzle(3, 3, [
    Tile(O, O, II, I, row=2, col=1),   # 1 Cell, 4 rotations
    Tile(I, O, O, O, rotates=false),   # 9 Cells, 1 rotation
    Tile(II, O, O, O)                  # 9 Cells, 4 rotations
  ])
  @test count_candidates(puzzle) == 4 + 9 + 4 * 9 + 9
  do_constraints(puzzle)
  if !solved(puzzle)
    print("\n\nNOT SOLVED: test_solve_1\n")
    show_candidates(puzzle)
    report(puzzle.log)
  end
  return puzzle
end

function test_solve_2()
  local puzzle = Puzzle(3, 3, [
    Tile(I, II, O, O, row=2, col=2),
    Tile(I, O, O, O, col = 3),
    Tile(II, O, O, O, row=3)
  ])
  @test estimate_initial_candidates(puzzle) == count_candidates(puzzle)
  do_constraints(puzzle)
  if !solved(puzzle)
    print("\n\nNOT SOLVED: test_solve_2\n")
    show_candidates(puzzle)
    report(puzzle.log)
  end
  return puzzle
end


function test_solve_advanced_112()
  local puzzle = advanced_112()
  @test estimate_initial_candidates(puzzle) == count_candidates(puzzle)
  do_constraints(puzzle)
  if !solved(puzzle)
    print("\n\ntest_solve_advanced_112\n")
    show_candidates(puzzle)
    report(puzzle.log)
  end
  @test solved(puzzle)
end


"""If all of the candidates for a Cell are equivalent in the
LinkCounts they present then pick the one that is (in some sense)
least general, lock it in and free the others.  This constraint
function retuirns after the first positive example it encounters so
that other rules can act on the outcome."""
struct PickFromEquivalentTiles <: Constraint; end
function (r::PickFromEquivalentTiles)(puzzle::Puzzle)::Vector{Effect}
  local effects = Vector{Effect}()
  for c in puzzle.grid
    if length(c.candidates) <= 1
      continue
    end
    if all(can -> equivalent(can, c.candidates[1]), c.candidates)
        local best_tile
        local best_score = rows(puzzle) * columns(puzzle) + 1
        for can in c.candidates
          local score = length(cells_with_candidate(puzzle, can.tile))
          if score < best_score
            best_tile = can.tile
            best_score = score
          end
        end
        best_tile::Tile
        local remove = Vector{Candidate}()
        for can in c.candidates
          if can.tile != best_tile
            push!(remove, can)
          end
        end
      push!(effects, Effect(c, remove))
      break
    end
  end
  return effects
end

# This puzzle has a pair of duplicate Tiles
function advanced_125()
  return Puzzle(5, 5, [
    Tile(II, III, IIII, I, rotates=false),
    Tile(III, III, O, II),
    Tile(O, O, I, II),
    Tile(O, IIII, O, O),
    Tile(O, II, III, III),
    Tile(III, IIII, O, O, rotates=false),
    Tile(I, I, I, I),
    Tile(III, O, O, III),
    Tile(II, O, O, O),
    Tile(II, I, O, III, row=3, col=4),
    Tile(III, II, III, O, rotates=false),
    Tile(IIII, I, IIII, II, row=4, rotates=false),
    Tile(III, I, II, O),
    Tile(O, III, IIII, III, rotates=false),
    Tile(IIII, I, I, III, rotates=false),
    Tile(IIII, III, O, IIII, row=5, col=2, rotates=false),
    Tile(IIII, I, O, II, rotates=false)
  ])
end

function test_solve_advances_125()
  local puzzle = advanced_125()
  do_constraints(puzzle)
  if !solved(puzzle)
    print("\n\ntest_solve_advances_125\n")
    show_candidates(puzzle)
    report(puzzle.log)
  end
  return puzzle
end

test()
