# A solver for the "Connect Me logic puzzle" Android app.


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


# Each edge of each Tile has an associated LinkCount.  To solve the
# puzzle, the LinkCounts of the common edge of two adjacent Tiles must
# match.
@enum LinkCount O=0 I=1 II=2 III=3 IIII=4


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
  # TODO: don't add all four rotations of tile has rotational symetry.
  for rotation in (tile.rotates ? [0, 1, 2, 3] : [0])
    push!(cell.candidates, Candidate(tile, rotation))
  end
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


function cell(puzzle::Puzzle, row::Int, column::Int)::Union{Cell, Nothing}
  if row < 1 return nothing end
  if row > size(puzzle.grid, 1) return nothing end
  if column < 1 return nothing end
  if column > size(puzzle.grid, 2) return nothing end
  return puzzle.grid[row, column]
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
  local tiles = [
    Tile(I, I, I, II),
    Tile(O, II, III, O, row=1, col=1, rotates=false),
    Tile(III, II, I, IIII, row=3),
    Tile(IIII, II, IIII, II, col=2)
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
  @test length(puzzle.grid[1, 3].candidates) == 4
  @test length(puzzle.grid[1, 1].candidates) == 5
  @test length(puzzle.grid[3, 2].candidates) == 12
end

