# A solver for the "Connect Me logic puzzle" Android app.


using Test


"""A Connect Me puzzle sonsists of a square grid.  There are also
tiles which can be placed in the grid."""
struct Puzzle
  tiles  # ::Array{Tile}
  grid
end

# function Puzzle(width::Int, height::Int, tiles...)
#   for tile in tiles
#     @assert(isa(tile, Tile))
#   end
#   # Create Cells
#   local grid = Array{Cell}(undef, width, height)
#   for row = 1 : width
#     for colunm = 1 : height
#       local cell == Cell(row, column)
#       grid[row, column] = cell
#     end
#   end
#   # Create Edges
# end


mutable struct Cell
  row::Int
  column::Int 
  edges
  candidates
end


# Each edge of each Tile has an associated LinkCount.  To solve the
# puzzle, the LinkCounts of the common edge of two adjacent Tiles must
# match.
@enum LinkCount O=0 I=1 II=2 III=3 IIII=4


# Each Tile has four sides identified by their directions.
# Directions start at 1 because they are used as the index into
# the link_count field of Tile.
@enum Direction UP=1 RIGHT=2 DOWN=3 LEFT=4

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

  function Tile(link_counts...; row=nothing, col=nothing, rotates=true)
    @assert(length(link_counts) == 4)
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
