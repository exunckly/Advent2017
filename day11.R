# Day 11

# aka settleRs of Catan
# Used the technique described here for representing a hexagonal grid and calculating distances for games
# http://keekerdc.com/2011/03/hexagon-grids-coordinate-systems-and-distance-calculations/
# Rotate the image on the page 60' anticlockwise to match it up with our problem

mapmove <- function(x) {
  # Takes a string and maps it to a vector that represents a single move in that direction
  if (x == "n") {myvec <- c(0, 1)
  }else if (x == "s") {myvec <- c(0, -1)
  }else if (x == "ne") {myvec <- c(1, 0)
  }else if (x == "sw") {myvec <- c(-1, 0)
  }else if (x == "se") {myvec <- c(1, -1)
  }else if (x == "nw") {myvec <- c(-1, 1)
  }else {myvec <- c(0, 0)
  print("Direction not recognised, move set to (0, 0)")
  }
  return (myvec)
}

hexdist <- function(a){
  # a is 2 element vector (x, y) as described here: http://keekerdc.com/2011/03/hexagon-grids-coordinate-systems-and-distance-calculations/
  # We are assuming that we always start at (0,0) as there is no need for anything more complicated for this puzzle
  # In the co-ordinate system, the z component of this tile can be generated using x + y + z = 0
  z <- 0 - a[1] - a[2]
  
  # The number of steps is the maximum magnitude of any of the three co-ordinates
  steps <- max(abs(c(a, z)))
  return(steps)
}


# Parse inpjut

myinput_orig <- read_file("day11input.txt")
maze <- strsplit(myinput_orig, ",")[[1]]

# Part 1
moves <- matrix(0, length(maze), 2)

# Work out how to use apply later, a loop will do for now
for (i in 1:length(maze)){
  moves[i,] <- mapmove(maze[i]) 
}

# Add up all the individual components in the x and y directions to find the tile we are on
# We started at (0,0)
allmoves <- colSums(moves)
ans <- hexdist(allmoves)
print(ans)

# Part 2
# This time we need to keep track of the position, work out the z co-ordinate of each position and calculate the distance
positions <- apply(moves, 2, cumsum)

for (i in 1:length(positions[,1])){
  distances[i] <- hexdist(positions[i,])
}

ans2 <- max(distances) # We already took the absolute value earlier on
print(ans2)