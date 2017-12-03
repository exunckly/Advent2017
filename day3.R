library(dplyr) # for case statement

# Day 3 part 1
# I reused more of this than expected in part 2...

fn_whichring <- function(x){
  # Work out which ring the number is in, centre number is ring 1, ring ending in 9 is ring 2 etc.
  # The last number in each ring is the square of an odd number
  
  # Take square root and round up to nearest odd number
    y <- x^0.5
  z <- ceiling(y)
  if (z %% 2 == 0){
    z = z + 1
  }
  # work out where this is in the series of odd numbers
  myans <- (z + 1)/2
  myans
  }

fn_cornerdist <- function(x, ring){
  # In any one ring:
  # the minimum distance from the centre is fn_whichring - 1 steps, i.e. the middle of a side
  # the maximum distance from the centre is a corner
  
  # work out how far x is from the bottom right corner, spiral-wise
  bottom_right = (2*ring - 1)^2
  around_spiral_dist = bottom_right - x # We already know that x <= bottom_right and that it's in the correct ring
  
  # Work out the distance that the number is away from any corner
  # The side length is 2*(ring-1)
  corner_dist <- around_spiral_dist %% (2*(ring-1))
  if (corner_dist > (ring - 1)){
    corner_dist <- 2*(ring-1) - corner_dist # i.e. mirror it if it's actually closer to the other corner
  }
  corner_dist
}

fn_nosteps <- function(x, ring, corner_dist){
  # corners are 2*(ring-1) steps away from the centre of the matrix
  centre_dist <- 2*(ring-1) - corner_dist
}
  
 
x = 312051
myring <- fn_whichring(x)
mycornerdist <- fn_cornerdist(x, myring)
part1_sol <- fn_nosteps(x, myring, mycornerdist)
part1_sol
  
  
# Day 3 part 2

fn_spiralize <- function(spirallength, x = 0, y = 0, direction = "right"){
  # construct a spiral
  # x and y are the starting co-ordinates
  # initial direction out of the centre is left, right, up or down
  
  myloc <- matrix (nrow = 2, ncol = spirallength) # Matrix of locations: x in row 1, y in row 2
  mydir <- matrix (nrow = 2, ncol = spirallength) # Matrix of direction vectors, what direction to move NEXT
  
  # Definitions
  up <- matrix (c(0,1), nrow = 2, ncol = 1)
  down <- matrix (c(0,-1), nrow = 2, ncol = 1)
  left <- matrix (c(-1,0), nrow = 2, ncol = 1)
  right <- matrix (c(1,0), nrow = 2, ncol = 1)
  rotate90 <- matrix (c(0, 1, -1, 0), nrow = 2, ncol = 2, byrow = TRUE) # anticlockwise rotations
  
  if (direction == "right"){
      mydir[,1] = right
  }else if (direction == "left"){
      mydir[,1] = left
  }else if (direction == "up"){
      mydir[,1] = up
  }else if (direction == "down"){
      mydir[,1] = down
  } # Should trap error here if direction unrecognised
  
  # start at x,y
  myloc[1,1] = x
  myloc[2,1] = y

  # Now spiralize...
  for (i in 2:length(myloc[1,])){
    ring <- fn_whichring(i)
    corner_dist <- fn_cornerdist(i, ring)
    
    # Go in the direction already indicated at the previous iteration
    myloc[,i] <- myloc[,i-1] + mydir[,i-1]
    
    # Set direction for next time. We change direction if we are in either of these states:
    # 1. In a corner that's not the bottom-right corner
    # 2. When we have just entered a new ring
    if (corner_dist == 0 && i != (2*ring - 1)^2){
      mydir[,i] <- mydir[,i-1] %*% rotate90 
    }else if (i == (2*(ring-1) - 1)^2 + 1){
      mydir[,i] <- mydir[,i-1] %*% rotate90 
    } else{
      mydir[,i] <- mydir[,i-1] #Continue in the same direction
    }
  }
   myloc # Return this matrix
}

#Arbitrary
myspirallength = 100

# Set up the scenario:
#Values matrix to fill in
myval <- matrix (rep(0,myspirallength), nrow = 1, ncol = myspirallength) # Matrix of the value at each location
myval[1,1] <- 1 # Need to give it the first value

#Generate locations matrix
myloc <- fn_spiralize(myspirallength, 0, 0, "right")

# Calculate values

# Each location has 8 adjacent locations, which may or may not already be filled in
# These will be locations 2 to 9 returned by a separate call of spiralize

for (i in 2:length(myval)){
  # Get the adjacent cells
  adjacent_cells <- fn_spiralize(9, myloc[1,i],myloc[2,i],"right")
  
  # Add the adjacent cell values
  for (j in 2:length(adjacent_cells[1,])){
  
  # Do a lookup of the indices - know that they will appear only once in the myloc vector
  # and add the value of the adjacent cell (only if it's actually there...)
   a <- intersect (which(myloc[1,] == adjacent_cells[1,j]), which(myloc[2,] == adjacent_cells[2,j]))
   if (length(a)>0){ # Took ages to work out how to test for this...
     myval[1,i] <- myval[1,i] + myval[1,a]
   }
  }
}

part2_sol <- myval[min(which(myval > x))]
part2_sol