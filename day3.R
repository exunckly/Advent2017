# Day 3 part 1
# I suspect that I will not get away with this for part 2, but it seems quicker than working out how to write in a spiral

# The last number in each ring is the square of an odd number
# Work out which ring the number is in, centre number is ring 1, ring ending in 9 is ring 2 etc.
fn_whichring <- function(x){
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

# The minimum distance from the centre is fn_whichring (i.e. the middle of a side)
# The maximum distance from the centre is fn_whichring + (sidelength - 1)/2 (i.e. a corner)

# Work out how far along the side our number is

fn_nosteps <- function(x, ring){
  # work out how far x is from the bottom right corner, spiral-wise
  bottom_right = (2*ring - 1)^2  # I appreciate that this 'undoes' what I did in the other fn :-)
  around_spiral_dist = bottom_right - x # We already know that x <= bottom_right and that it's in the correct ring
  
  # Work out the distance that the number is away from a corner
  # The side length is 2*(ring-1)
  corner_dist <- around_spiral_dist %% (2*(ring-1))
  if (corner_dist > (ring - 1)){
    corner_dist <- 2*(ring-1) - corner_dist # i.e. mirror it if it's actually closer to the other corner
  }

  # corners are (ring-1) * 2 steps away from the centre of the matrix
  centre_dist <- (ring-1)*2 - corner_dist
}
  
 
x = 312051
myring <- fn_whichring(x)
banana <- fn_nosteps(x, myring)
  
  
  
