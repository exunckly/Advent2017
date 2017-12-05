library(dplyr)

# Day 5 part 1

fn_move <- function(x){
  # Input is a data frame with
  # $dirns = directions
  # $pos[1] = current position
  # $steps[1] = number of steps taken
  
  initial_pos <- x$pos[1]
  # Move as directed
  x$pos[1] <- x$pos[1] + x$dirns[x$pos]
  
  # Update vector of locations and number of steps
  x$dirns[initial_pos] <- x$dirns[initial_pos] + 1
  x$steps[1] <- x$steps[1] + 1
  
  # Return the updated df
  return(x)
}

locs <- data.frame(dirns = parse_double(readLines("day5input.txt")))
ubound <- length(locs$dirns)

locs$pos = 0
locs$steps = 0
locs$pos[1] <- 1

# while the current location is within the bounds of the tibble
while(locs$pos[1] > 0 && locs$pos[1] <= ubound){
  locs <- fn_move(locs)
}

locs$steps[1]