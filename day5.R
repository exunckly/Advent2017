library(dplyr)

# Day 5 part 1

fn_steps <- function(x, default_offset = 1, usecutoff = FALSE, cutoff = 0, op = `>=`, other_offset = 1){
  # Input is a data frame with
  # $dirns = directions
  # $pos[1] = current position
  # $steps[1] = number of steps taken
  # fnmove(blah) will increment the offset by 1 each time
  # Define the other inputs if you want to use a different offset based on a single condition
  ubound <- length(x$dirns)
  
  while(x$pos[1] > 0 && x$pos[1] <= ubound){
  
    initial_pos <- x$pos[1]
    # Move as directed
    x$pos[1] <- x$pos[1] + x$dirns[x$pos]
    
    # Update vector of locations
    if (usecutoff == TRUE && op(x$dirns[initial_pos],cutoff) == TRUE){
      x$dirns[initial_pos] <- x$dirns[initial_pos] + other_offset
    }else{
      x$dirns[initial_pos] <- x$dirns[initial_pos] + default_offset
    }
    
    x$steps[1] <- x$steps[1] + 1

  }
  # Return the updated df
  return(x)
}


# Day 5 part 1

locs <- data.frame(dirns = parse_integer(readLines("day5input.txt")))

locs$pos = 0
locs$steps = 0
locs$pos[1] <- 1
original_locs <- locs

# while the current location is within the bounds of the tibble

steps <- fn_steps(locs)

print(paste("Part 1: ", steps$steps[1], "steps"))

# Day 5 part 2
# Reset
locs <- original_locs

steps <- fn_steps(locs, default_offset = 1, usecutoff = TRUE, cutoff = 3, op = `>=`, other_offset = -1)

print(paste("Part 2: ", steps$steps[1], "steps"))
