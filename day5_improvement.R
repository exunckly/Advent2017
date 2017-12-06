library(dplyr)

# Day 5 part 1
# Optimisation edit: now that the while loop is inside the function, I just need a 1D vector

fn_steps <- function(x, pos, default_offset = 1, usecutoff = FALSE, cutoff = 0, op = `>=`, other_offset = 1){
  # fnmove(blah) will increment the offset by 1 each time
  # Define the other inputs if you want to use a different offset based on a single condition
  ## Edit: it is possible to pass a function to a function.
  
  ubound <- length(x)
  steps <- 0
  
  while(pos > 0 && pos <= ubound){
  
    initial_pos <- pos
    # Move as directed
    pos <- pos + x[pos]
    
    # Update vector of locations
    if (usecutoff == TRUE && op(x[initial_pos],cutoff) == TRUE){
      x[initial_pos] <- x[initial_pos] + other_offset
    }else{
      x[initial_pos] <- x[initial_pos] + default_offset
    }
    steps <- steps + 1
  }
  return(steps)
}


# Day 5 part 1

locs <- parse_integer(readLines("day5input.txt"))

steps <- 0
position <- 1

# while the current location is within the bounds of the vector

steps <- fn_steps(locs, pos = position)

print(paste("Part 1: ", steps, "steps"))

# Day 5 part 2
# Reset
steps <- 0
position <- 1

steps <- fn_steps(locs, pos = position, default_offset = 1, usecutoff = TRUE, cutoff = 3, op = `>=`, other_offset = -1)

print(paste("Part 2: ", steps, "steps"))
