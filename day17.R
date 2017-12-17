# Day 17

stepforward <- function(pos, steps, len){
  # if pos > len return an error!
  #Normalise
  steps <- steps %% len
  pos <- (pos + steps)
  if (pos > len) pos <- pos - len
  return(pos)
}

spinlock <- function (lent, valc, stepsize, queryval = 0){
  # lent is the total length of the spinlock after it has inserted all of the values
  # valc is the current value
  # stepsize does what it says on the tin
  # queryval - function returns the value in the position after queryval
  
  # NB: if queryval is 0, the function keeps track of the value in position 1
  # and doesn't actually create the buffer (as 0 never moves)
  
  # initialise
  lenc <- 1 # Current length
  posc <- 1 # Current position
  
  if (queryval != 0){
    buff <- c(rep(NA,lent)) # Vector to hold values in
    buff[1] <- 0
  }

  for (i in 1:(lent-1)){
    # Step forward my number of steps
    posn <- stepforward (posc, stepsize, lenc)
    if (posn == 1) pos1 <- valc
    
    if (queryval != 0){
    # Insert the current value after posn
      buff[(posn+2):lent] <- buff[(posn+1):(lent-1)]
      buff[posn+1] <- valc
    }
    
    # Increment stuff
    valc <- valc + 1
    lenc <- lenc + 1
    posc <- posn + 1 #(as the spinlock is one element longer than before)
  }
  
  if (queryval != 0){
    mylast <- which (buff == (queryval))
    ans <- buff[mylast + 1]
    return(ans)
  } else{
    return(pos1)
  }
  
}


# Part 1 solution
valc <- 1 # Current value
lent <- 2018 # Total length of buffer, eventually
stepsize <- 363 # My stepsize
ans <- spinlock(lent, valc, stepsize, queryval = lent-1)
print (ans)

# Part 2
# The value after 0 will be the 2nd element, as the 0 can never be moved
valc <- 1 # Current value
lent <- 50000000 # Total length of buffer, eventually
stepsize <- 363 # My stepsize
ans2 <- spinlock(lent, valc, stepsize, 0)
print (ans2)