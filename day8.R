library(dplyr)

fn_updatereg <- function(x, i = 1, j = 1, default_inc = 0, limit = 0, op = `>=`, inciftrue = 0){
  # x is a 1 column vector of numbers 
  # Function updates location i based on testing location j and the limit with the operator
  # Learned on day 5 that vectors are zillions of times faster than data frames for things like this
  if (op(x[j],limit) == TRUE){
    x[i] <- x[i] + inciftrue
  }else{
    x[i] <- x[i] + default_inc
  }
  return(x)
}

mytibble <- read_delim("day8input.txt", delim = " ", col_names = c("reg", "dirn", "mag", "logic", "testreg", "op", "limit"))
mytibble$dirn2 <- ifelse(mytibble$dirn == "inc", 1, -1)
mytibble$inc <- mytibble$mag * mytibble$dirn2 # We add this value to do the correct increment
mytibble$op <- paste("`",mytibble$op,"`", sep="") # Need to escape operators with backticks, e.g op = `<`

# Dig a vector of unique registers out of columns reg and testreg combined
# Order doesn't matter
xnames <- unique(c(mytibble$reg, mytibble$testreg))
xvals <- c(rep(0, times = length(xnames)))

maxsofar <- max(xvals) # to track part 2

for (i in 1:length(mytibble$reg)){
 ipos <- which(xnames == mytibble$reg[i])
 jpos <- which(xnames == mytibble$testreg[i])
  xvals <- fn_updatereg(xvals, i = ipos, j = jpos, default_inc = 0, limit = mytibble$limit[i], inciftrue = mytibble$inc[i], op = eval(parse(text = mytibble$op[i])) )
  
  # we only need to test the value that changed to see if it is higher than the maximum so far
  if (xvals[ipos] > maxsofar){
    maxsofar <- xvals[ipos]
  }
}

ans <- max(xvals) 
print(ans) # Part 1 solution
print(maxsofar) # Part 2 solution
