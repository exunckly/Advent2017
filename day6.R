# Day 6

# Hashtable implementation using env is from https://stackoverflow.com/questions/17278591/r-integer-key-value-map
# Modified to trap errors and return an index of -999 if there is no match
new.hashtable <- function() {
  e <- new.env()
  list(set = function(key, value) assign(as.character(key), value, e),
       get = function(key) tryCatch(get(as.character(key), e), error = function(f) {cat('In error handler\n'); print(f); -999}),
       rm = function(key) rm(as.character(key), e))
  # NB this will work only for storing character strings
}

# Calcxulate new distribution
redistribute <- function(x){
  # x is a vector of integers
  # We find the maximum value and redistribute it among all of the buckets, one at a time, in sequence
  mylen = length(x)
  mymax <- max(x)
  maxindex <- which.max(x) # Returns the lowest index if there, is a tie, which is what we want
  
  # Reduce max location to zero
  x[maxindex] <- 0
  
  # Distribute the integers along the vector
  # Every position gets this much (reduce the amount of looping needed)
  givetoall <- mymax %/% mylen
  x <- x + givetoall
  
  # Distribute the remainder, starting one cell along from the max vakue 
  remainder <- mymax %% mylen
  j <- maxindex
  if (remainder > 0){
    for (i in 1:remainder){
      if (j == mylen) {j=0}
      j = j + 1
      x[j] = x[j] + 1
    }
  }
  return(x)
}



# Read input
rawtext <- (readLines("day6input.txt"))
myvec <- as.numeric(strsplit(rawtext,"\t")[[1]])


# myvec <- c(0, 2, 7, 0)

index <- 1
ht <- new.hashtable()

# Store initial state
ht$set(paste(myvec, collapse=","),index)

# Fake repeat-until loop (as we want it to execute once)
iterations <- 0
success <- FALSE
while (!success){
  myvec <- redistribute(myvec)
  index <- index + 1

  # Check to see if new state is in hashtable
  check <- ht$get(paste(myvec, collapse=","))
  success <- check > 0
  
  # If not, then put new state in table
  ht$set(paste(myvec, collapse=","),index)
}
# Part 1
print(index-1) # Initial state does not couht as a rearrangement

# Part 2
print(index - check)
