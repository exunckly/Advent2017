# Day 16
library(tidyverse)

spin <- function(vec, X){
  # Makes x items move from the end to the front of vec
  lenvec <- length(vec)
  if (X >= lenvec) X <- X %% lenvec # trap errors
  ifelse (X == 0, return(vec),return (c(vec[(lenvec-X+1):lenvec], vec[1:(lenvec-X)])))
}

exchange <- function(vec, A, B){
  #Items at positions A and B swap places
  #In the problem, the vector indicing starts at 0 not 1
  vec[c(A+1,B+1)] <- vec[c(B+1,A+1)]
  return(vec)
}

partner <- function(vec, A, B){
  # Items named A and B swap places, assumes that the names are unique
  # Need the -1 as the exchange function allows for the indices starting at 0 not 1
  a <- which(vec == A) - 1
  b <- which(vec == B) - 1
  vec <- exchange (vec,a,b)
  return(vec)
}

dance <- function(vec, op, locs){
  for (i in seq_along(op)){
    if(op[i] == "s") vec <- spin(vec, as.numeric(locs[[i]][1]))
    if(op[i] == "x") vec <- exchange(vec, as.numeric(locs[[i]][1]), as.numeric(locs[[i]][2]))
    if(op[i] == "p") vec <- partner(vec, locs[[i]][1], locs[[i]][2])
  }  
  return(vec)
}

# Parse input
a <- readLines("day16input.txt")
b <- unlist(strsplit(a,","))

op <- substr(b,1,1)
locs <- strsplit(substr(b,2,nchar(b)),"/")

# Part 1

noprogs <- 16
progs <- unlist(strsplit(paste(letters[1:noprogs]),""))
progs <- dance(progs, op, locs)
ans <- paste(progs,collapse="")
print(ans)

# Part 2
# Work out whether the dance repeats (like yesterday's generator)
progsorig <- unlist(strsplit(paste(letters[1:noprogs]),""))
progs <- progsorig

flag <- FALSE
myrepeat <- 0

# Dance complete cycles until what comes out is what we started with
while (flag == FALSE){
  progs <- dance(progs, op, locs)
  myrepeat <- myrepeat + 1
  if(identical(progs, progsorig)) flag <- TRUE
}  

# Find out the remainder when we divide 1 billion by myrepeat - actual number of dances we need to do
nodances <- 1000000000 %% myrepeat
print(nodances)

progs <- progsorig

for (j in 1:nodances){
  progs <- dance(progs, op, locs)
}

ans2 <- paste(progs,collapse="")
print(ans2)
