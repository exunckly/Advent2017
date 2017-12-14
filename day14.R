# Day 14

# Revisit the knot hash functions I wrote for day 10, using reduce() this time

myshift <- function(x,m){
  # Function shifts a loop-ised vector by m places
  # It is easier to work in a frame of reference in which we are reversing the first n characters
  # and don't have to worry about how to loop things round
  
  # Deal with negative shifts
  lenx <- length(x)
  if (m < 0) m <- lenx + m
  
  # Deal with a shift greater then the length of x
  m <- m %% lenx
  
  # Deal with zero (every other case has already been dealt with)
  ifelse (m > 0, y <- c(x[(m+1):lenx],x[1:m]), y <- x)
  
  return (y)
}

singleknot <- function(x,n){
  # This function reverses the first n entities in vector x
  # n = 0, n = 1 and n > length(x) have no effect
  lenx <- length(x)
  if(n > 0 && n <= lenx){
    if (n < lenx){
      y <- c(rev(x[1:n]),x[(n+1):lenx]) # need to put (n+1) or it goes n + (1:lenx)
    }
    else{
      y <- rev(x[1:n]) # or else we get one element added on at the end
    }
  } else{
    y <- x
  }
  return (y)
}

singleround <- function (x, n, m){
  # x is the vector that we are manipulating
  # n is the length of the section that we want to reverse
  # m is the current position (starting to count at 0) that we want to reverse from
  
  # We shift m places to a convenient frame of reference, reverse the first n chars then shift -m back again
  x <- myshift(x,m)
  x <- singleknot(x,n)
  x <- myshift(x,-m)
  return (x)
}

multiround <- function(x, pinches, reps = 1){
  # x is the vector that we are manipulating
  # pinches is the vector containing the locations at which we pinch x to reverse parts of it
  # reps is the number of repetitions
  currentpos <- 0
  skip <- 0
  
  for (j in 1:reps){
    for (i in 1:length(pinches)){
      x <- singleround(x, pinches[i], currentpos)
      currentpos <- (currentpos + pinches[i] + skip) %% length(x)
      skip <- skip + 1
    }
  }
  return(x)
}

fn_densehash <- function(x, y){
  # x is the vector
  # y is the size of chunk you want to use (should be a power of 2, as should be length of x)
  densehash <- c(1:(length(x)/y)) # Edit: I will investiogate seq_along next time I need to do this
  for (i in 1:(length(x)/y)){
    from <- y*(i-1) + 1
    to <- y*(i-1) + y
    densehash[i] <- reduce(x[from:to], bitwXor)
  }
  return(densehash)
}


entireknothash <- function(x){
  
  myvec <- c(0:255)
  extrabits <- c(17, 31, 73, 47, 23)
  reps <- 64
  
  a <- strsplit(x, "")[[1]]
  b <- c(1:(length(a)))
  for(i in 1:length(a)){
    b[i] <- utf8ToInt(a[i])
  }
  
  d <- c(b,extrabits)
  
  # Do 64 rounds of magic
  sparsehash <- multiround(myvec, d, reps)
  
  # Numeric bitwise XOR on 16 bit chunks
  densehash <- fn_densehash(sparsehash, 16)
  
  # Convert to a hex string
  ans2 <- paste(as.hexmode(densehash), collapse="")
  
  return(ans2)
}


# Part 1
mykeystring <- read_file("day14input.txt")

mem <- matrix(nrow = 128, ncol = 128)

for (i in 1:128){
  hashinput <- paste(mykeystring, "-", i-1, sep="")
  mem[i,] <- hex2bin(entireknothash(hashinput))
}
print(sum(mem))

# Part 2 - find regions
labels <- matrix(nrow = 128, ncol = 128) # starts out being filled with NA

regionlabel <- 0
equivcount <- 1
edgelist <- matrix(nrow = 128*128, ncol = 2)

# Implement the first pass of the two-pass connected region algorithm to generate a list of equivalent regions:
# https://en.wikipedia.org/wiki/Connected-component_labeling

# Then put the list of equivalent regions into igraph as an edgelist and see how many clusters there are

for (i in 1:128){ 
  for (j in 1:128){
   # If the memory element is in use (==1), then test to see if the neighbouring elements to the left and up == 1
    # If they are in use, find out if they already have region labels
    if (mem[i,j] == 1){
        if (mem[i-1,j] == 1 && i > 1){ # Need to test for the edges
          leftlabel <- labels[i-1,j]
        }else{
          leftlabel <- NA
        }
        if (mem[i,j-1] == 1 && j > 1){
          uplabel <- labels[i,j-1]
        }else{
          uplabel <- NA
        }
      #If none of the neighbours already have a region label, give the current element a new region label
        if (is.na(leftlabel) && is.na(uplabel)){
          regionlabel <- regionlabel + 1
          labels[i,j] <- regionlabel
      # If at least one neighbour has a label, give the current element the lowest label of either neighbour
        }else if (is.na(leftlabel)){
          labels[i,j] <- uplabel
        }else if (is.na(uplabel)){
          labels[i,j] <- leftlabel
        } else{
          labels[i,j] <- min(leftlabel, uplabel)
        }
        
      #Add equivalent labels to the edgelist for igraph
      # Record occasions where leftlabel = uplabel as a region may have only one label and we want it to be included
        if (!is.na(leftlabel) && !is.na(uplabel)){
          edgelist[equivcount,] <- c(min(leftlabel, uplabel), max(leftlabel,uplabel))
          equivcount <- equivcount + 1
        }
        else{ # Also includes unconnected individual elements in the edgelist (forgot this bit the first time)
          edgelist[equivcount,] <- c(regionlabel, regionlabel)
        }
      
    }
  }
}

# Keep only unique rows
equivdf <- as.tibble(edgelist)
dedupe <- equivdf %>% unique

# As we initialised the edge list with NAs there will still be one row of them left
dedupe <- filter(dedupe, !is.na(dedupe$V2))

# Put the edge list into igraph and count the number of clusters (as per day 12 of AoC)
mygraph <- (graph_from_edgelist(as.matrix(dedupe), directed = FALSE))
mygroups <- components(mygraph)

part2 <- mygroups$no
print(part2)


