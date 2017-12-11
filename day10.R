#Day 10

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

# Edit: Reduce() is designed to do exactly this kind of thing
vectorxor <- function (x){
  # perform length(vector) - 1 comparisons in total
  currentxor <- bitwXor(x[1], x[2])
  
  for (i in 3:(length(x))){
    currentxor <- bitwXor(currentxor, x[i])
  }
  return(currentxor)
}

fn_densehash <- function(x, y){
  # x is the vector
  # y is the size of chunk you want to use (should be a power of 2, as should be length of x)
  densehash <- c(1:(length(x)/y))
  for (i in 1:(length(x)/y)){
    from <- y*(i-1) + 1
    to <- y*(i-1) + y
    densehash[i] <- vectorxor(x[from:to])
  }
  return(densehash)
}



# Part 1
# Mustn't get confused with the Advent of code array being numbered from 0 but R numbering from 1...
myinput_orig <- read_file("day10input.txt")
myinput <- as.numeric(strsplit(myinput_orig, ",")[[1]])
myvec <- c(0:255)

myvec <- multiround(myvec, myinput, 1)

ans <- myvec[1] * myvec[2]
print(ans)

# Part 2
# Reset
myinput2 <- read_file("day10input.txt")
myvec <- c(0:255)
extrabits <- c(17, 31, 73, 47, 23)

# Convert each character in the input into its ASCII value
# utf8ToInt isn't vectorized
# b <- apply(a, FUN = function(x) {apply(x, FUN = utf8ToInt)}) # Various permutations of this didn't work

a <- strsplit(myinput2, "")[[1]]
b <- c(1:(length(a)))
for(i in 1:length(a)){
  b[i] <- utf8ToInt(a[i])
}

# Add the extra items to the end of the sequence
d <- c(b,extrabits)

# Do 64 rounds of magic
sparsehash <- multiround(myvec, d, 64)

# Numeric bitwise XOR on 16 bit chunks
densehash <- fn_densehash(sparsehash, 16)

# Convert to a hex string
ans2 <- paste(as.hexmode(densehash), collapse="")
print(ans2)