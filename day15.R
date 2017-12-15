#day15.R

library(R.utils)
library(stringr)
library(binaryLogic)

# Only the third of these functions ended up being used, the others were too slow.

testpairs <- function(x, y, bits = 16){
  # Bitwise XOR the two numbers together, convert to binary and test for the last 16 (or all) digits being 0
  mytest <- intToBin(bitwXor(x,y))
  
  # intToBin returns a string, which may be shorter than 16 characters
  # Pad if necessary (has no effect if it's already 16 chars wide) then take the last 16 chars
  mytest <- str_pad(mytest, 16, side = "left", pad = "0")
  mytest <- str_sub(mytest,-16,-1)

  ifelse(mytest == "0000000000000000", return(1), return(0))
  # Unsurprisingly, faffing with strings took far too long
}

testpairs2 <- function(x, y, bits = 16){
  # Bitwise XOR the two numbers together, convert to binary and test for the last 16 (or all) digits being 0
  mytest <- bit(length = 6)
  mytest <- as.binary(bitwXor(x,y), littleEndian = TRUE) # little endian to put the zeros at the front
  ifelse(sum(mytest[1:bits]) == 0, return(1), return(0))
  # Surprisingly, this was even slower than the first version
}

testpairs3 <- function(x, y){ # THIS ONE IS THE WINNER!!!
  # Bitwise XOR the two numbers together, as before
  # But no need to convert to binary and faff: if the last 16 bits are zero it divides by 65536!
  ifelse(bitwXor(x,y) %% 65536 == 0, return(1), return(0))
}


nextgen <- function (val, factor, divisor){
  return ((val * factor) %% divisor)
}


# Part 1
# My input
#Generator A starts with 289
#Generator B starts with 629

# Work with individual values so as not to store a large data structure

# Setup
factA <- 16807
factB <- 48271
divisor <- 2147483647

# Speedtest of function testpairs
A <- 65
B <- 8921

mytotal <- 0

system.time(for (i in 1:100000){
  # Generate new values
  A <- nextgen(A, factA, divisor)
  B <- nextgen(B, factB, divisor)
  # Compare
  mytotal <- mytotal + testpairs(A, B, bits = 16)
})
print(mytotal)
# Function testpairs for 100000 iterations, test input:
#user  system elapsed 
#17.775   0.039  17.840 

# Speedtest of function testpairs2
A <- 65
B <- 8921
mytotal <- 0

system.time(for (i in 1:100000){
# Generate new values
  A <- nextgen(A, factA, divisor)
  B <- nextgen(B, factB, divisor)
# Compare
  mytotal <- mytotal + testpairs2(A, B, bits = 16)
})
print(mytotal)
# Function testpairs2 for 100000 iterations, test input:
#   user  system elapsed 
#27.271   0.224  27.922

# Speedtest of function testpairs3
A <- 65
B <- 8921
mytotal <- 0

system.time(for (i in 1:100000){
  # Generate new values
  A <- nextgen(A, factA, divisor)
  B <- nextgen(B, factB, divisor)
  # Compare
  mytotal <- mytotal + testpairs3(A, B)
})
print(mytotal)
# Function testpairs3 for 100000 iterations, test input:
# user  system elapsed 
# 0.425   0.004   0.430 

# This speed is OK, now get my solution for part 1 using my values for A and B
A <- 289
B <- 629
mytotal <- 0

system.time(for (i in 1:40000000){
  # Generate new values
  A <- nextgen(A, factA, divisor)
  B <- nextgen(B, factB, divisor)
  # Compare
  mytotal <- mytotal + testpairs3(A, B)
})
print(mytotal) # Part 1 solution
#  user  system elapsed 
#162.640   0.804 164.296 


# Part 2
A <- 289
B <- 629
mytotal <- 0
nopairs <- 0 

system.time(while (nopairs <= 5000000){
  while (A %% 4 !=0){
    A <- nextgen(A, factA, divisor)
  }
  while (B %% 8 !=0){
    B <- nextgen(B, factB, divisor)
  }
  # Compare
  mytotal <- mytotal + testpairs3(A, B)
  nopairs <- nopairs + 1 # Test case to implement this last
  # Generate to trigger the while loops again
  A <- nextgen(A, factA, divisor)
  B <- nextgen(B, factB, divisor)
})
print(mytotal)
# Time for my values
#  user  system elapsed 
#67.393   0.238  67.937 


