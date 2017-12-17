#day15.R
# Edit: further modification to create testpairs4()-
# ifelse is 5-10x slower than if {} else{} if the length of the test condition is 1

library(R.utils)
library(stringr)
library(binaryLogic)
library(bit)

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

testpairs3 <- function(x, y){ # THIS ONE (WAS) THE WINNER!!! |(originally)
  # Bitwise XOR the two numbers together, as before
  # But no need to convert to binary and faff: if the last 16 bits are zero it divides by 65536
  ifelse(bitwXor(x,y) %% 65536 == 0, return(1), return(0))
}

testpairs4 <- function(x, y){ # if{} else{} is 5-10x faster than ifelse if the length of the test condition is 1
  # Bitwise XOR the two numbers together, as before
  # But no need to convert to binary and faff: if the last 16 bits are zero it divides by 65536
  if(bitwXor(x,y) %% 65536 == 0){
    return(1)
    }else {return(0)
  }
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
profvis({factA <- 16807
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


system.time(for (i in 1:100000){
  # Generate new values
  A <- nextgen(A, factA, divisor)
  B <- nextgen(B, factB, divisor)
  # Compare
  mytotal <- mytotal + testpairs4(A, B)
})
print(mytotal)
# Function testpairs4 for 100000 iterations, test input:
# user  system elapsed 
# 0.307   0.017   0.333  

# This speed is OK. Get solution for part 1 using my values for A and B
A <- 289
B <- 629
mytotal <- 0

system.time(for (i in 1:40000000){
  # Generate new values
  A <- nextgen(A, factA, divisor)
  B <- nextgen(B, factB, divisor)
  # Compare
  mytotal <- mytotal + testpairs4(A, B)
})
print(mytotal) # Part 1 solution
#  user  system elapsed 
# 105.993   0.596 106.849 


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
  mytotal <- mytotal + testpairs4(A, B)
  nopairs <- nopairs + 1 # Test case, so implement this after incementing the total
  # Generate to trigger the while loops again
  A <- nextgen(A, factA, divisor)
  B <- nextgen(B, factB, divisor)
})
print(mytotal)
# Time for my values
#  user  system elapsed 
#56.602   0.146  56.773  


### ADDENDUM
# Apparently this is all a known problem. 2147483647 is 2^31 - 1 (and a Mersenne prime)
# The algorithm is an established pseudo-RNG and the seeds given to A and B are known too
# This page is an AMAZING read: http://www.firstpr.com.au/dsp/rand31/#History-PMMS

# But for some reason the stuff I wrote in R is faster than running these algorithms in C++ from R
# (Possibly because my processor is 64 bit rather than 32 bit? Maybe?!)

library(R.utils)
library(Rcpp)

# The Park-Millar-Carta 16807 random number generator
# Hardwire the 16807 so that it's compiled in there
cppFunction('long unsigned int rand31_16807(long unsigned int seed){
            long unsigned int hi, lo;
            lo = 16807 * (seed & 0xFFFF);
            hi = 16807 * (seed >> 16);
            lo += (hi & 0x7FFF) << 16;
            lo += hi >> 15;
            lo = (lo & 0x7FFFFFFF) + (lo >> 31);
            return (seed = (long)lo);
            }')

# The Park-Millar-Carta 48271 random number generator
# Hardwire the 48271 so that it's compiled in there
cppFunction('long unsigned int rand31_48271(long unsigned int seed){
            long unsigned int hi, lo;
            lo = 48271 * (seed & 0xFFFF);
            hi = 48271 * (seed >> 16);
            lo += (hi & 0x7FFF) << 16;
            lo += hi >> 15;
            lo = (lo & 0x7FFFFFFF) + (lo >> 31);
            return (seed = (long)lo);
            }')

A <- 65
B <- 8921
system.time(for (i in 1:5000000){
    A <- rand31_16807(A)
    B <- rand31_48271(B)
})
# user  system elapsed 
# 14.063   5.084  19.174 

A <- 65
B <- 8921
system.time(for (i in 1:5000000){
  A <- nextgen(A, factA, divisor)
  B <- nextgen(B, factB, divisor)
})})
# user  system elapsed 
# 7.569   0.049   7.651 