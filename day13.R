# Day 13
a <- read_delim("day13input.txt", delim = ":\\ ", col_names = FALSE)
a$X2 <- parse_integer(a$X2)
a <- rename(a, x = X1, d = X2)

# Part 1
# d = depth (height of a single scanner)
# x = position of scanner (origin is 0)
# T = period of scanner = 2(d-1)
# t = current time

# We reach scanner 0 at t = 0 (initial state), 1 at t = 1, scanner 2 at t = 2 etc. so x = t in part 1
# We intersect if t %% T[t] is zero, as all scanners start at the top

# Calculate period of each oscillator
add_column(a, T = 0, intersect = 0)
a$T <- 2*(a$d - 1)

# Test for intersection
a$intersect <- ifelse(a$x %% a$T == 0, 1, 0)

# Calculate severity
a$severity <- a$x * a$d * a$intersect
ans <- sum(a$severity)
print(ans)

# Part 2 - introduce a delay
# Intersections now happen if (x + delay) %% T is zero
# Also work with vectors as it's zillions of times faster than using data frames

myx <- as.matrix(a$x)
myT <- as.matrix(a$T)

clearrun <- FALSE
i <- 0 # delay
while (clearrun == FALSE){
  i <- i + 1
  myvec <- ifelse((myx + i) %% myT == 0, 1, 0)
  if (sum(myvec) == 0){
    clearrun <- TRUE
  }
}

print(i)
