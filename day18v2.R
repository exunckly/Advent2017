library(tidyverse)

# Day 18 part 1

# Set up tibble of instructions
inst <- read_delim("day18input.txt", delim=" ", col_names = FALSE)
inst <- inst %>% rename(op = X1, X = X2, Y = X3)

# Set up tibble of registers
regs <- inst %>%
  distinct(X) %>%
  rename(name = X) %>%
  filter(name != "1") # hardwire for now - my jgz 1 3 instruction puts 1 into the name of a register when it isn't
regs$val = 0 # DON'T use as.integer() as R handles large numbers using doubles and the input looks tricksy

# Implement functions for instructions
# Many of the instructions can take either a register (a single letter) or a number.
# The value of a register is the integer it contains; the value of a number is that number.

# This function helps with overloading
is.integer0 <- function(x)
{
  is.integer(x) && length(x) == 0L
}

my_snd <- function(regs, X){
#  snd X plays a sound with a frequency equal to the value of X.
  Xpos <- which(regs$name %in% X)
  paste("Playing sound from register", X, "with a value of", regs$val[Xpos])
  return(regs$val[Xpos]) # Note does not return regs
}

my_set <- function(regs, X, Y){
# set X Y sets register X to the value of Y.
  Xpos <- which(regs$name %in% X)
  Ypos <- which(regs$name %in% Y)
  if (is.integer0(Ypos)){
    regs$val[Xpos] <- as.numeric(Y)
  } else {
    regs$val[Xpos] <- regs$val[Ypos]
  }
  return(regs)
}

# regs <- my_set(regs, "a", 10)
# regs <- my_set(regs, "p", "a")
# regs <- my_set(regs, 1, 100)
# regs <- my_set(regs, "1", 1000)

my_add <- function(regs, X, Y){
# add X Y increases register X by the value of Y.
  Xpos <- which(regs$name %in% X)
  Ypos <- which(regs$name %in% Y)
  if (is.integer0(Ypos)){
    regs$val[Xpos] <- regs$val[Xpos] + as.numeric(Y)
  } else {
    regs$val[Xpos] <- regs$val[Xpos] + regs$val[Ypos]
  }
  return(regs)  
}


my_mul <- function(regs, X, Y){
# mul X Y sets register X to the result of multiplying the value contained in register X by the value of Y.
  Xpos <- which(regs$name %in% X)
  Ypos <- which(regs$name %in% Y)
  if (is.integer0(Ypos)){
    regs$val[Xpos] <- regs$val[Xpos] * as.numeric(Y)
  } else {
    regs$val[Xpos] <- regs$val[Xpos] * regs$val[Ypos]
  }
  return(regs)  
}

my_mod <- function(regs, X, Y){
# mod X Y sets register X to the remainder of dividing the value contained in register X by the value of Y (that is, it sets X to the result of X modulo Y).
  Xpos <- which(regs$name %in% X)
  Ypos <- which(regs$name %in% Y)
  if (is.integer0(Ypos)){
    regs$val[Xpos] <- regs$val[Xpos] %% as.numeric(Y)
  } else {
    regs$val[Xpos] <- regs$val[Xpos] %% regs$val[Ypos]
  }
  return(regs) 
}

my_rcv <- function(regs, X){
#  rcv X recovers the frequency of the last sound played, but only when the value of X is not zero.
#  (If it is zero, the command does nothing.)
  Xpos <- which(regs$name %in% X)
  if (is.integer0(Xpos)){
    ifelse(as.numeric(X) == 0, output <- FALSE, output <- TRUE)
  } else {
    ifelse(regs$val[Xpos] == 0, output <- FALSE, output <- TRUE)
  }
  return (output)
}

my_jgz <- function(regs, X, Y){
# jgz X Y jumps with an offset of the value of Y, but only if the value of X is greater than zero.
# (An offset of 2 skips the next instruction, an offset of -1 jumps to the previous instruction, and so on.)

  Xpos <- which(regs$name %in% X)
  Ypos <- which(regs$name %in% Y)
  ifelse (is.integer0(Xpos), x_use <- as.numeric(X), x_use <- regs$val[Xpos])
  ifelse (is.integer0(Ypos), y_use <- as.numeric(Y), y_use <- regs$val[Ypos])
  ifelse (x_use > 0, offset <- y_use, offset <- 1)
  return(offset)
}

last_sound <- NA # value of last sound played

# After each jump instruction, the program continues with the instruction to which the jump jumped.
# After any other instruction, the program continues with the next instruction.
# Continuing (or jumping) off either end of the program terminates it.

# Loop through instructions, testiong for whether we jumped off of either end
i <- 1
escape <- 0

while (i > 0 && i <= length(inst$op) && escape == 0){
  # Follow the instruction
  if (inst$op[i] == "snd"){
    last_sound <- my_snd(regs, inst$X[i])
    i <- i + 1
  } else if (inst$op[i] == "set"){
    regs <- my_set(regs, inst$X[i], inst$Y[i])
    i <- i + 1
  } else if (inst$op[i] == "add"){
    regs <- my_add(regs, inst$X[i], inst$Y[i])
    i <- i + 1
  } else if (inst$op[i] == "mul"){
    regs <- my_mul(regs, inst$X[i], inst$Y[i])
    i <- i + 1
  } else if (inst$op[i] == "mod"){
    regs <- my_mod(regs, inst$X[i], inst$Y[i])
    i <- i + 1
  } else if (inst$op[i] == "rcv"){
    if (my_rcv(regs, inst$X[i]) == TRUE){
        escape <- 1} else {
        paste("Nothing to recover")
        i <- i + 1
    }
  } else if (inst$op[i] == "jgz"){
    i <- i + my_jgz(regs, inst$X[i], inst$Y[i])
  }
 # print(i)
 # print(regs)
}
paste ("Last sound played:", last_sound)


