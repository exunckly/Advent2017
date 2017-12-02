library(testthat)
library(dplyr)
library(data.table)
library(readr)

## Advent of code day 2

### Part 1
fn_day2 <- function(x){
 sum(apply(x , 1 , max, na.rm=T) - apply(x , 1 , min, na.rm=T))
# Edit: From looking at other people's solutions, I now know that diff(range(x)) is designed for this kind of thing
}

myfile <- "day2part1.tsv"
myday2 <- read_tsv(myfile, col_names = FALSE)
fn_day2(myday2)

# Edit: From looking at other people's solutions I also now know that checksum() is a thing

### Part 2

fn_day2pt2 <- function(x){
  mycombo <- combn(x, 2)
  mylen <- length(mycombo[1,])
  
  alldivides <- c(unlist(mycombo[1,]) %% unlist(mycombo[2,]),unlist(mycombo[2,]) %% unlist(mycombo[1,]))
  
  myloc <- which(alldivides == 0)
  if (myloc > mylen){
    myloc = myloc - mylen
  }
  
  myans <- max(unlist(mycombo[,myloc]))/min(unlist(mycombo[,myloc]))
  myans
}

sum(apply(myday2,1,fn_day2pt2))