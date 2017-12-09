library(readr)

# Day 9

# Read in the data
origdata <- read_file("day9input.txt")
data <- origdata

# Part 1

# Get rid of ! and the character after it, working from left to right
# gsub replaces all, while sub replaces the first which is what we want
while (grepl("!", data)){
  data <- sub("!.", "", data)
}

nopling <- data

# Now search for garbage and get rid of it too
while(grepl("<", data)){
  data <- sub("<[^>]*>","", data) # [^>] is short for 'any character except for >
}

# Loop through a vector keeping track of the level of nesting and incrementing a count every time we close a }
datavec <- unlist(strsplit(data, "")) 
lev <- 0
count <- 0

for (i in 1:length(datavec))
{
  if(datavec[i] == "{") lev <- lev + 1
  if(datavec[i] == "}"){
    count <- count + lev
    lev <- lev - 1
  }
}

print(count)

# Part 2
# Start with nopling as we are not interested in the cancelled characters
# Subtract the lengths of the strings with and without the garbage
withgarbage <- nchar(nopling)
nogarbage <- nopling

# This time we retain the <>, as we don't want to count them
# but sub in square brackets so that the while loop doesn't go on for ever!
while(grepl("<", nogarbage)){
  nogarbage <- sub("<[^>]*>","[]", nogarbage) 
}

garbagechars <- withgarbage - nchar(nogarbage)
print(garbagechars)
