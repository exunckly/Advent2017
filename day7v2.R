library(tidyverse)
library(stringr)
library(igraph)
library(dplyr)

# Tidy data into two columns, parent and child (drop the weights for now)
a <- readLines("day7input.txt")


# Part 1

# Make a data frame of parent -> child pairs
# Lose the pattern ' ([0-9]*]) ' to leave just the names of the discs
pairs <- gsub("\\s\\(\\d+\\)","",a)
pairsdf <- as.data.frame(pairs)

# Extract only the columns containing pairs, and separate into two columns
pairsdf <- dplyr::filter(pairsdf, grepl("->", a))
pairsdf <- pairsdf %>% separate(pairs, c("from","to"), sep = "\\s->\\s")

# Separate the 'to' into the right nunber of columns (by finding the max no of commas)
maxcols <- max(str_count(pairsdf$to, ",")) + 1
pairsdf <- pairsdf %>% separate(to, paste("to", 1:maxcols, sep=""), sep=",\\s")

# Use gather to turn this into genuine pairs, then delete the helper column created by gather
pairsdf <- gather(pairsdf, "blah", "to", 2:length(pairsdf))
pairsdf <- subset(pairsdf, select = -c(blah) )                                   

# Filter out any rows with empty/na in column 2
row.has.na <- apply(pairsdf, 1, function(x){any(is.na(x))})
pairsdf <- pairsdf[!row.has.na,]

# Find the index of the disc that does not appear in the to column
ans <- pairsdf$from[is.na(match(pairsdf$from,pairsdf$to))]
print(ans[1])


# Part 2

# Make a version in which we preserve the numbers and delete the connections (as we need the weights for part 2)
weights <- unlist(strsplit(a, " -> "))
weights <- gsub(")","",weights)
weightsdf <- as.data.frame(weights)

# Filter out lines that do not contain numbers
weightsdf <- dplyr::filter(weightsdf, grepl("[0-9]", weights))
weightsdf <- weightsdf %>% separate(weights, c("to", "weight"), sep = " \\(")

# Make weights a number
weightsdf$weight <- as.numeric(as.character(weightsdf$weight))

alldiscs <- as.data.frame(c(pairsdf$to, ans[1])) # The discs in the 'to' column plus the base
colnames(alldiscs)[1] <- "to"


# Do a thing where we:
# 1. find the tips and fill them in
# 2. find the discs that have only filled in discs on top, and fill them in
# 3. Rinse, repeat
# 4. Along the way, make a note of the first disc we find where the load isn't balanced
#    (as we're working from the top down, the problem will be with one of the discs on top of this one)
# 5. Compare the discs above with each other and find the odd one out
# 6. Having done that by eye and got the star, get fed up and bodge together a way to make the computer output the answer

# Find the tips
tips <- filter(pairsdf, !(to %in% from))
colnames(tips)[1] <- "tip"
tips$tip <- TRUE

# Find the weights for the tips
confirmedweights <- left_join(alldiscs, tips, by = "to")
confirmedweights <- left_join(confirmedweights, weightsdf, by = "to")
confirmedweights$weightontop <- 0
confirmedweights$discweight <- confirmedweights$weight
  
# Set weights for non-tips at NA
confirmedweights$weight[is.na(confirmedweights$tip)] <- NA
confirmedweights$no_ontop <- NA
confirmedweights$no_ontop[confirmedweights$tip == TRUE] <- 0

# Set flags in case I mess up
test <- FALSE
problem <- -999

# Run the loop until everything is filled in

while(any(is.na(confirmedweights$weight))){

for (i in 1:length(confirmedweights$to)){

  if(is.na(confirmedweights$weight[i])){
    # Make a little data frame containing only the discs that sit on top of this one
    tempdf <- filter(pairsdf, from == confirmedweights$to[i])
    # Pull in the weights (I could have speeded this up by doing it in advance, but oh well)
    tempdf2 <- inner_join(tempdf, confirmedweights, by = "to")
    
    if(all(!is.na(tempdf2$weight))){ # i.e. if we know the weights of all discs on top of this one
      # Test to see if the weights are equal
      if (sum(tempdf2$weight)/length(tempdf2$weight) != tempdf2$weight[1] && test == FALSE){
        problem <- i
        test <- TRUE
      }
      # Calculate the sum of the weights on top of this disc, the weight that this disc contributes to the stack,
      # and the number of weightsa on top of this one
      confirmedweights$weightontop[i] <- sum(tempdf2$weight)
      confirmedweights$weight[i] <- confirmedweights$discweight[i] + confirmedweights$weightontop[i]
      confirmedweights$no_ontop[i] <- length(tempdf2$weight)
      }
    }
  }
}

# We know that only one disc has a problem
tempdf <- filter(pairsdf, from == confirmedweights$to[problem])
tempdf2 <- inner_join(tempdf, confirmedweights, by = "to")

# Need to find the odd one out in tempdf2$weight
myuniques <- unique(tempdf2$weight)
weightdiff <- diff(range(myuniques))

# I got fed up here and relied on knowing that there is only one disc with a problem
if (length(tempdf2$weight[tempdf2$weight == myuniques[1]]) > length(tempdf2$weight[tempdf2$weight == myuniques[2]])){
  change <- 2
  other <- 1
} else{
  change <- 1
  other <-2
}
  
newweight <- tempdf2$discweight[tempdf2$weight == myuniques[change]] + tempdf2$weight[tempdf2$weight == myuniques[other]] - tempdf2$weight[tempdf2$weight == myuniques[change]]
print (newweight[1])

