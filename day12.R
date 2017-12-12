library(igraph)

# Day 12

# Tidy data into pairs, then make graph from edgelist
a <- readLines("day12test.txt")
pairsdf <- as.data.frame(a)
pairsdf <- pairsdf %>% separate(a, c("X1","X2"), sep = "\\s<->\\s")

# Separate X2 into the right nunber of columns (by finding the max no of commas)
maxcols <- max(str_count(pairsdf$X2, ",")) + 1
pairsdf <- pairsdf %>% separate(X2, paste("Y", 1:maxcols, sep=""), sep=",\\s")

# Use gather to turn this into genuine pairs, delete the helper column created by gather, delete any that go to NA
pairsdf <- gather(pairsdf, "blah", "X2", 2:length(pairsdf))
pairsdf <- subset(pairsdf, select = -c(blah) )
pairsdf2 <- filter(pairsdf, !is.na(pairsdf$X2)) # Make a new df in case part 2 asks us to look for orphans

# Put this into an igraph
mygraph <- (graph_from_edgelist(as.matrix(pairsdf2), directed = FALSE))
mygroups <- components(mygraph) # This are named num vectors, because, obviously

# Look up the cluster size of the part of the graph my vertex is in
myvertex <- 0
thisgroup <- mygroups$membership[as.character(myvertex)]
ans <- mygroups$csize[thisgroup]
print(ans)
