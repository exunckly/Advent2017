# Day 4 part 1
# Unless you do this, it works out the number of columns from the content of the first 5 rows then
# doesn't just roll with it, but starts newlines for anything that doesn't fit...
mycol <- max(count.fields("day4download.txt", sep = " "))

# Now we know how many columns there are!
day4data <- read.table("day4download.txt", header = FALSE, sep=" ", fill = TRUE, col.names = 1:mycol)

mylen = length(day4data)

# The variable names indicate the order in which I did things :-)
a <- apply(day4data, 1, function(x) sum(x != ""))  # Number of non="" elements in each row
a2 <- ifelse(a==mylen, 0, 1) # Flag to say if there is a "" in the row. I think I like ifelse!
b <- apply(day4data, 1, function(x) length(unique(x))) # number of unique elements in each row, including ""

invalid <- a + a2 - b # 1 = invalid, 0 = valid

day4pt1ans <- sum(invalid == 0)
day4pt1ans

