library(stringr)

# Day 4
# Rewrote the part 1 answer as a function once I found out what part 2 was

fn_validpasswordno <- function (pwarray){
  
  # Input is an array of strings, one string per cell, with "" in empty cells
  # Each row represents a password
  # Function returns the number of valid passwords, by applying the following rule:
  #     - all of the strings in a row must be unique
  
  #### Edit: it turns out that duplicated() and any() are good things to know about!
  
  # This is the maximum number of strings on a line
  mylen = length(pwarray)
  
  a <- apply(pwarray, 1, function(x) sum(x != ""))  # Number of non="" elements in each row
  a2 <- ifelse(a==mylen, 0, 1) # Flag to say if there is a "" in the row, or if all cells are occupied
  b <- apply(pwarray, 1, function(x) length(unique(x))) # number of unique elements in each row, including ""
  
  invalid <- a + a2 - b # 0 = valid, anything else = invalid 
  
  novalidpasswords <- sum(invalid == 0)
  return(novalidpasswords)
}


# Day 4 part 1

### Edit: it turns out that tibble(readlines()) woiuld have saved most of this pain

# By default, R works out the number of columns from the content of the first 5 rows. For read.table, at least
# The work-around is to go through the file first and find the maximum number of columns required:
mycol <- max(count.fields("day4download.txt", sep = " "))

# Now we know how many columns there are, we can feed it the right number of col.names up front
# It puts "" in empty cells
# Not using stringsasfactors = FALSE had me stuck FOR EVER on part 2
day4data <- read.table("day4download.txt", header = FALSE, sep=" ", fill = TRUE, col.names = 1:mycol, stringsAsFactors = FALSE)

fn_validpasswordno(day4data)

# Day 4 part 2
# This time anagrams are not valid
# Sort the characters in each of the strings into alphabetical order,
# then run the resulting array through the same function as previously

alphasort <- function(x){
  # More pipes than the 'Mull of Kintyre' overdub session
  # It concatenated all of the elements together when hit with apply, which was unexpected...
    unlist(strsplit(x,"")) %>% sort() %>% str_c(collapse="")
  
  ### Edit: looks like I somehow have to nest this inside some kind of apply to stop the above from happening.
  ### Will work on it another time
}

# The line below didn't work. I got fed up and used a loop instead.
# sorteddata <- apply(day4data, 1, function(x) alphasort(x))

sorteddata <- day4data # Assign it up front

# Replace any actual strings with alphabetical versions
for (i in 1:length(day4data[,1])){
  for (j in 1:length(day4data[1,])){
    if (length(sorteddata[i,j]) != 0 && sorteddata[i,j] != ""){
      sorteddata[i,j] <- alphasort(day4data[i,j])
    }
  }
}

fn_validpasswordno(sorteddata)
