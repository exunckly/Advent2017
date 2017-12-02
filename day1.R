
original <- readLines("data:day1.txt")
ptm <- proc.time()
mysplit <- as.numeric(strsplit(original,"")[[1]])

total = 0

minusone = length(mysplit)-1

if (mysplit[1] == mysplit[length(mysplit)]){
  total = total + mysplit[1]
}

# Day 1 part 1
for (i in 1:minusone){
  if(mysplit[i] == mysplit[i+1]){
    total = total + mysplit[i]
  }
}

# Day 1 part 2
ptm <- proc.time()
total = 0

stepsize = length(mysplit)/2

for (i in 1:stepsize){
  if(mysplit[i] == mysplit[i+stepsize]){
      total = total + mysplit[i]
  }
}    

total = total*2
proc.time() - ptm

print(total)