library(readr)
library(stringr)
day07 <- read_file("day07.txt")

crabs <- as.numeric(str_split(day07, ",")[[1]])

# Part 1
position <- median(crabs)
print(sum(abs(crabs - position)))

# Part 2
fuel_spends <- sapply(min(crabs):max(crabs), function(pos) {
  distances <- abs(crabs - pos)
  sum(distances * (distances + 1) / 2)
})

print(min(fuel_spends))
