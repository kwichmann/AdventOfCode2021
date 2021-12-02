library(readr)
day01 <- read_csv("day01.txt", col_names = "depth")
depth <- day01$depth

# Part 1
differences <- diff(depth)
print(sum(differences > 0))

# Part 2
