library(readr)
day01 <- read_csv("day01.txt", col_names = "depth")
depth <- day01$depth

# Part 1
differences <- diff(depth)
print(sum(differences > 0))

# Part 2
window_depth <- c(depth, 0, 0) + c(0, depth, 0) +  c(0, 0, depth)
window_differences <- diff(window_depth[3:2002])
print(sum(window_differences > 0))
