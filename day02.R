library(readr)
library(dplyr)
day02 <- read_table2("day02.txt", col_names = c("direction", "distance"))

# Part 1
depth_difference <- function(direction, distance) {
  mapply(function(dir, dist) {
    if (dir == "up") return(-dist)
    if (dir == "down") return(dist)
    return(0)
  }, direction, distance)
}

horizontal_difference <- function(direction, distance) {
  mapply(function(dir, dist) {
    if (dir == "forward") return(dist)
    return(0)
  }, direction, distance)
}

navigation <- day02 %>%
  mutate(depth_diff = depth_difference(direction, distance),
         horizontal_diff = horizontal_difference(direction, distance))

final_depth <- sum(navigation$depth_diff)
final_horizontal_position <- sum(navigation$horizontal_diff)

print(final_depth * final_horizontal_position)

# Part 2
navigation2 <- day02 %>%
  mutate(aim_diff = depth_difference(direction, distance)) %>%
  mutate(aim = cumsum(aim_diff)) %>%
  mutate(horizontal_diff = horizontal_difference(direction, distance),
         depth_diff = aim * horizontal_difference(direction, distance))

final_depth2 <- sum(navigation2$depth_diff)
final_horizontal_position2 <- sum(navigation2$horizontal_diff)

print(final_depth2 * final_horizontal_position2)
