library(readr)
library(stringr)
library(dplyr)
day05 <- read_file("day05.txt")

lines <- str_split(day05, "\n")[[1]]
vent_lines <- str_split(lines, "->")
vent_starts <- sapply(vent_lines, function(vent_line) {
  str_split(str_trim(vent_line[1]), ",")
})
vent_ends <- sapply(vent_lines, function(vent_line) {
  str_split(str_trim(vent_line[2]), ",")
})

vents <- data.frame(
  start_x = sapply(vent_starts, function(vent_start) {
    as.numeric(vent_start[1])
  }),
  start_y = sapply(vent_starts, function(vent_start) {
    as.numeric(vent_start[2])
  }),
  end_x = sapply(vent_ends, function(vent_end) {
    as.numeric(vent_end[1])
  }),
  end_y = sapply(vent_ends, function(vent_end) {
    as.numeric(vent_end[2])
  })
)

grid_size <- max(vents)

# Day 1
grid <- matrix(0, nrow = grid_size, ncol = grid_size)
for (n in 1:dim(vents)[1]) {
  current_x <- vents$start_x[n]
  current_y <- vents$start_y[n]
  dx <- sign(vents$end_x[n] - vents$start_x[n])
  dy <- sign(vents$end_y[n] - vents$start_y[n])
  if (any(c(dx == 0, dy == 0))) {
    repeat {
      grid[current_x, current_y] <- grid[current_x, current_y] + 1
      if (current_x == vents$end_x[n] & current_y == vents$end_y[n])
        break
      current_x <- current_x + dx
      current_y <- current_y + dy
    }
  }
}

print(sum(grid > 1))

# Day 2
grid <- matrix(0, nrow = grid_size, ncol = grid_size)
for (n in 1:dim(vents)[1]) {
  current_x <- vents$start_x[n]
  current_y <- vents$start_y[n]
  dx <- sign(vents$end_x[n] - vents$start_x[n])
  dy <- sign(vents$end_y[n] - vents$start_y[n])
  repeat {
    grid[current_x, current_y] <- grid[current_x, current_y] + 1
    if (current_x == vents$end_x[n] & current_y == vents$end_y[n])
      break
    current_x <- current_x + dx
    current_y <- current_y + dy
  }
}

print(sum(grid > 1))
