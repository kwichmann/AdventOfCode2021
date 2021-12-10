library(readr)
library(stringr)
day09 <- read_file("day09.txt")

landscape_lines <- str_split(day09, "\n")[[1]]
landscape_sites <- lapply(str_split(landscape_lines, ""), as.numeric)
landscape <- matrix(unlist(landscape_sites), nrow = length(landscape_lines))

# Part 1
total_risk_level <- 0
risk_points <- list()
for (n in 1:dim(landscape)[1]) {
  for (m in 1:dim(landscape)[2]) {
    current <- landscape[n, m]
    # Left
    if (n != 1) {
      left <- landscape[n - 1, m]
      if (left <= current) next
    }
    # Right
    if (n != dim(landscape)[1]) {
      right <- landscape[n + 1, m]
      if (right <= current) next
    }
    # Up
    if (m != 1) {
      up <- landscape[n, m - 1]
      if (up <= current) next
    }
    # Down
    if (m != dim(landscape)[2]) {
      down <- landscape[n, m + 1]
      if (down <= current) next
    }
    total_risk_level <- total_risk_level + current + 1
    risk_points[[length(risk_points) + 1]] <- c(n, m)
  }
}

print(total_risk_level)

# Part 2
vec_in_list <- function(li, vec) {
  for (l in li) {
    if (l[1] == vec[1] & l[2] == vec[2]) {
      return(TRUE)
    }
  }
  return(FALSE)
}

non_nine_neighbours <- function(point, previous_points) {
  n <- point[1]
  m <- point[2]
  neighbours <- list()
  # Left
  if (n != 1) {
    if (!vec_in_list(previous_points, c(n - 1, m))) {
      if (landscape[n - 1, m] < 9) {
        neighbours[[length(neighbours) + 1]] <- c(n - 1, m)
      }
    }
  }
  # Right
  if (n != dim(landscape)[1]) {
    if (!vec_in_list(previous_points, c(n + 1, m))) {
      if (landscape[n + 1, m] < 9) {
        neighbours[[length(neighbours) + 1]] <- c(n + 1, m)
      }
    }
  }
  # Up
  if (m != 1) {
    if (!vec_in_list(previous_points, c(n, m - 1))) {
      if (landscape[n, m - 1] < 9) {
        neighbours[[length(neighbours) + 1]] <- c(n, m - 1)
      }
    }
  }
  # Down
  if (m != dim(landscape)[2]) {
    if (!vec_in_list(previous_points, c(n, m + 1))) {
      if (landscape[n, m + 1] < 9) {
        neighbours[[length(neighbours) + 1]] <- c(n, m + 1)
      }
    }
  }
  return(neighbours)
}

find_basin <- function(frontier_points, previous_points) {
  new_frontier <- list()
  for (frontier in frontier_points) {
    if (!vec_in_list(previous_points, frontier)) {
      new_frontier <- c(new_frontier, non_nine_neighbours(frontier, previous_points))
      previous_points[[length(previous_points) + 1]] <- frontier
    }
  }
  if (length(new_frontier) == 0) {
    return(previous_points)
  }
  find_basin(new_frontier, previous_points)
}

basin <- function(risk_point) {
  find_basin(list(risk_point), list())
}

basin_sizes <- sapply(lapply(risk_points, basin), length)
sorted_basin_sizes <- sort(basin_sizes, decreasing = TRUE)

print(sorted_basin_sizes[1] * sorted_basin_sizes[2] * sorted_basin_sizes[3])
