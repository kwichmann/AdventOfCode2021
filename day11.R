library(readr)
library(stringr)
day11 <- read_file("day11.txt")

octopus_lines <- str_split(day11, "\n")[[1]]
octopus_sites <- lapply(str_split(octopus_lines, ""), as.numeric)
octopuses <- matrix(unlist(octopus_sites), nrow = length(octopus_lines))

# Part 1
flash <- function(octo_grid, n, m) {
  for (dn in -1:1) {
    for (dm in -1:1) {
      if (dn != 0 | dm != 0) {
        new_n <- n + dn
        new_m <- m + dm
        if (new_n > 0 & new_n <= 10 & new_m > 0 & new_m <= 10) {
          octo_grid[new_n, new_m] <- octo_grid[new_n, new_m] + 1
        }
      }
    }
  }
  return(octo_grid)
}

step_grid <- function(octo_grid) {
  octo_grid <- octo_grid + 1
  flashed <- matrix(FALSE, nrow = 10, ncol = 10)
  still_flashing <- TRUE
  while (still_flashing) {
    still_flashing <- FALSE
    for (n in 1:10) {
      for (m in 1:10) {
        if (octo_grid[n, m] > 9 & !flashed[n, m]) {
          octo_grid <- flash(octo_grid, n, m)
          flashed[n, m] <- TRUE
          still_flashing <- TRUE
        }
      }
    }
  }
  octo_grid * (octo_grid <= 9)
}

octo_grid <- octopuses
total_flashes <- 0
for (step in 1:100) {
  octo_grid <- step_grid(octo_grid)
  total_flashes <- total_flashes + sum(octo_grid == 0)
}

print(total_flashes)

# Part 2
octo_grid <- octopuses
step <- 0
while (TRUE) {
  step <- step + 1
  octo_grid <- step_grid(octo_grid)
  if (all(octo_grid == 0)) break
}
print(step)
