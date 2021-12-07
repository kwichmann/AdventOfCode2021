library(readr)
library(stringr)
library(gmp)
day06 <- read_file("day06.txt")

initial_lantern <- as.numeric(str_split(day06, ",")[[1]])

# Part 1
evolve <- function(lantern) {
  zeros <- which(lantern == 0)
  lantern[zeros] <- 7
  lantern <- lantern - 1
  c(lantern, rep(8, length(zeros)))
}

evolve_n <- function(lantern, n) {
  for (i in 1:n) {
    lantern <- evolve(lantern)
  }
  return(lantern)
}

print(length(evolve_n(initial_lantern, 80)))

# Part 2
smart_evolve <- function(population_vector) {
  # Everything is off by one due to 1-indexing
  spawners <- population_vector[1]
  shift <- population_vector[2:9]
  new_population <- c(shift, spawners)
  new_population[7] <- new_population[7] + spawners
  return(new_population)
}

smart_evolve_n <- function(population_vector, n) {
  for (i in 1:n) {
    population_vector <- smart_evolve(population_vector)
  }
  return(population_vector)
}

initial_population <- as.bigz(sapply(0:8, function(i) {
  sum(initial_lantern == i)
}))

print(sum(smart_evolve_n(initial_population, 256)))
