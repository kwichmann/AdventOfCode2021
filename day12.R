library(stringr)
day12 <- readLines("day12.txt")

# Part 1
connects <- str_split(day12, "-")
sites <- unique(unlist(connects))

find_site_connects <- function(connects, site) {
  site_connects <- c()
  for (connect in connects) {
    if (site %in% connect) {
      site_connects <- c(site_connects, setdiff(connect, site))
    }
  }
  return(site_connects)
}

find_paths <- function(connects, path) {
  current <- tail(path, 1)
  if (current == "end") return(list(path))
  site_connects <- find_site_connects(connects, current)
  uppers <- sapply(site_connects, function(connect) {
    toupper(connect) == connect
  })
  valid_connects <- site_connects[uppers]
  for (lower in site_connects[!uppers]) {
    if (!(lower %in% path)) {
      valid_connects <- c(valid_connects, lower)
    }
  }
  new_paths <- list()
  for (valid_connect in valid_connects) {
    new_paths <- c(new_paths, find_paths(connects, c(path, valid_connect)))
  }
  new_paths
}

paths <- find_paths(connects, "start")
print(length(paths))

# Part 2
find_paths2 <- function(connects, path) {
  current <- tail(path, 1)
  if (current == "end") return(list(path))
  site_connects <- find_site_connects(connects, current)
  uppers <- sapply(site_connects, function(connect) {
    toupper(connect) == connect
  })
  valid_connects <- site_connects[uppers]
  for (lower in setdiff(site_connects[!uppers], "start")) {
    if (!(lower %in% path)) {
      valid_connects <- c(valid_connects, lower)
    } else {
      if (sum(path == lower) == 1) {
        site_names <- unique(path)
        repeats <- lapply(site_names, function(site) sum(path == site))
        names(repeats) <- site_names
        repeats[["start"]] <- NULL
        repeats[[lower]] <- NULL
        for (site_name in names(repeats)) {
          if (site_name == toupper(site_name)) {
            repeats[[site_name]] <- NULL
          }
        }
        if (all(unlist(repeats) < 2)) {
          valid_connects <- c(valid_connects, lower)
        }
      }
    }
  }
  new_paths <- list()
  for (valid_connect in valid_connects) {
    new_paths <- c(new_paths, find_paths2(connects, c(path, valid_connect)))
  }
  new_paths
}

paths2 <- find_paths2(connects, "start")
print(length(paths2))
