library(readr)
library(stringr)
day04 <- read_file("day04.txt")

lines <- str_split(day04, "\n")[[1]]
bingo_numbers <- as.numeric(str_split(lines[1], ",")[[1]])

boards <- lapply(seq(3, 595, 6), function(n) {
  trimmed <- str_trim(lines[n:(n+4)])
  split <- as.numeric(unlist(str_split(trimmed, "\\s+")))
  matrix(split, nrow = 5, ncol = 5)
})

# Part 1
bingo_bool <- function(board, numbers) {
  apply(board, 1:2, function(b) b%in% numbers)
}

is_bingo <- function(board, numbers) {
  bool <- bingo_bool(board, numbers)
  any(apply(bool, 1, all)) | any(apply(bool, 2, all))
}

for (n in 1:length(bingo_numbers)) {
  numbers <- bingo_numbers[1:n]
  is_bingos <- sapply(boards, function(board) is_bingo(board, numbers))
  if (any(is_bingos)) break
}

winner <- which(is_bingos)
winner_board <- boards[[winner]]
unmarked <- winner_board[!bingo_bool(winner_board, numbers)]

print(sum(unmarked) * bingo_numbers[n])

# Part 2
last_bingos <- rep(FALSE, length(boards))

for (n in 1:length(bingo_numbers)) {
  numbers <- bingo_numbers[1:n]
  is_bingos <- sapply(boards, function(board) is_bingo(board, numbers))
  if (all(is_bingos)) {
    loser <- which(!last_bingos)
    break
  }
  last_bingos <- is_bingos
}

loser_board <- boards[[loser]]
unmarked <- loser_board[!bingo_bool(loser_board, numbers)]

print(sum(unmarked) * bingo_numbers[n])
