library(readr)
library(stringr)
day08 <- read_file("day08.txt")

entries <- str_split(day08, "\n")[[1]]
input_output <- str_split(entries, "\\s\\|\\s")

# Part 1
match1478 <- sapply(input_output, function(entry) {
  lengths <- nchar(str_split(entry[2], "\\s")[[1]])
  sum(lengths %in% c(2, 3, 4, 7))
})

print(sum(match1478))

# Part 2
digits <- list(
  "1" = c("c", "f"),
  "2" = c("a", "c", "d", "e", "g"),
  "3" = c("a", "c", "d", "f", "g"),
  "4" = c("b", "c", "d", "f"),
  "5" = c("a", "b", "d", "f", "g"),
  "6" = c("a", "b", "d", "e", "f", "g"),
  "7" = c("a", "c", "f"),
  "8" = letters[1:7],
  "9" = c("a", "b", "c", "d", "f", "g"),
  "0" = c("a", "b", "c", "e", "f", "g")
)

num_seg_digits <- list(
  "2" = "1",
  "3" = "7",
  "4" = "4",
  "5" = c("2", "3", "5"),
  "6" = c("0", "6", "9"),
  "7" = "8"
)

overlaps <- lapply(num_seg_digits, function(digs) {
  segments <- letters[1:7]
  for (dig in digs) {
    segments <- intersect(segments, digits[[dig]])
  }
  segments
})
  
infer_map <- function(scrambled_digits) {
  possible_mappings <- lapply(1:7, function(l) letters[1:7])
  names(possible_mappings) <- letters[1:7]
  for (scramble in scrambled_digits) {
    segment_overlaps <- overlaps[[as.character(nchar(scramble))]]
    segments <- str_split(scramble, "")[[1]]
    for (segment in segment_overlaps) {
      possible_mappings[[segment]] <- intersect(possible_mappings[[segment]], segments)
    }
  }
  # Prune - doing it six times is easier than explicit bookkeeping
  for (i in 1:6) {
    for (segment in letters[1:7]) {
      if (length(possible_mappings[[segment]]) == 1) {
        for (sg in letters[1:7]) {
          if (sg != segment) {
            possible_mappings[[sg]] <- setdiff(possible_mappings[[sg]], possible_mappings[[segment]])
          }
        }
      }
    }
  }
  possible_mappings
}

inferred_maps <- lapply(entries, function(entry) {
  all_scrambles_string <- word(entry, 1, sep = " \\|")
  all_scrambles <- str_split(all_scrambles_string, "\\s+")[[1]]
  infer_map(all_scrambles)
})

infer_mapped_digit <- function(scrambled_output, map) {
  scrambled_segments <- str_split(scrambled_output, "")[[1]]
  segments <- sort(sapply(scrambled_segments, function(sg) {
    names(map)[which(map == sg)]
  }))
  # Will produce warnings
  for (digit in names(digits)) {
    if (all(digits[[digit]] == segments)) {
      return(as.numeric(digit))
    }
  }
}

mapped_digits <- lapply(1:length(entries), function(entry_n) {
  entry <- entries[[entry_n]]
  all_outputs_strings <- word(entry, 2, sep = "\\| ")
  all_outputs <- str_split(all_outputs_strings, "\\s+")[[1]]
  sapply(all_outputs, function(out) infer_mapped_digit(out, inferred_maps[[entry_n]]))
})

mapped_values <- sapply(mapped_digits, function(digs) {
  1000 * digs[1] + 100 * digs[2] + 10 * digs[3] + digs[4]
})

print(sum(mapped_values))
