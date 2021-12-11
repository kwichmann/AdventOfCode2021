library(stringr)
day10 <- readLines("day10.txt")

# Part 1
check_chunks <- function(line, stackOut = FALSE) {
  line_chars <- str_split(line, "")[[1]]
  open_stack <- c()
  for (ch in line_chars) {
    if (ch %in% c("(", "[", "{", "<")) {
      open_stack <- c(open_stack, ch)
    } else {
      last_stack <- tail(open_stack, 1)
      if ((ch == ")" & last_stack != "(") |
          (ch == "]" & last_stack != "[") |
          (ch == "}" & last_stack != "{") |
          (ch == ">" & last_stack != "<")) {
        return(ch)
      }
      open_stack <- head(open_stack, length(open_stack) - 1)
    }
  }
  if (stackOut) return(open_stack)
}

error_chars <- sapply(day10, check_chunks)

error_value <- function(error_char) {
  if (is.null(error_char)) return(0)
  switch(error_char,
         ")" = 3,
         "]" = 57,
         "}" = 1197,
         ">" = 25137,
         "(" = 1,
         "[" = 2,
         "{" = 3,
         "<" = 4)
}

error_values <- sapply(error_chars, error_value)

print(sum(error_values))

# Part 2
error_score <- function(stack) {
  score <- 0
  for (sc in rev(stack)) {
    score <- score * 5 + error_value(sc)
  }
  score
}

incomplete <- day10[error_values == 0]
stacks <- lapply(incomplete, function(il) check_chunks(il, stackOut = TRUE))
error_scores <- sapply(stacks, error_score)

print(median(error_scores))

