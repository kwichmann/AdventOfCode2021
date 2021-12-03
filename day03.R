library(readr)
library(stringr)
day03 <- read_csv("day03.txt", col_names = "diagnostics")

diag <- day03$diagnostics
bits <- 12

# Part 1
gamma_bits <- sapply(bits:1, function(bit) {
  digits <- str_sub(diag, bit, bit)
  sum(as.numeric(digits)) >= length(diag) / 2
})

gamma <- 0
epsilon <- 0
for (bit in 1:bits) {
  gamma <- gamma + 2 ^ (bit - 1) * gamma_bits[bit]
  epsilon <- epsilon + 2 ^ (bit - 1) * !gamma_bits[bit]
}

print(gamma * epsilon)

# Part 2
oxygen_left <- diag
current_bit <- 1

while (length(oxygen_left) > 1) {
  remaining_bits <- as.numeric(str_sub(oxygen_left, current_bit, current_bit))
  criterion <- sum(remaining_bits) >= length(remaining_bits) / 2
  oxygen_left <- oxygen_left[remaining_bits == criterion]
  current_bit <- current_bit + 1
}

co2_left <- diag
current_bit <- 1

while (length(co2_left) > 1) {
  remaining_bits <- as.numeric(str_sub(co2_left, current_bit, current_bit))
  criterion <- sum(remaining_bits) < length(remaining_bits) / 2
  co2_left <- co2_left[remaining_bits == criterion]
  current_bit <- current_bit + 1
}

print(strtoi(oxygen_left, base = 2) * strtoi(co2_left, base = 2))

