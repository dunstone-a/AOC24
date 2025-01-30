# Advent of Code 2024
# Amelia Dunstone
# 2025-01-30

library(here)

# PART 1 -----------------------------------------------------------------------

# Sort two lists. Find the total distance between pairs of numbers in the sorted 
# lists, where distance is the absolute difference between the numbers.

## Example data ----------------------------------------------------------------

df_ex <- read.table(here("day1/example.txt"), header = FALSE)

V1_ex <- sort(df_ex$V1)
V2_ex <- sort(df_ex$V2)

sum(abs(V1_ex - V2_ex))

## Test data -------------------------------------------------------------------

df <- read.table(here("day1/data.txt"), header = FALSE)

# Quick solution
V1 <- sort(df$V1)
V2 <- sort(df$V2)

answer <- sum(abs(V1 - V2))

message(paste("Answer: ", answer))
# Answer:  1197984

# Or, to also show all the distances:
sorted <- data.frame(V1 = V1, V2 = V2)

sorted$distances <- abs(sorted$V1 - sorted$V2)
head(sorted)

sum(sorted$distances)

# PART 2 -----------------------------------------------------------------------

# "This time, you'll need to figure out exactly how often each number from the 
# left list appears in the right list. Calculate a total similarity score by 
# adding up each number in the left list after multiplying it by the number of  
# times that number appears in the right list."

## Example data ----------------------------------------------------------------

sorted_ex <- data.frame(V1 = sort(df_ex$V1), V2 = sort(df_ex$V2))

sorted_ex$distances <- abs(df_ex$V1 - df_ex$V2)

colnames(sorted_ex) <- c("left", "right", "distances")

# Multiplication factor / frequency, number of times a number from the left list 
# appears in the right list.
freq_ex <- table(sorted_ex$right)[as.character(sorted_ex$left[sorted_ex$left %in% sorted_ex$right])]

# Note NA values occur where number does not appear in list 2. 
sorted_ex$freq <- freq_ex[as.character(sorted_ex$left)]
sorted_ex$freq[is.na(sorted_ex$freq)] <- 0

sorted_ex$sim_score <- sorted_ex$left * sorted_ex$freq

sum(sorted_ex$sim_score, na.rm = TRUE)

## Test data -------------------------------------------------------------------

left <- V1
right <- V2

colnames(sorted) <- c("left", "right", "distances")

# Multiplication factor / frequency, number of times a number from the left list 
# appears in the right list.
freq <- table(sorted$right)[as.character(sorted$left[sorted$left %in% sorted$right])]

# Add frequencies into data frame in correct locations. 
sorted$freq <- freq[as.character(sorted$left)]
# Set NA frequencies to 0
sorted$freq[is.na(sorted$freq)] <- 0

head(sorted)
head(sorted[sorted$left %in% sorted$right,])

# Similarity score, multiply left list by scaling factors.
sorted$sim_score <- sorted$left * sorted$freq

message(paste("Answer: ", sum(sorted$sim_score)))
# Answer:  23387399



