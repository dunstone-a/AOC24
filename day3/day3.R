# Advent of Code 2024: Day 3.
# Amelia Dunstone
# 2025-02-03

library(here)

# Load data --------------------------------------------------------------------

# Example data
data_ex <- readr::read_file(here("day3/example.txt"))

# Input data
data <- read.table(here("day3/data.txt"), header = FALSE, sep = "\n")

data <- readr::read_file(here("day3/data.txt"))

# PART 1 -----------------------------------------------------------------------

## Script version --------------------------------------------------------------

# Split the string whenever there is exactly "mul"
x <- unlist(strsplit(data, "mul"))

# Search for occurrences that begin with a bracket, 1-3 digits, a common, 1-3 digits, 
# and a close bracket. 
x_filt <- grep(pattern = "^\\([0-9]{1,3},[0-9]{1,3}\\)", x = x, value = TRUE)

# Split at closing brackets and just keep everything to the left.
x_trim <- strsplit(x_filt, split = "\\)")
x_match <- sapply(x_trim, "[", 1)

# Split the string by opening bracket and a comma to separate the number
x_split <- strsplit(x_match, split = c("\\(|,"))

# Get the arguments to be pre and post multiplied
left_arg <- as.numeric(sapply(x_split, FUN = "[", 2))
right_arg <- as.numeric(sapply(x_split, FUN = "[", 3))

sum(left_arg*right_arg)

## Same code as a function -----------------------------------------------------

multiply_mul_phrases <- function(data) {
    
    # Split the string whenever there is exactly "mul"
    x <- unlist(strsplit(data, "mul"))
    
    # Search for occurrences that begin with a bracket, 1-3 digits, a common, 1-3 digits, 
    # and a close bracket. 
    x_filt <- grep(pattern = "^\\([0-9]{1,3},[0-9]{1,3}\\)", x = x, value = TRUE)
    
    # Split at closing brackets and just keep everything to the left.
    x_trim <- strsplit(x_filt, split = "\\)")
    x_match <- sapply(x_trim, "[", 1)
    
    # Split the string by opening bracket and a comma to separate the number
    x_split <- strsplit(x_match, split = c("\\(|,"))
    
    # Get the arguments to be pre and post multiplied
    left_arg <- as.numeric(sapply(x_split, FUN = "[", 2))
    right_arg <- as.numeric(sapply(x_split, FUN = "[", 3))
    
    # Multiply arguments and calculate the sum
    return(sum(left_arg*right_arg))
}

multiply_mul_phrases(data_ex)

message(paste("Answer: ", multiply_mul_phrases(data)))
# Answer:  169021493

