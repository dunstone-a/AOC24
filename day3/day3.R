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

## Same code as the function ---------------------------------------------------

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


# PART 2 -----------------------------------------------------------------------

# There are new instructions:
# do() enables future mul instructions
# don't() disables future mul instructions (until the next do() appears).

data_ex2 <- readr::read_file(here("day3/example2.txt"))

## Script version for example 2 ------------------------------------------------

# Split the string whenever there is exactly "mul"
x <- unlist(strsplit(data_ex2, "do"))

# Phrases to keep
keep <- rep(FALSE, length(x))
# Begin with do() command. 
keep[1] <- TRUE

# Remove anything that starts with `don't()`
keep <- keep & !grepl("^n't\\(\\)", x)

# Aside from first value, future occurrences must have 'do()
keep <- keep | grepl("^\\(\\)", x)

x_filt <- x[keep]

multiply_mul_phrases(glue::glue_collapse(x_filt))

## Script version for data -----------------------------------------------------

# Split the string whenever there is exactly "mul"
x <- unlist(strsplit(data, "do"))

# Phrases to keep
keep <- rep(FALSE, length(x))
# Begin with do() command. 
keep[1] <- TRUE

# Remove anything that starts with `don't()`
keep <- keep & !grepl("^n't\\(\\)", x)

# Aside from first value, future occurrences must have 'do()
keep <- keep | grepl("^\\(\\)", x)

# Contingency table of phrases that begin with don'() and do().
# For the real data, there is only one element which does not fall into one
# of these two categories, and it is the first element. 
table(grepl("^n't\\(\\)", x), grepl("^\\(\\)", x))
which(!grepl("^n't\\(\\)", x) & !grepl("^\\(\\)", x))

x_filt <- x[keep]

multiply_mul_phrases(glue::glue_collapse(x_filt))


# Function to multiply the numbers in mul(a,b) but subject to actions given
# by do() and don't() commands in the string.
do_mul_phrases <- function(data) {
    
    # Split the string whenever there is exactly "do"
    x <- unlist(strsplit(data, "do"))
    
    # Phrases to keep
    keep <- rep(FALSE, length(x))
    # Begin with do() command. 
    keep[1] <- TRUE
    
    # Remove anything that starts with `don't()`
    keep <- keep & !grepl("^n't\\(\\)", x)
    
    # Aside from first element, future occurrences must have `do()`
    keep <- keep | grepl("^\\(\\)", x)
    
    # Remove sections of string which are deactivated by `don'()`
    x_filt <- x[keep]
    
    # Collapse string and run regular multiplication function.
    multiply_mul_phrases(glue::glue_collapse(x_filt))
}

do_mul_phrases(data_ex2)

message(paste("Answer: ", do_mul_phrases(data)))
# Answer:  111762583
