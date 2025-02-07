# Advent of Code 2024: Day 4.
# Amelia Dunstone
# 2025-02-07

# Load data --------------------------------------------------------------------

library(here)
library(stringr)

# Read in data as character vector. Each element corresponds to a row. 
# Example data
ex <- read.table(here("day4/example.txt"), header = FALSE, sep = "\n")[,1]

# Input data
data <- read.table(here("day4/data.txt"), header = FALSE, sep = "\n")[,1]

# Helper functions -------------------------------------------------------------

# Convert character vector of length n to matrix with characters in each element.
# Output is an n by n matrix. 
data_as_matrix <- function(data) {
    # Splitting on empty string results in splitting after any character
    # Unlist initially creates a character vector of length equal to the number
    # of elements in the data. e.g. 10 x 10 = 100.
    matrix(unlist(strsplit(data, "")), ncol = length(data), byrow = TRUE)
}

# Input is a character vector of length equal to the number of rows. 
# Output is the number of horizontal instances of XMAS or SAMX.
count_horizontal <- function(data) {
    count_h <- 0
    for (i in 1:length(data)) {
        count_h <- count_h + str_count(data[i], "XMAS") + str_count(data[i], "SAMX")
    }
    count_h
}

# Function to take a matrix where each element is a character and obtain
# a character vector corresponding to each column. 
glue_matrices_vertical <- function(mat) {
    apply(t(mat), 1, function(x) {
        glue::glue_collapse(x)
    })
}

# Requires matrix input!
count_vertical <- function(mat) {
    
    # Glue matrices first to obtain character vector
    rotated <- glue_matrices_vertical(mat)
    
    count_horizontal(rotated)
}

# Counts the number of diagonal occurrences.
count_diagonal <- function(mat) {
    
    # Initialising two matrices to hold the "shifted" rows. 
    # up: diagonals which are slanted up / in original matrix. 
    # down: diagonals which are slanted down \ in original matrix.
    diag_mat_up <- matrix("", nrow = nrow(mat), ncol = ncol(mat)*2 - 1)
    diag_mat_down <- diag_mat_up
    
    for (i in 1:nrow(mat)) {
        # NOTE: Using an R trick here, 0:9 + i is the same as (0+i):(9+i)
        # Diagonals up
        diag_mat_up[i, 1:ncol(mat) + i - 1] <- mat[i,] 
        
        # Diagonals down
        diag_mat_down[i, 1:ncol(mat) + ncol(mat) - i] <- mat[i,] 
    }
    
    count_diag_up <- count_vertical(diag_mat_up)
    print(paste0("Diagonal up: ", count_diag_up))
    
    count_diag_down <- count_vertical(diag_mat_down)
    print(paste0("Diagonal down: ", count_diag_down))
    
    return(list(up = count_diag_up, down = count_diag_down))
    
}

# Main -------------------------------------------------------------------------

xmas_search <- function(data) {
    
    # Count number of horizontal occurrences.
    count_h <- count_horizontal(data)
    print(paste0("Horizontal: ", count_h))
    
    # Convert data to matrix for vertical and diagonal searches.
    data_mat <- data_as_matrix(data)
    
    # Count number of vertical occurrences. 
    count_v <- count_vertical(data_mat)
    print(paste0("Vertical: ", count_v))
    
    # Count number of diagonal occurrences.
    count_d <- count_diagonal(data_mat)
    
    print(paste0("Total: ", count_h + count_v + sum(unlist(count_d))))
}

xmas_search(ex)
# [1] "Horizontal: 5"
# [1] "Vertical: 3"
# [1] "Diagonal up: 5"
# [1] "Diagonal down: 5"
# [1] "Total: 18"
xmas_search(data)
# [1] "Horizontal: 432"
# [1] "Vertical: 428"
# [1] "Diagonal up: 830"
# [1] "Diagonal down: 840"
# [1] "Total: 2530"







