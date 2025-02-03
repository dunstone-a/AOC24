# Advent of Code 2024: Day 2. 
# Amelia Dunstone
# 2025-01-31

library(here)

# Load data --------------------------------------------------------------------

# Since the number of columns is inconsistent, I am reading this in as a single
# column of character vectors and use string split later to convert to numeric. 

# Example data
df_ex <- read.table(here("day2/example.txt"), header = FALSE, sep = "\n")

# Input data
df <- read.table(here("day2/data.txt"), header = FALSE, sep = "\n")

# PART 1 -----------------------------------------------------------------------

# Determine which reports (containing variable number of levels) are safe.
# Levels must be either increasing or decreasing. 
# Each level can only differ from the adjacent levels by 1 to 3 points. 


check_report_safety <- function(reports) {
    
    # Classify each report as safe (1) or unsafe (0)
    results <- apply(reports, MARGIN = 1, FUN = function(x) {
        
        # Convert character of numbers to vector
        x <- as.numeric(unlist(strsplit(x, split = " ")))
            
        # Number of levels for this report
        n <- length(x)
        
        # Check if all increasing or all decreasing
        if ((identical(x, sort(x)) | identical(x, sort(x, decreasing = TRUE)))) {
            # Differences to next level
            level_diffs <- abs(x[2:n] - x[1:(n-1)])
            if (all(level_diffs %in% 1:3)) {
                1
            } else {
                # Differences are not only 1, 2 or 3. 
                0
            }
        } else {
            # Not increasing or decreasing
            0
        }
    }) 
    
    return(list(
        results = results,
        total = sum(results)
    ))
}

check_report_safety(df_ex)
df_result <- check_report_safety(df)

message(paste("Answer: ", df_result$total))
# Answer:  257

# PART 2 -----------------------------------------------------------------------

# The code to classify a single report has been changed slightly from above, 
# as I have removed the use of sort() after seeing the code by @OliverVoogd 
# for day 2, part 1.

# Function to classify a single report.
# report: numeric vector of levels for a particular report
check_report <- function(x) {
    
    # Number of levels for this report
    n <- length(x)
    
    # Get differences to next level
    level_diffs <- x[2:n] - x[1:(n-1)]
    
    if (length(unique(sign(level_diffs))) == 1) {
        if (all(abs(level_diffs) %in% 1:3)) {
            return(1)
        } else {
            return(0)
        }
    } else {
        return(0)
    }
}

# Function to classify all of the reports, using the problem dampener. 
# reports: data frame with 1 column
check_report_dampened <- function(reports) {
    
    # Classify each report as safe (1) or unsafe (0)
    results <- apply(reports, MARGIN = 1, FUN = function(x) {
        
        # Convert character of numbers to vector
        x <- as.numeric(unlist(strsplit(x, split = " ")))
        
        # Number of levels for this report
        n <- length(x)

        if (check_report(x) == 1) {
            current <- 1
        } else {
            current <- 0
            # Allow one mistake if conditions are otherwise met
            i <- 1
            while (i <= n) {
                if (check_report(x[-i]) == 1) {
                    current <- 1
                    break
                }
                i <- i + 1
            }
        }
    current
    })
        
    return(list(
        results = results,
        total = sum(results)
    ))
}

check_report_dampened(df_ex)
df_result <- check_report_dampened(df)
message(paste("Answer: ", df_result$total))
# Answer:  328
