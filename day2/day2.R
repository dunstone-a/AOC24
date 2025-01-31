# Advent of Code 2024
# Amelia Dunstone
# 2025-01-31

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
                0
            }
        } else {
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


