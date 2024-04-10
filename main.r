# Alternative Language Project (in R)

# NOTE: The following code is to be uncommented for unit testing ONLY. It does not
# and will not run on repl.it, as repl.it is NOT compatible with R packages. This
# code checks if the testthat package is installed. If not, it will install it.
#if (!require("testthat", character.only = TRUE)) {
#  options(repos = c(CRAN = "https://cloud.r-project.org/"))
#  install.packages("testthat")
#}
#library(testthat)

#Creates the Cell class, with the requisite attributes:
setClass("Cell", slots = list(
  oem = "character", # OEM should be a character
  model = "character", # Model should be a character
  launch_announced = "integer",  # Year should be an integer
  launch_status = "character", # Launch status should be a character
  body_dimensions = "character",  # Dimensions should be a character
  body_weight = "numeric",  # Weight is a string/numeric
  body_sim = "character",  # Sim should be a character
  display_type = "character", # Display type should be a character
  display_size = "character",  # Display size should be a character
  display_resolution = "character", # Display resolution should be a character
  features_sensors = "character", # Sensors should be a character
  platform_os = "character" # OS should be a character
))

#The next block of code here is the constructor for the Cell class.
#It creates a new cell, initializing the attributes to default values.
create_cell <- function(oem, model, launch_announced, launch_status,
                        body_dimensions, body_weight, body_sim,
                        display_type, display_size, display_resolution,
                        features_sensors, platform_os) {
  new("Cell", oem = oem, model = model, launch_announced = launch_announced,
      launch_status = launch_status, body_dimensions = body_dimensions,
      body_weight = body_weight, body_sim = body_sim, display_type = display_type,
      display_size = display_size, display_resolution = display_resolution,
      features_sensors = features_sensors, platform_os = platform_os)
}

# Generic and method definitions for getters and setters.
# One exists for each of the attributes.
setGeneric("get_oem", function(obj) standardGeneric("get_oem"))
setMethod("get_oem", "Cell", function(obj) obj@oem)

setGeneric("set_oem", function(obj, oem) standardGeneric("set_oem"))
setMethod("set_oem", "Cell", function(obj, oem) {
  obj@oem <- oem
  obj
})

setGeneric("get_model", function(obj) standardGeneric("get_model"))
setMethod("get_model", "Cell", function(obj) obj@model)

setGeneric("set_model", function(obj, model) standardGeneric("set_model"))
setMethod("set_model", "Cell", function(obj, model) {
  obj@model <- model
  obj
})

setGeneric("get_launch_announced", function(obj) standardGeneric("get_launch_announced"))
setMethod("get_launch_announced", "Cell", function(obj) obj@launch_announced)

setGeneric("set_launch_announced", function(obj, launch_announced) standardGeneric("set_launch_announced"))
setMethod("set_launch_announced", "Cell", function(obj, launch_announced) {
  obj@launch_announced <- launch_announced
  obj
})

setGeneric("get_launch_status", function(obj) standardGeneric("get_launch_status"))
setMethod("get_launch_status", "Cell", function(obj) obj@launch_status)

setGeneric("set_launch_status", function(obj, launch_status) standardGeneric("set_launch_status"))
setMethod("set_launch_status", "Cell", function(obj, launch_status) {
  obj@launch_status <- launch_status
  obj
})

setGeneric("get_body_dimensions", function(obj) standardGeneric("get_body_dimensions"))
setMethod("get_body_dimensions", "Cell", function(obj) obj@body_dimensions)

setGeneric("set_body_dimensions", function(obj, body_dimensions) standardGeneric("set_body_dimensions"))
setMethod("set_body_dimensions", "Cell", function(obj, body_dimensions) {
  obj@body_dimensions <- body_dimensions
  obj
})

setGeneric("get_body_weight", function(obj) standardGeneric("get_body_weight"))
setMethod("get_body_weight", "Cell", function(obj) obj@body_weight)

setGeneric("set_body_weight", function(obj, body_weight) standardGeneric("set_body_weight"))
setMethod("set_body_weight", "Cell", function(obj, body_weight) {
  obj@body_weight <- body_weight
  obj
})

setGeneric("get_body_sim", function(obj) standardGeneric("get_body_sim"))
setMethod("get_body_sim", "Cell", function(obj) obj@body_sim)

setGeneric("set_body_sim", function(obj, body_sim) standardGeneric("set_body_sim"))
setMethod("set_body_sim", "Cell", function(obj, body_sim) {
  obj@body_sim <- body_sim
  obj
})

setGeneric("get_display_type", function(obj) standardGeneric("get_display_type"))
setMethod("get_display_type", "Cell", function(obj) obj@display_type)

setGeneric("set_display_type", function(obj, display_type) standardGeneric("set_display_type"))
setMethod("set_display_type", "Cell", function(obj, display_type) {
  obj@display_type <- display_type
  obj
})

setGeneric("get_display_size", function(obj) standardGeneric("get_display_size"))
setMethod("get_display_size", "Cell", function(obj) obj@display_size)

setGeneric("set_display_size", function(obj, display_size) standardGeneric("set_display_size"))
setMethod("set_display_size", "Cell", function(obj, display_size) {
  obj@display_size <- display_size
  obj
})

setGeneric("get_display_resolution", function(obj) standardGeneric("get_display_resolution"))
setMethod("get_display_resolution", "Cell", function(obj) obj@display_resolution)

setGeneric("set_display_resolution", function(obj, display_resolution) standardGeneric("set_display_resolution"))
setMethod("set_display_resolution", "Cell", function(obj, display_resolution) {
  obj@display_resolution <- display_resolution
  obj
})

setGeneric("get_features_sensors", function(obj) standardGeneric("get_features_sensors"))
setMethod("get_features_sensors", "Cell", function(obj) obj@features_sensors)

setGeneric("set_features_sensors", function(obj, features_sensors) standardGeneric("set_features_sensors"))
setMethod("set_features_sensors", "Cell", function(obj, features_sensors) {
  obj@features_sensors <- features_sensors
  obj
})

setGeneric("get_platform_os", function(obj) standardGeneric("get_platform_os"))
setMethod("get_platform_os", "Cell", function(obj) obj@platform_os)

setGeneric("set_platform_os", function(obj, platform_os) standardGeneric("set_platform_os"))
setMethod("set_platform_os", "Cell", function(obj, platform_os) {
  obj@platform_os <- platform_os
  obj
})

# Reads the data from the CSV file
data <- read.csv("cells.csv")

# The following function accepts a column name and a value and returns the appropriate
# cleaned value. It parses the data in each Cell and returns the appropriate value, 
# based on the column name, removing any extraneous characters, or returning NA if
# the value is invalid, empty, -, or some other variation thereof.
clean_data <- function(value, column_name) {
  # Handling NA or empty values upfront
  # This immediately returns appropriate NA based on expected type for the column
  if (is.na(value) || value == "" || value == "-") {
    # Returns the appropriate NA based on expected type for the column
    if (column_name == "launch_announced") {
      return(NA_integer_) 
    } else if (column_name == "body_weight") {
      return(NA_real_)
    } else if (column_name == "display_size") {
      return(NA_character_)
    } else if (column_name == "launch_status") {
      if (value %in% c("Discontinued", "Cancelled")) {
        return(value)
      }
    } else {
      return(NA_character_) # Generically, returns NA as a character for most sub-values.
    }
  }
  # Switches through the column, selecting based upon the column name the invalid values
  # that need to be cleaned up. Replaces said values based upon the aforesaid logic.
  switch(column_name,
    launch_announced = {
      matches <- regmatches(value, regexpr("\\b\\d{4}\\b", value)) # Matches any 4-digit year and uses RegEx to parse it
      if (length(matches) > 0 && nchar(matches[1]) == 4) {
        return(as.integer(matches[1]))
      } else {
        return(NA_integer_)
      }
    },
    body_weight = {
      return(as.numeric(sub(" .*", "", value))) # Returns the numeric part of the string
    },
    display_size = {
      matches <- regmatches(value, regexpr("^(\\d+(\\.\\d+)?) inches", value)) # Matches any number of digits and an "inches" and uses RegEx to parse it
      if (length(matches) > 0) {
        numeric_part <- as.character(matches[1])
        if (!is.na(numeric_part)) {
          return(numeric_part)
        }
      }
      return(NA_character_)  # Return NA if the format does not match or lacking an "inches"
    },
    body_sim = {
      if (value == "Yes" || value == "No") {
        return(NA_character_) # Return NA if the value is invalid, e.g., if it is Yes/No
      } else {
        return(value)
      }
    },
    launch_status = {
       # Defines a regex pattern that captures a four-digit year after "Released" or "Exp. release"
      pattern <- "(?:Released\\s|Exp\\.\\srelease\\s)(\\d{4})"
      # Finds matches for the pattern
      matches <- regmatches(value, gregexpr(pattern, value))
      
      # Checks if there are any matches
      if (length(matches) > 0 && length(matches[[1]]) > 0) {
        # Extracts the first match which should be the year
        year <- regmatches(value, regexec(pattern, value))[[1]][2]
        if (nchar(year) == 4) {
          return(year)
        }
      } else {
        return(NA_character_)
      }
    },
    features_sensors = {
      if (typeof(value) == "integer" || typeof(value) == "double") { # If the value is numerical, return NA
        return(NA_character_)
      } else {
        return(value) # Otherwise, return the value
      }
    },
    platform_os = {
      shortened_value <- sub(",.*", "", value) # Removes all after the first comma
      return(shortened_value)
    },
    # Default case
    as.character(value) # Returns the value as a character in the default case.
  )
}

create_and_clean_cells <- function(df) {
  cells_map <- new.env()  # Using an environment as an analog to a hashmap

  for (i in 1:nrow(df)) { # Iterate through each row and clean the data
    tryCatch({ # Added error handling to handle invalid data
      row_data <- lapply(names(df), function(col_name) {
        clean_data(df[i, col_name], col_name)
      })
      # Ensure row_data now contains correctly typed elements
      cell_object <- do.call(create_cell, row_data) # Creates the Cell object for each row
      key <- paste("Cell", i, sep = "_")  # Unique key for each object
      cells_map[[key]] <- cell_object # Adds the object to the map at the key
    }, error = function(e) {
      cat(sprintf("Error in row %d: %s\n", i, e$message))
    })
  }

  return(cells_map)
}

# This is the interactive version of the function that adds a new Cell object to the map.
# It is a member of the Cell class, and prompts the user for each field and reads the input. 
# If the input is invalid or empty, it repeats the prompt. The function returns the updated map.
# Runtime O(m), where m is the number of columns in data_frame.
add_cell_to_map <- function(cell_map, data_frame) {
  last_row <- nrow(data_frame) + 1
  data_frame <- rbind(data_frame, NA)
  for (column_name in names(data_frame)) {
    repeat {
      input_prompt <- paste("Enter new value for", column_name, ifelse(column_name == "display_size", "(in inches):", ":"))
      input_value <- readline(prompt = input_prompt)
      if (input_value == "") {
        cat("Input cannot be empty. Please enter a value.\n")
      } else {
        tryCatch({
          if (column_name == "display_size") {
            if (is.numeric(as.numeric(input_value))) {
              data_frame[last_row, column_name] <- paste(input_value, "inches")
              break
            } else {
              cat("Please enter a numeric value for display size.\n")
            }
          } else if (is.numeric(data_frame[[column_name]])) {
            num_value <- as.numeric(input_value)
            if (!is.na(num_value)) {
              data_frame[last_row, column_name] <- num_value
              break
            } else {
              cat("Please enter a valid numeric value.\n")
            }
          } else {
            data_frame[last_row, column_name] <- input_value
            break
          }
        }, error = function(e) {
          cat("Error during input processing: ", e$message, "\n")
        })
      }
    }
  }
  new_cell <- data_frame[last_row, ]
  next_index <- length(ls(envir = cell_map)) + 1
  key <- paste("Cell", next_index, sep = "_")
  cell_map[[key]] <- new_cell
  tryCatch({
    write.csv(data_frame, "add_example.csv", row.names = FALSE)
  }, error = function(e) {
    cat("Error writing to file: ", e$message, "\n")
  })
  return(cell_map)
}

# This is the interactive version of the function that deletes a Cell object from the map. 
# It prompts the user for the number of the cell to delete and checks if it has already been deleted.
# If it has not, it deletes the object from the map and returns the updated map.
# If it has already been deleted, it prints a message and does not delete the object.
# The function returns the updated map.
# The function is a member of the Cell class.
# Runtime O(m), where m is the number of columns in data_frame.
delete_cell_from_map <- function(cell_map, data_frame) {
  if (!exists("deleted_cells", envir = .GlobalEnv)) {
    .GlobalEnv$deleted_cells <- integer(0)  # Initialize global variable to track deleted cells
  }
  cat("\nWhen you are done, a new .csv will be created reflecting your deletions.\n")
  repeat {
    cell_number_str <- readline(prompt = "Enter the number of the cell you want to delete (q to quit): ")
    if (toupper(cell_number_str) == "Q") {
      cat("Exiting deletion process.\n")
      break
    }
    tryCatch({ # Added error handling to handle invalid input
      cell_number <- as.numeric(cell_number_str)
      if (!is.na(cell_number) && cell_number > 0 && cell_number <= nrow(data_frame)) {
        cell_key <- paste("Cell_", cell_number, sep = "")
        
        if (cell_number %in% .GlobalEnv$deleted_cells) {
          cat(paste("Cell", cell_number, "has already been deleted.\n"))
        } else if (exists(cell_key, envir = cell_map)) {
          rm(list = cell_key, envir = cell_map)  # Delete from cell_map
          .GlobalEnv$deleted_cells <- c(.GlobalEnv$deleted_cells, cell_number)  # Add to the list of deleted cells
          cat(paste("Cell with key", cell_key, "deleted successfully.\n"))
        } else {
          cat(paste("Cell with key", cell_key, "not found in the map. Please try again.\n"))
        }
      } else {
        cat("Invalid cell number. Please enter a valid numeric value.\n")
      }
    }, error = function(e) {
      cat("Error in processing input or deleting cell: ", e$message, "\n")
    })
  }
  # Safely attempts to create a modified data frame excluding the deleted cells
  tryCatch({
    if (length(.GlobalEnv$deleted_cells) > 0) {
      temp_frame <- data_frame[-.GlobalEnv$deleted_cells, ]
      write.csv(temp_frame, "updated_and_deleted.csv", row.names = FALSE)
      cat("Updated data frame saved.\n")
    } else {
      cat("No cells deleted. No changes made to the data frame.\n")
    }
  }, error = function(e) {
    cat("Error in modifying data frame or writing to file: ", e$message, "\n")
  })
  return(data_frame)  # Return the original data_frame if no deletion is performed
}

# This function calculates and outputs statistics for each column in the data frame.
# It accepts a data frame and returns a .txt file containing the most common value for
# non-numeric columns and statistics for each numeric column, comprised of the 
# minimum, maximum, average, standard deviation, and variance.
# Runtime O(m x n), where m is the number of column and n is the number of rows.
calc_stats_cells_and_output <- function(cell_data) {
  # Define the output file name
  output_file <- "stats.txt"

  # Open a connection to the output file safely
  tryCatch({
    file_conn <- file(output_file, "w")  # opens output in write mode

    # Iterates through each column to calculate and format statistics
    for (col_name in names(cell_data)) {
      column_data <- cell_data[[col_name]]
      # Writes the column name as a header for each block
      writeLines(sprintf("\n%s:", col_name), file_conn)

      # Process numeric and character data
      if (is.numeric(column_data)) {
        oem_data <- cell_data$oem  # Associate each stat with its OEM

        # Calculate statistics and find corresponding OEM for numeric columns
        min_value <- min(column_data, na.rm = TRUE)
        max_value <- max(column_data, na.rm = TRUE)
        avg_value <- mean(column_data, na.rm = TRUE)
        std_dev_value <- sd(column_data, na.rm = TRUE)
        var_value <- var(column_data, na.rm = TRUE)

        min_oem <- oem_data[which.min(column_data)]
        max_oem <- oem_data[which.max(column_data)]

        stats <- sprintf("Min: %s (oem: %s)\nMax: %s (oem: %s)\nAvg: %s\nStd Deviation: %s\nVariance: %s",
                          min_value, min_oem,
                          max_value, max_oem,
                          avg_value,
                          std_dev_value,
                          var_value)
        writeLines(stats, file_conn)
      } else if (is.character(column_data) && col_name != "oem") {
        most_common <- {
          ux <- unique(column_data[!is.na(column_data)])
          if (length(ux) == 0) NA else ux[which.max(tabulate(match(column_data, ux)))]
        }
        writeLines(sprintf("Most Common: %s", most_common), file_conn)
      }

      # Handle the OEM column specifically
      if (col_name == "oem") {
        most_common_oem <- {
          ux <- unique(column_data[!is.na(column_data)])
          if (length(ux) == 0) NA else ux[which.max(tabulate(match(column_data, ux)))]
        }
        writeLines(sprintf("Most Common OEM: %s", most_common_oem), file_conn)
      }
    }

    # After all stats are written, check for phones announced and released in different years
    if ("launch_announced" %in% names(cell_data) && "launch_status" %in% names(cell_data)) {
    # Find the indices where launch year is different from the status year
      different_years_indices <- which(as.character(cell_data$launch_announced) != as.character(cell_data$launch_status))
      if (length(different_years_indices) > 0) {
          writeLines("\nDifferences found in announced/release years:", file_conn)
          for (i in different_years_indices) {
              oem <- cell_data$oem[i]
              model <- cell_data$model[i]
              launch_announced <- cell_data$launch_announced[i]
              launch_status <- cell_data$launch_status[i]
              writeLines(sprintf("oem: %s\nmodel: %s\nlaunch_announced: %s\nlaunch_status: %s\n", oem, model, launch_announced, launch_status), file_conn)
          }
      } else {
          writeLines("\nNo differences found between launch announced and launch status years.", file_conn)
      }
    }
    if ("features_sensors" %in% names(cell_data)) {
      # Count entries with only one feature sensor (no comma present)
      single_feature_sensor_count <- sum(!grepl(",", cell_data$features_sensors))
      # Count entries with more than one feature sensor (comma present)
      multiple_feature_sensor_count <- sum(grepl(",", cell_data$features_sensors))
      writeLines(sprintf("\nCount of phones with only one feature/sensor: %d", single_feature_sensor_count), file_conn)
      writeLines(sprintf("Count of phones with more than one feature/sensor: %d", multiple_feature_sensor_count), file_conn)
    }
    if ("launch_status" %in% names(cell_data)) {
    # Ensure launch_status is treated as a numeric year for comparison
    cell_data$launch_status <- as.numeric(as.character(cell_data$launch_status))
    launch_years <- cell_data$launch_status

    # Filter years after 1999 and count the occurrences
    post_1999_counts <- table(launch_years[!is.na(launch_years) & launch_years > 1999])
    if (length(post_1999_counts) > 0) {
        year_most_launches <- as.integer(names(which.max(post_1999_counts)))
        num_launches <- max(post_1999_counts)
        
        writeLines(sprintf("\nYear with the most phones launched after 1999: %d --> (Number of phones launched: %d)", year_most_launches, num_launches), file_conn)
    } else {
        writeLines("\nNo phones were launched after 1999 in the dataset.", file_conn)
    }
  }
    if ("body_weight" %in% names(cell_data)) {
      cell_data <- cell_data[!is.na(cell_data$body_weight), ]

      # Initialize a list to hold the sum of weights and count of phones for each OEM
      weights_sum_and_count <- list()

      # Iterate over each cell to aggregate weights by OEM
      for (i in seq_along(cell_data$body_weight)) {
        weight <- cell_data$body_weight[i]
        oem <- cell_data$oem[i]

        # Next line if weight is NA or not a number
        if (is.na(weight) || !is.numeric(weight)) next

        # If the OEM key doesn't exist, initialize it
        if (!oem %in% names(weights_sum_and_count)) {
          weights_sum_and_count[[oem]] <- list(sum = 0, count = 0)
        }

        # Aggregate the total weight and increment the count
        weights_sum_and_count[[oem]]$sum <- weights_sum_and_count[[oem]]$sum + weight
        weights_sum_and_count[[oem]]$count <- weights_sum_and_count[[oem]]$count + 1
      }

      # Calculate the average weight for each OEM
      average_weights <- unlist(lapply(weights_sum_and_count, function(x) x$sum / x$count))
      names(average_weights) <- names(weights_sum_and_count)

      # Find the OEM with the highest average weight
      highest_average_index <- which.max(average_weights)
      highest_average <- names(weights_sum_and_count)[highest_average_index]
      highest_avg_weight <- average_weights[highest_average_index]

      # Write the average weight for each OEM
      writeLines("\nAverage body weight for each OEM:", file_conn)
      for (oem in names(average_weights)) {
        writeLines(sprintf("%s: %#.2f", oem, average_weights[oem]), file_conn)
      }

      # Write the OEM with the highest average weight
      output_line <- sprintf("\nOEM with the highest average body weight: %s (Average weight: %#.2f)", highest_average, highest_avg_weight)
      writeLines(output_line, file_conn)
    }
  # Closes the file connection inside tryCatch to ensure it always gets closed
    close(file_conn)
  }, error = function(e) {
    cat("An error occurred: ", e$message, "\n")
    # Close the file connection in case it's still open when an error occurs
    try(close(file_conn), silent = TRUE)
  })
}

# This function calculates and outputs the number of unique values for each column in the data frame.
# It accepts a data frame and returns a .txt file containing the number of unique values
# for each column.
# Runtime O(m x n), where m is the number of columns in data_frame, and n is the number of rows in data_frame.
calculate_unique_values_and_output <- function(data) {
  unique_counts <- sapply(data, function(col) length(unique(na.omit(col))))

  # Open a connection to the output file and
  # catch any errors in opening/creating the
  # output file
  tryCatch({
      output_file <- "uniquevals.txt"
      file_conn <- file(output_file, "w")
  }, error = function(e) {
    cat("Error: ", e$message, "\n")
  })
  # Write unique values to the file with fixed-width formatting
  cat(sprintf("%-20s %-20s\n", "Column Name", "Unique Values"), file = file_conn)
  for (i in seq_along(unique_counts)) {
    cat(sprintf("%-20s %-20s\n", names(unique_counts)[i], unique_counts[i]), file = file_conn, append = TRUE)
  }
  # Close the file connection
  close(file_conn)
}

# This function prints the unique values for each column in the data frame.
# It accepts a data frame and returns a .txt file containing the unique values
# for each column.
# Runtime O(m x n), where m is the number of columns in data_frame, and n is the number of rows in data_frame.
print_unique_values <- function(data) {
  # Opens a connection to the output file
  tryCatch({
    output_file <- "uniquelist.txt"
    file_conn <- file(output_file, "w")
  }, error = function(e) {
    cat("Error: ", e$message, "\n")
  })
  
  # Iterates through each column to print unique values
  for (col_name in names(data)) {
    column_data <- data[[col_name]]
    
    # Write the column name as a header
    writeLines(sprintf("\n%s:", col_name), file_conn)
    
    # Get unique values and print them
    unique_values <- unique(na.omit(column_data))
    for (value in unique_values) {
      writeLines(sprintf("%s", value), file_conn)
    }
  }
  
  # Close the file connection
  close(file_conn)
}

# This function exports the data frame to a .csv file.
# Runtime O(m x n), where m is the number of columns in data_frame, and n is the number of rows in data_frame.
export_to_csv <- function(data_frame, file_name = "cleaned_data.csv") {
  write.csv(data_frame, file_name, row.names = FALSE)
}

# This function updates the data frame with user input.
# It accepts a data frame and returns the updated data frame.
# It prompts the user for the row number, column name, and new value.
# If the row number is valid, it prompts the user if they want to update the column.
# If the column name is valid, it updates the data frame with the new value.
# Finally, it returns the updated data frame.
# Runtime O(m x n), where m is the number of columns in data_frame, and n is the number of rows in data_frame.
update_data <- function(data_frame) {
  # Prompt the user for the row number
  cat("Enter the row number to update (e.g., 1 for the first row): \n")
  row_number <- as.integer(readline())

  # Check if the row number is valid (in range)
  if (row_number < 1 || row_number > nrow(data_frame)) {
    cat("Row number is out of range.\n")
    return(data_frame)
  }

  # Iterate over all columns to prompt for updates
  for (column_name in names(data_frame)) {
    current_value <- data_frame[row_number, column_name]  # Get the current value for this column
    cat("Current value for", column_name, "is", current_value, "\n")  # Print the current value
    
    # Prompt the user if they want to update this column
    update_prompt <- readline(prompt = paste("Update", column_name, "? (yes/no): "))
    
    # Pressing Enter will not update the column, and skips the prompt.
    # Pressing 'n' or 'no' will not update the column.
    # Pressing 'y' or 'yes' will update the column.
    if (tolower(update_prompt) == "yes" || tolower(update_prompt) == "y") {
      # Prompt for the new value
      new_value <- readline(prompt = paste("Enter new value for", column_name, ": "))
      
      # Update the data frame with the new value
      if (is.numeric(data_frame[[column_name]])) {
        data_frame[row_number, column_name] <- as.numeric(new_value)
      } else if (is.integer(data_frame[[column_name]])) {
        data_frame[row_number, column_name] <- as.integer(new_value)
      } else {
        data_frame[row_number, column_name] <- new_value
      }
    }
  }
  write.csv(data_frame, "updated_data.csv", row.names = FALSE) # Write the updated data frame to a .csv file
}

######################################################
# Non-interactive functions
# Runtimes are same for both non-interactive and interactive versions.
######################################################

# This is a non-interactive version of add_cell_to_map. It creates a new cell
# object and adds it to the cell_map. It also writes the updated data frame
# to a .csv file. It returns the updated cell_map.
add_cell_to_map_ni <- function(cell_map, data_frame) {
  last_row <- nrow(data_frame) + 1
  for (column_name in names(data_frame)) {
    if (is.numeric(data_frame[[column_name]])) {
      data_frame[last_row, column_name] <- 1111  # new numeric value
    } else if (column_name == "display_size") {
      data_frame[last_row, column_name] <- "new_value inches"  # specific case for display_size
    } else {
      data_frame[last_row, column_name] <- "new_value"  # new string value
    }
  }
  new_cell <- data_frame[last_row, ]  # Assuming this row can represent a Cell
  next_index <- length(ls(envir = cell_map)) + 1
  key <- paste("Cell", next_index, sep = "_")
  cell_map[[key]] <- new_cell
  write.csv(data_frame, "add_example.csv", row.names = FALSE) # Write the updated data frame to a .csv file
  return(cell_map)
}

# This is a non-interactive version of delete_cell_from_map. It deletes the
# first cell from the cell_map and the data_frame. It also prints the first 5
# rows of the data_frame, but each row vertically. It returns the updated
# cell_map.

delete_cell_from_map_ni <- function(cell_map, data_frame) {
  # Delete the first cell
  cell_key <- "Cell_1"
  if (exists(cell_key, envir = cell_map)) {
    rm(list = cell_key, envir = cell_map)  # Delete from cell_map
    if (nrow(data_frame) > 0) {
      data_frame <- data_frame[-1, ]  # Delete the first row from data_frame
      cat("Cell with key", cell_key, "deleted successfully.\n")
    }
  } else {
    cat("Cell with key", cell_key, "not found.\n")
  }

  # Print the first 5 rows of the data_frame, but each row vertically
  output <- character()  # Initialize an empty character vector
  for (i in 1:min(5, nrow(data_frame))) { # Print only the first 5 rows
    output <- c(output, "\n====================\n")
    output <- c(output, sprintf("Row %d:\n", i))
    output <- c(output, "====================\n")
    output <- c(output, capture.output(print(t(data_frame[i, , drop = FALSE]))))
  }
  cat("Printing the first 5 cells in the map, each row vertically:\n")
  cat(output, sep = "\n")
}

reset_deletion_tracking <- function() {
  if (exists("deleted_cells", envir = .GlobalEnv)) {
    rm(deleted_cells, envir = .GlobalEnv)  # Remove the variable from the global environment
    cat("Deletion tracking has been reset.\n")
  } else {
    cat("No deletion tracking to reset.\n")
  }
}

# This is a non-interactive version of update_data. It updates the data_frame
# with new values. It also prints the first 5 rows of the data_frame, but
# each row vertically. It returns the updated data_frame.
update_data_ni <- function(data_frame) {
  row_number <- 1 # Select the first row to update
  updated_value <- "new_value" # Set the updated character value to "new_value"
  updated_numeric <- 9999 # Set the updated numeric value to 9999
  
  # Iterate over all columns to automatically perform updates
  for (column_name in names(data_frame)) {
    if (is.numeric(data_frame[[column_name]])) {
      data_frame[row_number, column_name] <- updated_numeric
    } else if (column_name == "display_size") {
      data_frame[row_number, column_name] <- "new_value inches" # specific case for display_size
    } else {
      data_frame[row_number, column_name] <- updated_value
    }
  }
  
  # Print the first 5 rows of the data_frame, but each row vertically
  cat("Printing the first 5 cells in the map, each row vertically:\n")
  for (i in 1:min(5, nrow(data_frame))) {
    cat("\n====================\n")
    cat(sprintf("Row %d:\n", i))
    cat("====================\n")
    print(t(data_frame[i, , drop = FALSE]))
  }
  write.csv(data_frame, "updated_data_ni.csv", row.names = FALSE) # Write the updated data frame to a .csv file
}

#######################################################
# Menu function
#######################################################
menu <- function() {
  repeat {
    data_frame <- cell_data_combined 
    cat("\nMenu Options:\n")
    cat("1: Add cells to the end and print the new CSV\n")
    cat("2: Remove a cell from the CSV and print a new CSV\n")
    cat("3: Export the cleaned data into a new CSV\n")
    cat("4: Update specified data and print a new CSV\n")
    cat("5: Calculate statistics\n")
    cat("6: Calculate unique values and give a descriptive .txt file\n")
    cat("7: Print the unique values to a file\n")
    cat("8: Exit (or press 'q'/'Q' to quit)\n")
    choice_input <- readline(prompt = "Enter your choice: ")
    if (toupper(choice_input) == "Q") {
      cat("Exiting the program.\n")
      break
    }
    if (choice_input %in% c("1", "2", "3", "4", "5", "6", "7", "8")) {
      choice <- as.integer(choice_input)
    } else {
      cat("Invalid input. Please enter a number between 1 and 8, or 'q'/'Q' to quit.\n")
      next
    }
    switch(as.character(choice),
      `1` = cell_map <- add_cell_to_map(cell_objects_map, data_frame),
      `2` = {
        cell_map <- delete_cell_from_map(cell_objects_map, data_frame)
        reset_deletion_tracking()
      },
      `3` = export_to_csv(data_frame),
      `4` = data_frame <- update_data(data_frame),
      `5` = calc_stats_cells_and_output(data_frame),
      `6` = calculate_unique_values_and_output(data_frame),
      `7` = print_unique_values(data_frame),
      `8` = break
    )
  }
}

#############################################################
# UNIT TESTS -- UNCOMMENT THESE TO RUN THEM
# NOTE, THESE REQUIRE INTERACTIVE MODE 
# CANNOT BE RUN IN REPL.IT DUE TO REQUIREMENTS IN R.PACKAGES
# SEE ABOVE AT TOP OF PROGRAM
#############################################################

# Unit test #1 : Determines if the CSV is read correctly
# test_that("File is not empty", {
#    data <- read.csv("cells.csv")
#    expect_true(nrow(data) == 1000)
# })

# Unit test #2 : Determines if the data types are correct in the objects
# test_that("Object data types", {
#  data <- read.csv("cells.csv")
#  cell_map <- create_and_clean_cells(data)
#  for (key in names(cell_map)) {
#    expect_true(is.character(slot(cell_map[[key]], "oem")), "oem should be character")
#    expect_true(is.character(slot(cell_map[[key]], "model")), "model should be character")
#    expect_true(is.integer(slot(cell_map[[key]], "launch_announced")), "launch_announced should be integer")
#    expect_true(is.character(slot(cell_map[[key]], "launch_status")), "launch_status should be character")
#    expect_true(is.character(slot(cell_map[[key]], "body_dimensions")), "body_dimensions should be character")
#    expect_true(is.numeric(slot(cell_map[[key]], "body_weight")), "body_weight should be numeric")
#    expect_true(is.character(slot(cell_map[[key]], "body_sim")), "body_sim should be character")
#    expect_true(is.character(slot(cell_map[[key]], "display_type")), "display_type should be character")
#    expect_true(is.character(slot(cell_map[[key]], "display_size")), "display_size should be character")
#    expect_true(is.character(slot(cell_map[[key]], "display_resolution")), "display_resolution should be character")
#    expect_true(is.character(slot(cell_map[[key]], "features_sensors")), "features_sensors should be character")
#    expect_true(is.character(slot(cell_map[[key]], "platform_os")), "platform_os should be character")
#  }
# })

# Unit test #3 : Determines if the NA values are correct and if data was cleaned properly
# test_that("NA test", {
#  data <- read.csv("cells.csv")
#  cell_map <- create_and_clean_cells(data)
#  for (key in names(cell_map)) {
#    expect_true(is.character(slot(cell_map[[key]], "oem")) || is.na(slot(cell_map[[key]], "oem")), "oem should be character")
#    expect_true(is.character(slot(cell_map[[key]], "model")) || is.na(slot(cell_map[[key]], "model")), "model should be character")
#    expect_true(is.integer(slot(cell_map[[key]], "launch_announced")) || is.na(slot(cell_map[[key]], "launch_announced")), "launch_announced should be integer or NA")
#    expect_true(is.character(slot(cell_map[[key]], "launch_status")) || is.na(slot(cell_map[[key]], "launch_status")), "launch_status should be character")
#    expect_true(is.character(slot(cell_map[[key]], "body_dimensions")) || is.na(slot(cell_map[[key]], "body_dimensions")), "body_dimensions should be character")
#    expect_true(is.numeric(slot(cell_map[[key]], "body_weight")) || is.na(slot(cell_map[[key]], "body_weight")), "body_weight should be numeric or NA")
#    expect_true(is.character(slot(cell_map[[key]], "body_sim")) || is.na(slot(cell_map[[key]], "body_sim")), "body_sim should be character")
#    expect_true(is.character(slot(cell_map[[key]], "display_type")) || is.na(slot(cell_map[[key]], "display_type")), "display_type should be character")
#    expect_true(is.character(slot(cell_map[[key]], "display_size")) || is.na(slot(cell_map[[key]], "display_size")), "display_size should be character")
#    expect_true(is.character(slot(cell_map[[key]], "display_resolution")) || is.na(slot(cell_map[[key]], "display_resolution")), "display_resolution should be character")
#    expect_true(is.character(slot(cell_map[[key]], "features_sensors")) || is.na(slot(cell_map[[key]], "features_sensors")), "features_sensors should be character")
#    expect_true(is.character(slot(cell_map[[key]], "platform_os")) || is.na(slot(cell_map[[key]], "platform_os")), "platform_os should be character")
#  }
# })

# Unit test #4 : Tests the add_cell function
# test_that("Adding test", {
#   data <- read.csv("cells.csv")
#   cell_map <- create_and_clean_cells(data)
#   cell_data_combined <- do.call(rbind, cell_data)
#   add_cell_to_map_ni(cell_map, cell_data_combined)
#   expect_true(length(ls(envir = cell_map)) == 1001)
# })

# Unit test #5 : Tests the delete_cell function
# test_that("Deleting test", {
#   data <- read.csv("cells.csv")
#   cell_map <- create_and_clean_cells(data)
#   cell_data_combined <- do.call(rbind, cell_data)
#   delete_cell_from_map_ni(cell_map, cell_data_combined)
#   expect_true(length(ls(envir = cell_map)) == 999)
# })

# Unit test #6 : Tests the export_to_csv function
# test_that ("Export test", {
#   data <- read.csv("cells.csv")
#   cell_map <- create_and_clean_cells(data)
#   cell_data_combined <- do.call(rbind, cell_data)
#   export_to_csv(cell_data_combined)
#   expect_true(file.exists("cleaned_data.csv"))
# })

# Unit test #7 : Tests the update functionality (non-interactive)
# test_that ("Update test", {
#   data <- read.csv("cells.csv")
#   cell_map <- create_and_clean_cells(data)
#   # Here we recreate cell_data_combined
#   sorted_keys <- names(cell_map)[order(as.numeric(sub("Cell_", "", names(cell_map))))]
#   cell_data <- lapply(sorted_keys, function(key) {
#     cell <- cell_map[[key]]
#     if (class(cell) == "Cell") {
#       data.frame(
#         oem = get_oem(cell),
#         model = get_model(cell),
#         launch_announced = get_launch_announced(cell),
#         launch_status = get_launch_status(cell),
#         body_dimensions = get_body_dimensions(cell),
#         body_weight = get_body_weight(cell),
#         body_sim = get_body_sim(cell),
#         display_type = get_display_type(cell),
#         display_size = get_display_size(cell),
#         display_resolution = get_display_resolution(cell),
#         features_sensors = get_features_sensors(cell),
#         platform_os = get_platform_os(cell)
#       )
#     } else {
#       stop("The retrieved item is not a Cell object.")
#     }
#   })
#   cell_data_combined <- do.call(rbind, cell_data)
#   update_data_ni(cell_data_combined)
#   expect_true(file.exists("updated_data_ni.csv"))
# })

# Unit test #8 : Tests the statistics analyzer
# test_that("Statistics Analyzer test", {
#   data <- read.csv("cells.csv")
#   cell_map <- create_and_clean_cells(data)
#   cell_data_combined <- do.call(rbind, cell_data)
#   calc_stats_cells_and_output(cell_data_combined)
#   expect_true(file.exists("stats.txt"))
# })

# Unit test #9 : Tests the unique value calculator
# test_that("Unique Value Calculator test", {
#   data <- read.csv("cells.csv")
#   cell_map <- create_and_clean_cells(data)
#   cell_data_combined <- do.call(rbind, cell_data)
#   calculate_unique_values_and_output(cell_data_combined)
#   expect_true(file.exists("uniquevals.txt"))
# })

# # Unit test #10 : Tests the unique value printer
# test_that("Print Unique Values test", {
#   data <- read.csv("cells.csv")
#   cell_map <- create_and_clean_cells(data)
#   cell_data_combined <- do.call(rbind, cell_data)
#   print_unique_values(cell_data_combined)
#   expect_true(file.exists("uniquelist.txt"))
# })

#######################################################
# Main program starts here -- R has no "main" function
#######################################################

cell_objects_map <- create_and_clean_cells(data) # Create and clean the cell objects map

# Sort the keys based on the numeric part of the key
sorted_keys <- names(cell_objects_map)[order(as.numeric(sub("Cell_", "", names(cell_objects_map))))]

# Extract slot values from each Cell object using the sorted keys
cell_data <- lapply(sorted_keys, function(key) {
  # Retrieve the actual Cell object from the map using the key
  cell <- cell_objects_map[[key]]
  # Check if the retrieved item is indeed a Cell object before extracting data
  if (class(cell) == "Cell") { # Added check for class
    data.frame(
      oem = get_oem(cell),
      model = get_model(cell),
      launch_announced = get_launch_announced(cell),
      launch_status = get_launch_status(cell),
      body_dimensions = get_body_dimensions(cell),
      body_weight = get_body_weight(cell),
      body_sim = get_body_sim(cell),
      display_type = get_display_type(cell),
      display_size = get_display_size(cell),
      display_resolution = get_display_resolution(cell),
      features_sensors = get_features_sensors(cell),
      platform_os = get_platform_os(cell)
    )
  } else {
    stop("The retrieved item is not a Cell object.") # Added error message for debugging/exception handling
  }
})
# Combine data frames into a single data frame
cell_data_combined <- do.call(rbind, cell_data)

#===============================================#
#     Interactive Options -- Uncomment to Run
#===============================================#

#menu() #UNCOMMENT THIS TO RUN IN INTERACTIVE MODE

#===============================================#
#  Non-Interactive Options -- Uncomment to Run
#===============================================#
cat("First, let's add some cells to the CSV at row 1001 and create a new CSV called add_example.csv.\n")
add_cell_to_map_ni(cell_objects_map, cell_data_combined)
cat("Now, let's delete the cell at row 1 and print the first 5 rows, updated.\n")
delete_cell_from_map_ni(cell_objects_map, cell_data_combined)
cat("\nNext, let's export the cleaned data to a new CSV called cleaned_data.csv.\n")
export_to_csv(cell_data_combined)
cat("\nFurther, let's update the data in cleaned CSV and create a new CSV called updated_data.csv.\n")
update_data_ni(cell_data_combined)
cat("\nContinuing, let's calculate statistics and create a new text file containing all of them called stats.txt.\n")
calc_stats_cells_and_output(cell_data_combined)
cat("\nLet's calculate unique values and create a new text file containing all of them called uniquevals.txt.\n")
calculate_unique_values_and_output(cell_data_combined)
cat("\nFinally, let's print the unique values to a new text file called uniquelist.txt.\n")
print_unique_values(cell_data_combined)
