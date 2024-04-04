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
      matches <- regmatches(value, regexpr("\\b\\d{4}\\b", value)) # Matches any 4-digit year and uses RegEx to parse it
      if (length(matches) > 0 && nchar(matches[1]) == 4) {
        return(as.character(matches[1]))
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
calc_stats_cells_and_output <- function(cell_data) {
  # Define the output file name
  output_file <- "stats.txt"
  # Open a connection to the output file safely
  tryCatch({
    file_conn <- file(output_file, "w") # opens output in write mode
    # Iterates through each column to calculate and format statistics
    for (col_name in names(cell_data)) {
      column_data <- cell_data[[col_name]]
      # Writes the column name as a header for each block
      writeLines(sprintf("\n%s:", col_name), file_conn)
      # Checks if the column is numeric or character and write statistics accordingly
      if (is.numeric(column_data)) {
        stats <- sprintf("Min: %s\nMax: %s\nAvg: %s\nStd Deviation: %s\nVariance: %s",
                          min(column_data, na.rm = TRUE),
                          max(column_data, na.rm = TRUE),
                          mean(column_data, na.rm = TRUE),
                          sd(column_data, na.rm = TRUE),
                          var(column_data, na.rm = TRUE))
        writeLines(stats, file_conn)
      } else if (is.character(column_data)) {
        most_common <- {
          ux <- unique(column_data[!is.na(column_data)])
          if (length(ux) == 0) NA else ux[which.max(tabulate(match(column_data, ux)))]
        }
        writeLines(sprintf("Most Common: %s", most_common), file_conn)
      }
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
print_unique_values <- function(data) {
  # Opens a connection to the output file
  tryCatch({
    output_file <- "uniquevals.txt"
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
