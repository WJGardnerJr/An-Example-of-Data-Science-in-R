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
