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
