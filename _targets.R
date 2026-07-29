# Load packages required to define the pipeline:
library(targets)
library(tarchetypes)

# Set target options:
tar_option_set(
  format = "qs"
)

# Run the R scripts in the R/ folder with your custom functions:
tar_source('R/')

# targets pipeline
c(
  tar_file_read(
  	gentry_raw,
  	'input/gentry_transects.csv',
  	read.csv(!!.x)
  ),

  tar_file_read(
  	birds_raw,
  	'input/breeding_birds.csv',
  	read.csv(!!.x)
  ),

  tar_file_read(
  	quadrats_raw,
  	'input/quadrats.csv',
  	read.csv(!!.x)
  ),

  tar_target(
  	bird_div,
  	calc_bird_div(birds_raw)
  )


)
