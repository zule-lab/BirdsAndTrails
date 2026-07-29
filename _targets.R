# Load packages required to define the pipeline:
library(targets)
library(tarchetypes)

# Set target options:
tar_option_set(
  format = "qs"
)

# Run the R scripts in the R/ folder with your custom functions:
tar_source('R/')

# Pipeline
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
  ),

  tar_target(
    SR_dist_model,
    lm(val ~ form + dist, data = bird_div %>% filter(div == "SR"))
  ),

  tar_target(
    shan_dist_model,
    lm(val ~ form + dist, data = bird_div %>% filter(div == "Shan"))
  ),

  tar_render(
    residuals,
    'graphics/diagnostics.qmd'
  ),

  tar_render(
    results,
    'output/results.qmd'
  )
)
