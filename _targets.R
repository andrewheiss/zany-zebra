# Packages required to define the pipeline:
library(targets)
library(tarchetypes)
suppressPackageStartupMessages(library(dplyr))


# General options
options(tidyverse.quiet = TRUE,
        dplyr.summarise.inform = FALSE)


set.seed(126942)  # From random.org


# Global target options
tar_option_set(
  packages = c("tidyverse"),  # Packages available to all targets
  format = "qs",  # Storage format
  workspace_on_error = TRUE  # Automatically create a debug workspace on errors
)


# here::here() returns an absolute path, which then gets stored in tar_meta and
# becomes computer-specific (i.e. /Users/andrew/Research/blah/thing.Rmd).
# There's no way to get a relative path directly out of here::here(), but
# fs::path_rel() works fine with it (see
# https://github.com/r-lib/here/issues/36#issuecomment-530894167)
here_rel <- function(...) {fs::path_rel(here::here(...))}


# Load all the scripts in the R/ folder that contain the functions to be used in
# the pipeline
tar_source()


# Pipeline ----------------------------------------------------------------
list(
  ## Raw data files ----
  tar_target(acs_vars_file,
             here_rel("data", "manual-data", "acs_vars.csv"),
             format = "file"),
  
  ## Process and clean data ----
  tar_target(acs_clean, clean_acs_data(acs_vars_file)),
  
  ## Analysis notebook ----
  tar_quarto(website, path = ".", quiet = FALSE),
  tar_target(deploy_script, here_rel("deploy.sh"), format = "file"),
  tar_target(deploy, {
    # Force a dependency
    website
    # Run the deploy script
    if (Sys.getenv("UPLOAD_WEBSITES") == "TRUE") processx::run(paste0("./", deploy_script))
  })
)
