# ---- Load ----
library(tidyverse)
library(sf)
library(geographr)

# Data must be extracted from Stat-Xplore. It is stored locally on disk and
# appended to .gitignore.
raw <- read_csv("data/on-disk/carers-allowance.R")
