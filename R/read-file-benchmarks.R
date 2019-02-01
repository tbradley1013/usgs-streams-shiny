#===============================================================================
# Testing the difference in time between loading a rds file compared to a 
# featehr file
# 
# Tyler Bradley
# 2019-01-17 
#===============================================================================

library(tidyverse)
library(feather)
library(microbenchmark)

microbenchmark(
  rds = read_rds("data/usgs-stream-sites.rds"),
  feather = read_feather("data/usgs-stream-sites.feather")
)

# Feather is ~10x faster