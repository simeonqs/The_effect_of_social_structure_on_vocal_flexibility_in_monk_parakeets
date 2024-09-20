# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# Project: complexity paper
# Date started: 07-02-2023
# Date last modified: 04-03-2023
# Author: Simeon Q. Smeele
# Description: Running model checks.  
# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

# Set-up ----

# Loading libraries
libraries = c('tidyverse', 'cmdstanr', 'rethinking')
for(lib in libraries){
  if(! lib %in% installed.packages()) lapply(lib, install.packages)
  lapply(libraries, require, character.only = TRUE)
}

# Clean R
rm(list=ls()) 

# Paths
source('ANALYSIS/CODE/paths.R')

# Load data
load(path_all_results_div)

# Get fits 
fits = ls()[str_detect(ls(), 'fit')]

# Check fits
for(fit in fits){
  f = get(fit)
  p = precis(f, depth = 3)
  if(any(p$Rhat4 > 1.01) | any(p$Rhat4 < 0.99)) warning(sprintf('Rhat not good for %s.', fit))
}
message('Done.')



