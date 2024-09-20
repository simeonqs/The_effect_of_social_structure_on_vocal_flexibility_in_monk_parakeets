# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# Project: complexity paper
# Date started: 21-04-2022
# Date last modified: 02-03-2023
# Author: Simeon Q. Smeele
# Description: This script loads the ringing data and retrieves age estimates from that. For now assuming 
# hatching occurs on 1st of July for all individuals. 
# TO-DO:
# Need to create a distribution later for hatching.
# Need to check hatch date for ringed chicks. 
# This version is independent of year, just summarises all ages from ringing data. 
# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

# Loading libraries
libraries = c('tidyverse', 'readxl')
for(lib in libraries){
  if(! lib %in% installed.packages()) lapply(lib, install.packages)
  lapply(libraries, require, character.only = TRUE)
}

# Clean R
rm(list=ls()) 

# Paths
source('ANALYSIS/CODE/paths.R')

# Load data 
ring_dat = read_xlsx(path_ringing_data, guess_max = 10000) %>% as.data.frame()
slim_ring = ring_dat[,c('MEDALLA', 'DATA', 'ED SEG')]

# Fill in NAs
slim_ring$`ED SEG` = ifelse(is.na(slim_ring$`ED SEG`), 'U', slim_ring$`ED SEG`)

# Get estimates
inds = unique(na.omit(ring_dat$MEDALLA))
first_ringing = sapply(inds, function(ind){
  as.character(as.Date(min(ring_dat$DATA[which(ring_dat$MEDALLA == ind)])))
})

# Start data frame to save data
out = data.frame(ind = inds, 
                 hatch_known = NA,
                 hatch_min = NA,
                 hatch_max = NA)

# Run through all individuals
for(ind in inds){
  
  # Subset for that ind
  sub_dat = unique(slim_ring[which(ring_dat$MEDALLA == ind),])
  
  # Find min hatch
  ## if record for P -> chick, then take the ringing data as hatch date
  if(any(sub_dat$`ED SEG` == 'P')){
    out$hatch_known[out$ind == ind] = 
      out$hatch_max[out$ind == ind] = 
      out$hatch_min[out$ind == ind] = as.character(min(sub_dat$DATA[sub_dat$`ED SEG` == 'P']))
    next
  } 
  ## if not, but record for J -> juvenile, then take the 1st of July before that record as hatch date
  ## if record before May, take the previous year, else go back to July in same year
  if(any(sub_dat$`ED SEG` == 'J')){
    first_ringing = min(sub_dat$DATA[sub_dat$`ED SEG` == 'J'])
    if(as.numeric(str_sub(first_ringing, 6, 7)) < 05) 
      hatch_date = paste0(as.numeric(str_sub(first_ringing, 1, 4)) - 1, '-07-01') else 
        hatch_date = paste0(str_sub(first_ringing, 1, 4), '-07-01')
      ## check if there was a record before the hatch date
      if(any(sub_dat$DATA < as.Date(hatch_date))) stop('Record before hatch!')
      out$hatch_known[out$ind == ind] = 
        out$hatch_max[out$ind == ind] = 
        out$hatch_min[out$ind == ind] = hatch_date
      next
  } 
  ## if not, but record for A -> adult, then there are two options:
  ## when recorded in JUL-OCT the individual is at least a year and a few months
  ## when recorded in any other month we can't be sure, and the maximum should be the 1st of July before
  ## that date
  first_ringing = min(sub_dat$DATA)
  month = str_sub(first_ringing, 6, 7) %>% as.numeric
  year = str_sub(first_ringing, 1, 4) %>% as.numeric
  if(sub_dat$`ED SEG`[sub_dat$DATA == first_ringing] == 'A' & 
     month > 6 & month < 11) hatch_date = paste0(year-1, '-07-01') else
       if(as.numeric(str_sub(first_ringing, 6, 7)) < 07) hatch_date = paste0(year-1, '-07-01') else
         hatch_date = paste0(year, '-07-01')
  out$hatch_known[out$ind == ind] = 
    out$hatch_max[out$ind == ind] = 
    out$hatch_min[out$ind == ind] = hatch_date
  
} # end ind loop

# Save
age_dat = out
save(age_dat, file = path_age_dat)
message('Saved age_dat.')