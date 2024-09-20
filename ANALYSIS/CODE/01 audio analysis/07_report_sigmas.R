# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# Project: complexity paper
# Date started: 08-03-2023
# Date last modified: 14-03-2023
# Author: Simeon Q. Smeele
# Description: Report the sigmas for the similarity and diversity models.  
# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

# Loading libraries
libraries = c('rethinking')
for(lib in libraries){
  if(! lib %in% installed.packages()) lapply(lib, install.packages)
  lapply(libraries, require, character.only = TRUE)
}

# Clean R
rm(list=ls()) 

# Paths
source('ANALYSIS/CODE/paths.R')

# Load post diversity
load(path_div_model_results)

# Get sigmas
ms = post_20$sigma_ind |> mean() |> round(2)
pis = post_20$sigma_ind |> PI() |> round(2)
message('Diversity:')
message(sprintf('2020 - sigma individual effect diversity: %s (%s-%s).', ms, pis[1], pis[2]))
ms = post_21$sigma_ind |> mean() |> round(2)
pis = post_21$sigma_ind |> PI() |> round(2)
message(sprintf('2021 - sigma individual effect diversity: %s (%s-%s).', ms, pis[1], pis[2]))

# Load post similarity
load(path_post_similarity)

# Get sigmas
ms = post_20$sigma_ind |> mean() |> round(2)
pis = post_20$sigma_ind |> PI() |> round(2)
message('Similarity:')
message(sprintf('2020 - sigma individual effect diversity: %s (%s-%s).', ms, pis[1], pis[2]))
ms = post_21$sigma_ind |> mean() |> round(2)
pis = post_21$sigma_ind |> PI() |> round(2)
message(sprintf('2021 - sigma individual effect diversity: %s (%s-%s).', ms, pis[1], pis[2]))

# Also report number of dyads
message('Number dyads')
message(sprintf('2020 - %s.', ncol(post_20$z_ind_pair)))
message(sprintf('2021 - %s.', ncol(post_21$z_ind_pair)))


