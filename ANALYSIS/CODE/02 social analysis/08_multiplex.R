# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# Project: complexity paper
# Date started: 25-02-2023
# Date last modified: 02-03-2023
# Author: Simeon Q. Smeele
# Description: Compiling the multiplex from foraging, spatial, mate, tolerance and aggressive networks.  
# This version includes the 2020 data. 
# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

# Loading libraries
libraries = c('tidyverse', 'sna', 'igraph')
for(lib in libraries){
  if(! lib %in% installed.packages()) lapply(lib, install.packages)
  lapply(lib, require, character.only = TRUE)
}

# Clean R
rm(list=ls()) 
set.seed(1)

# Paths
source('ANALYSIS/CODE/paths.R')

# Import functions
.functions = sapply(list.files(path_functions, pattern = '*R', full.names = T), source)

# Load data
load(path_foraging_network)
load(path_mate_network)
load(path_tolerance_network)
load(path_aggressive_network)

## 2020 ##

# Reformat networks
for_net = ceiling(foraging_network_20)
mate_net = mate_network_20
tol_net = ceiling(tolerance_network_20/1e6)
agg_net = aggressive_network_20
for(i in seq_len(nrow(agg_net))) 
  for(j in seq_len(ncol(agg_net))) 
    agg_net[i,j] = agg_net[j,i] = ceiling(sum(agg_net[i,j], agg_net[j,i])/1e6)

# Print sample sizes
message(sprintf('2020 - foraging: %s, mate: %s, tolerance: %s, aggressive: %s', 
                nrow(for_net), nrow(mate_net), nrow(tol_net), nrow(agg_net)))

# Plot networks
pdf(path_pdf_networks_20, 7, 7)
par(mfrow = c(2, 2), mar = c(0, 0, 1, 0))
gplot(for_net, gmode = 'graph', label = NULL, mode = 'kamadakawai',
      main = 'a)                                                         ')
gplot(mate_net, gmode = 'graph', label = NULL, 
      main = 'b)                                                         ')
gplot(tol_net, gmode = 'graph', label = NULL, 
      main = 'c)                                                         ')
gplot(agg_net, gmode = 'graph', label = NULL, 
      main = 'd)                                                         ')
dev.off()

# Function to calculate degree versatility (just summed degree really)
calc.dv = function(ind){
  dv = 0
  if(ind %in% rownames(for_net)) dv = dv + sum(for_net[ind,], na.rm = TRUE)
  if(ind %in% rownames(mate_net)) dv = dv + sum(mate_net[ind,], na.rm = TRUE)
  if(ind %in% rownames(tol_net)) dv = dv + sum(tol_net[ind,], na.rm = TRUE)
  if(ind %in% rownames(agg_net)) dv = dv + sum(agg_net[ind,], na.rm = TRUE)
  return(dv)
}

# Calculate degree across networks
multi_out_20 = data.frame(ind = unique(c(rownames(for_net), rownames(mate_net), 
                                         rownames(tol_net), rownames(agg_net))))
multi_out_20$deg_ver = vapply(multi_out_20$ind, calc.dv, numeric(1))

## 2021 ##

# Reformat networks
for_net = ceiling(foraging_network_21)
mate_net = mate_network_21
tol_net = ceiling(tolerance_network_21/1e6)
agg_net = aggressive_network_21
for(i in seq_len(nrow(agg_net))) 
  for(j in seq_len(ncol(agg_net))) 
    agg_net[i,j] = agg_net[j,i] = ceiling(sum(agg_net[i,j], agg_net[j,i])/1e6)

# Print sample sizes
message(sprintf('2021 - foraging: %s, mate: %s, tolerance: %s, aggressive: %s', 
                nrow(for_net), nrow(mate_net), nrow(tol_net), nrow(agg_net)))

# Plot networks
pdf(path_pdf_networks_21, 7, 7)
par(mfrow = c(2, 2), mar = c(0, 0, 1, 0))
gplot(for_net, gmode = 'graph', label = NULL, mode = 'kamadakawai',
      main = 'a)                                                         ')
gplot(mate_net, gmode = 'graph', label = NULL, 
      main = 'b)                                                         ')
gplot(tol_net, gmode = 'graph', label = NULL, 
      main = 'c)                                                         ')
gplot(agg_net, gmode = 'graph', label = NULL, 
      main = 'd)                                                         ')
dev.off()

# Function to calculate degree versatility (just summed degree really)
calc.dv = function(ind){
  dv = 0
  if(ind %in% rownames(for_net)) dv = dv + sum(for_net[ind,], na.rm = TRUE)
  if(ind %in% rownames(mate_net)) dv = dv + sum(mate_net[ind,], na.rm = TRUE)
  if(ind %in% rownames(tol_net)) dv = dv + sum(tol_net[ind,], na.rm = TRUE)
  if(ind %in% rownames(agg_net)) dv = dv + sum(agg_net[ind,], na.rm = TRUE)
  return(dv)
}

# Calculate degree across networks
multi_out_21 = data.frame(ind = unique(c(rownames(for_net), rownames(mate_net), 
                                         rownames(tol_net), rownames(agg_net))))
multi_out_21$deg_ver = vapply(multi_out_21$ind, calc.dv, numeric(1))

## Save ##
save(multi_out_20, multi_out_21, file = path_multi_out)
