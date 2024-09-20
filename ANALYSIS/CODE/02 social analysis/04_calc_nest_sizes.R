# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# Project: complexity paper
# Date started: 28-07-2022
# Date last modified: 02-03-2023
# Author: Simeon Q. Smeele
# Description: This script calculates the nest and tree size for each individual (if possible).
# This version renames nest to entry and includes the actual nesting structure counts (nests). 
# This version includes the 2020 data. 
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

# Import functions
.functions = sapply(list.files(path_functions, pattern = '*R', full.names = T), source)

# Load data 
nesting_data_20 = read.csv2(path_nest_entry_overview_20, na.string = '')
nest_per_ind_20 = read.csv2(path_nest_per_ind_20, na.string = '')
nesting_data_21 = read.csv2(path_nest_entry_overview_21, na.string = '')
nest_per_ind_21 = read.csv2(path_nest_per_ind_21, na.string = '')

# Test if individuals are assigned to more than one nest
table_20 = nesting_data_20$birds |> strsplit(', ') |> unlist() |> na.omit() |> table()
if(any(table_20[!names(table_20) %in% c('un', 'stump')] > 1)) stop('Individuals cross assigned.')
table_21 = nesting_data_21$birds |> strsplit(', ') |> unlist() |> na.omit() |> table()
if(any(table_21[!names(table_21) %in% c('un', 'stump')] > 1)) stop('Individuals cross assigned.')

# For each tree count number of assigned birds and add untagged
tree_counts_20 = data.frame(tree = unique(nesting_data_20$tree))
tree_counts_20$n = sapply(tree_counts_20$tree, function(tree){
  birds = nesting_data_20$birds[nesting_data_20$tree == tree] %>% 
    na.omit %>%
    strsplit(', ') %>% 
    unlist
  return(length(birds))
})
tree_counts_20$n = sapply(tree_counts_20$tree, function(tree){
  birds = nesting_data_20$birds[nesting_data_20$tree == tree] %>% 
    na.omit %>%
    strsplit(', ') %>% 
    unlist
  return(length(birds))
})
nesting_data_20$tree_nest = paste(nesting_data_20$tree, nesting_data_20$nest)
nest_counts_20 = data.frame(nest = unique(nesting_data_20$tree_nest))
nest_counts_20$n = sapply(nest_counts_20$nest, function(nest){
  birds = nesting_data_20$birds[nesting_data_20$tree_nest == nest] %>% 
    na.omit %>%
    strsplit(', ') %>% 
    unlist
  return(length(birds))
})
nesting_data_20$tree_nest_entry = paste(nesting_data_20$tree, nesting_data_20$nest, nesting_data_20$entry)
entry_counts_20 = data.frame(entry = unique(nesting_data_20$tree_nest_entry))
entry_counts_20$n = sapply(entry_counts_20$entry, function(entry){
  birds = nesting_data_20$birds[nesting_data_20$tree_nest_entry == entry] %>% 
    na.omit %>%
    strsplit(', ') %>% 
    unlist
  return(length(birds))
})
tree_counts_21 = data.frame(tree = unique(nesting_data_21$tree))
tree_counts_21$n = sapply(tree_counts_21$tree, function(tree){
  birds = nesting_data_21$birds[nesting_data_21$tree == tree] %>% 
    na.omit %>%
    strsplit(', ') %>% 
    unlist
  return(length(birds))
})
tree_counts_21$n = sapply(tree_counts_21$tree, function(tree){
  birds = nesting_data_21$birds[nesting_data_21$tree == tree] %>% 
    na.omit %>%
    strsplit(', ') %>% 
    unlist
  return(length(birds))
})
nesting_data_21$tree_nest = paste(nesting_data_21$tree, nesting_data_21$nest)
nest_counts_21 = data.frame(nest = unique(nesting_data_21$tree_nest))
nest_counts_21$n = sapply(nest_counts_21$nest, function(nest){
  birds = nesting_data_21$birds[nesting_data_21$tree_nest == nest] %>% 
    na.omit %>%
    strsplit(', ') %>% 
    unlist
  return(length(birds))
})
nesting_data_21$tree_nest_entry = paste(nesting_data_21$tree, nesting_data_21$nest, nesting_data_21$entry)
entry_counts_21 = data.frame(entry = unique(nesting_data_21$tree_nest_entry))
entry_counts_21$n = sapply(entry_counts_21$entry, function(entry){
  birds = nesting_data_21$birds[nesting_data_21$tree_nest_entry == entry] %>% 
    na.omit %>%
    strsplit(', ') %>% 
    unlist
  return(length(birds))
})

# Compile list of all inds and tree size plus nest size
nesting_sizes_20 = data.frame(ind = nest_per_ind_20$id)
nesting_sizes_20$entry_count = sapply(nesting_sizes_20$ind, function(ind){
  n = entry_counts_20$n[which(entry_counts_20$entry == 
                                nest_per_ind_20$nest_manual[which(nest_per_ind_20$id == ind)])]
  return(ifelse(length(n) == 0, NA, n))
})
nesting_sizes_20$nest_count = sapply(nesting_sizes_20$ind, function(ind){
  nest = nest_per_ind_20$nest_manual[which(nest_per_ind_20$id == ind)] %>% strsplit(' ') %>% 
    sapply(`[`, 1:2) %>% unlist %>% paste(collapse = ' ')
  n = nest_counts_20$n[which(nest_counts_20$nest == nest)]
  return(ifelse(length(n) == 0, NA, n))
})
nesting_sizes_20$tree_count = sapply(nesting_sizes_20$ind, function(ind){
  n = tree_counts_20$n[which(tree_counts_20$tree == 
                               nest_per_ind_20$tree_manual[which(nest_per_ind_20$id == ind)])]
  return(ifelse(length(n) == 0, NA, n))
})
nesting_sizes_21 = data.frame(ind = nest_per_ind_21$id)
nesting_sizes_21$entry_count = sapply(nesting_sizes_21$ind, function(ind){
  n = entry_counts_21$n[which(entry_counts_21$entry == 
                                nest_per_ind_21$nest_manual[which(nest_per_ind_21$id == ind)])]
  return(ifelse(length(n) == 0, NA, n))
})
nesting_sizes_21$nest_count = sapply(nesting_sizes_21$ind, function(ind){
  nest = nest_per_ind_21$nest_manual[which(nest_per_ind_21$id == ind)] %>% str_sub(1, 5)
  n = nest_counts_21$n[which(nest_counts_21$nest == nest)]
  return(ifelse(length(n) == 0, NA, n))
})
nesting_sizes_21$tree_count = sapply(nesting_sizes_21$ind, function(ind){
  n = tree_counts_21$n[which(tree_counts_21$tree == 
                               nest_per_ind_21$tree_manual[which(nest_per_ind_21$id == ind)])]
  return(ifelse(length(n) == 0, NA, n))
})

# Save
save(nesting_sizes_20, nesting_sizes_21, file = path_nesting_sizes)
message('Saved all nesting sizes!')

