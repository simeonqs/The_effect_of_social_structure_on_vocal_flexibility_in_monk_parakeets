# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# Project: complexity paper
# Date started: 20-07-2022
# Date last modified: 09-03-2023
# Author: Simeon Q. Smeele
# Description: Preparing all data for: sex, degree, age and diversity 
# This version adds the classical sna measures.
# This version also has other measures such as group size. 
# This version includes the 2020 data. 
# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

# Loading libraries
libraries = c('tidyverse', 'rethinking', 'readxl')
for(lib in libraries){
  if(! lib %in% installed.packages()) lapply(lib, install.packages)
  lapply(libraries, require, character.only = TRUE)
}

# Clean R
rm(list=ls()) 

# Paths
source('ANALYSIS/CODE/paths.R')

# Load data age
load(path_age_dat)
inds_age = age_dat$ind %>% unique

# Load data sex
sexing_f = read.csv(path_sexing_f)
sexing_v = read_xlsx(path_sexing_v)
sexing = na.omit(rbind(sexing_f, sexing_v))
sexing$ID = toupper(sexing$ID)
sexing$sex = toupper(sexing$sex)
inds_sex = sexing$ID %>% unique

# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
## 2020 ----
# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

# Load data diversity
load(path_div_results_slim)
inds_div = div_results_slim_20$ind

# Load nesting results
load(path_nesting_sizes)
inds_nesting = nesting_sizes_20$ind

# Load multiplex results
load(path_multi_out)

# Make master list of individuals
master_dat = data.frame(ind = unique(c(inds_div, inds_age, inds_sex, inds_nesting,
                                       multi_out_20$ind)))

# Add to master diversity
master_dat = merge(master_dat, div_results_slim_20, by = 'ind', all.x = T, all.y = F)

# Add age
age_dat$age = as.Date('2021-11-01') - as.Date(age_dat$hatch_max)
master_dat = merge(master_dat, age_dat[,c('ind', 'age')], by = 'ind', all.x = T, all.y = F)

# Add sex
master_dat = merge(master_dat, sexing, by.x = 'ind', by.y = 'ID', all.x = T, all.y = F)

# Add classical SNA measures and sample size
load(path_net_meas)
master_dat = merge(master_dat, net_meas_20, by = 'ind', all.x = T, all.y = F)

# Add degree versatility
master_dat = merge(master_dat, multi_out_20, by = 'ind', all.x = T, all.y = F)

# Add nesting counts
master_dat = merge(master_dat, nesting_sizes_20, by = 'ind', all.x = T, all.y = F)

# Subset to only sexed individuals
master_dat = master_dat[which(!is.na(master_dat$sex)),]

# Plot overview
pdf(path_pdf_data_overview_20, 10, 10)
plot(master_dat[,-1], pch = 16, col = alpha(4, 0.5))
dev.off()

# Check for duplicates
if(length(unique(master_dat$ind)) > nrow(master_dat)) warning('Duplicate IDs found in master_dat.')

# Make the average diversity and sd normally distributed
sd = sd(master_dat$average_div, na.rm = T)
master_dat$average_div = master_dat$average_div / sd
master_dat$se_div = master_dat$se_div / sd

# Store
master_dat_20 = master_dat

# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
## 2021 ----
# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

# Load data diversity
load(path_div_results_slim)
inds_div = div_results_slim_21$ind

# Load nesting results
load(path_nesting_sizes)
inds_nesting = nesting_sizes_21$ind

# Load multiplex results
load(path_multi_out)

# Make master list of individuals
master_dat = data.frame(ind = unique(c(inds_div, inds_age, inds_sex, inds_nesting,
                                       multi_out_21$ind)))

# Add to master diversity
master_dat = merge(master_dat, div_results_slim_21, by = 'ind', all.x = T, all.y = F)

# Add age
age_dat$age = as.Date('2021-11-01') - as.Date(age_dat$hatch_max)
master_dat = merge(master_dat, age_dat[,c('ind', 'age')], by = 'ind', all.x = T, all.y = F)

# Add sex
master_dat = merge(master_dat, sexing, by.x = 'ind', by.y = 'ID', all.x = T, all.y = F)

# Add classical SNA measures and sample size
load(path_net_meas)
master_dat = merge(master_dat, net_meas_21, by = 'ind', all.x = T, all.y = F)

# Add degree versatility
master_dat = merge(master_dat, multi_out_21, by = 'ind', all.x = T, all.y = F)

# Add nesting counts
master_dat = merge(master_dat, nesting_sizes_21, by = 'ind', all.x = T, all.y = F)

# Subset to only sexed individuals
master_dat = master_dat[which(!is.na(master_dat$sex)),]

# Plot overview
pdf(path_pdf_data_overview_21, 10, 10)
plot(master_dat[,-1], pch = 16, col = alpha(4, 0.5))
dev.off()

# Check for duplicates
if(length(unique(master_dat$ind)) > nrow(master_dat)) warning('Duplicate IDs found in master_dat.')

# Make the average diversity and sd normally distributed
sd = sd(master_dat$average_div, na.rm = T)
master_dat$average_div = master_dat$average_div / sd
master_dat$se_div = master_dat$se_div / sd

# Store
master_dat_21 = master_dat

# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# Save
# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

message(sprintf('2020 - age: %s, sex: %s, degree: %s, betw: %s, eig: %s, ver: %s, entry: %s, tree: %s',
                length(which(!is.na(master_dat_20$age))),
                length(which(!is.na(master_dat_20$sex))),
                length(which(!is.na(master_dat_20$degree))),
                length(which(!is.na(master_dat_20$betweenness))),
                length(which(!is.na(master_dat_20$eig_cent))),
                length(which(!is.na(master_dat_20$deg_ver))),
                length(which(!is.na(master_dat_20$entry_count))),
                length(which(!is.na(master_dat_20$tree_count)))))
message(sprintf('2021 - age: %s, sex: %s, degree: %s, betw: %s, eig: %s, ver: %s, entry: %s, tree: %s',
                length(which(!is.na(master_dat_21$age))),
                length(which(!is.na(master_dat_21$sex))),
                length(which(!is.na(master_dat_21$degree))),
                length(which(!is.na(master_dat_21$betweenness))),
                length(which(!is.na(master_dat_21$eig_cent))),
                length(which(!is.na(master_dat_21$deg_ver))),
                length(which(!is.na(master_dat_21$entry_count))),
                length(which(!is.na(master_dat_21$tree_count)))))

save(master_dat_20, master_dat_21, file = path_master_dat)
