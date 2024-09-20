# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# Project: complexity paper
# Date started: 17-05-2023
# Date last modified: 22-05-2023
# Author: Simeon Q. Smeele
# Description: Preparing the real data for the distribution models. 
# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

# Loading libraries
libraries = c('tidyverse', 'parallel', 'warbleR')
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

# Import call type classification
source(path_call_type_classification)

## 2020 ##

# Load data 
st_20 = load.selection.tables(path_selection_tables, path_annotations = path_annotations_2020,
                              path_context = path_context)

# # Reclassify call types to 11 main types
# st_20$`call type` = vapply(st_20$`call type`, function(ct){
#   nct = names(types_include)[vapply(types_include, function(ti) ct %in% ti, logical(1))]
#   if(length(nct) == 0) return('NA') else return(nct)
# }, character(1))
# st_20 = st_20[st_20$`call type` != 'NA',]

# Remove handling and release from 2021 data
st_20 = st_20[!str_detect(st_20$file, '10_26'),]
st_20 = st_20[!str_detect(st_20$file, '10_27'),]

# Subset for individuals with enough calls
table_calls = table(st_20$bird)
inds_enough = names(table_calls[table_calls > 30])

# Compile X -> the matrix with distributions for each individual
X_20 = vapply(inds_enough, function(ind){
  sub = st_20[st_20$bird == ind,]
  vapply(names(types_include), function(type) 
    length(which(vapply(sub$`call type`, function(ct) 
      ct %in% types_include[[type]], logical(1)))), numeric(1))
}, numeric(length(types_include))) # gives numeric with rownames is types, colnames is IDs

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

# Load nesting results
load(path_nesting_sizes)
inds_nesting = nesting_sizes_20$ind

# Load multiplex results
load(path_multi_out)

# Make master list of individuals
master_dat = data.frame(ind = unique(c(inds_enough, inds_age, inds_sex, inds_nesting)))

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

# Add nest type
nest_overview = read.csv2(path_nest_overview_20, na.strings = 'NA')
nest_per_ind = read.csv2(path_nest_per_ind_20)
nest_type_data = merge(nest_per_ind, nest_overview, 
                       by.x = 'tree_manual', by.y = 'tree', all.x = T, all.y = F)
# master_dat$tree_type = sapply(master_dat$ind, function(ind){
#   y = nest_type_data$type[nest_type_data$id == ind]
#   return(ifelse(length(y) > 0, y, NA))
# })

# Add tree ID
master_dat$tree_ID = as.character(sapply(master_dat$ind, function(ind){
  y = nest_per_ind$tree_manual[nest_per_ind$id == ind]
  return(ifelse(length(y) > 0, y, NA))
}))

# Plot
pdf(path_pdf_data_overview_rep_size_20, 12, 12)
plot(master_dat[,-1], pch = 16, col = alpha(4, 0.5))
dev.off()
master_dat_20 = master_dat

## 2021 ##

# Load data 
st_21 = load.selection.tables(path_selection_tables, path_annotations_2021 = path_annotations_2021)

# # Reclassify call types to 11 main types
# st_21$`call type` = vapply(st_21$`call type`, function(ct){
#   nct = names(types_include)[vapply(types_include, function(ti) ct %in% ti, logical(1))]
#   if(length(nct) == 0) return('NA') else return(nct)
# }, character(1))
# st_21 = st_21[st_21$`call type` != 'NA',]

# Remove handling and release from 2021 data
st_21 = st_21[!str_detect(st_21$file, '10_26'),]
st_21 = st_21[!str_detect(st_21$file, '10_27'),]

# Subset for individuals with enough calls
table_calls = table(st_21$bird)
inds_enough = names(table_calls[table_calls > 30])

# Compile X -> the matrix with distributions for each individual
X_21 = vapply(inds_enough, function(ind){
  sub = st_21[st_21$bird == ind,]
  vapply(names(types_include), function(type) 
    length(which(vapply(sub$`call type`, function(ct) 
      ct %in% types_include[[type]], logical(1)))), numeric(1))
}, numeric(length(types_include))) # gives numeric with rownames is types, colnames is IDs

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

# Load nesting results
load(path_nesting_sizes)
inds_nesting = nesting_sizes_21$ind

# Load multiplex results
load(path_multi_out)

# Make master list of individuals
master_dat = data.frame(ind = unique(c(inds_enough, inds_age, inds_sex, inds_nesting)))

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

# Add nest type
nest_overview = read.csv2(path_nest_overview_21, na.strings = 'NA')
nest_per_ind = read.csv2(path_nest_per_ind_21)
nest_type_data = merge(nest_per_ind, nest_overview, 
                       by.x = 'tree_manual', by.y = 'tree', all.x = T, all.y = F)
# master_dat$tree_type = sapply(master_dat$ind, function(ind){
#   y = nest_type_data$type[nest_type_data$id == ind]
#   return(ifelse(length(y) > 0, y, NA))
# })

# Add tree ID
master_dat$tree_ID = as.character(sapply(master_dat$ind, function(ind){
  y = nest_per_ind$tree_manual[nest_per_ind$id == ind]
  return(ifelse(length(y) > 0, y, NA))
}))

# Plot
pdf(path_pdf_data_overview_rep_size_21, 12, 12)
plot(master_dat[,-1], pch = 16, col = alpha(4, 0.5))
dev.off()
master_dat_21 = master_dat

## Test if sample size depends on tree size ##
ss = sapply(master_dat_20$ind, function(ind) return(nrow(st_20[st_20$bird == ind,])))
plot(master_dat_20$tree_count[ss>30], ss[ss>30])
# ss = sapply(master_dat_21$ind, function(ind) return(nrow(st_21[st_21$bird == ind,])))
# plot(master_dat_21$tree_count[ss>30], ss[ss>30])

# Plot some raw data
par(mfrow = c(3, 3))
X = X_20
X = X[rowSums(X) > 0,]
n_cat = nrow(X)
master_dat = master_dat
rownames(master_dat) = master_dat$ind
master_dat = master_dat[colnames(X),]
for(i in seq_len(n_cat)){
  plot(master_dat$age, X[i,]/colSums(X), ylim = c(0, 1))
}

## Save ##
# message(sprintf('2020 - %s, 2021 - %s', length(which(!is.na(master_dat_20$entropy))), 
#         length(which(!is.na(master_dat_21$entropy)))))
save(master_dat_20, X_20, st_20,
     master_dat_21, X_21, st_21,
     file = path_master_dat_rep_dists)
message('All results saved.')
