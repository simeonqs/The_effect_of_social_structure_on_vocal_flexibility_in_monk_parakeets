# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# Project: complexity paper
# Date started: 03-03-2023
# Date last modified: 02-05-2023
# Author: Simeon Q. Smeele
# Description: Analysing the microsat data. Output is relatedness matrix. This script first cleans the data so
# that there are no replicates or other rows/columns that do not contain data. It then reformats the data
# such that each allele is in it's own column and can be read by *related*. Finally it runs the relatedness
# analysis. 
# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

# Loading libraries
libraries = c('readxl', 'related', 'stringr', 'callsync')
for(lib in libraries){
  if(! lib %in% installed.packages()) lapply(lib, install.packages)
  lapply(libraries, require, character.only = TRUE)
}

# Clean R
rm(list=ls()) 

# Paths
source('ANALYSIS/CODE/paths.R')

# Read data
dat_orig = suppressMessages( read_xlsx(path_microsat) )

# Clean 
dat = as.data.frame(dat_orig)
colnames(dat) = dat[1,] # set proper headers
dat = dat[-(1:4),] # remove unused rows
dat = dat[-grep('Rerun', dat$`Vetgenomics ID`),] # remove duplicates
dat = dat[,-1] # remove vetgenomic ID (keep our ID)
dat = dat[,colnames(dat) != 'NA'] # remove empty column

# Prepare in format where every allele has two columns
rel_dat = data.frame(ID = dat$ID)
for(col in 2:ncol(dat)){
  new_dat = data.frame(V1 = rep(0, nrow(dat)), 
                       V2 = rep(0, nrow(dat))) 
  for(row in seq_len(nrow(dat))){
    if(nchar(dat[row,col]) > 3) 
      new_dat[row,] = dat[row,col] |> str_replace('/', '-') |> strsplit('-') |> unlist() |> as.integer() else
        new_dat[row,] = dat[row,col] |> as.integer()
  }
  rel_dat = cbind(rel_dat, new_dat)
}
colnames(rel_dat) = NULL # rename columns

# Run relatedness analysis
coancestry_out = coancestry(rel_dat, dyadml = 1, trioml = 1, wang = 1, trioml.num.reference = 98)
wang_out = o.to.m(coancestry_out$relatedness$wang, dat$ID)
trioml_out = o.to.m(coancestry_out$relatedness$trioml, dat$ID)

# Save
save(wang_out, file = path_wang_out)
save(trioml_out, file = path_trioml_out)
