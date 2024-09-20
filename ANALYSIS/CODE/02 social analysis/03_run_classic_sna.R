# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# Project: complexity paper
# Date started: 27-07-2022
# Date last modified: 02-03-2023
# Author: Simeon Q. Smeele
# Description: Running classical social network analaysis to get weighted degree, betweenness, eigenvector 
# centrality and clustering coefficient.
# This version includes the 2020 data. 
# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

# Loading libraries
libraries = c('tidyverse', 'asnipe', 'igraph', 'sna')
for(lib in libraries){
  if(! lib %in% installed.packages()) lapply(lib, install.packages)
  lapply(lib, require, character.only = TRUE)
}

# Clean R
rm(list=ls()) 

# Paths
source('ANALYSIS/CODE/paths.R')

# Load data
load(path_foraging_network)

# Create graph object
graph_20 = graph_from_adjacency_matrix(ceiling(foraging_network_20), mode = 'undirected')
graph_21 = graph_from_adjacency_matrix(ceiling(foraging_network_21), mode = 'undirected')

# Detect communities
# run louvain with edge weights 
# louvain_partition <- igraph::cluster_louvain(graph) 
# assign communities to graph 
# graph$community <- louvain_partition$membership 

# Plot network
# pdf(path_pdf_foraging_network, 20, 20)
# gplot(foraging_network, gmode = 'graph', label = rownames(foraging_network),
#       label.col = graph$community, vertex.col = graph$community, edge.lwd = 10*foraging_network)
# dev.off()

# Run degree
deg_out_20 = sna::degree(as.matrix(foraging_network_20), gmode = 'graph', ignore.eval = TRUE)
deg_out_21 = sna::degree(as.matrix(foraging_network_21), gmode = 'graph', ignore.eval = TRUE)

# Run betweenness
between_out_20 = sna::betweenness(as.matrix(foraging_network_20), gmode = 'graph')
between_out_21 = sna::betweenness(as.matrix(foraging_network_21), gmode = 'graph')

# Run eigenvector centrality
eig_out_20 = igraph::eigen_centrality(graph_20)$vector
eig_out_21 = igraph::eigen_centrality(graph_21)$vector

# # Run clustering coefficient
# clust_out = transitivity(graph)
# hist(eig_out, breaks = 30)

# Plot everything
pdf(path_pdf_foraging_measures, 7, 7)
par(mfrow = c(2, 2))
hist(deg_out_20, breaks = 30, main = 'degree 2020')
hist(between_out_20, breaks = 30, main = 'betweenness 2020')
hist(eig_out_20, breaks = 30, main = 'eigenvector 2020')
plot.new()
hist(deg_out_21, breaks = 30, main = 'degree 2021')
hist(between_out_21, breaks = 30, main = 'betweenness 2021')
hist(eig_out_21, breaks = 30, main = 'eigenvector 2021')
dev.off()

# Calculate community size
# com_size = sapply(1:nrow(foraging_network), function(i) 
#   length(which(graph$community == graph$community[i])))

# Plot degree vs sample size
# plot(samp_size_gbi$samp_size_gbi, deg_out)
# print(summary(lm(deg_out ~ samp_size_gbi$samp_size_gbi)))

# Combine into data frame and saved
net_meas_20 = data.frame(ind = colnames(foraging_network_20),
                         degree = deg_out_20,
                         eig_cent = eig_out_20,
                         betweenness = between_out_20)
net_meas_21 = data.frame(ind = colnames(foraging_network_21),
                         degree = deg_out_21,
                         eig_cent = eig_out_21,
                         betweenness = between_out_21)
save(net_meas_20, net_meas_21, file = path_net_meas)
