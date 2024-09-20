# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# Project: chapter II
# Date started: 18-01-2021
# Date last modified: 23-02-2023
# Author: Simeon Smeele
# Description: From previous chapter. Takes GBI and gives you a network that takes care of random 
# interactions.
# This version is cleaned and includes location. 
# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

require(asnipe)

julias.random.thing = function(gbi){
  
  # Create network
  network = get_network(association_data = gbi[,-c(1, 2)],
                        data_format = "GBI",
                        association_index = "SRI")

  # Function to calculate CV
  cv = function(x) return(sd(x, na.rm = T) / mean(x, na.rm = T))
  
  # Calculate CVobs
  diag(network) <- NA
  CVobs.pa <- cv(network)
  
  # Calculate CV random
  ### calculating random associations by permutations on data
  ### including only individuals with more than 10 sightings
  ### performing swaps only within a day
  inds.total <- colnames(gbi) #list of all individuals recorded
  inds.subset<- colnames(network) #creating list of individuals with more than 10 obs
  permutation <- 10000
  networks.random.pa  <- network_permutation(association_data = gbi,
                                             association_matrix = network,
                                             data_format = "GBI",
                                             association_index = "SRI",
                                             identities = inds.total,
                                             which_identities = inds.subset,
                                             permutations = permutation,
                                             locations = gbi$cluster,
                                             within_location = T,
                                             days = gbi$date, 
                                             within_day = T) 
  
  # Loop through each of random network to calculate CVran
  CVran.pa <- rep(NA, permutation)
  for (i in 1: permutation) {
    CVran.pa[i] <- cv(networks.random.pa [i,,])
  }
  plot(CVran.pa,type='l')
  abline(h=CVobs.pa,col="red", lwd=2)
  
  # Calculate significance
  Prand.pa <- sum(CVran.pa>=CVobs.pa)/permutation
  
  # Network of preferred / avoided associates
  network.pref.pa <- matrix(NA, nrow=nrow(network),ncol=ncol(network))
  rownames(network.pref.pa) <- rownames(network)
  colnames(network.pref.pa) <- colnames(network)
  for (i in 1:nrow(network)) {
    for (j in 1:ncol(network)){
      Pij <- sum(networks.random.pa[,i,j] >= network[i,j])/permutation
      # network.pref.pa[i,j] <- Pij <= 0.05
      network.pref.pa[i,j] <- Pij <= 0.5
    }
  }
  table(network.pref.pa)
  network.asso <- network*as.numeric(network.pref.pa) #network only including links above randomness
  
  # Return
  return(network.asso)
  
} # end julias.random.thing
