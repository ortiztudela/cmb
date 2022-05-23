greedy<-function(Q){
  # ---------------------------------------------------------------------------#
  # Function that computes the choices by exploiting the one with the 
  # higher value
  #
  # Input:
  #   Q: value of the Rescorla-Wagner model
  #
  # Output:
  #   a vector with the probability distribution with length = length(Q)
  # ---------------------------------------------------------------------------#
  
  # initialize choice probability
  cp <-seq(1:length(Q))
  
  cp<-ifelse(cp==which(Q == max(Q)), 1, 0)
  
  return(cp)
  }