explorative<-function(Q, prev_choice){
  # ---------------------------------------------------------------------------#
  # Function that computes the choices by always exploring the other option
  #
  # Input:
  #   Q: value of the Rescorla-Wagner model
  #   prev_choice: Previous choice
  #
  # Output:
  #   a vector with the probability distribution with length = length(Q)
  # ---------------------------------------------------------------------------#
  
  # initialize choice probability
  cp <-seq(1:length(Q))
  
  cp<-c(0.5, 0.5)
  
  return(cp)
}