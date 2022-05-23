softmax<-function(Q, beta){
  # ---------------------------------------------------------------------------#
  # Function to compute the softmax choice probabilities
  #
  # Input:
  #   Q: value of the Rescorla-Wagner model
  #   beta: inverse temperature parameter
  #
  # Output:
  #   a vector with the probability distribution with length = length(Q)
  # ---------------------------------------------------------------------------#
  p<-exp(beta*Q) / sum(exp(beta*Q))
  return(p)
}