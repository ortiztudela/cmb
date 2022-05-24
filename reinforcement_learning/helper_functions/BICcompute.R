BICcompute<-function(X0length, alength, NegLL){
  # This function computes the Bayesian Information Criterion, given length of the initial random values (X0len) and 
  # length of actual choices (alength)
  #
  # Input
  #   X0len: length of the initial random values (number of free parameters)
  #   alength: length of the actual choices made by participants (number of trials)
  #   NegLL: Negative Log likelihood
  #
  # Output:
  #  BIC (Bayesian Information Criterion)
  # -------------
  BIC = X0length * log(alength) + 2*NegLL;
}