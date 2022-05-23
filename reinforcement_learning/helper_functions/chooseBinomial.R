# choose binomial function

chooseBinomial <- function(p){
  # Function to simulate choice made by particiapnt, using 
  #   binomial function in R (generating random vectors from the multinomial distribution)
  # 
  # Input
  #  p : a vector with the probabilities
  #
  # Output
  #   a : a vector indicating the choice
  # ------------------
  a<-sample(c(1,2),1, p, replace =T)
  
  # transform the matrix into a vector
  #a<-a[,1]
  
  # make it a number between 1 and 3
  return(a)
}

