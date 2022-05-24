simulate_RW_pav<-function( r, alpha){
  #----------------------------------------------------------------------------#
  # Simulate RW without action. This function takes the reward and applies
  # Rescorla-Wagner model to create expected values on each trial and prediction
  # Error
  #     INPUTS: r - a vector with rewards
  #             alpha - learning rate
  #     OUTPUTS: a dataframe with t - number of trials
  #                               r - rewards
  #                               V - Expected values
  #                               delta - prediction error
  #----------------------------------------------------------------------------#
  
  # Initialize V at 0
  V<-rep(0, length(r)+1)
  
  # Initialize prediction error
  Delta<-rep(NA, length(r))
  
  # loop through the rewards
  for(t in 1:length(r)){
    
    # generate prediction error
    Delta[t] <- r[t]-V[t]
    
    # Update the expected Values
    V[t+1]<-V[t]+ alpha* Delta[t]
    
  }
  
  # create a dataframe with the values
  df<-data.frame("t" = seq(1:(length(r)+1)), "V" = V,  "r" = c(r, NA), #add buffer,
                    "Delta" = c(Delta, NA) )
  
  return(df)
}
