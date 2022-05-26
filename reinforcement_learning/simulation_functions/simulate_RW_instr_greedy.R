simulate_RW_instr_greedy<-function( df, alpha, beta){
  #----------------------------------------------------------------------------#
  # Simulate RW without action. This function takes the reward and applies
  # Rescorla-Wagner model to create expected values on each trial and prediction
  # Error
  #     INPUTS: df - a dataframe with the structure of the env
  #     OUTPUTS: a dataframe with choice probabilities and rewards
  #----------------------------------------------------------------------------#
  
  # we have a red and a yellow slot
  slots<-c("red", "yellow")
  
  # Initialize Qs and choice p
  for (i in (slots)){
    df[[paste0("Q", i)]]<-0.5
    df[[paste0("p", i)]]<-NA
  }
  
  # Initialize the choice
  df$choice_slot<-NA
  
  # initialize the reward
  df$r<-NA
  
  # create an empty rowa at the end
  df[nrow(df)+1, ]<-NA
  
  # loop through the trials
  for(t in 1:(nrow(df)-1)){
    
    # get the expected values
    Q = df[t, c("Qred", "Qyellow")]
    
    # choice probability through softmax
    cp<-greedy(Q )
    
    # make the choice
    choice<-chooseBinomial(cp)
    
    # convert it into a slot
    df$choice_slot[t]<-ifelse(choice==1, "red", "yellow")
    
    # generate the reward
    df$r[t]<-ifelse(df$choice_slot[t]==df$win[t], 1, 0)
    
    # compute prediction error
    df$Delta[t]<- as.numeric(df$r[t]-Q[choice])
    
    # Update the expected Values
    Q[choice]<-Q[choice]+ alpha*df$Delta[t]
    
    # assign the values to the dataframe
    df[t+1, c("Qred", "Qyellow")]<-Q
    df[t, c("pred", "pyellow")]<-cp
    
    
  }
  
  return(df)
}
