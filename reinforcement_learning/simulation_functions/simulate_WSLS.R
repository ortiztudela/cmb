simulate_WSLS<-function ( df, alpha, beta){
  #----------------------------------------------------------------------------#
  # This function computes choices
  #  conditional on a win stay lose shift model
  #
  # Input
  #   df: data containing the structure of the task
  #   beta : beta parameter
  # UTPUTS: a dataframe with choice, choice probabilities and rewards
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
  
  # loop over trials
  for (t in 1: (nrow(df)-1)){

    # get the expected values
    Q = df[t, c("Qred", "Qyellow")]
    
    # choice probability through softmax
    cp<-softmax(Q , beta = beta)
    
    # make the choice
    choice<-chooseBinomial(cp)
    
    # convert it into a slot
    df$choice_slot[t]<-ifelse(choice==1, "red", "yellow")
  
    # generate the reward
    df$r[t]<-ifelse(df$choice_slot[t]==df$win[t], 1, 0)
    
    # Updating rule
    if (df$r[t]==1){
      Q[choice]<-1
      Q[choice]<-0
      
    } else{
      Q[-choice]<-0
      Q[-choice]<-1
    }

    # assign the values to the dataframe
    df[t+1, c("Qred", "Qyellow")]<-Q
    df[t, c("pred", "pyellow")]<-cp
    
  }
  
  return(df)
  
  }

