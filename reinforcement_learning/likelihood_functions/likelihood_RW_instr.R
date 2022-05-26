likelihood_RW_instr<-function( df, alpha, beta, out){
  #----------------------------------------------------------------------------#
  # Compute the likelihood of  participants' choice according to RW 
  # model. 
  #
  #     INPUTS: df - a dataframe with the structure of the env
  #             alpha - candidate learning rate
  #             beta - candidate inverse temperature parameter
  #             out - 1: return only the negative log-likelihood; 
  #                   2: return all data
  #     OUTPUTS: see "out" argument
  #----------------------------------------------------------------------------#
  
  # we have a red and a yellow slot
  slots<-c("red", "yellow")
  
  # Initialize Qs and choice p
  for (i in (slots)){
    df[[paste0("Q", i)]]<-0.5
    df[[paste0("p", i)]]<-NA
  }
  
  p<-vector()
  
  df$Delta<-NA

  # initialize Q
  Q<-rep(0.5, 2)
  
  # loop through the trials
  for(t in 1:(nrow(df)-1)){
    
    # get the expected values
   # Q = as.numeric(df[t, c("Qred", "Qyellow")])
    
    # choice probability through softmax
    cp<-softmax(Q , beta = beta)
    
    choice<-ifelse(df$choice_slot[t] == "red", 1,2)
    # probability of the choice given the parameter and the model
    p[t]<-cp[choice]
    
    df$Delta[t]<-df$r[t] -  Q[choice]
    
    # Update the expected Values
    Q[choice]<-Q[choice]+ alpha*df$Delta[t]
    
    # assign the values to the dataframe
    df[t, c("Qred", "Qyellow")]<-Q
    df[t, c("pred", "pyellow")]<-cp
    
  }
  
  NegLL<- -sum(log(p))
  
  ifelse (out ==1,  return(NegLL), return(df))

}
