fit_model<-function(df,alphaBound, betaBound, model){
  
  #----------------------------------------------------------------------------#
  # This function finds the parameters that 
  # minimize the negative log-likelihood
  #
  # Input    
  #    df: a long dataset where each row represents a trial. 
  #    alphaBound<-a two-element vector with upper and lower boundaries for the 
  #                 alpha parameter 
  #    betaBound<- a two-element vector with upper and lower boundaries for the 
  #                 beta parameter 
  #    model<- model that we want to fit
  #
  # Output:
  #   A list with: 
  #   [[1]] "alphabetaPAR" : alpha [1], beta [2] parameters that minimize the 
  #           negative log-likelihood
  #   [[2]] "loglikel": log-likelihood for the model with the parameters 
  #           of best fit
  #   [[3]] "BIC" : Bayesian Information Criterion for the model with the 
  #         parameters of best fit
  #----------------------------------------------------------------------------#
  
  X0<-c( runif(1),  rexp(1,1))  # rexp generates random numbers from  uniform 
  # and exponential distribution 
  LB<-c(alphaBound[1],betaBound[1]) # lower boundaries
  UB<-c(alphaBound[2], betaBound[2]) # upper boundaries

  # get the appropriate likelihood function
  lik_function<-get(paste0("likelihood_", model))
    
  obfunc<-function(x) lik_function(df = df, alpha = x[1], beta =  x[2],
                                          out = 1) # this function 
  # is similar to the MATLAB "handle" function
  
  # Find best-fitting parameters
  NegLL<-optim(X0, obfunc, method = "L-BFGS-B",lower = LB, upper=UB) 
  
  # get log-likelihood
  LL<--NegLL[[2]] # log likelihood
  
  # compute BIC
  BIC <- BICcompute(length(X0), nrow(df), NegLL[[2]])
  
  # Prepare results for output
  data <- list(NegLL[[1]], LL, BIC)
  names(data)<-c("alpha_beta", "logLikel", "BIC" )
  
  return(data)
}
