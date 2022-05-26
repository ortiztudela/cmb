#------------------------------------------------------------------------------#
# model recovery
#------------------------------------------------------------------------------#
rm(list=ls())

sims<-30
alpha_bound<-c(0,1) # Boundary of the alpha
beta_bound<-c(0, 5) # Boundaries of the beta

# generate a number of randomparameters
alpha_seq<-seq(alpha_bound[1], alpha_bound[2], length.out = sims)
# randomly shuffle them
alpha_seq<-sample(alpha_seq)
beta_seq<-rexp(sims,2/5)+1

# create a structure for the dataframe
nTrials<-50
p_red<-0.85

data<-matrix(NA, nrow=4,ncol = 4)

# write the file
name<- paste("recovery_files/modelRecovery." sep="")

models<-c("WSLS",  "RW_instr", "RW_instr_greedy" )
