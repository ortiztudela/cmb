source("simulation_functions/simulate_RW_instr.R")
# scrip to simulate data
part<-50

#n_param<-10# number of simulations
alpha_bound<-c(0,1) # Boundary of the alpha
beta_bound<-c(0, 5) # Boundaries of the beta

# generate a number of randomparameters
alpha_seq<-seq(alpha_bound[1], alpha_bound[2], length.out = part)
beta_seq<-seq(beta_bound[1], beta_bound[2], length.out = part)
# randomly shuffle them
alpha_seq<-sample(alpha_seq)
beta_seq<-sample(beta_seq)

# create a structure for the dataframe
nTrials<-300
p_red<-0.70

df<-data.frame("t" = 1:nTrials)
df$win <- sample(c("red", "yellow"), size = nTrials, prob = c(p_red, 1-p_red),
                 replace =T)


for (participant in 1: part){
  
# simulate
simulated<-simulate_RW_instr(df, alpha = alpha_seq[participant], 
                             beta = beta_seq[participant])

# reduce it
simulated<-simulated %>% select(c("t","choice_slot" ,"r" ))

# attach participant name
simulated$participant<-participant

# eliminate buffer
simualted<-simulated[-nrow(simulated), ]

# save it
write.csv(simulated, paste0("Data/", participant, ".csv"))
}
