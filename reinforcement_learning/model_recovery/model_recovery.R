#------------------------------------------------------------------------------#
# model recovery
#------------------------------------------------------------------------------#
rm(list=ls())
library(reshape2)
library(viridis)
library(ggplot2);theme_set(theme_classic())
cd<-getwd()

#----------------------------get functions-------------------------------------#
# helper functions
setwd(paste(cd, "/helper_functions",sep=""))
helpfun<-list.files()
for (f in 1:length(helpfun)){
  source(helpfun[f])
}

setwd(cd)

# likelihood functions
setwd(paste(cd, "/likelihood_functions",sep=""))
likfun<-list.files()
for (f in 1:length(likfun)){
  source(likfun[f])
}

setwd(cd)

# simulation functions
setwd("simulation_functions")
simfun<-list.files()
for (f in 1:length(simfun)){
  source(simfun[f])
}

setwd(cd)

# get the fitting function
source("helper_functions/fit_model.R")
#------------------------------------------------------------------------------#
set.seed(12345)
sims<-30
alpha_bound<-c(0,1) # Boundary of the alpha
beta_bound<-c(0, 5) # Boundaries of the beta

# generate a number of randomparameters
alpha_seq<-seq(alpha_bound[1], alpha_bound[2], length.out = sims)
# randomly shuffle them
alpha_seq<-sample(alpha_seq)
beta_seq<-rexp(sims,2/3)+1

# create a structure for the dataframe
nTrials<-50
p_red<-0.70

conf_matr<-matrix(0, nrow=3,ncol = 3)

models<-c("WSLS",  "RW_instr", "RW_instr_greedy" )

# create a structure
df<-data.frame("t" = 1:nTrials)
df$win <- sample(c("red", "yellow"), size = nTrials, prob = c(p_red, 1-p_red),
                 replace =T)


# loop
for (mod in 1:length(models)){ # loop through the models
  
  # get the current model
  curr_mod<-models[mod]
  
  print(paste0("simulating model ", curr_mod))
  
  # get the simulation function
  simulation_function<-get(paste("simulate_", curr_mod, sep=""))
  
  for (sim in 1:sims){ # loop through the simulations
    
  # simulate
  simulation<-simulation_function(df, alpha = alpha_seq[sim],
                                  beta = beta_seq[sim] )
  
  # fit all 4 models
  # initialize BIC
  BIC<-vector()
  
  for (n in 1:length(models)){
    
    model_fit<-models[n]
    BIC[n]<-fit_model(simulation, alphaBound = alpha_bound,
                      betaBound = beta_bound , model = model_fit)[[3]]
  }

  # which model had the best fit?
  iBest<-as.numeric(BIC==min(BIC))
  
  # add to the matrix
  conf_matr[mod, ]<-conf_matr[mod, ]+iBest
  
  }
  
}
  
# add names
mt<-conf_matr
colnames(mt)<-models
rownames(mt)<-models

getCM<-function(matrix){
  # initialise the matrix
  Matrixmean<-matrix(NA, nrow = ncol(matrix), ncol=ncol(matrix))
  
  # compute the percentages in each cell
  for (r in 1: ncol(matrix)){
    sum<-sum(matrix[r,1:ncol(matrix)])
    for (c in 1:ncol(matrix)){
      Matrixmean[r, c]<-matrix[r,c]/sum
    }
  }
  Matrixmean<-data.frame(Matrixmean)
  names(Matrixmean)<-rownames(matrix)
  rownames(Matrixmean)<-rownames(matrix)
  
  # convert to matrix
  mat<-data.matrix(Matrixmean)
  # melt it to create a long dataset
  melted_matrix<-melt(mat)
  
  # plot it 
  # transpose matrix
  matrTrans<-t(Matrixmean)[,nrow(mat):1]
  
  melted_transmatr<-melt(matrTrans)
  
  return(melted_transmatr)
  
}

longmatr<-getCM(mt)

p<-ggplot(longmatr, aes(x=(Var1), y=(Var2), fill=value))
p+geom_tile(aes(fill = value)) +#
  geom_text(aes(label =round(value, 2)))+
  
  #theme_ipsum()+
  labs(y="", x = "")+
  theme_bw()+
  scale_x_discrete(position = "top") +
  theme(legend.title = element_blank())+
  #scale_fill_gradient(low = "blue", high = "yellow")
  scale_fill_viridis()

ggsave(file=paste0("output_files/ConfusionMatrix.jpg"))
