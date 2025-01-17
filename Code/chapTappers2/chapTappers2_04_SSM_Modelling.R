# 04_Modelling_StateSpaceModels"
# Be sure you an 02_DataPreparation


# ###############################################################
# run all state-space models
run = FALSE
#run = TRUE
if(run){
# Warning! This takes a lot of time on a laptop
# We run all models on a calculation server using parallelization
# To try with one single state-space model, 
# just run the routine provided in the text.

  # 1. Define the do_TapFitting_Stan function 
  do_TapFitting_Stan <- function(Dat, fn){ 
    # Dat = datt    # Dat is the data frame given as input
    # fn = filenameFitted # Filename of fitted model 
    # Define general variables needed in the StanData
    n_gen_times = 100
    time_gen = seq(0.0001,max(Dat$time0c), length.out = n_gen_times) # time generated
    filename <- paste(fn,unique(Dat$codycy),".rds",sep="")
    # Organize our data for Stan
    StanData <- list(
      n_times = length(Dat$time0c),
      n_gen_times = n_gen_times, # number of model-generated times requested
      time0 = 0.0,
      observed = Dat$phase,
      time = Dat$time0c,
      time_gen = time_gen,
      names_indicator = Dat$names_indicator
    )
    fit <- rstan::stan(
      data = StanData,
      #  file = "../chapTappers/StateSpaceModel_2_oct2024.stan",
      file = "Code/chapTappers2/chapTappers2_StateSpaceModel.stan",
      init = 0, iter = 4000,
      warmup = 2000
    )
    #saveRDS(file = filename,fit)
    result <- list(fit,filename)
    return(result)
  }
  
  # 2. Run cluster: results per codycy (condition-dyad-cycle)
  # are written to separate files
  cores=detectCores()
  cl <- makeCluster(cores[1]-1) #not to overload your computer
  registerDoParallel(cl)
  foreach(ucodycy = unique(DTapping$codycy)) %dopar% {
    # udycy = "C.dyad_20.cycle_10"
    library(tidyverse)
    datt <- DTapping %>% filter(codycy == ucodycy) %>% arrange(time0c)
    do_TapFitting_Stan(datt, filenameFitted)
  }
  stopCluster(cl)
  print("Finished")
run = FALSE
# up to here for state-space modelling
# ################################
}

# ###############################################################
# get posterior: collect the posteriors of all fitted state-space models
# only if the previous section has been executed
run = FALSE
#run = TRUE
if(run){
  cores=detectCores()
  cl <- makeCluster(cores[1]-1) #not to overload your computer
  registerDoParallel(cl)
  Posterior <- data.frame()
  Posterior <- foreach(ucodycy = unique(DTapping$codycy), .combine=rbind) %dopar% {
    fn = paste(FilenameFitted,ucoudycy,".rds",sep="")
    fit <- readRDS(fn)
    fit_posterior <- fit %>% 
      gather_draws(ks1m1,ks2m2,ds1m1,ds1s2,ds2m2,ds2s1,
                   `observation_std[1]`, `observation_std[2]`, 
                   `observation_std[3]`,`observation_std[4]`,
                   y0_s1, y0_s2) 
    fit_ps <- fit_posterior %>% median_qi() %>% mutate(id = ucodycy)
  }
  stopCluster(cl)
} else {
  Posterior = readRDS("Data/chapTappers2_Posterior.rds") 
  # Has been processed separately on dedicated parallel server
}
  run = FALSE
  # end of collecting posteriors
  # ################################

  
  



