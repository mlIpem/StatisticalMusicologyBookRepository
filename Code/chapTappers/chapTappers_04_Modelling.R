# chapTappers_04_Modelling


# In this code, the state-space model is used to fit parameters
# So, we go from Observations to posterior distributions of state-space model parameters. 
# We have:
#   - parameters of the system equation (for the state transitions)
#   - parameters of the observation equation (only one)
# Once the parameters are estimated, we generate states and observations, as posterior predictions.
# 
run = FALSE
# run = TRUE
if(run){
  do_StateSpaceModelling_STAN <- function(Stan_data_toprocess,filestring,filestringtime, filenameStancode){
    # #######################################
    # Part 1. clean data and prepare for STAN
    # Stan_data_toprocess = Observations
    # filestring = filestring
    # filenameStancode = StateSpaceModel
    
    library(rstan)
    library(tidyverse)
    dat <- Stan_data_toprocess %>% arrange(time)
    # dat <- Observations
    n_gen_times = 100
    time_gen = seq(0.0001, max(dat$time), length.out = n_gen_times) # generated time
    
    StanData <- list(
      n_times = as.integer(length(dat$time)),
      n_gen_times = as.integer(n_gen_times), # number of model-generated times requested
      time0 = 0.0,
      observed = dat$observed,
      time = dat$time,
      names_indicator = as.integer(dat$names_indicator),
      time_gen = time_gen
    )

    # #######################################
    # Part 2. Run stan
    start.time <- Sys.time()
    fit <- stan(
      data = StanData,
      file = filenameStancode,
      #verbose = FALSE,
      init = 0,
      thin = 2,
      iter = 6000,
      warmup = 2000,
      cores = 2,
      chain = 2
    )
    end.time <- Sys.time()
    time = c(start.time, end.time)
    saveRDS(file = filestringtime, time)
    saveRDS(file = filestring,fit)
  }
  
  # ##################
  # 1. Select dataset
  Observations <- readRDS("Data/chapTappers_Observations.rds")
  aligned_beat_sec = (600 * 610/10) /1000 
   if(1){
    # IMPORTANT !!!
    # select cycle 3 from Observations and do modulo time to reset time
    w <- which(Observations$time > 2*aligned_beat_sec)
    Observations_selected <- Observations[w,] %>% 
      mutate(time = time%%aligned_beat_sec)
  }
  filenameStanCode = "Code/chapTappers/chapTappers_StateSpaceModel.stan" # 
  # ##################
  # 2. Run cluster --- needed if you run multiple dynamic models at one.
  datum = "oct2024"
  cores=detectCores()
  cl <- makeCluster(cores[1]-1) #not to overload your computer
  registerDoParallel(cl)
  foreach(s = unique(Observations_selected$sim) ) %dopar% { # Each simulation
    library(tidyverse)
    # s = 2
    dat <- Observations_selected %>% filter(sim == s) %>% dplyr::select(time, observed, names_indicator)
    filestring = paste("Fitted/chapTappers_sim_StateSpaceModel_", datum, "_", s ,".rds",sep="")
    filestringtime = paste("Fitted/chapTappers_sim_StateSpaceModel_", datum, "_", s , "_calctimeinfo",".rds",sep="")
    do_StateSpaceModelling_STAN(dat, filestring, filestringtime, filenameStanCode)
  }
  stopCluster(cl)
  
}
run = FALSE

