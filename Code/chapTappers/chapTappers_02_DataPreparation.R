# ODE data preparation

# ###############################################
# generating parameters for the dynamic models
# ###############################################

run = FALSE
# run = TRUE
if(run){
  # Use these precalculated K and D values
  K <- readRDS("Data/chapTappers_K_5_jun_2024.rds")
  D <- readRDS("Data/chapTappers_D_5_jun_2024.rds")
  
  
  # Or recalculate the K and D values as below
  # Currently not calculated!!!
  if(0){
  K1 <- data.frame(ks1m1 = rbeta(20,4,2)) 
  K1 <- K1 %>% mutate(ks2m2 = sample(ks1m1) )
  
  # distribution of delay times
  D2 <- data.frame(ds1m1 = rsn(n=20, xi=0, omega=.6, alpha=3, tau=0, dp=NULL)) 
  D2 <- D2 %>% mutate(ds2m2 = sample(ds1m1), ds1s2 = 0, ds2s1 = 0)
  
  # delays set to zero because we want to test the system without delays and with delays
  D1 <- D2 %>%  mutate( ds1m1 = 0, ds2m2 = 0) 
  
  # row bind the same coupling parameters
  K <- rbind(K1,K1)
  # row bind different delay parameters
  D <- rbind(D1,D2)
  
  # check distributions visually
  cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
  
  K_long <- K %>% pivot_longer(cols = everything())
  pk <- ggplot(K_long) + geom_point(aes(x=name,y=value, color = name), size = 1, shape = 1) +
    theme_bw() +
    scale_fill_manual(values=cbPalette) +
    scale_colour_manual(values=cbPalette) + 
    ylim(0,1)
  
  D_long <- D %>% pivot_longer(cols = everything())
  pd <- ggplot(D_long) + geom_point(aes(x=name,y=value, color = name), size = 1, shape = 1) +
    theme_bw() +
    scale_fill_manual(values=cbPalette) +
    scale_colour_manual(values=cbPalette) + 
    ylim(0,1)
  #pk + pd
  }
}
run = FALSE

# ###############################################
# Calculate the states of all 40 model simulations
# Store them in: States
# ###############################################

run = FALSE
# run = TRUE
if(run){
  # K <- readRDS("Data/chapTappers_K_5_jun_2024.rds")
  # D <- readRDS("Data/chapTappers_D_5_jun_2024.rds")
  
  # Define the differential equation dydt to work at defined samplingrate
  samplingrate = 1000 # sa per second
  
  dydt <- function(t, y, parms) {
    with(as.list(c(y, parms)), {
      ############################## dynamic system
      p600 = 2*pi*samplingrate/600
      p610 = 2*pi*samplingrate/610
      dm1 = p600
      ds1 = p600  + .5*(ks1m1*sin(m1 - s1 + ds1m1) + (1-ks1m1)*sin(s2 - s1 + ds1s2) )
      dm2 = p610
      ds2 = p610  + .5*(ks2m2*sin(m2 - s2 + ds2m2) + (1-ks2m2)*sin(s1 - s2 + ds2s1) )
      return(list(c(dm1,ds1,dm2,ds2)))
    })
  }
  
  n_cycles = 3
  aligned_beat_sec = (600 * 610/10) /1000 # time when the two metronomes align
  print(paste("number of seconds when phase is aligned", aligned_beat_sec))
  n_steps = n_cycles * (samplingrate* aligned_beat_sec) # how many cycles do we want to see, at samplingrate?
  print(paste("n_steps in the entire sequence =", n_steps))
  dt = 1/samplingrate # time of 1 sample 
  
  # Parallel processing of the 40 models
  cores=detectCores()
  cl <- makeCluster(cores[1]-1) #not to overload your computer
  registerDoParallel(cl)
  
  States <- data.frame()
  States <- foreach(k = 1:nrow(K), .combine= rbind) %dopar% {
    library(deSolve)
    library(tidyverse)
    # k = 1
    coupling <- K[k,] %>% dplyr::select(ks1m1,ks2m2)
    delay <- D[k,] 
    print(paste("row in K = ",k,"::coupling parameters = ",toString(round(coupling,2))) )
    print(paste("row in D = ",k,"::delay parameters = ",toString(round(delay,2))) )
    
    # 2. Running the simulation
    parameters <- c(
      ks1m1 = coupling$ks1m1,
      ks2m2 = coupling$ks2m2,
      ds1m1 = delay$ds1m1,
      ds2m2 = delay$ds2m2,
      ds1s2 = delay$ds1s2,
      ds2s1 = delay$ds2s1
    )
    state      <- c(m1 = 0.0, s1 = 0.0, m2 = 0.0, s2 = 0.0 ) # all curves start at zero
    times      <- seq(0, n_cycles*aligned_beat_sec, by = dt) # first value must be initial time
    
    states <- ode(func = dydt, y = state, times = times, parms = parameters)
    states <- states %>% data.frame() %>% 
      mutate(sim = factor(k), 
             param = paste(toString(round(coupling,2)),"\n", toString(round(delay,2))))
  }
  stopCluster(cl)
  rm(cl)
  # This saves all simulated states
  saveRDS(file = "Data/chapTappers_States.rds", States)
}
run = FALSE

# ########################################################
# Peak extraction to get the times at which phase = 2pi
# ########################################################

run = FALSE
# run = TRUE
if(run){
  # States <- readRDS(file = "Data/chapTappers_States.rds")
  # assume we calculated these States
  
  p <- findpeaks(States$m1 %% (2*pi))
  p = p[,2]
  m1 <- States[p,] %>% mutate(phase = m1, names_indicator = 1, names="metronome 1")
  
  p <- findpeaks(States$s1 %% (2*pi))
  p = p[,2]
  s1 <- States[p,] %>% mutate(phase = s1, names_indicator = 2, names="subject 1")
  
  p <- findpeaks(States$m2 %% (2*pi))
  p = p[,2]
  m2 <- States[p,] %>% mutate(phase = m2, names_indicator = 3, names="metronome 2")
  
  p <- findpeaks(States$s2 %% (2*pi))
  p = p[,2]
  s2 <- States[p,] %>% mutate(phase = s2, names_indicator = 4, names="subject 2")
  
  StatePeaks <- rbind(m1,s1,m2,s2) %>% arrange(time) #dplyr::select(time,sim,names,names_indicator,phase) 
  rm(m1,s1,m2,s2)
  saveRDS(file = paste("Data/chapTappers_StatePeaks.rds",sep=""),StatePeaks)
}
run = FALSE


# ###############################################
# Add noise to the extracted time of subjects
# ###############################################

run = FALSE
# run = TRUE
if(run){
StatePeaks <-  readRDS(file = "Data/chapTappers_StatePeaks.rds")
 sd_time = 0.005 # standard deviation of taps, in seconds
 m1 <- StatePeaks %>% filter(names == "metronome 1") 
 s1 <- StatePeaks %>% filter(names == "subject 1") %>% mutate(time = time + (rnorm(length(time),0,sd_time)) )
 m2 <- StatePeaks %>% filter(names == "metronome 2") 
 s2 <- StatePeaks %>% filter(names == "subject 2") %>% mutate(time = time + (rnorm(length(time),0,sd_time)) )
 Observations <- rbind(m1,s1,m2,s2) %>% mutate(observed = do_rotate(phase)) 
 rm(m1,s1,m2,s2)
  #%>% dplyr::select(time,observed,sim,names_indicator)
 # This is the dataset with simulate observations. The main colums are: time,observed,sim,names_indicator
saveRDS(file = paste("Data/chapTappers_Observations.rds",sep=""),Observations)
}
run = FALSE



