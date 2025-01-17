# chapTappers_05_ModelPlotting


# ######################################################
# Here we plot the generated states and generated observation
# of a single specified dynamic system, taken from 
# the fitted models obtained in 04:
# ######################################################
run = FALSE
# run = TRUE
if(run){
  # K <- readRDS("Data/chapTappers_K_5_jun_2024.rds")
  # D <- readRDS("Data/chapTappers_D_5_jun_2024.rds")
  # States <- readRDS("Data/chapTappers_States.rds")
  
  # First we read the fitted model
  s = 21 # number of dynamic model to be plotted
  fit <- readRDS(paste("Fitted/chapTappers_sim_StateSpaceModel_oct2024_",s,".rds", sep=""))
  # From fit, get system equation parameters k and d
  S <- fit %>% 
    gather_draws(ks1m1,ks2m2,ds1m1,ds2m2,ds1s2,ds2s1)  %>% 
    median_qi() %>% dplyr::select(.variable,.value) %>% 
    mutate(.value= round(.value,2))
  # Add designed D and K
  S <- S %>% rename(parameter = .variable, fitted = .value) %>% 
    arrange(match(parameter,c("ds1m1","ds2m2","ds1s2","ds2s1","ks1m1","ks2m2"))) %>% 
           mutate(defined = round(as.numeric(c(D[s,],K[s,c("ks1m1","ks2m2")])),2) ) %>% 
    data.frame() 
  # Prepare string for plot
  ss = paste("Fitted   ", "k=", toString(S$fitted[5:6]), "; d=", toString(S$fitted[1:4]),"\n",
             "Defined", "k=", toString(S$defined[5:6]), "; d=", toString(S$defined[1:4]))                         
  # aligned_beat_sec 
  aligned_beat_sec = 36.6
  
  
  # From fit, get generated observations (they are continuous based on continuous states)
  res <- fit %>%
    spread_draws(observations_generated[time,oscillator]) %>%
    median_qi(.width=c(.95)) %>% mutate(oscillator = oscillator) #, time = time*aligned_beat_sec/100) 
  # Rotate phase from [0, 2pi] to [-pi, pi] when plotting
  m1 <- res %>% filter(oscillator == 1) #we select m1 and its generated observation (= phase)
  s1 <- res %>% filter(oscillator == 2)
  m2 <- res %>% filter(oscillator == 3)
  s2 <- res %>% filter(oscillator == 4)
  
  #########################################
  # Data preparation:
  # 1. Relative phase of continuous generated observations
  dummy_data1 = data.frame(time = s1$time, relphase.y = m1$observations_generated - s1$observations_generated) %>% mutate(names = "m1s1")
  dummy_data1.l = data.frame(relphase.lower = m1$.lower - s1$.lower)
  dummy_data1.u = data.frame(relphase.upper = m1$.upper - s1$.upper) 
  dummy_data2 = data.frame(time = s2$time, relphase.y = m2$observations_generated - s2$observations_generated) %>% mutate(names = "m2s2")
  dummy_data2.l = data.frame(relphase.lower = m2$.lower - s2$.lower) 
  dummy_data2.u = data.frame(relphase.upper = m2$.upper - s2$.upper) 
  y = rbind(dummy_data1,dummy_data2)
  ymin = rbind(dummy_data1.l,dummy_data2.l)
  ymax = rbind(dummy_data1.u,dummy_data2.u)
  observations_generated_relphase = cbind(y,ymin,ymax)
  rm(dummy_data1,dummy_data1.l,dummy_data1.2)
  rm(dummy_data2,dummy_data2.l,dummy_data2.2)
  
  # 2. Original discrete data, use do_calc_phase2 to calculate the relative phase of the original 
  # Observations, where we have to extract the relphase from time given
  Dummy <- Observations %>% filter(sim == s)
  if( length(w<-which(Dummy$time==0)) > 0) Dummy <- Dummy[-w,]
  Os1 <- Dummy %>% filter(names == "subject 1") %>% dplyr::select(time,names)
  Om1 <- Dummy %>% filter(names == "metronome 1") %>% dplyr::select(time,names)
  Os2 <- Dummy %>% filter(names == "subject 2") %>% dplyr::select(time,names)
  Om2 <- Dummy %>% filter(names == "metronome 2") %>% dplyr::select(time,names)
  dummy_C1 <- do_calc_phase2(Os1,Om1)
  dummy_C2 <- do_calc_phase2(Os2,Om2)
  Dummy <- rbind(dummy_C1,dummy_C2) 
  w <- which(Dummy$time > 2*aligned_beat_sec)
  Observations_relphase <- Dummy[w,] %>% mutate(time = time%%aligned_beat_sec)
  rm(Dummy,Om1,Os1,Om2,Os2)
  rm(dummy_C1,dummy_C2)
  
  # 3. Original continous states
  Dummy <- States %>% filter(sim == s)
  # relative phase is given by : Dummy$m1 - Dummy$s1
  w <- which(Dummy$time > 2*aligned_beat_sec)
  Dummy <- Dummy[w,]
  # select cycle 3 from Observations and do modulo time to reset time
  w <- which(Dummy$time > 2*aligned_beat_sec)
  States_selected <- Dummy[w,] %>% mutate(time = time%%aligned_beat_sec)
  
  # #########################################
  # Create the plot
  cbPalette <- c("#E69F00","#56B4E9","#E69F00","#56B4E9", "black", "gray","#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7","#999999", "#E69F00", "#999999" )
  
  # 1. plot relative phase
  p0 <- ggplot() +
    # ########
  # 1. Use Observations generated continuous
  geom_lineribbon(data = observations_generated_relphase, aes(x = (time*aligned_beat_sec/99),# %% aligned_beat_sec, 
                                                              y = do_rotate(relphase.y),
                                                              ymin = do_rotate(relphase.lower),
                                                              ymax = do_rotate(relphase.upper),
                                                              color = names),alpha=.5) +
    # ########
  # 2. Use Observations original discrete
  geom_point(data=Observations_relphase,
             aes(x=time%%aligned_beat_sec,
                 y=do_rotate(relphase),
                 color = names), shape = 1, size = 2, alpha = 1) +
    # ########
  # 3. Use Original continuous states
  # geom_line(data = States_selected,  aes(x=time%%aligned_beat_sec,
  #                                       y=do_rotate(m1-s1)), linetype = 6, linewidth = .5, color = "black") +
  #  geom_line(data = States_selected,  aes(x=time%%aligned_beat_sec,
   #                                        y=do_rotate(m2-s2)), linetype = 6, linewidth = .5, color = "gray") +
    scale_fill_manual(values=cbPalette) +
    scale_colour_manual(labels = c("fitted m1-s1", "fitted m2-s2", "defined m1-s1", "defined m2-s2"), values=cbPalette) +
    #ylim(-pi/2,+pi/2) + 
    theme_bw() + 
    ggtitle(paste("model",s,"\n",ss) ) +
    xlab("time [s]") + ylab("relative phase")
  
  
  
  # #########################################
  # Data preparation:
  
  # 2. Instantaneous period of continuous generated observations
  m1_ip <- m1 %>% mutate(dobservations_generated = c(NA,diff(observations_generated)),names = "m1") 
  s1_ip <- s1 %>% mutate(dobservations_generated = c(NA,diff(observations_generated)),names = "s1")
  m2_ip <- m2 %>% mutate(dobservations_generated = c(NA,diff(observations_generated)),names = "m2")
  s2_ip <- s2 %>% mutate(dobservations_generated = c(NA,diff(observations_generated)),names = "s2")
  observations_generated_instperiod = rbind(m1_ip,s1_ip,m2_ip,s2_ip) 
  observations_generated_instperiod$names = factor(observations_generated_instperiod$names, levels = c("m1","s1","m2","s2"))
  rm(m1_ip,s1_ip,m2_ip,s2_ip)
  
  
  observations_generated_instperiod_m <- observations_generated_instperiod %>% filter(names %in% c("m1","m2"))
  observations_generated_instperiod_s <- observations_generated_instperiod %>% filter(names %in% c("s1","s2"))
  
  # #########################################
  
  States_selected_long <- States_selected %>%
    mutate(ds1 = 2*pi/c(NA,diff(s1)), ds2 = 2*pi/c(NA,diff(s2)) ) %>%
    pivot_longer(cols = c("ds1","ds2"))
  #2. Plot instantaneous phase
  cbPalette <- c("#E69F00","#56B4E9","#E69F00","#56B4E9")
  p1 <-    ggplot() + 
    geom_line(data = observations_generated_instperiod_s, 
              aes(x=time*aligned_beat_sec/99,y=(aligned_beat_sec/99)*2000*pi/(dobservations_generated),color=names),linewidth=1.5)  + 
    geom_line(data = States_selected_long, 
              aes(x=time, y= value, color = name), linetype = "dashed", linewidth = 1) +
    theme_bw()+
    scale_fill_manual(values=cbPalette) +
    scale_colour_manual(labels = c("fitted ds1", "fitted ds2", "defined ds1", "defined ds2"), values=cbPalette) +
   # ggtitle(paste("States (black) and generated states (color), model",s,"\n",ss) )+
    ylab("inst. period") +
    xlab("time [s]")
  
  
  make_fig(paste("Figures/chapTappers_StateSpaceModel_PosteriorPredictions_Relphase_model_",s,sep=""), p0)
  make_fig(paste("Figures/chapTappers_StateSpaceModel_PosteriorPredictions_InstPeriod_model_",s,sep=""), p1)
  
  p01 <-  p0/p1 
  p01
}
run = FALSE



