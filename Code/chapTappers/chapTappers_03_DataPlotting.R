# chapTappers_DataPlotting

run = FALSE
# run = TRUE
if(run){
  States <- readRDS(file = "Data/chapTappers_States.rds")
  
  # Here we define which dynamic model we want to see plotted
  for(KDsample in c(1,21)){
    print(paste("--------->KDsample = ", KDsample))
    # KDsample = 1
    # plot States and Observations as relative phase
    States_KDsample <- States %>% filter(sim == KDsample)
    # In States we have the phase in m1,s1,m2,s1
    # In Observations we have the times of the names_indicator (also: m1,s1,m2,s1)
    Observations_KDSample <- Observations %>% filter(sim == KDsample)
    Om1 <- Observations_KDSample %>% filter(names_indicator == 1)
    Os1 <- Observations_KDSample %>% filter(names_indicator == 2)
    Om2 <- Observations_KDSample %>% filter(names_indicator == 3)
    Os2 <- Observations_KDSample %>% filter(names_indicator == 4)
    # do_calc_phase2 is based on times
    Om1s1 <- do_calc_phase2(Os1,Om1)
    Om2s2 <- do_calc_phase2(Os2,Om2)
    rm(Om1,Os1,Om2,Os2)
    
    # ####################
    # plot relative phase
    p1 <- 
      # relative phase of States
      ggplot()+
      geom_line(data=States_KDsample,aes(x=time,y=do_rotate(m1 - s1)),linewidth=1.5, colour = "#56B4E9") +
      geom_line(data=States_KDsample,aes(x=time,y=do_rotate(m2 - s2)),linewidth=1.5, colour = "#E69F00") +
      # relative phase of Observations
      geom_point(data = Om1s1, aes(x=time,y=do_rotate(relphase)),shape=1,size=2,colour = "#56B4E9") +
      geom_point(data = Om2s2, aes(x=time,y=do_rotate(relphase)),shape=1,size=2,colour = "#E69F00") +
      
      # vertical line
      geom_vline(xintercept=aligned_beat_sec, linetype="dotted", size=1.5, colour = "#D55E00")+
      theme_bw()+
      ggtitle(paste("simulated states and observations, model", KDsample, "\n",
                    "k=", toString(round(K[KDsample,c("ks1m1","ks2m2")],2) ), 
                    "d=", toString(  round(D[KDsample,c("ds1m1","ds2m2")],2) )) ) +
      #ylim(-pi,pi) +
      labs(colour = "names") +
      ylab("relative phase")
    # p1 
    rm(Om1s1,Om2s2)
    
    # ##############################################
    # plot the states as instantaneous period
    cbPalette <- c("#56B4E9","#E69F00","#56B4E9","#E69F00", "#009E73", "#999999", "#E69F00", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
    dummy <- States_KDsample %>% 
      mutate(
        dphi_m1 = c(NA,diff(m1)) ,
        dphi_s1 = c(NA,diff(s1)),
        dphi_m2 = c(NA,diff(m2)),
        dphi_s2 = c(NA,diff(s2)),
        p_m1 = 2*pi/dphi_m1, # instantaneous period
        p_s1 = 2*pi/dphi_s1,
        p_m2 = 2*pi/dphi_m2,
        p_s2 = 2*pi/dphi_s2
      ) %>% 
      select(time,sim,p_m1,p_s1,p_m2,p_s2)
    dummy_Long <- dummy %>% pivot_longer(cols = c(p_m1,p_s1,p_m2,p_s2))
    
    p2 <-    ggplot(dummy_Long) + 
      geom_line(aes(x=time,y=value,color=name),size=1.5) +  
      geom_vline(xintercept=aligned_beat_sec, linetype="dotted", color = "#D55E00", size=1.5) +
      theme_bw()+
      scale_fill_manual(values=cbPalette) +
      scale_colour_manual(labels = c("m1","m2","s1","s2"),values=cbPalette) +
      ggtitle(paste("simulated states and observations, model", KDsample, "\n",
                    "k=", toString(round(K[KDsample,c("ks1m1","ks2m2")],2) ), 
                    "d=", toString(round(D[KDsample,c("ds1m1","ds2m2")],2) )) ) +
      ylab("instantaneous period")
    # p2
    rm(dummy,dummy_Long)
    
    # p3 <- p1 / p2
    # p3
    
    fn = paste("Figures/chapTappers_SimulatedStatesObservations_model_",KDsample,"_RelativePhase", sep="")
    make_fig(fn, p1)
    fn = paste("Figures/chapTappers_SimulatedStatesObservations_model_",KDsample,"_InstPeriod", sep="")
    make_fig(fn, p2)
  }
  
}
run = FALSE
