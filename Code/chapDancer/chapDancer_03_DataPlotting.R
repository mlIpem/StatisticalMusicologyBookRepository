# chapDancer_DataPlotting


run = FALSE
# run = TRUE
if(run){
  cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7", "#509999","#A072B2","#FF8442","#F02242")
  #cbPalette <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
  #  scale_fill_manual(values=cbPalette) +
  #  scale_colour_manual(values=cbPalette) +
  
  ############################################
  # plotting Data per fragment: without and with axis/data rotation 
  Dataplot_p1 <- Data %>% filter(Heel == "HeelsDown") %>% 
    ggplot() +
    geom_point(aes(x=Fragment,y=phase, color = Trialf), 
               position = position_jitterdodge(jitter.width = .05, dodge.width = .5), shape=1, alpha = .7) +
    theme_bw() + theme(legend.position = "none") +
    scale_fill_manual(values=cbPalette) +
    scale_colour_manual(values=cbPalette) +
    ylab("relative phase")+ xlab("")
  Dataplot_p2 <- Data %>% filter(Heel == "HeelsDown") %>% 
    ggplot() +
    geom_point(aes(x=Fragment,y=((phase-pi) %% (2*pi)) -pi, color = Trialf), 
               position = position_jitterdodge(jitter.width = .05, dodge.width = .5), shape=1, alpha = .7) +
    theme_bw() + theme(legend.position = "none")+
    scale_fill_manual(values=cbPalette) +
    scale_colour_manual(values=cbPalette) +
    ylab("relative phase rotated") + xlab("")
  Dataplot1 <- Dataplot_p1 + Dataplot_p2
  
  ############################################
  # plotting Data of one single trial, one fragment, heels_down
  Dat <- Data %>% filter(Trialf %in% c("01"), Fragment == "F1", Heel == "HeelsDown") %>%
    dplyr::select(phase,time0,Participant)
  #head(Dat)
  
  Dataplot_p3 <- ggplot(Dat) +
    geom_point(aes(x=time0,y=((phase-pi)%%(2*pi)) -pi , color = Participant)) +
    theme_bw() + theme(legend.position = "none")+
    scale_fill_manual(values=cbPalette) +
    scale_colour_manual(values=cbPalette) +
    xlab("time [sec]\n F1 trial 1") +
    ylab("relative phase") + 
    theme(legend.position = "none") 
  
  # plotting Data of all trials, one fragment, heels_down
  Dat <- Data %>% filter(Fragment == "F1", Heel == "HeelsDown") %>%
    dplyr::select(phase,time0,Participant, Trialf)
  head(Dat)
  Dataplot_p4 <- ggplot(Dat) +
    geom_point(aes(x=time0,y=((phase-pi)%%(2*pi)) -pi , color = Participant)) +
    theme_bw() + theme(legend.position = "none") +
    scale_fill_manual(values=cbPalette) +
    scale_colour_manual(values=cbPalette) +
    xlab("time [sec]\n F1 all trials") +
    ylab("")
  
  Dataplot2 <- Dataplot_p3 + Dataplot_p4
  
  make_fig("Figures/chapDancer_Dataplot_p1", Dataplot_p1)
  make_fig("Figures/chapDancer_Dataplot_p2", Dataplot_p2)
  make_fig("Figures/chapDancer_Dataplot_p3", Dataplot_p3)
  make_fig("Figures/chapDancer_Dataplot_p4", Dataplot_p4)
}
run = FALSE
