# #########################################################################################################
# Global Histograms: all coupling strengths, all phase delays
# #########################################################################################################
# To get Posterior, see chapTappers2_05_SSM_DataPreparation
run = FALSE
# run = TRUE
if(run){
  # hist(dat_selected_K$.value, n=60)
  #  hist(dat_selected_D$phasedelayvalue, n=60)
  # 
  cbPalette <- c("#E69F00", "#56B4E9", "#009E73", "#999999", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
  # scale_fill_manual(values=cbPalette) +
  #  scale_colour_manual(values=cbPalette) +
  
  chapTappers2_distrK <-  Posterior %>% filter(.variable %in% c("ks1m1","ks2m2")) %>%
    separate(codycy, into = c("condition", "dyad", "cycle"), sep = "\\.") %>%
    ggplot() + 
    geom_histogram(aes(x=.value, fill=condition),
                   position = position_dodge(width =.02), alpha = 0.7) +
    theme_bw() + 
    scale_fill_manual(values=cbPalette) +
    scale_colour_manual(values=cbPalette) +
    xlab("coupling strength") + facet_grid(~ .variable)
  
  chapTappers2_distrD <- Posterior %>% filter(.variable %in% c("ds1m1","ds2m2","ds1s2","ds2s1")) %>%
    separate(codycy, into = c("condition", "dyad", "cycle"), sep = "\\.") %>%
    ggplot() + 
    geom_histogram(aes(x=.value, fill = condition, color=condition),
                   position = position_dodge(width =.15), alpha = 0.7) +
    theme_bw() + 
    scale_fill_manual(values=cbPalette) +
    scale_colour_manual(values=cbPalette) +
    xlab("phase delay to metronome") + facet_grid(~ .variable)
  
  chapTappers2_distrObserved <- Posterior %>% 
    filter(.variable %in% c("observation_std[1]","observation_std[2]","observation_std[3]","observation_std[4]")) %>%
    separate(codycy, into = c("condition", "dyad", "cycle"), sep = "\\.") %>%
    ggplot() + 
    geom_histogram(aes(x=.value, fill = condition, color=condition),
                   position = position_dodge(width =.15), alpha = 0.7) +
    theme_bw() + 
    scale_fill_manual(values=cbPalette) +
    scale_colour_manual(values=cbPalette) +
    xlab("phase delay to partner") + facet_grid(~ .variable)
  
  make_fig("Figures/chapTappers2_distrK", chapTappers2_distrK)
  make_fig("Figures/chapTappers2_distrD", chapTappers2_distrD)
  make_fig("Figures/chapTappers2_distrObserved", chapTappers2_distrObserved)
}
run = FALSE






# #########################################################################################################
# Histograms in more detail of dyads/cycles/conditions in 2 graphs
# Based on the Posterior summary obtained from 06_GetPosterior (ASIL)
# #########################################################################################################

  do_plot_postdist <- function(dataa){
    # The palette with grey:
    cbPalette <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7","#999999")
    # scale_fill_manual(values=cbPalette) +
    #  scale_colour_manual(values=cbPalette) +
    p <- ggplot(dataa) +
      geom_pointinterval(aes(y = .value, 
                             x = cycle,  
                             ymin = .lower, ymax = .upper, color =  .variable), 
                         position = position_dodge(width = .0),
                         shape=1) + 
      scale_x_discrete(
        breaks = c(0,5,10),
        labels = c("0"="0","5"="5","10"="10")  # Custom labels
      ) +
      scale_fill_manual(values=cbPalette) +
      scale_colour_manual(values=cbPalette) +
      theme_bw() +
     # ylab("coupling") + xlab("cycles") +
     # ylim(0,1) +
      # theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1)) +

      facet_grid(condition ~dyad) #+ ggtitle(condi)
    return(p)
  }



  
  run = FALSE
  # run = TRUE
  if(run){
  ############# K : select coupling strength
  p0_K <- Posterior %>% 
    filter(.variable %in% c("ks1m1","ks2m2"))  %>% 
    mutate(dyad = factor(dyad, label = all_dyads_numbers)) %>% 
    do_plot_postdist() + ylab("coupling strength") + xlab("cycles")
  ############# D : select phase delay
  p0_D <- Posterior %>% 
    filter(.variable %in% c("ds1m1","ds2m2","ds1s2","ds2s1")) %>%
    mutate(dyad = factor(dyad, label = all_dyads_numbers)) %>% 
    do_plot_postdist() + ylab("phase delay") + xlab("cycles")
  ############# D : select observation_std
  p0_Observation_std <- Posterior %>% 
    filter(.variable %in% c("observation_std[1]","observation_std[2]",
                            "observation_std[3]","observation_std[4]")) %>%
    mutate(dyad = factor(dyad, label = all_dyads_numbers)) %>% 
    do_plot_postdist() + ylab("fluctuation") + xlab("cycles") +
    scale_colour_manual(label = c("m1","s1",
                                  "m2","s2"), values=cbPalette) +
    labs(color = "subjects")
 

  # ############# fold function
  # make_pdf_figure <- function(fn,fig){
  #   fn_pdf = paste(fn,".pdf",sep="")
  #   print(paste("plotting", fn_pdf))
  #   pdf(file = fn_pdf ,width=9, height=3)  #Note that you can convert inches to centimeters dividing by 2.54.
  #   print(fig)
  #   dev.off()
  # }
  # make_png_figure <- function(fn,fig){
  #   fn_png = paste(fn,".png",sep="")
  #   print(paste("plotting", fn_png))
  #   png(filename=fn_png, width=9, height=3, units = "in", res = 600)
  #   print(fig)
  #   dev.off()
  # }
  
  make_fig(paste("Figures/chapTappers2_Overview_postdistr_K",sep=""), p0_K)
  make_fig(paste("Figures/chapTappers2_Overview_postdistr_D",sep=""), p0_D)
  make_fig(paste("Figures/chapTappers2_Overview_postdistr_Observation_std",sep=""), p0_Observation_std)
  
}
run = FALSE


#################################################################
# Leader-follower
#################################################################
# calc leader-follower
#   dat_selected_Ks <- dat_selected_K %>% dplyr::filter(condition %in% c("1PU","2PC")) %>% droplevels()

run = FALSE
# run = TRUE
if(run){
  dat_w <- Posterior %>% 
    filter(.variable %in% c("ks1m1","ks2m2"))  %>% 
    select(.variable, .value, condition, dyad, cycle, codycy) %>% 
    pivot_wider(names_from = .variable, values_from = .value) %>% 
    mutate(lf = ks1m1-ks2m2) %>% 
    select(-ks1m1,-ks2m2) 
 
  M_U <- dat_w %>% filter(condition=="U") %>% summarize(m=mean(lf, na.rm = TRUE),sd = sd(lf,na.rm=TRUE))
  M_C <- dat_w %>% filter(condition=="C") %>% summarize(m=mean(lf, na.rm = TRUE),sd = sd(lf,na.rm=TRUE))
  
  
  
  # plot
  chapTappers2_LeaderFollower <- ggplot(dat_w) +
    geom_point(aes(x=dyad,y=lf, color = condition), 
               position = position_jitterdodge(jitter.width = .2, jitter.height = 0),
               size = 2, shape = 1, alpha = .5) +
    stat_pointinterval(data = dat_w, aes(x=dyad,y=(lf), color = condition)) +  
    geom_hline(yintercept = 0, color = "#D55E00",linetype = "dotted") +
    scale_fill_manual(values=cbPalette) +
    scale_colour_manual(values=cbPalette) +
    theme_bw() + 
    theme(axis.text.x = element_text(angle = 75, vjust = 0.4, hjust=.5)) +
    labs(x="dyads",y="LF = ks1m1 - ks2m2") +
    facet_wrap(~condition,nrow=2)
  
  make_fig("Figures/chapTappers2_LeaderFollower",chapTappers2_LeaderFollower)
  
}
run = FALSE

#################################################################
#################################################################
# Plotting generated observations (posterior predictions)
#################################################################

#  model_id = "C.dyad_1.cycle_5"
#  model_id = "C.dyad_10.cycle_2"
#  model_id = "C.dyad_12.cycle_2"
do_plot_gen_obs <- function(model_id){
  # ################## 1. Get the proper posteriors
  fit <- readRDS(paste("Fitted/chapTappers2_", model_id, ".rds", sep=""))
fit_posterior <- fit %>% gather_draws(ks1m1,ks2m2,ds1m1,ds1s2,ds2m2,ds2s1, 
                                      `observation_std[1]`, `observation_std[2]`,
                                      `observation_std[3]`,`observation_std[4]`, 
                                      y0_s1, y0_s2) 
fit_ps <- fit_posterior %>% median_qi()  %>% mutate(names = factor(.variable))


# gemiddelden
# D <- dat_selected_K %>% droplevels()
# newdat <- D %>% data_grid(names, condition)
# pred <- epred_draws(fit, newdata = newdat, re_formula = NA) %>% median_qi()





# generated states summary
post_states <- fit %>%
  spread_draws(states_generated[time,unit]) %>%
  median_qi(.width=c(.75)) 

# generated observations summary
post_observations <- fit %>%
  spread_draws(observations_generated[time,unit]) %>%
  median_qi(.width=c(.75)) 

head(post_states)
head(post_observations)

# ###############################################################
# Get raw data and corresponding relative phase values

Dummy <- DTapping %>% filter(codycy == model_id) %>% arrange(time0c)
if( length(w<-which(Dummy$time0c==0)) > 0) {Dummy <- Dummy[-w,]
print(paste("---->",w,"is deleted"))}
Om1 <- Dummy %>% filter(names_indicator == 1) %>% mutate(time = time0c) %>% dplyr::select(time,names_indicator)
Os1 <- Dummy %>% filter(names_indicator == 2) %>% mutate(time = time0c) %>% dplyr::select(time,names_indicator)
Om2 <- Dummy %>% filter(names_indicator == 3) %>% mutate(time = time0c) %>% dplyr::select(time,names_indicator)
Os2 <- Dummy %>% filter(names_indicator == 4) %>% mutate(time = time0c) %>% dplyr::select(time,names_indicator)
dummy_C1 <- do_calc_phase2(Os1,Om1)
dummy_C2 <- do_calc_phase2(Os2,Om2)
Observations_relphase <- rbind(dummy_C1,dummy_C2) 
rm(Dummy,Om1,Os1,Om2,Os2)
rm(dummy_C1,dummy_C2)



# ################################################################
# calculate relative phase and instantaneous period
# From fit_ps (summary of posterior selecting parameters)
S <- fit_ps %>% 
  dplyr::select(.variable,.value) %>% 
  mutate(.value= round(.value,2)) %>% 
  rename(parameter = .variable, fitted = .value) %>% 
  arrange(match(parameter,c("ds1m1","ds2m2","ds1s2","ds2s1","ks1m1","ks2m2"))) %>% 
  data.frame() 
# Prepare string for plot
string_plot = paste(model_id, "\n",
                    "k= ", toString(S$fitted[5:6]), 
                    "; d= ", toString(S$fitted[1:4]),"\n",
                    "fluct= ", toString(S$fitted[c(8,10)]), sep="")                     
# aligned_beat_sec 
aligned_beat_sec = 39

# From post_observations (summary of posterior selecting generated quantities)
# we rotate the phase from [0, 2pi] to [-pi, pi] when plotting
m1 <- post_observations %>% filter(unit == 1) 
s1 <- post_observations %>% filter(unit == 2)
m2 <- post_observations %>% filter(unit == 3)
s2 <- post_observations %>% filter(unit == 4)

# 1. Relative phase calculation.
# Given the fact that we have the phase, we can just take the difference between the 
# phase of the metronome unit and the phase of the subject unit
dummy_data1 = data.frame(time = s1$time, 
                         relphase.y = m1$observations_generated - s1$observations_generated) %>% 
  mutate(names = "m1-s1")
dummy_data1.l = data.frame(relphase.lower = m1$.lower - s1$.lower)
dummy_data1.u = data.frame(relphase.upper = m1$.upper - s1$.upper) 

dummy_data2 = data.frame(time = s2$time, 
                         relphase.y = m2$observations_generated - s2$observations_generated) %>% 
  mutate(names = "m2-s2")
dummy_data2.l = data.frame(relphase.lower = m2$.lower - s2$.lower) 
dummy_data2.u = data.frame(relphase.upper = m2$.upper - s2$.upper) 

y = rbind(dummy_data1,dummy_data2)
ymin = rbind(dummy_data1.l,dummy_data2.l)
ymax = rbind(dummy_data1.u,dummy_data2.u)
Observations_generated_relphase = cbind(y,ymin,ymax)
# remove dummy variables
rm(dummy_data1,dummy_data1.l,dummy_data1.u)
rm(dummy_data2,dummy_data2.l,dummy_data2.u)
# 2. Instantaneous period of continuous generated observations

m1 <- post_states %>% filter(unit == 1) 
s1 <- post_states %>% filter(unit == 2)
m2 <- post_states %>% filter(unit == 3)
s2 <- post_states %>% filter(unit == 4)

m1_ip <- m1 %>% mutate(states_generated = c(NA,diff(states_generated)),names = "m1") 
s1_ip <- s1 %>% mutate(states_generated = c(NA,diff(states_generated)),names = "s1")
m2_ip <- m2 %>% mutate(states_generated = c(NA,diff(states_generated)),names = "m2")
s2_ip <- s2 %>% mutate(states_generated = c(NA,diff(states_generated)),names = "s2")
states_generated_instperiod = rbind(m1_ip,s1_ip,m2_ip,s2_ip) 
states_generated_instperiod$names = factor(states_generated_instperiod$names, levels = c("m1","s1","m2","s2"))
rm(m1_ip,s1_ip,m2_ip,s2_ip)

# #########################################
# 1. Plot relative phase
cbPalette <- c("#E69F00","#56B4E9","#E69F00","#56B4E9")

p0 <- ggplot() +
  geom_lineribbon(data = Observations_generated_relphase, 
                  aes(x = (time*aligned_beat_sec/100.5),
                      y = (relphase.y),
                      ymin = (relphase.lower),
                      ymax = (relphase.upper),
                      color = names),alpha=.5) +
  geom_point(data = Observations_relphase,
             aes(x=time,#%%aligned_beat_sec,
                 y=(relphase - (2*pi)),
                 color = factor(names_indicator)), shape = 1, size = 2, alpha = 1) +
  scale_fill_manual(values=cbPalette) +
  scale_colour_manual(labels = c("m1-s1", "m2-s2"), values=cbPalette) +
  #ylim(-pi/2,+pi/2) + 
  theme_bw() + 
  ggtitle(paste("model",string_plot) ) +
  xlab("time [s]") + ylab("relative phase")

#2. Plot instantaneous phase
cbPalette <- c("#E69F00","#E69F00","#56B4E9","#56B4E9")
p1 <-    ggplot() + 
  geom_line(data = states_generated_instperiod, 
            aes(x=time*aligned_beat_sec/100.5,
                y=(aligned_beat_sec/100.5)*2000*pi/(states_generated),color=names),linewidth=1.5)  + 
  theme_bw() +
  scale_fill_manual(values=cbPalette) +
  scale_colour_manual(labels = c("dm1", "ds1", "dm2", "ds2"), values=cbPalette) +
  # ggtitle(paste("States (black) and generated states (color), model",s,"\n",ss) )+
  ylab("inst. period") +
  xlab("time [s]")
p01 <-  p0/p1 
make_fig(paste("Figures/chapTappers2_", model_id, sep=""), p01)
return(p01)
}
