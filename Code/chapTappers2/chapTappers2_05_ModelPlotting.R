# chapTappers2_05_ModelPlotting
# 
cbPalette <- c("#E69F00", "#56B4E9", "#009E73" ,"#999999", "#F0E442", "#0072B2", "#D55E00", "#CC79A7" )
# scale_fill_manual(values=cbPalette) +
#  scale_colour_manual(values=cbPalette) +

########################################
# Smooth regression analysis 
########################################

run = FALSE
# run = TRUE
if(run){
fit <- readRDS(file="Fitted/chapTappers2_SmoothRegression_RelPhase.rds") # calculated with server
# DTapping_RelativePhase <- readRDS("Data/chapTappers2_DTapping_RelativePhase.rds")

Data <- DTapping_RelativePhase %>% mutate(condition = ifelse(condition == "U", "1PU", "2PC"), 
                                          condition = factor(condition,levels = c("1PU","2PC")),
                                          namescondition = interaction(names,condition)) %>% 
  dplyr::filter(!(dyad %in% c("dyad_1", "dyad_3", "dyad_5", "dyad_6", "dyad_8", "dyad_13", "dyad_15")), 
                  cycle %in% c("cycle_2","cycle_4","cycle_6"))
                  
NewData = Data %>% data_grid(namescondition, time0c = seq(0,39,length.out=50))

ed <- epred_draws(newdata = NewData, fit, re_formula = NA) %>% median_qi(.epred)

p0 <- ed %>%
  ggplot() +
  geom_ribbon(aes(x=time0c, y= .epred, ymin = .lower, ymax = .upper, fill = namescondition), 
              alpha = .7) +
  theme_bw() +
  scale_fill_manual(values=cbPalette) +
  scale_colour_manual(values=cbPalette) +
  labs(x="time[sec]", y="relative phase")

make_fig("Figures/chapTappers2_SmoothRegression_RelPhase",p0)


s1 <- ed %>% filter(namescondition == "subject 1.2PC") %>% select(.epred)
s2 <- ed %>% filter(namescondition == "subject 2.2PC") %>% select(.epred)
plot(s1$.epred + s2$.epred)

}
run = FALSE






#######################################################################
# After modelling the dataset with parameters, we show condition * names
# for K
# see chapTappers2_04_Modelling.R
#######################################################################

run = FALSE
# run = TRUE
if(run){
  fit <- readRDS(file = "Fitted/chapTappers2_fit_K00.rds")
  Posterior_selected <- Posterior %>% 
    filter(.variable %in% c("ks1m1","ks2m2")) %>% droplevels() %>% 
    select(condition,dyad,cycle, .variable,.value)
  
  pred <- Posterior_selected %>% 
    add_epred_draws(fit, re_formula = NA) %>% ungroup() %>% 
    group_by(condition,.variable) %>% median_qi(.epred )
  
  cbPalette <- c("#E69F00", "#56B4E9", "#009E73" ,"#999999", "#F0E442", "#0072B2", "#D55E00", "#CC79A7" )
  
  p <- ggplot(pred) +
    geom_errorbar(aes(x=condition,y=.epred, ymin = .lower, ymax = .upper, color = .variable), 
                  width = .5, linewidth = 1,
                  position = position_dodge(width = .5) ) +
    geom_point(aes(x=condition,y=.epred, color = .variable), 
               size = 3,
               position = position_dodge(width = .5) ) +
    scale_fill_manual(values=cbPalette) +
    scale_colour_manual(values=cbPalette) +
    geom_point(data = Posterior_selected, 
               aes(x=condition,y=.value, color=.variable), 
               alpha = .45, size = 2, shape=1, #color = "#999999",
               position = position_jitterdodge(jitter.width = .2, dodge.width = .5) ) +
    theme_bw() +  #theme(legend.position="none") +
    ylab("coupling strength") + xlab("condition") 
  
  make_fig("Figures/chapTappers2_modelK00",p)
}
run = FALSE


#######################################################################
# After modelling the dataset with parameters, we show condition * names
# for D
#######################################################################

run = FALSE
# run = TRUE
if(run){
  fit <- readRDS(file = "Fitted/chapTappers2_fit_D00.rds")

  Posterior_selected <- Posterior %>% 
    filter(.variable %in% c("ds1m1","ds2m2","ds1s2","ds2s1")) %>% droplevels() %>% 
    select(condition,dyad,cycle, .variable,.value)
  
    pred <- Posterior_selected %>% droplevels() %>% select(condition,dyad,cycle,.variable) %>% 
    add_epred_draws(fit, re_formula = NA) %>% ungroup() %>% group_by(condition,.variable) %>% median_qi(.epred )
  
  p <- ggplot(pred) +
    geom_errorbar(aes(x=condition,y=.epred, ymin = .lower, ymax = .upper, color = .variable), 
                  width = .5, linewidth = 1,
                  position = position_dodge(width = .5) ) +
    geom_point(aes(x=condition,y=.epred, color = .variable), 
               size = 3,
               position = position_dodge(width = .5) ) +
    scale_fill_manual(values=cbPalette) +
    scale_colour_manual(values=cbPalette) +
    geom_point(data = Posterior_selection, 
               aes(x=condition,y=.value, color=.variable), 
               alpha = .45, size = 2, shape=1, #color = "#999999",
               position = position_jitterdodge(jitter.width = .2, dodge.width = .5) ) +
    theme_bw() +  #theme(legend.position="none") +
    ylab("phase delay") + xlab("condition") 
  
  make_fig("Figures/chapTappers2_modelD00",p)
  
  # to be deleted:
  # fit <- readRDS(file = "Fitted/chapTappers2_fit_D00.rds")
  # pred <- dat_selected_DDD %>% droplevels() %>% select(condition,dyad,names,cycle) %>% 
  #   add_epred_draws(fit, re_formula = NA) %>% ungroup() %>% group_by(condition,names) %>% median_qi(.epred )
  # 
  # T1 <- pred %>% dplyr::select(condition,names,.epred)
  # 
  
}
run = FALSE

#######################################################################
# After modelling the dataset with parameters, we show condition * names
# for observation_std (fluctuation)
#######################################################################

run = FALSE
# run = TRUE
if(run){
  fit <- readRDS(file = "Fitted/chapTappers2_fit_Observation_std00.rds")
  
  Posterior_selection <- Posterior %>% 
    filter(.variable %in% c("observation_std[1]",
                            "observation_std[2]",
                            "observation_std[3]",
                            "observation_std[4]")) %>%
    mutate(dyad = factor(dyad, levels = all_dyads), cycle = factor(cycle, labels = all_cycles_numbers),
           condition = factor(condition, levels = all_conditions),
           .variable = factor(.variable))
  
  pred <- Posterior_selection %>% droplevels() %>% select(condition,dyad,cycle,.variable) %>% 
    add_epred_draws(fit, re_formula = NA) %>% ungroup() %>% group_by(condition,.variable) %>% median_qi(.epred )
  
  p <- ggplot(pred) +
    geom_errorbar(aes(x=condition,y=.epred, ymin = .lower, ymax = .upper, color = .variable), 
                  width = .8, linewidth = 1,
                  position = position_dodge(width = .5) ) +
    geom_point(aes(x=condition,y=.epred, color = .variable), 
               size = 3,
               position = position_dodge(width = .5) ) +
    scale_fill_manual(values=cbPalette) +
    scale_colour_manual(labels = c("fluct_m1","fluct_s1","fluct_m2","fluct_s2"),  values=cbPalette) +
    geom_point(data = Posterior_selection, 
               aes(x=condition,y=.value, color=.variable), 
               alpha = .45, size = 2, shape=1, #color = "#999999",
               position = position_jitterdodge(jitter.width = .2, dodge.width = .5) ) +
    theme_bw() +  #theme(legend.position="none") +
    ylab("fluctuation") + xlab("condition") 
  
  make_fig("Figures/chapTappers2_modelObservation_std00",p)
  
  # to be deleted:
  # fit <- readRDS(file = "Fitted/chapTappers2_fit_D00.rds")
  # pred <- dat_selected_DDD %>% droplevels() %>% select(condition,dyad,names,cycle) %>% 
  #   add_epred_draws(fit, re_formula = NA) %>% ungroup() %>% group_by(condition,names) %>% median_qi(.epred )
  # 
  # T1 <- pred %>% dplyr::select(condition,names,.epred)
  # 
  
}
run = FALSE










# #############################################
# #############################################
# Needed???
# Plotting the relative phase and instantaneous frequency graphs of selected models
##############################################
##############################################
# run = FALSE
# # run = TRUE
# if(run){
#   ############# fold function
#   make_pdf_figure <- function(fn,fig){
#     fn_pdf = paste(fn,".pdf",sep="")
#     print(paste("plotting", fn_pdf))
#     pdf(file = fn_pdf ,width=9, height=2)  #Note that you can convert inches to centimeters dividing by 2.54.
#     print(fig)
#     dev.off()
#   }
#   make_png_figure <- function(fn,fig){
#     fn_png = paste(fn,".png",sep="")
#     print(paste("plotting", fn_png))
#     png(filename=fn_png, width=9, height=2, units = "in", res = 600)
#     print(fig)
#     dev.off()
#   }
#   
# load("Data/chapTappers2_DTapping_2PC.rda")
# DTapping_2PC <- DTapping
# load("Data/chapModelling_DTapping_1PU.rda")
# DTapping_1PU <- DTapping
# 
# ###### function ----> collapse the function to keep the overview
# do_plotting_relphase_instfreq <- function(fit,dy,cy,condi,dtapping){
#   dat <- dtapping %>% filter(dyad == dy, cycle == cy)
#   
# # Take draws from the posterior distribution's parameters
# S <- fit %>% 
#   gather_draws(ks1m1,ks2m2,ds1m1,ds2m2)  %>% 
#   #    S <-   fit %>%  gather_draws(ks1m1,ks2m2,ds1m1,ds1s2,ds2m2,ds2s1)  %>% 
#   median_qi() %>% 
#   dplyr::select(.variable,.value) %>% 
#   mutate(.value= round(.value,2)) 
# 
# # prepare string for plot
# ss <- paste("\n","k=", toString(S$.value[3:4]), 
#             "d=", toString(S$.value[1:2]))
# 
# # posterior prediction from fitted, to get phase_functions
# res <- fit %>%
#   spread_draws(phase_gen[time,oscillator]) %>%
#   median_qi(.width=c(.95)) %>% 
#   mutate(oscillator = oscillator) #, time = time*aligned_beat_sec/100) 
# 
# # Rotate generated quantities from [0, 2pi] to [-pi, pi]
# m1 <- res %>% filter(oscillator == 1)
# s1 <- res %>% filter(oscillator == 2)
# m2 <- res %>% filter(oscillator == 3)
# s2 <- res %>% filter(oscillator == 4)
# data1 = data.frame(time = s1$time, sm_y = m1$phase_gen - s1$phase_gen) %>% mutate(names = "s1")
# data1.l = data.frame(.lower = m1$.lower - s1$.lower)
# data1.u = data.frame(.upper = m1$.upper - s1$.upper) 
# data2 = data.frame(time = s2$time, sm_y = m2$phase_gen - s2$phase_gen) %>% mutate(names = "s2")
# data2.l = data.frame(.lower = m2$.lower - s2$.lower) 
# data2.u = data.frame(.upper = m2$.upper - s2$.upper) 
# y = rbind(data1,data2)
# ymin = rbind(data1.l,data2.l)
# ymax = rbind(data1.u,data2.u)
# dd = cbind(y,ymin,ymax)
# 
# # Instantaneous period ODE
# m1_ip <- m1 %>% mutate(dphase_gen = c(NA,diff(phase_gen)), names = "m1", condition = condi, dyad = dy, cycle = cy) 
# s1_ip <- s1 %>% mutate(dphase_gen = c(NA,diff(phase_gen)), names = "s1", condition = condi, dyad = dy, cycle = cy) 
# m2_ip <- m2 %>% mutate(dphase_gen = c(NA,diff(phase_gen)), names = "m2", condition = condi, dyad = dy, cycle = cy) 
# s2_ip <- s2 %>% mutate(dphase_gen = c(NA,diff(phase_gen)), names = "s2", condition = condi, dyad = dy, cycle = cy) 
# IP = rbind(m1_ip,s1_ip,m2_ip,s2_ip) 
# IP$names = factor(IP$names, levels = c("m1","s1","m2","s2"))
# 
# # Store IP in InstPhase data.frame
# #InstPhase <- rbind(InstPhase,IP)
# 
# 
# # relative phase from original data
# Dummy <- dat
# if( length(w<-which(Dummy$time==0)) > 0) Dummy <- Dummy[-w,]
# s1 <- Dummy %>% filter(names_indicator == 2) %>% 
#   dplyr::select(time,names_indicator) %>% ungroup() %>% mutate(names = factor("s1"))
# m1 <- Dummy %>% filter(names_indicator == 1) %>% 
#   dplyr::select(time,names_indicator) %>% ungroup() %>% mutate(names = factor("m1"))
# s2 <- Dummy %>% filter(names_indicator == 4) %>% 
#   dplyr::select(time,names_indicator) %>% ungroup() %>% mutate(names = factor("s2"))
# m2 <- Dummy %>% filter(names_indicator == 3) %>% 
#   dplyr::select(time,names_indicator) %>% ungroup() %>% mutate(names = factor("m2"))
# C1 <- do_calc_phase2(s1,m1)
# C2 <- do_calc_phase2(s2,m2)
# C_noisy <- rbind(C1,C2) 
# 
# cbPalette <- c("#999999", "#E69F00", "#999999", "#E69F00","#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
# # 1. plot rerlative phase
# #  # aligned_beat_sec : can change in 02_DataPreparation
# aligned_beat_sec = 39
# p0 <- ggplot() +
#   geom_lineribbon(data = dd, aes(x = time*aligned_beat_sec/100.55, 
#                                  y = ((sm_y + pi) %%(2*pi)) -pi,
#                                  ymin = ((.lower + pi) %%(2*pi)) -pi,
#                                  ymax = ((.upper + pi) %%(2*pi)) -pi,
#                                  color = names),alpha=.5) +
#   geom_point(data=C_noisy,
#              aes(x=time%%aligned_beat_sec,
#                  y=((relphase + pi) %%(2*pi)) -pi,
#                  color = factor(names)),shape = 1,size = 3,alpha = 1) +
#   scale_fill_manual(values=cbPalette) +
#   scale_colour_manual(values=cbPalette) +
#   ylim(-pi,+pi) + theme_bw() + 
#   ggtitle(paste(dy, cy, "condition" ,condi, ss) ) +
#   xlab("time [s]") + ylab("relative phase [rad]")
# #p0
# 
# cbPalette <- c("#999999", "#E69F00", "#999999", "#E69F00","#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
# 
# IPm <- IP %>% filter(names %in% c("m1","m2"))
# IPs <- IP %>% filter(names %in% c("s1","s2"))
# 
# #2. Plot instantaneous phase
# p1 <-    ggplot() + 
#   geom_line(data = IPm, 
#             aes(x=time*aligned_beat_sec/100.55, y=(aligned_beat_sec/100.55)*2000*pi/(dphase_gen),color=names),
#             linewidth=1, linetype = "dashed")  +  
#   geom_line(data = IPs, 
#             aes(x=time*aligned_beat_sec/100.55,y=(aligned_beat_sec/100.55)*2000*pi/(dphase_gen),color=names),
#             linewidth=1.5)  +  
#   theme_bw()+
#   scale_fill_manual(values=cbPalette) +
#   scale_colour_manual(values=cbPalette) +
#   ggtitle(paste(dy, cy, "condition", condi, ss) ) +
#   ylab("inst. period [ms]") +
#   xlab("time [s]")
# #p01 <-  p0+p1  
# return = list(p0,p1)
# }
# ###### end function
# 
# load("Fitted/chapTappers2_Fitted_015_aug_tap_1PU_dyad_9_cycle_8.rda") # get: fit
# dy = "dyad_9"; cy = "cycle_8"; condi = "U"
# Plot_U = do_plotting_relphase_instfreq(fit,dy,cy,condi,DTapping_1PU)
# Plot_U
# make_fig("Figures/chapTappers2_Fitted_015_aug_tap_1PU_dyad_9_cycle_8", Plot_U[[1]])
# make_fig("Figures/chapTappers2_Fitted_015_aug_tap_1PU_dyad_9_cycle_8_InstPeriod", Plot_U[[2]])
# 
# 
# load("Fitted/chapTappers2_Fitted_015_aug_tap_2PC_dyad_9_cycle_8.rda") # get: fit
# dy = "dyad_9"; cy = "cycle_8"; condi = "C"
# Plot_C = do_plotting_relphase_instfreq(fit,dy,cy,condi,DTapping_2PC)
# Plot_C
# make_fig("Figures/chapTappers2_Fitted_015_aug_tap_2PC_dyad_9_cycle_8", Plot_C[[1]])
# make_fig("Figures/chapTappers2_Fitted_015_aug_tap_2PC_dyad_9_cycle_8_InstPeriod", Plot_C[[2]])
# 
# #################
# load("Fitted/chapTappers2_Fitted_015_aug_tap_1PU_dyad_8_cycle_5.rda") # get: fit
# dy = "dyad_8"; cy = "cycle_5"; condi = "U"
# Plot_U = do_plotting_relphase_instfreq(fit,dy,cy,condi,DTapping_1PU)
# Plot_U
# make_fig("Figures/chapTappers2_Fitted_015_aug_tap_1PU_dyad_8_cycle_5", Plot_U[[1]])
# make_fig("Figures/chapTappers2_Fitted_015_aug_tap_1PU_dyad_8_cycle_5_InstPeriod", Plot_U[[2]])
# 
# 
# load("Fitted/chapTappers2_Fitted_015_aug_tap_2PC_dyad_8_cycle_5.rda") # get: fit
# dy = "dyad_8"; cy = "cycle_5"; condi = "C"
# Plot_C = do_plotting_relphase_instfreq(fit,dy,cy,condi,DTapping_2PC)
# Plot_C
# make_fig("Figures/chapTappers2_Fitted_015_aug_tap_2PC_dyad_8_cycle_5", Plot_C[[1]])
# make_fig("Figures/chapTappers2_Fitted_015_aug_tap_2PC_dyad_8_cycle_5_InstPeriod", Plot_C[[2]])
# 
# #################
# load("Fitted/chapTappers2_Fitted_015_aug_tap_1PU_dyad_1_cycle_5.rda") # get: fit
# dy = "dyad_1"; cy = "cycle_5"; condi = "U"
# Plot_C = do_plotting_relphase_instfreq(fit,dy,cy,condi,DTapping_1PU)
# Plot_C
# make_fig("Figures/chapTappers2_Fitted_015_aug_tap_1PU_dyad_1_cycle_5", Plot_C[[1]])
# make_fig("Figures/chapTappers2_Fitted_015_aug_tap_1PU_dyad_1_cycle_5_InstPeriod", Plot_C[[2]])
# 
# }
# run = FALSE


# ##########################################
# Needed???
# Plotting the instantaneous phase of 10 cycles of one dyad in one figure, two conditions
# ##########################################
# get the Instantaneous Phase calculations

# run = FALSE
# # run = TRUE
# if(run){
# load(paste("Data/chapTappers2_InstantaneousPhase_015_aug.rda")) # to get: InstPhase
# load(paste("Data/chapTappers2_InstantaneousPhase_011may2024.rda")) # to get: InstPhase
# 
# # 1. Get instantaneous phase cleaned
# InstPhase <- InstPhase %>% mutate(cdc = interaction(condition,dyad,cycle)) 
# InstPhase_selected <- InstPhase %>% filter(cdc %in% dat_selected_manual_clean$cdc, 
#                                            condition %in% c("1PU","2PC")) %>%
#   mutate(condition = ifelse(condition == "1PU", "U", "C"), 
#          condition = factor(condition,levels = c("U","C")),
#          dyad = factor(dyad,levels = all_dyads),
#          cycle = factor(cycle, levels = all_cycles))
# 
# dy = "dyad_9"
# try <- InstPhase_selected %>% filter(dyad %in% c(dy), 
#                             names %in% c("s1","s2") ) %>% droplevels()
# chapTappers2_InstantaneousPeriods <- try %>% ggplot()+
#   geom_line(aes(x=39*time/100,y=(39/100.5)*2000*pi/(dphase_gen), color = cycle) )+
#   theme(legend.position = "bottom") +
#   theme_bw()+
#   #scale_fill_manual(values=cbPalette) +
#   #scale_colour_manual(values=cbPalette) +
#   theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
#   ylab("inst. period [ms]") +
#   xlab("time [s]") +
#   ylim(560,680) + facet_wrap(names ~ condition) +
#   ggtitle(paste(dy))
# 
# make_fig("Figures/chapTappers2_InstantaneousPeriods",chapTappers2_InstantaneousPeriods)
# }
# run = FALSE
# 

# ##################################################
# Estimation of coupling strenght parameters per dyad (end of the chapter)
# ##################################################
 
run = FALSE
# run = TRUE
if(run){
  cbPalette <- c("#E69F00", "#56B4E9", "#009E73", "#999999", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
  # scale_fill_manual(values=cbPalette) +
  #  scale_colour_manual(values=cbPalette) +
  
  # Df <- readRDS(file = "Data/chapTappers2_EstimatedParameters_perDyad")
  
  
  p <-  Df %>% mutate(dyad = factor(dyad,levels=all_dyads), names = factor(names), condition = factor(condition)) %>%
    ggplot() +
    geom_errorbar(aes(x = dyad, ymin = .lower, ymax = .upper, color = names),
                  position = position_dodge(width = .4)) +
    geom_point(aes(x = dyad, y = .epred, color = names),
               position = position_dodge(width = .4)) +
    geom_hline(yintercept = c(0.765), color = c("#D55E00")) +
    #geom_hline(yintercept = , color = "#009E73") +
    scale_fill_manual(values=cbPalette) +
    scale_colour_manual(values=cbPalette) +
    theme_bw() +
    labs(y = "coupling strength", x="dyads") +
    ylim(0,1) +
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) + 
    facet_wrap(~condition,ncol = 1)
  
  make_fig("Figures/chapTappers2_EstimatedParameters_perDyad",p)
  
  Df %>% group_by(condition, names) %>% summarize(m = mean(.epred))
}
run = FALSE
