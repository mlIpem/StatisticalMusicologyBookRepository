# chapTappers2_03_DataPlotting

# ################################################################################
# Raw data plotting. Data come from the tapping experiment of Rosso et al. 2023
# DTapping_RelativePhase has been calculated in 02_DataPreparaation
# ################################################################################
cbPalette <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7","#999999" )

run = FALSE
# run = TRUE
if(run){
  # First figure showing for each dyad 
  # the performance over 10 cycles, for both conditions
  chapTappers2_AllRawdata_U_d1 <- DTapping_RelativePhase %>% 
    filter(condition == "U", dyad %in% all_dyads1) %>% ggplot() +
    geom_point(aes(x=time,
                   y=do_rotate(relphase), #make it circular between -pi and +pi
                   color = factor(names_indicator)),shape=1,size=1) +
    ylim(-pi,+pi) + 
    theme_bw() +
    scale_fill_manual(values=cbPalette) +
    scale_colour_manual(labels = c("m1-s1", "m2-s2"), values=cbPalette) +
    labs(x="time[s]", y="relative phase[rad]", color = "units") +
    facet_wrap(condition ~dyad, ncol = 3, scales = "fixed")
  
  chapTappers2_AllRawdata_U_d2 <- DTapping_RelativePhase %>% 
    filter(condition == "U", dyad %in% all_dyads2) %>% ggplot() +
    geom_point(aes(x=time,
                   y=do_rotate(relphase), #make it circular between -pi and +pi
                   color = factor(names_indicator)),shape=1,size=1) +
    ylim(-pi,+pi) + 
    theme_bw() +
    scale_fill_manual(values=cbPalette) +
    scale_colour_manual(labels = c("m1-s1", "m2-s2"), values=cbPalette) +
    labs(x="time[s]", y="relative phase[rad]", color = "units") +
    facet_wrap(condition ~dyad, ncol = 3, scales = "fixed")
  
  chapTappers2_AllRawdata_C_d1 <- DTapping_RelativePhase %>% 
    filter(condition == "C", dyad %in% all_dyads1) %>% ggplot() +
    geom_point(aes(x=time,
                   y=do_rotate(relphase), #make it circular between -pi and +pi
                   color = factor(names_indicator)),shape=1,size=1) +
    ylim(-pi,+pi) + 
    theme_bw() +
    scale_fill_manual(values=cbPalette) +
    scale_colour_manual(labels = c("m1-s1", "m2-s2"), values=cbPalette) +
    labs(x="time[s]", y="relative phase[rad]", color = "units") +
    facet_wrap(condition ~dyad, ncol = 3, scales = "fixed")
  
  chapTappers2_AllRawdata_C_d2 <- DTapping_RelativePhase %>% 
    filter(condition == "C", dyad %in% all_dyads2) %>% ggplot() +
    geom_point(aes(x=time,
                   y=do_rotate(relphase), #make it circular between -pi and +pi
                   color = factor(names_indicator)),shape=1,size=1) +
    ylim(-pi,+pi) + 
    theme_bw() +
    scale_fill_manual(values=cbPalette) +
    scale_colour_manual(labels = c("m1-s1", "m2-s2"), values=cbPalette) +
    labs(x="time[s]", y="relative phase[rad]", color = "units") +
    facet_wrap(condition ~dyad, ncol = 3, scales = "fixed")
  
  
# Second figure showing for each dyad all cycles, squeezed to one cycle
# Here we use time0c which means that each cycle starts again at 0
  chapTappers2_AllRawdata0c_U_d1 <- DTapping_RelativePhase %>% 
    filter(condition == "U", dyad %in% all_dyads1) %>%
    ggplot() +
    geom_point(aes(x=time0c,
          y=do_rotate(relphase), #make it circular between -pi and +pi
          color = factor(names_indicator)),shape=1,size=1) +
    ylim(-pi,+pi) + 
    theme_bw() +
    scale_fill_manual(values=cbPalette) +
    scale_colour_manual(labels = c("m1-s1", "m2-s2"), values=cbPalette) +
    labs(x="time[s]", y="relative phase[rad]", color = "units") +
    facet_wrap(condition ~dyad, ncol = 3)
    
    
  chapTappers2_AllRawdata0c_U_d2 <- DTapping_RelativePhase %>% 
    filter(condition == "U", dyad %in% all_dyads2) %>%
    ggplot() +
    geom_point(aes(x=time0c,
                   y=do_rotate(relphase), #make it circular between -pi and +pi
                   color = factor(names_indicator)),shape=1,size=1) +
    ylim(-pi,+pi) + 
    theme_bw() +
    scale_fill_manual(values=cbPalette) +
    scale_colour_manual(labels = c("m1-s1", "m2-s2"), values=cbPalette) +
    labs(x="time[s]", y="relative phase[rad]", color = "units") +
    facet_wrap(condition ~dyad, ncol = 3)
  
  chapTappers2_AllRawdata0c_C_d1 <- DTapping_RelativePhase %>% 
    filter(condition == "C", dyad %in% all_dyads1) %>%
    ggplot() +
    geom_point(aes(x=time0c,
                   y=do_rotate(relphase), #make it circular between -pi and +pi
                   color = factor(names_indicator)),shape=1,size=1) +
    ylim(-pi,+pi) + 
    theme_bw() +
    scale_fill_manual(values=cbPalette) +
    scale_colour_manual(labels = c("m1-s1", "m2-s2"), values=cbPalette) +
    labs(x="time[s]", y="relative phase[rad]", color = "units") +
    facet_wrap(condition ~dyad, ncol = 3)
  
  chapTappers2_AllRawdata0c_C_d2 <- DTapping_RelativePhase %>% 
    filter(condition == "C", dyad %in% all_dyads2) %>%
    ggplot() +
    geom_point(aes(x=time0c,
                   y=do_rotate(relphase), #make it circular between -pi and +pi
                   color = factor(names_indicator)),shape=1,size=1) +
    ylim(-pi,+pi) + 
    theme_bw() +
    scale_fill_manual(values=cbPalette) +
    scale_colour_manual(labels = c("m1-s1", "m2-s2"), values=cbPalette) +
    labs(x="time[s]", y="relative phase[rad]", color = "units") +
    facet_wrap(condition ~dyad, ncol = 3)
  
  make_fig("Figures/chapTappers2_AllRawdata_U_d1", chapTappers2_AllRawdata_U_d1)
  make_fig("Figures/chapTappers2_AllRawdata_U_d2", chapTappers2_AllRawdata_U_d2)
  make_fig("Figures/chapTappers2_AllRawdata_C_d1", chapTappers2_AllRawdata_C_d1)
  make_fig("Figures/chapTappers2_AllRawdata_C_d2", chapTappers2_AllRawdata_C_d2)
  
  make_fig("Figures/chapTappers2_AllRawdata0c_U_d1", chapTappers2_AllRawdata0c_U_d1)
  make_fig("Figures/chapTappers2_AllRawdata0c_U_d2", chapTappers2_AllRawdata0c_U_d2)
  make_fig("Figures/chapTappers2_AllRawdata0c_C_d1", chapTappers2_AllRawdata0c_C_d1)
  make_fig("Figures/chapTappers2_AllRawdata0c_C_d2", chapTappers2_AllRawdata0c_C_d2)
  
  
}
run = FALSE
# end of raw data plotting
# #########################################################################################################









