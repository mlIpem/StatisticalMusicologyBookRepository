# chapTappers_07_EvaluationPlotting
# ######################################################
# Here we plot the parameters of the 40 dynamic systems:
# Designed parameters versus estimated parameters
# ######################################################
run = FALSE
# run = TRUE
if(run){
  # this was calculated in chapTappers_06_Evaluation
  PosteriorSummary_States <-  readRDS(file = paste("Data/chapTappers_PosteriorSummary_States.rds",sep=""))
  D <- readRDS(file = "Data/chapTappers_D_5_jun_2024.rds")
  K <- readRDS(file = "Data/chapTappers_K_5_jun_2024.rds")
  
  # The palette with grey:
  cbPalette <- c("#E69F00", "#56B4E9", "#009E73", "#999999","#F0E442", "#0072B2", "#D55E00", "#CC79A7" )

  K_fitted <- PosteriorSummary_States %>% filter(names %in% c("ks1m1","ks2m2"))
  D_fitted <- PosteriorSummary_States %>% filter(names %in% c("ds1m1","ds2m2","ds1s2","ds2s1")) 
  
  # Turn K into K_long and make distinction between no phase delay and with phase delay
  K_Long <- K %>% data.frame() %>% dplyr::select(ks1m1,ks2m2) %>% 
    mutate(sim = row_number(), part = c(rep("No phase delay",20),rep("With phase delay",20))) %>% 
    pivot_longer(cols = starts_with("k"), names_to = "names" ,values_to = "value") %>% mutate(names = factor(names))
  
  # Turn D into D_long and make distinction between no phase delay and with phase delay
  D_Long <- D %>% data.frame() %>% dplyr::select(ds1m1,ds1s2,ds2m2,ds2s1) %>% 
    #D_Long <- D %>% data.frame() %>% dplyr::select(ds1m1,ds2m2) %>% 
    mutate(sim = row_number(), part = c(rep("No phase delay",20),rep("With phase delay",20))) %>% 
    pivot_longer(cols = starts_with("d"), names_to = "names" ,values_to = "value") %>% mutate(names = factor(names))
  
  
  # ############################################
  # Show the coupling strength for A (zero phase delay) and B (phase delay to metronome)
  p0 <- K_fitted %>% ggplot() +
    geom_pointinterval(aes(y = .value, x = factor(sim), ymin = .lower, ymax = .upper, colour = names), position = position_dodge(width = 0), alpha = .6) + 
    geom_point(data = K_Long, aes(y = value, x = factor(sim), colour = names), position = position_dodge(width = 0),shape=1,size=5) +
    theme_bw() +
    labs(y = "coupling strength", x = "dynamic model") +
    theme(axis.text.x = element_text(angle = 45, vjust = 0.5)) +
    scale_fill_manual(values=cbPalette) +
    scale_colour_manual(values=cbPalette) #+
  #facet_grid(~k$part + names)
  #p0
  
  # ############################################
  # Show the phase delay to metronome
  d_Long <- D_Long %>% filter(!(names %in% c("ds1s2","ds2s1")))
  p1 <-  D_fitted  %>% filter(!(names %in% c("ds1s2","ds2s1"))) %>% 
    ggplot() +
    geom_pointinterval(aes(y = .value, x = factor(sim), ymin = .lower, ymax = .upper, colour = names), 
                       position = position_dodge(width = 0.6)) + 
    geom_point(data = d_Long, aes(y = value, x = factor(sim), colour = names), 
               position = position_dodge(width = 0),shape=1,size=5) +   
    theme_bw() +
    labs(y= "phase delay" , x = "dynamic model") +
    theme(axis.text.x = element_text(angle = 45, vjust = 0.5)) +
    #scale_x_continuous("sim", labels = as.character(sim), breaks = sim) +
    scale_fill_manual(values=cbPalette) +
    scale_colour_manual(values=cbPalette) 
  #p1
  
  # ############################################
  # Show the phase delay to partner
  d_Long <- D_Long %>% filter(!(names %in% c("ds1m1","ds2m2")))
  p2 <-  D_fitted  %>% filter(!(names %in% c("ds1m1","ds2m2"))) %>% 
    ggplot() +
    geom_pointinterval(aes(y = .value, x = factor(sim), ymin = .lower, ymax = .upper, colour = names), 
                       position = position_dodge(width = 0.6)) + 
    geom_point(data = d_Long, aes(y = value, x = factor(sim), colour = names), 
               position = position_dodge(width = 0),shape=1,size=5) +   
    theme_bw() +
    labs(y= "phase delay" , x = "dynamic model") +
    theme(axis.text.x = element_text(angle = 45, vjust = 0.5)) +
    #scale_x_continuous("sim", labels = as.character(sim), breaks = sim) +
    scale_fill_manual(values=cbPalette) +
    scale_colour_manual(values=cbPalette) 
  #p2
  
  pp <- p0 / p1 / p2
  fn_KD = paste("Figures/chapTappers_PosteriorSummary_States_fig")
  
  make_fig(fn_KD,pp)
  
}
run = FALSE




# ############################################
# Here we calculate a summary table of the 
# difference between designed and estimated parameters
# ############################################
run = FALSE
# run = TRUE
if(run){

  # rename to get decent tables
  KK_fitted <- K_fitted %>% rename(value_fitted=.value) %>% mutate(sim = factor(sim), names = factor(names)) %>% 
    select(names, value_fitted, sim)
  KK_long <- K_Long %>% rename(value_designed=value) %>% mutate(sim = factor(sim), names = factor(names)) %>% 
    select(value_designed,part)
  KK <- cbind(KK_fitted,KK_long)
  # calculate differences between designed and fitted variables
  KK_NoPhaseDelay <- KK %>% mutate(meandiff12 = value_fitted - value_designed) %>% filter(part == "No phase delay")
  KK_YesPhaseDelay <- KK %>% mutate(meandiff12 = value_fitted - value_designed) %>% filter(part == "With phase delay")
  # take summaries of absolute values of the differences 
  # because we want to know how big the error is, either positive or negative
  Res.K_NoPhaseDelay <- KK_NoPhaseDelay %>% 
    summarize(m_abs = mean(abs(meandiff12)), 
              sd_abs = sd(abs(meandiff12)), 
              Q95 = hdci((meandiff12),.width=.95)) %>% 
    round(2) %>%
    mutate(Q95.l = Q95[1], Q95.u = Q95[2], phasedelay = factor("No")) %>% 
    dplyr::select(-Q95)#,-Q50,-Q10)
  
  Res.K_YesPhaseDelay <- KK_YesPhaseDelay %>% 
    summarize(m_abs = mean(abs(meandiff12)), 
              sd_abs = sd(abs(meandiff12)), 
              Q95 = hdci((meandiff12),.width=.95)) %>% 
    round(2) %>%
    mutate(Q95.l = Q95[1], Q95.u = Q95[2], phasedelay = factor("Yes")) %>% 
    dplyr::select(-Q95)#,-Q50,-Q10)
  
  Res.K <- rbind(Res.K_NoPhaseDelay,Res.K_YesPhaseDelay)
  
  # Res.K %>%
  #   knitr::kable(caption = "Summary of errors K", booktabs=T, label = NA ) %>%
  #   kable_styling("striped", font_size = 11)
  
  
  DD_fitted <- D_fitted %>% rename(value_fitted=.value) %>% 
    mutate(sim = factor(sim), names = factor(names)) %>% 
    select(names, value_fitted, sim)
  DD_long <- D_Long %>% rename(value_designed=value) %>% 
    mutate(sim = factor(sim), names = factor(names)) %>% 
    select(value_designed,part)
  # KK <- cbind(KK_fitted,KK_long[-c(35,36,75,76),])
  DD <- cbind(DD_fitted,DD_long)
  #DD <- DD %>% mutate(meandiff12 = value_fitted - value_designed)
  DD_NoPhaseDelay <- DD %>% mutate(meandiff12 = value_fitted - value_designed) %>% filter(part == "No phase delay")
  DD_WithPhaseDelay <- DD %>% mutate(meandiff12 = value_fitted - value_designed) %>% filter(part == "With phase delay")
  
  Res.D_NoPhaseDelay <- DD_NoPhaseDelay %>% 
    summarize(m_abs = mean(abs(meandiff12)), 
              sd_abs = sd(abs(meandiff12)), 
              Q95 = hdci(meandiff12,.width=.95)) %>% round(2) %>%
    mutate(Q95.l = Q95[1], Q95.u = Q95[2], phasedelay = factor("No")) %>% dplyr::select(-Q95)#,-Q50,-Q10)
  Res.D_WithPhaseDelay <- DD_WithPhaseDelay %>% 
    summarize(m_abs = mean(abs(meandiff12)), 
              sd_abs = sd(abs(meandiff12)), 
              Q95 = hdci(meandiff12,.width=.95)) %>% round(2) %>%
    mutate(Q95.l = Q95[1], Q95.u = Q95[2], phasedelay = factor("Yes")) %>% dplyr::select(-Q95)#,-Q50,-Q10)
  Res.D <- rbind(Res.D_NoPhaseDelay,Res.D_WithPhaseDelay)
  
  # Res.D %>%
  #   knitr::kable(caption = "Summary of errors D", booktabs=T, label = NA ) %>%
  #   kable_styling("striped", font_size = 11)
  
  # bind everything together
  Res.KK <- Res.K %>% mutate(parameter = "K")
  Res.DD <- Res.D %>% mutate(parameter = "D")
  SummaryErrors <- rbind(Res.KK,Res.DD)
  
  saveRDS(file = "Data/chapTappers_PosteriorSummary_Errors.rds",SummaryErrors)
}
run = FALSE

