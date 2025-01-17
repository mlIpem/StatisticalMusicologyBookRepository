# chapDancer_05_ModelPlotting
cbPalette <- c("#E69F00", "#56B4E9", "#009E73", "#999999", "#F0E442", "#0072B2", "#D55E00", "#CC79A7", "#509999","#A072B2","#FF8442","#F02242")
#  scale_fill_manual(values=cbPalette) +
#  scale_colour_manual(values=cbPalette) +


#########################

model1 <- readRDS("Fitted/chapDancer_Model1.rds")
model3d <- readRDS(file = "Fitted/chapDancer_Model3d.rds")

#########################
# Plotting of model 1 based on conditional_effects

run = FALSE
# run = TRUE
if(run){
  # plot pp_check
  chapDancer_model1_pp_check <- pp_check(model1) +  theme_bw() +
    scale_fill_manual(values=cbPalette) +
    scale_colour_manual(values=cbPalette)
  make_fig("Figures/chapDancer_model1_pp_check",chapDancer_model1_pp_check)
  
  
  # Plotting without circular statistics
  sel_LL_data_1_2 <- LL_data_1_2 %>% dplyr::select(Phase1,Fragment,Heel, Participant)  
  pred  <- epred_draws(model1, newdata = sel_LL_data_1_2, 
                       re_formula = NA, ndraws = 500, prob = .95) %>% ungroup() %>%
    dplyr::select(-Participant) %>% group_by(Fragment,Heel) %>% median_qi()

  chapDancer_model1_Fragments <- ggplot() + 
    geom_point(data = sel_LL_data_1_2, aes(x = Fragment, y = Phase1, color = Heel), position =
                 position_jitterdodge(jitter.width = .2, jitter.height = 0, dodge.width = .4), 
               shape=0, size = .5, , alpha = .3) + 
    geom_pointinterval(data = pred, 
                    aes(x = Fragment, y = .epred, ymin = .epred.lower, ymax = .epred.upper, color = Heel),
                    position = position_dodge(width = .4),
                    size=3) +
    geom_errorbar(
      data = pred, aes(x= Fragment, y=.epred, ymin=.epred.lower, ymax = .epred.upper, color = Heel), 
      position = position_dodge(width = .4),
      width = .7) +
    theme_bw() +
    scale_fill_manual(values=cbPalette) +
    scale_colour_manual(values=cbPalette) +
    labs(x="Fragments",y="relative phase") 
  make_fig("Figures/chapDancer_model1_Fragments2",chapDancer_model1_Fragments)
  
  # ##############
  # Plotting with circular statistics, using the R-package "circular" to obtain the correct means
  pred <-
    epred_draws(model1, newdata = LL_data_1_2, re_formula = NA, ndraws = 500, prob = .95) %>% ungroup() %>%
    dplyr::select(Heel, Fragment, .epred) 

  predsum <- pred  %>% group_by(Fragment, Heel) %>% 
    summarize(mean_angle = mean.circular(.epred),
              sd_angle = sd.circular(.epred), 
              len = n()) %>% ungroup()
  pred <- predsum %>% group_by(Fragment, Heel) %>% 
    mutate(ci.lower = mean_angle - 1.96 * sd_angle ,
           ci.upper = mean_angle + 1.96 * sd_angle )
 # now plotting
  chapDancer_model1_Fragments <- ggplot() + 
    geom_point(data = LL_data_1_2, aes(x = Fragment, y = Phase1, color = Heel), position =
                 position_jitterdodge(jitter.width = .2, jitter.height = 0, dodge.width = .4), 
               shape=0, size = .5, , alpha = .3) + 
 
  geom_point(
    data = pred, aes(x = Fragment, y = mean_angle, color = Heel),
    position = position_dodge(width = .4),
    size=3) +
  geom_errorbar(
    data = pred, aes(x= Fragment, y=mean_angle, ymin=ci.lower, ymax = ci.upper, color = Heel), 
    position = position_dodge(width = .4),
    width = .7) +
  theme_bw() +
  scale_fill_manual(values=cbPalette) +
  scale_colour_manual(values=cbPalette) +
  labs(x="Fragments",y="relative phase") 
  
  make_fig("Figures/chapDancer_model1_Fragments",chapDancer_model1_Fragments)
  
  ################
  ################
  # 1. preditions with NA
  pred <-
    epred_draws(model1, newdata = LL_data_1_2, re_formula = NA, ndraws = 500, prob = .95) %>% ungroup() %>%
    dplyr::select(Heel, Fragment, Participant, .epred)
  
  predsum <- pred  %>% group_by(Fragment, Heel, Participant) %>% 
    summarize(mean_angle = mean.circular(.epred),
              sd_angle = sd.circular(.epred), 
              len = n()) %>% ungroup()
  
  pred <- predsum %>% group_by(Fragment, Heel, Participant) %>% 
    mutate(ci.lower = mean_angle - 1.96 * sd_angle ,
           ci.upper = mean_angle + 1.96 * sd_angle )
  
  # 2. predictions with NULL
  # 
  new <- c("I.F1" = "Fragment = I.F1","F1" = "Fragment = F1", "F2" = "Fragment = F2")
  new_labeller <- function(variable,value){ return(new[value]) }
  
  pred_NULL <-
    epred_draws(model1, newdata = LL_data_1_2, re_formula = NULL, ndraws = 500, prob = .95) %>% ungroup() %>%
    dplyr::select(Heel, Fragment, Participant, .epred)
  
  predsum_NULL <- pred_NULL  %>% group_by(Fragment, Heel, Participant) %>% 
    summarize(mean_angle = mean.circular(.epred),
              sd_angle = sd.circular(.epred), 
              len = n()) %>% ungroup()
  
  pred_NULL <- predsum_NULL %>% group_by(Fragment, Heel, Participant) %>% 
    mutate(ci.lower = mean_angle - 1.96 * sd_angle ,
           ci.upper = mean_angle + 1.96 * sd_angle )
  
  # now plotting
  chapDancer_model1_FragmentsParticipants <- ggplot() + 
    geom_point(data = LL_data_1_2, aes(x = Participant, y = Phase1, color = Heel), position =
                 position_jitterdodge(jitter.width = .2, jitter.height = 0, dodge.width = .4), 
               shape=0, size = .5, alpha = .5) + 
    geom_point(
      data = pred, aes(x = Participant, y = mean_angle, color = Heel),
      position = position_dodge(width = .4),
      size=3) +
    geom_errorbar(
      data = pred, aes(x= Participant, y=mean_angle, ymin=ci.lower, ymax = ci.upper, color = Heel), 
      position = position_dodge(width = .4),
      width = .7) +
    geom_errorbar(
      data = pred_NULL, aes(x= Participant, y=mean_angle, ymin=ci.lower, ymax = ci.upper, color = Heel), 
      position = position_dodge(width = .4),
      width = 1.4) +
    theme_bw() +
    scale_fill_manual(values=cbPalette) +
    scale_colour_manual(values=cbPalette) +
    labs(x="participants",y="relative phase") + 
    facet_grid(~ Fragment,labeller = new_labeller )
  
  make_fig("Figures/chapDancer_model1_FragmentsParticipants_new",chapDancer_model1_FragmentsParticipants)
  
  #########################
  # Plotting of model 3 based on conditional_effects
  # consider including group-level variance of trial in estimation of the mean
  # view per participant
  
  condi = make_conditions(model3d, vars = c("Fragment"))
  C_HDR_mode <- plot(conditional_effects(model3d, 
                                         conditions = condi, 
                                         effects = "time0:Heel", 
                                         re_formula = NA, 
                                         prob = .95),
                     #points = TRUE,  
                     #point_args = list(width = .1, size = .05), 
                     #errorbar_args =  list(width = 1) 
                     plot = FALSE)
  p3 <- ggplot(data = C_HDR_mode$`time0:Heel`$data) +
    geom_lineribbon(aes(x=time0,y= estimate__, ymin = lower__, ymax = upper__,  color = Heel)) +
    geom_point(data = LL_data_1_2, aes(x=time0,y= Phase1, color = Heel), size = .5, alpha = .5) +
    labs(x="time (in seconds)", y="relative phase") +
    theme_minimal() +
    guides(color = guide_legend(override.aes = list(alpha = 1, size = 4), title = "Heel")) + 
    scale_color_brewer(palette = "Dark2") +
    scale_fill_brewer(palette = "Dark2") +
    facet_wrap(~Fragment)
  
  chapDancer_model3_FragmentsParticipants <- p3
  

  make_fig("Figures/chapDancer_model3_FragmentsParticipants",chapDancer_model3_FragmentsParticipants)
  
  
  
  # 1. preditions with NA
  # 
  new <- c("I.F1" = "Fragment = I.F1","F1" = "Fragment = F1", "F2" = "Fragment = F2")
  new_labeller <- function(variable,value){ return(new[value]) }
  
  pred <-
    epred_draws(model3d, newdata = LL_data_3, re_formula = NA, ndraws = 500, prob = .95) %>% ungroup() %>%
    dplyr::select(Heel, Fragment, time0, .epred) 
  
  predsum <- pred  %>% group_by(Fragment, Heel,  time0) %>% 
    summarize(mean_angle = mean.circular(.epred),
              sd_angle = sd.circular(.epred), 
              len = n()) %>% ungroup()
  
  pred <- predsum %>% group_by(Fragment, Heel, time0) %>% 
    mutate(ci.lower = mean_angle - 1.96 * sd_angle ,
           ci.upper = mean_angle + 1.96 * sd_angle )
  
  # now plotting
  chapDancer_model3_FragmentsParticipants <- ggplot() + 
    geom_point(data = LL_data_3, aes(x = time0, y = Phase1, color = Heel), position =
                 position_jitterdodge(jitter.width = .2, jitter.height = 0, dodge.width = .4), 
               shape=0, size = .5, alpha = .5) + 
    # geom_point(
    #   data = pred, aes(x = time0, y = mean_angle, color = Heel),
    #   position = position_dodge(width = .4),
    #   size=3) +
#    geom_errorbar(
 #     data = pred, aes(x= time0, y=mean_angle, ymin=ci.lower, ymax = ci.upper, color = Heel), 
  #    position = position_dodge(width = .4),
   #   width = .7) +
    geom_lineribbon(data = pred, aes(x=time0,y= mean_angle, ymin = ci.lower, ymax = ci.upper,  color = Heel), alpha = 0.8) +
    theme_bw() +
    scale_fill_manual(values=cbPalette) +
    scale_colour_manual(values=cbPalette) +
    labs(x="time (in seconds)",y="relative phase") + 
    facet_grid(~ Fragment,labeller = new_labeller )
  
  make_fig("Figures/chapDancer_model3_FragmentsParticipants",chapDancer_model3_FragmentsParticipants)
  
  
}
run = FALSE
