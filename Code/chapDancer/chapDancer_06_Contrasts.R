# Contrasts
# The calculation of the contrasts, and the plotting are integrated and therefore presented together in this script
# 

cbPalette <- c("#999999", "#999999",  "#56B4D0", "#009E73", "#F0E442", "#0072B2","#E69F00", "#D55E00", "#CC79A7", "#509999","#A072B2","#FF8442","#F02242")
#cbPalette <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
#  scale_fill_manual(values=cbPalette) +
#  scale_colour_manual(values=cbPalette) +

## I. Contrasts of entire fragments
run = FALSE
# run = TRUE
if(run){
  # Model 1: Calculation of contrasts via posterior predictive distributions
  ################################################
  
  fit <- model1
  s <- summary(fit)
  
  ###################
  ## define function do_contrast
  ###################
  
  do_contrast_factors <- function(taak){
   # taak = 1
     Plots <- list()
    count = 0
    Reg_labels <- data.frame()
    Reg_numbers <- data.frame()
    
    
    if(taak == 1){
      Test = "Model1: Contrast I.F1 versus F1, HeelsDown"
      newdata_Fragment = c("I.F1","F1")
      newdata_Heel = c("HeelsDown", "HeelsDown")
      newdata_Participant = c("P4","P2","P3")
    }
    if(taak == 2){
      Test = "Model1: Contrast F1 versus F2, HeelsDown"
      newdata_Fragment = c("F1","F2")
      newdata_Heel = c("HeelsDown", "HeelsDown")
      newdata_Participant = c("P4","P2","P3")
    }
    if(taak == 3){
      Test = "Model1: Contrast I.F1 versus F2, HeelsDown"
      newdata_Fragment = c("I.F1","F2")
      newdata_Heel = c("HeelsDown", "HeelsDown")
      newdata_Participant = c("P4","P2","P3")
    }
    if(0){
      if(taak == 4){
        Test = "Model1: Contrast of P2+P3+P4 in I.F1 versus F1, for HeelsUp"
        newdata_Fragment = c("I.F1","F1")
        newdata_Heel = c("HeelsUp", "HeelsUp")
        newdata_Participant = c("P1","P2","P3")
      }
      if(taak == 5){
        Test = "Model1: Contrast of P2+P3+P4 in F1 versus F2, for HeelsUp"
        newdata_Fragment = c("F1","F2")
        newdata_Heel = c("HeelsUp", "HeelsUp")
        newdata_Participant = c("P1","P2","P3")
      }
      if(taak == 6){
        Test = "Model1: Contrast of P2+P3+P4 in I.F1 versus F2, for HeelsUp"
        newdata_Fragment = c("I.F1","F2")
        newdata_Heel = c("HeelsUp", "HeelsUp")
        newdata_Participant = c("P1","P2","P3")
      }
    }
    
    
    # 1. Posterior draws
    print("--> 1. Posterior draws")
    newdata1 <- LL_data_1_2 %>%  filter(Fragment == newdata_Fragment[1], 
                                        Heel == newdata_Heel[1], 
                                        Participant %in% newdata_Participant) %>% 
      droplevels()
    newdata2 <- LL_data_1_2 %>%  filter(Fragment == newdata_Fragment[2], 
                                        Heel == newdata_Heel[2], 
                                        Participant %in% newdata_Participant) %>% 
      droplevels()
    newdata <- rbind(newdata1,newdata2)
    
    p1 <- ggplot(newdata)+
      geom_point(aes(x=Fragment,y=Phase1, color = Fragment), 
                 position = position_jitter(), shape=1,size=2, alpha = .7) +
      theme_minimal_hgrid() +
      theme(legend.position = "none") +
      ggtitle(paste("data")) + ylab("relative phase") + xlab("time [sec]") +
        scale_fill_manual(values=cbPalette) +
        scale_colour_manual(values=cbPalette) +
    theme(plot.title = element_text(hjust = 0.5)) 
    
    
    # pp_seg1 <- posterior_predict(fit, newdata = newdata1, re_formula = NULL) #NULL = group level effects included
    # pp_seg2 <- posterior_predict(fit, newdata = newdata2, re_formula = NULL)
    
    pp_seg1 <- posterior_epred(fit, newdata = newdata1, re_formula = NA) #NULL = group level effects included
    pp_seg2 <- posterior_epred(fit, newdata = newdata2, re_formula = NA)
    
    diff_seg12 <- pp_seg1 - pp_seg2 %>% data.frame()
    
    post_diff_seg12 <- diff_seg12 %>% 
      mutate(Phase_difference = rowMeans(across(starts_with("X")) ),
             Phase_difference_mean = mean(Phase_difference)) %>%
      dplyr::select(Phase_difference,Phase_difference_mean)
    
    post_seg1 <- data.frame(pp_seg1) %>% 
      mutate(Phase = rowMeans(across(starts_with("X")) ),
             Phase_mean = mean(Phase)) %>%
      dplyr::select(Phase,Phase_mean)
    post_seg2 <- data.frame(pp_seg2) %>% 
      mutate(Phase = rowMeans(across(starts_with("X")) ),
             Phase_mean = mean(Phase)) %>%
      dplyr::select(Phase,Phase_mean)
    
    post_diff_seg12_prob <- post_diff_seg12 %>% 
      mutate(Counter = ifelse(Phase_difference > 0, 1, 0))
    Prob_of_Direction <- mean(post_diff_seg12_prob$Counter) * 100
    Prob_of_Direction2 <- 100 - Prob_of_Direction
    
    v12 <- post_diff_seg12$Phase_difference_mean[1]
    v1 <- post_seg1$Phase_mean[1]
    v2 <- post_seg2$Phase_mean[1]
    
    #2. Prepare plots
    print("--> 2. Prepare plots")
    
    p12 <- ggplot(data = post_diff_seg12, aes(x = Phase_difference)) +
      geom_histogram(fill="lightblue", binwidth = 0.02) + 
      scale_x_continuous(breaks = round(as.numeric(mode_hdi(post_diff_seg12$Phase_difference, .width = .95)[1, 1:3]),2),
                         labels = as.character(round(mode_hdi(post_diff_seg12$Phase_difference, .width = .95)[1, 1:3],2)) 
      ) +
      scale_y_continuous(NULL, breaks = NULL) +
      geom_vline(xintercept = v12, col = "#D55E00") +
      #    geom_vline(xintercept = 0, col = "red") +
      xlab("relative phase difference") +
      theme_minimal_hgrid() +
      ggtitle(paste("seg1 - seg2") ) +
      theme(plot.title = element_text(hjust = 0.5)) +
      theme(panel.border = element_rect(color = "gray",
                                        fill = NA,
                                        size = 2))
    
    #  theme(axis.text.x = element_text(angle = 35, vjust = -0.2, hjust=.5))
    
    
    p2 <- ggplot(data = post_seg1, aes(x = Phase)) +
      geom_histogram(fill="lightblue") + 
      scale_x_continuous(breaks = round(as.numeric(mode_hdi(post_seg1$Phase, .width = .95)[1, 1:3]),2),
                         labels = as.character(round(mode_hdi(post_seg1$Phase, .width = .95)[1, 1:3],2)) 
      ) +
      scale_y_continuous(NULL, breaks = NULL) +
      geom_vline(xintercept = v1, col = "#D55E00") +
      xlab("relative phase") +
      theme_minimal_hgrid() +
      ggtitle(paste("seg1")) +
      theme(plot.title = element_text(hjust = 0.5)) 
    
    p3 <- ggplot(data = post_seg2, aes(x = Phase)) +
      geom_histogram(fill="lightblue") + 
      scale_x_continuous(breaks = round(as.numeric(mode_hdi(post_seg2$Phase, .width = .95)[1, 1:3]),2),
                         labels = as.character(round(mode_hdi(post_seg2$Phase, .width = .95)[1, 1:3],2)) 
      ) +
      scale_y_continuous(NULL, breaks = NULL) +
      geom_vline(xintercept = v2, col = "#D55E00") +
      xlab("relative phase") +
      theme_minimal_hgrid() +
      ggtitle(paste("seg2")) +
      theme(plot.title = element_text(hjust = 0.5))
    
    
    plots <- (p2 + p3)  / (p1 + p12) +   plot_annotation(
      title = paste("posterior predictive distributions\npd>0 =", Prob_of_Direction, "%"),
      subtitle = paste(Test),
      theme = theme(plot.title = element_text(size = 14))   )
    
    
    
    # 3. calculate estimates and upper lower in loop timeregion
    print("--> 3. Calculate estimates")
    posseg1 <- round(as.numeric(mode_hdi(post_seg1$Phase, .width = .95)[1, 1:3]),2)
    posseg2 <- round(as.numeric(mode_hdi(post_seg2$Phase, .width = .95)[1, 1:3]),2)
    posseg12 <- round(as.numeric(mode_hdi(post_diff_seg12$Phase_difference, .width = .95)[1, 1:3]),2)
    
    regions_labels <- data.frame(taak, 
                                 toString(newdata_Fragment),
                                 toString(newdata_Heel),
                                 toString(newdata_Participant)) 
    
    
    regions_numbers  <- c(as.numeric(posseg1),
                          as.numeric(posseg2),
                          as.numeric(posseg12),
                          as.numeric(Prob_of_Direction),
                          as.numeric(Prob_of_Direction2)) %>% t() %>%data.frame()
    
    colnames(regions_labels) <- c("test","fragment","heel","participant")
    #colnames(regions_numbers) <- c("est1-low1-upp1","est2-low2-upp2","estdiff-lowdiff-uppdiff","pd>0","pd<0")
    colnames(regions_numbers) <- c("est1","low1", "upp1","est2","low2", "upp2","estdiff","lowdiff", "uppdiff","pd>0","pd<0")
    return(list(regions_labels,regions_numbers,plots))
    
  }
  
  
  Regions_labels <- data.frame()
  Regions_numbers <- data.frame()
  Plots <- list()
  count = 0
  for (taak in c(1,2,3)){
    count = count + 1
    print("-------------------------------------------")
    print(paste("Taak",taak))
    print("-------------------------------------------")
    Res <-  do_contrast_factors(taak)
    Regions_labels <- rbind(Regions_labels,Res[[1]])
    Regions_numbers <- rbind(Regions_numbers,Res[[2]])
    Plots[[count]] <- Res[[3]]
  }
  
  saveRDS(file = "Data/chapDancer_model1_Regions_labels.rds", Regions_labels)
  saveRDS(file = "Data/chapDancer_model1_Regions_numbers.rds", Regions_numbers)
  
  make_fig("Figures/chapDancer_ContrastFragments_test_1",Plots[[1]])
  make_fig("Figures/chapDancer_ContrastFragments_test_2",Plots[[2]])
  make_fig("Figures/chapDancer_ContrastFragments_test_3",Plots[[3]])
  
}
run = FALSE


## II. Contrasts of phrases selected in fragments
run = FALSE
# run = TRUE
if(run){
  cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7", "#509999","#A072B2","#FF8442","#F02242")
  #cbPalette <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
  #  scale_fill_manual(values=cbPalette) +
  #  scale_colour_manual(values=cbPalette) +
  
  fit <- model3d
  s <- summary(fit)
  
  ###################
  ## define function do_contrast
  ###################
  
  # Based on the time annotated music score, we consider contrasts between the following segments:
  # 
  # - contrasseg1: seg1 [0, 4.25] <-> seg2 [9, 13.25] = contrast between phrase 1 and its repeat in phrase 3
  # 
  # - contrasseg2: seg1 [4.26, 6] <-> seg2 [13.26, 15] = contrast between phrase 2 and its repeat in phrase 4
  # 
  # - contrast3: seg1 [2.51,4.25] <-> seg2 [4.26,6] = contrast between phrase 1 and phrase 2
  # 
  # - contrast4: seg1 [11.51,13.25] <-> seg2 [13.26,15] = contrast between phrase 3 and phrase 4
  
  
  do_contrast_segments <- function(taak){
    Plots <- list()
    count = 0
    Reg_labels <- data.frame()
    Reg_numbers <- data.frame()
    # 0. Define timeregions
    {
      
      timeregion1 = list(seg1 = c(0,4.25), seg2 = c(9,13.25))
      timeregion2 = list(seg1 = c(4.26,6), seg2 = c(13.26,15))
      timeregion3 = list(seg1 = c(2.51,4.25), seg2 = c(4.26,6))
      timeregion4 = list(seg1 = c(11.51,13.25), seg2 = c(13.26,15))
      
      if(taak == 1){
        Test = taak
        Timeregion <- list(timeregion1,timeregion2,timeregion3,timeregion4)
        Contrast <- list("[0.00, 4.25] [9.00, 13.25]", "[4.26, 6.00] [13.26, 15.00]","[2.51, 4.25] [4.26, 6.00]", "[11.51, 13.25] [13.26, 15]")
        Fragments = list(seg1 = "I.F1", seg2 = "I.F1")
        Heels = list(seg1 = "HeelsDown", seg2 = "HeelsDown")
      }
      if(taak == 2){
        Test = taak
        Timeregion <- list(timeregion1,timeregion2,timeregion3,timeregion4)
        Contrast <- list("[0.00, 4.25] [9.00, 13.25]", "[4.26, 6.00] [13.26, 15.00]","[2.51, 4.25] [4.26, 6.00]", "[11.51, 13.25] [13.26, 15]")
        Fragments = list(seg1 = "F1", seg2 = "F1")
        Heels = list(seg1 = "HeelsDown", seg2 = "HeelsDown")
      }
      if(taak == 3){
        Test = taak
        Contrast <- list("[0.00, 4.25] [9.00, 13.25]", "[2.51, 4.25] [4.26, 6.00]")
        Timeregion <- list(timeregion1,timeregion3)
        Fragments = list(seg1 = "F2", seg2 = "F2")
        Heels = list(seg1 = "HeelsDown", seg2 = "HeelsDown")
      }
      if(0){
        if(taak == 4){
          Test = taak
          timeregion1 = list(seg1 = c(2,13.5), seg2 = c(2,11))
          Fragments = list(seg1 = "F1", seg2 = "F2")
          Heels = list(seg1 = "HeelsDown", seg2 = "HeelsDown")
          Timeregion <- list(timeregion1)
          Contrast <- list("Contrast between-fragments")
        }
        if(taak == 5){
          Test = taak
          Timeregion <- list(timeregion1,timeregion2,timeregion3,timeregion4)
          Contrast <- list("C1 between-repeats 1-3", "C2 between-repeats 2-4","C3 within-section 1", "C4 within-section 2")
          Fragments = list(seg1 = "I.F1", seg2 = "I.F1")
          Heels = list(seg1 = "HeelsUp", seg2 = "HeelsUp")
        }
        if(taak == 6){
          Test = taak
          Timeregion <- list(timeregion1,timeregion2,timeregion3,timeregion4)
          Contrast <- list("C1 between-repeats 1-3", "C2 between-repeats 2-4","C3 within-section 1", "C4 within-section 2")
          Fragments = list(seg1 = "F1", seg2 = "F1")
          Heels = list(seg1 = "HeelsUp", seg2 = "HeelsUp")
        }
        if(taak == 7){
          Test = taak
          Contrast <- list("C1 between-repeats 1-3", "C3 within-section 1")
          Timeregion <- list(timeregion1,timeregion3)
          Fragments = list(seg1 = "F2", seg2 = "F2")
          Heels = list(seg1 = "HeelsUp", seg2 = "HeelsUp")
        }
        if(taak == 8){
          Test = taak
          timeregion1 = list(seg1 = c(2,13.5), seg2 = c(2,11))
          Fragments = list(seg1 = "F1", seg2 = "F2")
          Heels = list(seg1 = "HeelsUp", seg2 = "HeelsUp")
          Timeregion <- list(timeregion1)
          Contrast <- list("Contrast between-fragments")
        }
      }
    }
    
    
    #loop
    for(timeregion in Timeregion){
      
      # timeregion = Timeregion[[1]]
      count = count+1
      # timeregion <- timeregion1
      print(paste("Loop",count))
      print("=========")
      
      print(paste("contrast of segments in seconds: ", toString(timeregion)) ) 
      print(paste("contrast of fragments: ", toString(Fragments)) )
      
      # Waar begint Fragments$seg1
      dummy <- Data %>% dplyr::filter(Fragment == Fragments$seg1) %>% dplyr::select( time0)
      start_seg1 <- min(sort(dummy$time0))
      # Waar begint Fragments$seg2
      dummy <- Data %>% dplyr::filter(Fragment == Fragments$seg2) %>% dplyr::select( time0)
      start_seg2 <- min(sort(dummy$time0))
      
      # 1. selectie newdata = segmenten uit de tijdreeks
      print("--> 1. Prepare newdata")
      
      condi1 <- Data %>% 
        dplyr::filter(Heel == Heels$seg1, Fragment == Fragments$seg1, 
               time0 > timeregion$seg1[1] + start_seg1, 
               time0 < timeregion$seg1[2] + start_seg1) %>%
        mutate(Timeregion = factor("seg1")) %>% data.frame() %>% droplevels()
      condi2 <- Data %>% 
        dplyr::filter(Heel == Heels$seg2, Fragment == Fragments$seg2, 
               time0 > timeregion$seg2[1] + start_seg2, 
               time0 < timeregion$seg2[2] + start_seg2) %>% 
        mutate(Timeregion = factor("seg2")) %>% data.frame() %>% droplevels()
      
      
      # opgelet: hier gebruiken we time0 in data_grid, om de range correct te krijgen !!!!
      newdata1 <- condi1 %>% data_grid(Timeregion, Trialf, Participant, Fragment, Heel, time0 = seq_range(time0, 25))#,.model = fit)
      newdata2 <- condi2 %>% data_grid(Timeregion, Trialf, Participant, Fragment, Heel, time0 = seq_range(time0, 25))#,.model = fit)
      newdata <- rbind(newdata1,newdata2)
      
      # 2. get predictions for the Timeregions for plotting
      print("--> 2. Time segments for plotting")
      
      #preds <- conditional_effects(fit, effects = "time0", re_formula = NA) %>% .[[1]] %>% as.data.frame
      # pred_plot <- newdata %>% 
      #   add_epred_draws(fit, re_formula = NULL, ndraws=20) %>% data.frame() %>% droplevels() %>% 
      #   group_by(Timeregion) #only for drawing
      # 
      # pred_sum <- newdata %>% add_epred_draws(fit, re_formula = NA) %>% 
      #   droplevels() %>% 
      #   group_by(Timeregion)  %>% summarize(mean_hdci(.epred, .width = .95))
      
      dat <- Data %>% filter(Heel == Heels, Fragment == Fragments) %>% droplevels()
      condi <- rbind(condi1,condi2)
      
      # Facet Fragment
      new <- c("seg1" = "seg 1","seg2" = "seg 2")
      new_labeller <- function(variable,value){
        return(new[value])
      }
      # Plot data showing segments
      p1 <- ggplot(dat) +
        geom_point(aes(x = time0, y = Phase1), color = "gray") +
        geom_point(data = condi, aes(x = time0, y = Phase1), color = "red") +
        theme_minimal_hgrid() +
        theme(legend.position = "none") +
        # xlab(paste("seg1=", toString(timeregion$seg1), "sec", "\n", "seg2=", toString(timeregion$seg2),"sec" )) +
        ggtitle("data") + ylab("relative phase") + xlab("time [sec]") +
        theme(plot.title = element_text(hjust = 0.5)) +
        facet_grid(~ Timeregion, labeller = as_labeller(new_labeller ))
      
      
      # 3. Posterior draws
      print("--> 3. Posterior draws and plots")
      
      pp_seg1 <- posterior_epred(fit, newdata = newdata1, re_formula = NA) #NULL = group level effects included
      pp_seg2 <- posterior_epred(fit, newdata = newdata2, re_formula = NA)
      
      diff_seg12 <- pp_seg1 - pp_seg2 %>% data.frame()
      
      post_diff_seg12 <- diff_seg12 %>% 
        mutate(Phase_difference = rowMeans(across(starts_with("X")) ),
               Phase_difference_mean = mean(Phase_difference)) %>%
        dplyr::select(Phase_difference,Phase_difference_mean)
      
      post_seg1 <- data.frame(pp_seg1) %>% 
        mutate(Phase = rowMeans(across(starts_with("X")) ),
               Phase_mean = mean(Phase)) %>%
        dplyr::select(Phase,Phase_mean)
      post_seg2 <- data.frame(pp_seg2) %>% 
        mutate(Phase = rowMeans(across(starts_with("X")) ),
               Phase_mean = mean(Phase)) %>%
        dplyr::select(Phase,Phase_mean)
      
      post_diff_seg12_prob <- post_diff_seg12 %>% 
        mutate(Counter = ifelse(Phase_difference > 0, 1, 0))
      Prob_of_Direction <- mean(post_diff_seg12_prob$Counter) * 100
      Prob_of_Direction2 <- 100 - Prob_of_Direction
      
      v12 <- post_diff_seg12$Phase_difference_mean[1]
      v1 <- post_seg1$Phase_mean[1]
      v2 <- post_seg2$Phase_mean[1]
      
      p12 <- ggplot(data = post_diff_seg12, aes(x = Phase_difference)) +
        geom_histogram(fill='lightblue', binwidth = 0.02) + 
        scale_x_continuous(breaks = round(as.numeric(mode_hdi(post_diff_seg12$Phase_difference, .width = .95)[1, 1:3]),2),
                           labels = as.character(round(mode_hdi(post_diff_seg12$Phase_difference, .width = .95)[1, 1:3],2)) 
        ) +
        scale_y_continuous(NULL, breaks = NULL) +
        geom_vline(xintercept = v12, col = "red") +
        #    geom_vline(xintercept = 0, col = "red") +
        xlab("relative phase difference") +
        theme_minimal_hgrid() +
        ggtitle(paste("seg1 - seg2") ) +
        theme(plot.title = element_text(hjust = 0.5)) +
        theme(panel.border = element_rect(color = "gray",
                                          fill = NA,
                                          size = 2))
      #+
      #  theme(axis.text.x = element_text(angle = 35, vjust = -0.2, hjust=.5))
      
      
      p2 <- ggplot(data = post_seg1, aes(x = Phase)) +
        geom_histogram(fill='lightblue') + 
        scale_x_continuous(breaks = round(as.numeric(mode_hdi(post_seg1$Phase, .width = .95)[1, 1:3]),2),
                           labels = as.character(round(mode_hdi(post_seg1$Phase, .width = .95)[1, 1:3],2)) 
        ) +
        scale_y_continuous(NULL, breaks = NULL) +
        geom_vline(xintercept = v1, col = "red") +
        xlab("relative phase") +
        theme_minimal_hgrid() +
        ggtitle(paste("seg1")) +  
        theme(plot.title = element_text(hjust = 0.5)) 
      
      
      p3 <- ggplot(data = post_seg2, aes(x = Phase)) +
        geom_histogram(fill='lightblue') + 
        scale_x_continuous(breaks = round(as.numeric(mode_hdi(post_seg2$Phase, .width = .95)[1, 1:3]),2),
                           labels = as.character(round(mode_hdi(post_seg2$Phase, .width = .95)[1, 1:3],2)) 
        ) +
        scale_y_continuous(NULL, breaks = NULL) +
        geom_vline(xintercept = v2, col = "red") +
        xlab("relative phase") +
        theme_minimal_hgrid() +
        ggtitle(paste("seg2")) +
        theme(plot.title = element_text(hjust = 0.5)) 
      
      Plots[[count]] <- (p2 + p3)  / (p1 + p12) +   plot_annotation(
        title = paste("posterior predictive distributions of the mean\nseg1,seg2:", 
                      Contrast[[count]], "\npd>0 =", Prob_of_Direction, "%"),
        subtitle = paste("fragment:", toString(Fragments), "Heel:",toString(Heels)),
        theme = theme(plot.title = element_text(size = 16))
      )
      
      # 4. calculate estimates and upper lower in loop timeregion
      print("--> 4. Calculate estimates")
      
      posseg1 <- round(as.numeric(mode_hdi(post_seg1$Phase, .width = .95)[1, 1:3]),2)
      posseg2 <- round(as.numeric(mode_hdi(post_seg2$Phase, .width = .95)[1, 1:3]),2)
      posseg12 <- round(as.numeric(mode_hdi(post_diff_seg12$Phase_difference, .width = .95)[1, 1:3]),2)
      
      regions_labels <- c(Test, toString(Contrast[[count]]), toString(timeregion), toString(Fragments),toString(Heels))
      regions_numbers  <- c(as.numeric(posseg1),as.numeric(posseg2),as.numeric(posseg12),as.numeric(Prob_of_Direction),as.numeric(Prob_of_Direction2))
      
      Reg_labels <- rbind(Reg_labels,regions_labels)
      Reg_numbers <- rbind(Reg_numbers,regions_numbers)
      
    }
    
    fn = paste("Figures/chapDancer_Plots_",Test,sep="")
    make_fig(fn,Plots)
   # fn = paste("Figures/chapDancerPlotsList_",Test,".RData", sep="")
   # save(file = fn,Plots)
    
    
    colnames(Reg_labels) <- c("test", "contrast", "timeregion","fragment","heel")
    colnames(Reg_numbers) <- c("est1","low1", "upp1","est2","low2", "upp2","estdiff","lowdiff", "uppdiff","pd>0","pd<0")
    
    return(list(Reg_labels,Reg_numbers))
    
  }
  
  ###################
  ###################
  
  Regions_labels <- data.frame()
  Regions_numbers <- data.frame()
  for (taak in c(1,2,3)){
    print("-------------------------------------------")
    print(paste("Taak",taak))
    print("-------------------------------------------")
    Res <-  do_contrast_segments(taak)
    Regions_labels <- rbind(Regions_labels,Res[[1]])
    Regions_numbers <- rbind(Regions_numbers,Res[[2]])
  }
  saveRDS(file = "Data/chapDancer_Regions_labels_segments.rds", Regions_labels)
  saveRDS(file = "Data/chapDancer_Regions_numbers_segments.rds", Regions_numbers)
  
}
run = FALSE

