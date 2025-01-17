# chapViolinist_05_ModelPlotting.R
cbPalette <- c( "#2f70a1", "#ffa600", "#931e18",  "#176528",
               "#A8A8A8", "#BF5209", "#003f5c", "#3C7829", 
               "#325384", "#585853", "#a05195", "#633372")
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", 
               "#F0E442", "#0072B2", "#D55E00", "#CC79A7", 
               "#509999","#A072B2","#FF8442","#F02242")
#  scale_fill_manual(values=cbPalette) +
#  scale_colour_manual(values=cbPalette) +
#  
#  We calculate figures for all models 
#  but we could reduce ourselves to the ones shown in the book
########################################################################################################
#  1.  use NewData = bowing gesture events (Adriaan)
#########################################################
# 1.1. ModelPlotting 2 conditions per piece
#########################################################
run = FALSE
# run = TRUE
if(run){
  # generate plots for the models
  chapViolinist_modelplots_list <- list()
  for (p in c(1:4)){
    # p = 2
    print(paste("------> Processing ", Pieces[[p]]))
    fn = paste("Fitted/chapViolinist_model_", Pieces[[p]],".rds",sep="")
    fit <- readRDS(fn) #load fit
    Dat <- NewData %>% filter(piece == Pieces[[p]], !subject == "P006", trial == "T1") %>% droplevels()
    Dat_D <- data_grid(data = Dat, condition, trial, subject, time = seq(min(time),max(time),by=.5))
    print("# 2. Generate predictions")
    PostPred <- epred_draws(fit, Dat_D, ndraws = 1000, scale = "response" , re_formula = NA) 
    print("# 3. Calculate contrasts")
    PostPred_wide <- PostPred %>% group_by(condition) %>%
      mutate(rn = row_number()) %>%
      pivot_wider(id_cols = c(rn,trial,subject,time), names_from = condition, values_from = .epred) %>%
      rename(D2 = `2D` , D3 = `3D`) %>% arrange(time)
    PostPred_wide <- PostPred_wide %>%  mutate(Diff = D2 - D3 ) 
    print("# 4. Generate predictions")
    
    # load the appropriate audio file for plotting
    if(p==1) {load(file = "./Data/chapViolinist_AudioF1.RData")} 
    if(p==2) {load(file = "./Data/chapViolinist_AudioF2.RData")} 
    if(p==3) { load(file = "./Data/chapViolinist_AudioF3.RData") } 
    if(p==4) {load(file = "./Data/chapViolinist_AudioF4.RData") }
    
    paudio <-  
      ggplot() +
      geom_line(data = Audio_data[-1,], aes(x = time, y = amplitude)) +
      geom_rect(aes(xmin = Band_start[[p]], xmax = Band_end[[p]],
                    ymin = -Inf, ymax = Inf),fill = "white", alpha = 0.6) +
      geom_vline(xintercept = Vline[[p]], color = "#D55E00") +
      scale_x_continuous(n.breaks = 20) +
      scale_fill_manual(values=cbPalette) +
      scale_colour_manual(values=cbPalette) +
      theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +      theme_bw() +
      xlab("time [sec]") + ylab("amplitude") +
      xlim(0,190)
    
    
    pl0 <- ggplot() +
      geom_point(data = Dat, aes(x=time,y=logPD, color = condition), shape = 1, size=1, alpha=.55) +
      stat_lineribbon(data = PostPred_wide, aes(x=time, y = D2, color = condition), alpha = 1, size = 1, .width = .25, color = "#999999")+
      stat_lineribbon(data = PostPred_wide, aes(x=time, y = D3, color = condition), alpha = 1, size = 1, .width = .25 , color = "#E69F00")+
      geom_rect(aes(xmin = Band_start[[p]], xmax = Band_end[[p]],
                    ymin = -3, ymax = 3),fill = "white", alpha = 0.6) +
      geom_vline(xintercept = Vline[[p]], color = "#D55E00") +
      scale_x_continuous(n.breaks = 20) +
      scale_fill_manual(values=cbPalette) +
      scale_colour_manual(values=cbPalette) +
      theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))+      theme_bw() +
      xlab("time [sec]") + ylab("logPD") +
      ylim(-3,3)+
      xlim(0,190)
    
    pl1 <- ggplot() +
      stat_lineribbon(data = PostPred_wide, aes(x=time, y = Diff), alpha = .7, size = 1, .width = .9)+
      geom_point(data = Dat, aes(x=time,y = logPD*0),color = "#D55E00",size = .5) +
      geom_rect(aes(xmin = Band_start[[p]], xmax = Band_end[[p]],
                    ymin = -1.5, ymax = 1.5),fill = "white", alpha = 0.6) +
      geom_vline(xintercept = Vline[[p]], color = "#D55E00") +
      scale_x_continuous(n.breaks = 20) +
      scale_fill_manual(values=cbPalette) +
      scale_colour_manual(values=cbPalette) +
      theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))+
      theme_bw() +
      xlab("time [sec]") + ylab("2D-3D")+
      ylim(-1.5,1.5) +
      xlim(0,190)
    
    pp <- paudio / pl0 / pl1
    
    fnp <- paste("Figures/chapViolinist_Diff_model_",Pieces[[p]],sep="")
    #  print("# 5. Save plot in list")
    #   chapViolinist_modelplots_list[[p]] <- pp
    print("# 6. Save Pdf plot")
    make_fig(fnp,pp)
  }
  # save(file = paste("Figures/chapViolinist_modelplots_list.RData",sep=""),  chapViolinist_modelplots_list)
}
run = FALSE

##########################################################################################
# 1.2. Individual subjects and smooths of individual trials
# models have been fitted in 04
##########################################################################################
run = FALSE
# run = TRUE
if(run){
  for (s in 1:length(Subjects)){
    subject_str = Subjects[[s]]
    for ( c in c(1:2)){
      condition_str = Condition[[c]]
      Dat <- NewData %>% filter(condition == Condition[[c]],subject == Subjects[[s]]) %>% droplevels() %>% 
        mutate(conditiontrial = interaction(condition,trial))
      if(unique(Dat$piece) =="F1") p = 1
      if(unique(Dat$piece) =="F2") p = 2
      if(unique(Dat$piece) =="F3") p = 3
      if(unique(Dat$piece) =="F4") p = 4
      piece_str = Pieces[[p]] 
      print(paste("Processing", piece_str, condition_str, subject_str))
      
      fn= paste("Fitted/chapViolinist_model_subj_",subject_str,"_condi_", condition_str,".rds",sep="")
      fit <- readRDS(file = fn)
      Dat_D <- data_grid(data = Dat, conditiontrial, subject, time = seq(min(time),max(time),by=.5))
      print("# 2. Generate predictions")
      PostPred <- epred_draws(fit, Dat_D, ndraws = 1000, scale = "response" , re_formula = NA) 
      print("# 3. Calculate contrasts")
      #PostPred_wide <- PostPred %>% group_by(conditiontrial) %>% mutate(rn = row_number()) %>%
      #  pivot_wider(id_cols = c(rn,subject,time), names_from = conditiontrial, values_from = .epred) 
      if(p==1) {load(file = "./Data/chapViolinist_AudioF1.RData")} 
      if(p==2) {load(file = "./Data/chapViolinist_AudioF2.RData")} 
      if(p==3) { load(file = "./Data/chapViolinist_AudioF3.RData") } 
      if(p==4) {load(file = "./Data/chapViolinist_AudioF4.RData") }
      
      paudio <-  
        ggplot() +
        geom_line(data = Audio_data[-1,], aes(x = time, y = amplitude)) +
        geom_rect(aes(xmin = Band_start[[p]], xmax = Band_end[[p]],
                      ymin = -Inf, ymax = Inf),fill = "white", alpha = 0.6) +
        geom_vline(xintercept = Vline[[p]], color = "#D55E00") +
        scale_x_continuous(n.breaks = 20) +
        scale_fill_manual(values=cbPalette) +
        scale_colour_manual(values=cbPalette) +
        theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +      theme_bw() +
        xlab("time [sec]") + ylab("amplitude") +
        labs(title = paste("piece",Pieces[[p]],"subject",Subjects[[s]], "condition",Condition[[c]])) +
        xlim(0,190)
      
      pl0 <- ggplot() +
        #  stat_lineribbon(data = PostPred_wide, aes(x=time, y = D2, color = condition), alpha = 1, size = 1, .width = .25, color = "#999999")+
        #  stat_lineribbon(data = PostPred_wide, aes(x=time, y = D3, color = condition), alpha = 1, size = 1, .width = .25 , color = "#E69F00")+
        stat_lineribbon(data = PostPred, aes(x=time, y = .epred, color = conditiontrial), alpha = .85, fill = "lightgrey", size = 1, .width = .9) +
        geom_point(data = Dat, aes(x=time,y=logPD, color = conditiontrial), shape = 1, size=1, alpha=.55) +
        geom_rect(aes(xmin = Band_start[[p]], xmax = Band_end[[p]], ymin = -3, ymax = 3),fill = "white", alpha = 0.5) +
        geom_vline(xintercept = Vline[[p]], color = "#D55E00") +
        scale_x_continuous(n.breaks = 20) +
        scale_fill_manual(values=cbPalette) +
        scale_colour_manual(values=cbPalette) +
        theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))+      theme_bw() +
        xlab("time [sec]") + ylab("logPD") +
        ylim(-3,3) +
        xlim(0,190)
      
      
      pp <- paudio / pl0
      pp
      
      fnp <- paste("Figures/chapViolinist_smooths_piece_",Pieces[[p]],"_condition_",Condition[[c]],"_subject_",Subjects[[s]],sep="")
      print(paste("----> saving figure",fnp))
      make_pdf_figure(fnp,pp)
      make_png_figure(fnp,pp)
      
    }
  }
}
run = FALSE


#########################################################
# 2. ModelPlotting Res
#########################################################
# 2.1. Two conditions per piece
#    use Res (relative phase)
#########################################################
run = FALSE
# run = TRUE
 if(run){
for(p in c(1:4)){
  # p=1 ;  p=2 ; p=3 ; p=4
  print(paste("------> Processing ", Pieces[[p]]))
  fn = paste("Fitted/chapViolinist_GUSO2_Gaussian_model_2conditions_piece_", Pieces[[p]],".rds",sep="")
  fit <- readRDS(fn) #load fit
  Dat <- Res %>% filter(Piece == Pieces[[p]]) %>% droplevels() %>% drop_na()
  Dat_D <- data_grid(data = Dat, Condition, Trial, Participant, Time = seq(min(Time),max(Time),by=1))
  print("# 2. Generate predictions")
  PostPred <- epred_draws(fit, Dat_D, ndraws = 5000, scale = "response" , re_formula = NA) 
  print("# 3. Calculate contrasts")
  PostPred_wide <- PostPred %>% group_by(Condition) %>%
    mutate(rn = row_number()) %>%
    pivot_wider(id_cols = c(rn,Trial,Participant,Time), names_from = Condition, values_from = .epred) %>%
    rename(D2 = `2D` , D3 = `3D`) %>% arrange(Time)
  PostPred_wide <- PostPred_wide %>%  mutate(Diff = D2 - D3 ) 
  print("# 4. Generate predictions")
  
  
  
  # load the appropriate audio file for plotting
  if(p==1) {load(file = "./Data/chapViolinist_AudioF1.RData")} 
  if(p==2) {load(file = "./Data/chapViolinist_AudioF2.RData")} 
  if(p==3) { load(file = "./Data/chapViolinist_AudioF3.RData") } 
  if(p==4) {load(file = "./Data/chapViolinist_AudioF4.RData") }
  
  paudio <-  
    ggplot() +
    geom_line(data = Audio_data[-c(1),], aes(x = time, y = amplitude)) +
    # geom_rect(aes(xmin = Band_start[[p]], xmax = Band_end[[p]], ymin = -Inf, ymax = Inf),fill = "white", alpha = 0.6) +
    # geom_vline(xintercept = Vline[[p]], color = "#D55E00") +
    scale_x_continuous(n.breaks = 20) +
    scale_fill_manual(values=cbPalette) +
    scale_colour_manual(values=cbPalette) +
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +      theme_bw() +
    xlab("time [sec]") + ylab("amplitude") +
    xlim(0,190)
  
  
  
  pl0 <- ggplot() +
    geom_point(data = Dat, aes(x=Time,y=scale(log(1-as.numeric(R))), color = Condition), shape = 1, size=1, alpha=.55) +
    stat_lineribbon(data = PostPred_wide, aes(x=Time, y = D2, color = Condition), alpha = 1, size = 1, .width = .25, color = "#999999")+
    stat_lineribbon(data = PostPred_wide, aes(x=Time, y = D3, color = Condition), alpha = 1, size = 1, .width = .25 , color = "#E69F00")+
    # geom_rect(aes(xmin = Band_start[[p]], xmax = Band_end[[p]],ymin = -3, ymax = 3),fill = "white", alpha = 0.6) +
    #  geom_vline(xintercept = Vline[[p]], color = "#D55E00") +
    scale_x_continuous(n.breaks = 20) +
    scale_fill_manual(values=cbPalette) +
    scale_colour_manual(values=cbPalette) +
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))+      theme_bw() +
    xlab("time [sec]") + ylab("log(1-R)") +
    ylim(-3,3) +
    xlim(0,190)
  
  pl1 <- ggplot() +
    stat_lineribbon(data = PostPred_wide, aes(x=Time, y = Diff), alpha = .7, size = 1, .width = .9)+
    # geom_point(data = Dat, aes(x=Time,y = R*0),color = "#D55E00",size = .5) +
    # geom_rect(aes(xmin = Band_start[[p]], xmax = Band_end[[p]],ymin = -1.5, ymax = 1.5),fill = "white", alpha = 0.6) +
    #  geom_vline(xintercept = Vline[[p]], color = "#D55E00") +
    geom_hline(yintercept = 0, color = "#D55E00") +
    scale_x_continuous(n.breaks = 20) +
    scale_fill_manual(values=cbPalette) +
    scale_colour_manual(values=cbPalette) +
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))+
    theme_bw() +
    xlab("time [sec]") + ylab("2D-3D")+
    ylim(-1.5,1.5)+
    xlim(0,190)
  
  pp <- paudio / pl0 / pl1
  
  fnp = paste("Figures/chapViolinist_GUSO2_A2model_2conditions_piece_", Pieces[[p]],".rds",sep="")
  print(paste("----> saving figure",fnp))
  make_fig(fnp,pp)
  
}
 }
run = FALSE

##########################################################################################
# 2.2. Individual subjects and smooths of individual trials
# models have been fitted in 04
##########################################################################################

run = FALSE
# run = TRUE
if(run){
Pieces <- list("F1","F2","F3","F4")
Subjects <- as.list(as.character(unique(Res$Participant)))
Condition <- list("2D","3D")
if(run){
  for (s in 1:length(unique(Res$Participant))){
    subject_str = Subjects[[s]]
    for ( c in 1:length(Condition)){
      # subject_str = "P009"; c = 1
      condition_str = Condition[[c]]
      Dat <- Res %>% dplyr::filter(Condition == condition_str, Participant == subject_str) %>% 
        mutate(ConditionTrial = interaction(Condition,Trial),
               Time = as.numeric(Cut)) %>% droplevels() %>% drop_na()
      if(unique(Dat$Piece) =="F1") p = 1
      if(unique(Dat$Piece) =="F2") p = 2
      if(unique(Dat$Piece) =="F3") p = 3
      if(unique(Dat$Piece) =="F4") p = 4
      piece_str = Pieces[[p]] 
      print(paste("Processing", piece_str, condition_str, subject_str))
      
      fn= paste("Fitted/chapViolinist_GUSO2_Gaussian_model_subj_",subject_str,"_condi_", condition_str,".RData",sep="")
      load(file = fn)
      Dat_D <- data_grid(data = Dat, ConditionTrial, Participant, Time = seq(min(Time),max(Time),by=1))
      print("# 2. Generate predictions")
      PostPred <- epred_draws(fit, Dat_D, ndraws = 5000, scale = "response" , re_formula = NA) 
      print("# 3. Calculate contrasts")
      #PostPred_wide <- PostPred %>% group_by(conditiontrial) %>% mutate(rn = row_number()) %>%
      #  pivot_wider(id_cols = c(rn,subject,time), names_from = conditiontrial, values_from = .epred) 
      if(p==1) {load(file = "./Data/chapViolinist_AudioF1.RData")} 
      if(p==2) {load(file = "./Data/chapViolinist_AudioF2.RData")} 
      if(p==3) { load(file = "./Data/chapViolinist_AudioF3.RData") } 
      if(p==4) {load(file = "./Data/chapViolinist_AudioF4.RData") }
      
      paudio <-  
        ggplot() +
        geom_line(data = Audio_data[-1,], aes(x = time, y = amplitude)) +
        #   geom_rect(aes(xmin = Band_start[[p]], xmax = Band_end[[p]], ymin = -Inf, ymax = Inf),fill = "white", alpha = 0.6) +
        #    geom_vline(xintercept = Vline[[p]], color = "#D55E00") +
        scale_x_continuous(n.breaks = 20) +
        scale_fill_manual(values=cbPalette) +
        scale_colour_manual(values=cbPalette) +
        theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +      theme_bw() +
        xlab("time [sec]") + ylab("amplitude") +
        labs(title = paste("piece",Pieces[[p]],"subject",Subjects[[s]], "condition",Condition[[c]])) 
      
      pl0 <- ggplot() +
        #  stat_lineribbon(data = PostPred_wide, aes(x=time, y = D2, color = condition), alpha = 1, size = 1, .width = .25, color = "#999999")+
        #  stat_lineribbon(data = PostPred_wide, aes(x=time, y = D3, color = condition), alpha = 1, size = 1, .width = .25 , color = "#E69F00")+
        stat_lineribbon(data = PostPred, aes(x=Time, y = .epred, color = ConditionTrial), alpha = .85, fill = "lightgrey", size = 1, .width = .9) +
        geom_point(data = Dat, aes(x=Time,y=scale(log(1-as.numeric(R))), color = ConditionTrial), shape = 1, size=1, alpha=.55) +
        #    geom_rect(aes(xmin = Band_start[[p]], xmax = Band_end[[p]], ymin = -3, ymax = 3),fill = "white", alpha = 0.5) +
        #    geom_vline(xintercept = Vline[[p]], color = "#D55E00") +
        scale_x_continuous(n.breaks = 20) +
        scale_fill_manual(values=cbPalette) +
        scale_colour_manual(values=cbPalette) +
        theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))+      theme_bw() +
        xlab("time [sec]") + ylab("scale(log(1-R))") +
        ylim(-3,3) 
      
      
      pp <- paudio / pl0
      pp
      
      fnp <- paste("Figures/chapViolinist_GUSO2_A2smooths_piece_",Pieces[[p]],"_condition_",Condition[[c]],"_subject_",Subjects[[s]],sep="")
      print(paste("----> saving figure",fnp))
      make_fig(fnp,pp)
      
    }
  }
}
}
run = FALSE
