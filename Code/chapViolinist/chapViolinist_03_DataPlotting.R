# chapDancer_DataPlotting
cbPalette <- c("#509999","#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7", "#509999","#A072B2","#FF8442","#F02242")
#cbPalette <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
#  scale_fill_manual(values=cbPalette) +
#  scale_colour_manual(values=cbPalette) +

# ###########################################
# NewData: see chapViolinist_02_DataPreparation.R
# ###########################################

if(0){
  PDm = NewData %>% group_by(condition) %>% summarize(m = median(PD, na.rm=TRUE))
  logPDm = NewData %>% group_by(condition) %>% summarize(m = median(logPD, na.rm=TRUE))
  
  NewData_p1 <- ggplot(NewData) + 
    geom_density(aes(x=PD, color = condition), alpha = 0.5) +
    geom_vline(xintercept = PDm$m[1], color = "#509999", linetype = "dashed")   +
    geom_vline(xintercept = PDm$m[2], color = "#999999", linetype = "dashed")   +
    theme_bw() + 
    scale_fill_manual(values=cbPalette) +
    scale_colour_manual(values=cbPalette) +
    xlab("PD")
  NewData_p2 <- ggplot(NewData) + 
    geom_density(aes(x=logPD, color = condition), alpha = 0.5) +
    geom_vline(xintercept = logPDm$m[1], color = "#509999", linetype = "dashed")   +
    geom_vline(xintercept = logPDm$m[2], color = "#999999", linetype = "dashed")   +
    theme_bw() + 
    scale_fill_manual(values=cbPalette) +
    scale_colour_manual(values=cbPalette) +
    xlab("logPD")
  chapViolinist_PD_hist <- NewData_p1 + NewData_p2
  
  NewData_p3 <- NewData %>% #filter(subject %in% c("P001")) %>% #, "P002", "P003"
    ggplot() + 
    geom_point(aes(x=time,y=logPD), size = 1,  shape = 1, alpha = .1, color = "#509999") + #,
    # position = position_jitter(width = 0.1)) +
    facet_grid(condition ~ piece) +
    theme_bw() +
    scale_fill_manual(values=cbPalette) +
    scale_colour_manual(values=cbPalette) +
    theme(axis.text.x = element_text(angle = 45, vjust =1 , hjust=1))+
    xlab("time [sec]") + ylab("logPD")
  
  chapViolinist_PD_data <- NewData_p3

  
  make_fig("Figures/chapViolinist_PD_NewData_p1_hist",NewData_p1)
  make_fig("Figures/chapViolinist_PD_NewData_p2_hist",NewData_p2)
  make_fig("Figures/chapViolinist_PD_NewData_p3_data",NewData_p3)

}

 
 

 
 
 
 
