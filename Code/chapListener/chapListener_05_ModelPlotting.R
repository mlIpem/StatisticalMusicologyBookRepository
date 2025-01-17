# source_chapListener_05_ModelPlotting
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A0","#994400", "#E69EE0", "#5894E9")
#cbPalette <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
#  scale_fill_manual(values=cbPalette) +
#  scale_colour_manual(values=cbPalette) +


# Plotting
run = FALSE
# run = TRUE
if(run){
  
  # Plotting Predictions Bach
  
  Data_onlyBach <- Data %>% filter((Composer == "Bach, Johann Sebastian") ) 
  cbPalette <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A0","#994400", "#E69EE0", "#5894E9")
  chapListener_Bach_pred1 <- ggplot(Pred_Data_Bach) +
    geom_point(aes(x=scale(Q1_data),y=scale(Q1_predict),color = LD), shape = 1, size = 2, position = position_jitter(width = .2/5)) +
    geom_smooth(aes(x=scale(Q1_data),y=scale(Q1_predict)), method = "lm", formula = "y ~ x" )+
    geom_smooth(aes(x=scale(Q1_data),y=scale(Q1_predict)), method = "loess", formula = "y ~ x" )+
    theme_bw()+
    scale_fill_manual(values=cbPalette) +
    scale_colour_manual(values=cbPalette) +
    labs(title = "", y = "Predicted Q1", x = "True Q1") 
  #cor(Pred_Data$Q1_data,Pred_Data_Bach$Q1_predict)
  #lm(data = Pred_Data_Bach, scale(Q1_predict)  ~ scale(Q1_data))
  make_fig("Figures/chapListener_Bach_pred1", chapListener_Bach_pred1 )
  
  chapListener_Bach_pred1_nonscaled <- ggplot(Pred_Data_Bach) +
    geom_point(aes(x=(Q1_data),y=(Q1_predict),color = LD), shape = 1, size = 2, position = position_jitter(width = .2)) +
    geom_smooth(aes(x=(Q1_data),y=(Q1_predict)), method = "lm", formula = "y ~ x" )+
    geom_smooth(aes(x=(Q1_data),y=(Q1_predict)), method = "loess", formula = "y ~ x" )+
    theme_bw()+
    scale_fill_manual(values=cbPalette) +
    scale_colour_manual(values=cbPalette) +
    labs(title = "", y = "Predicted Q1", x = "True Q1") 
  #cor(Pred_Data$Q1_data,Pred_Data_Bach$Q1_predict)
  #lm(data = Pred_Data_Bach, scale(Q1_predict)  ~ scale(Q1_data))
  make_fig("Figures/chapListener_Bach_pred1_nonscaled", chapListener_Bach_pred1_nonscaled )
  
  # Plotting Spiegel
  
  cbPalette <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A0","#994400", "#E69EE0", "#5894E9")
  chapListener_Spiegel_pred2 <- ggplot(Pred_Data) +
    geom_point(aes(x=scale(Q1_data),y=scale(Q1_predict),color = LD), shape = 1, size = 2) +
    geom_smooth(aes(x=scale(Q1_data),y=scale(Q1_predict)), method = "lm", formula = "y ~ x" )+
    theme_bw()+
    scale_fill_manual(values=cbPalette) +
    scale_colour_manual(values=cbPalette) +
    labs(title = "", y = "Q1_prediction (onlySpiegel)", x = "Q1_data (withoutSpiegel)")
  cor(Pred_Data$Q1_data,Pred_Data$Q1_predict)
  lm(data = Pred_Data, scale(Q1_predict)  ~ scale(Q1_data))
  
  # Plotting ArousalValence

  cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A0","#994400", "#E69EE0", "#5894E9")
  chapListener_ArousalValence_pred <- ggplot(Pred_long) +
    # geom_point(aes(x=name,y=value,color=Code), position = position_jitterdodge(jitter.width = .4, dodge.width = .8),shape=2) +
    #stat_halfeye(aes(x=name,y=value,color=Code), position = position_dodge(width = 1)) +
    stat_dots(aes(y=Code,x=value),  quantiles = 50, size = 1, color = "#56B4E9") + #position = position_dodge(width = 1),
    theme_bw()+
    scale_y_discrete(guide = guide_axis(angle = 45)) +
    scale_fill_manual(values=cbPalette) +
    scale_colour_manual(values=cbPalette) +
    labs(title = "", y = "Attributed arousal-valence", x = "Predicted latent variables", color = "Arousal_Valence") +
    facet_grid(~name)
  
  make_fig("Figures/chapListener_ArousalValence_pred", chapListener_ArousalValence_pred )
}
run = FALSE


run = FALSE
# run = TRUE
if(run){
  # SEM
  # Plotting the SEM models

  chapListener_SEMgraph_model_1 <- semPaths(semfit1, what='est', edge.label.cex=1, curvePivot = TRUE, nCharNodes = 0, fade=FALSE, layout = "tree", rotation = 3) 
  chapListener_SEMgraph_model_1_structure <- semPaths(semfit1,  what='cons', whatLabels ='omit', edge.label.cex=1.25, curvePivot = TRUE, nCharNodes = 0, fade=FALSE, layout = "tree", rotation = 3) 

}

run = FALSE 
  


