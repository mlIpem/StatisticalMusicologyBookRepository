# source_chapListener_03_plotting
# The palette with grey:

run = FALSE
# run = TRUE
if(run){
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
#cbPalette <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
#  scale_fill_manual(values=cbPalette) +
#  scale_colour_manual(values=cbPalette) +

# plot distribution of Q1 (product value scoring) 
D <- Data %>% group_by(Q57) %>% summarize(mean = mean(Q1), sdev = std(Q1))

chapListeningPlot1 <- ggplot(Data,aes(x=Q1,fill = Q57)) +
  geom_histogram(alpha = 0.5, position = position_dodge(width = .2))  + 
  geom_vline(xintercept = D$mean[1],color = "#D55E00", linetype = "dashed",size = 1) +
  geom_vline(xintercept = D$mean[1]+D$sdev[1]/2,color = "gray", linetype = "dashed") +
  geom_vline(xintercept = D$mean[1]-D$sdev[1]/2,color = "gray", linetype = "dashed") +
  geom_vline(xintercept = D$mean[2],color = "#009E73", linetype = "dashed",size = 1) +
  geom_vline(xintercept = D$mean[2]+D$sdev[2]/2,color = "gray", linetype = "dashed") +
  geom_vline(xintercept = D$mean[2]-D$sdev[2]/2,color = "gray", linetype = "dashed") +
  theme_bw() +
scale_fill_manual(values=cbPalette) +
  scale_colour_manual(values=cbPalette) +
  scale_x_continuous(name = "Q1", 
                   #  limits = c(1, 10), 
                     breaks = 1:10, 
                     expand = c(0,0)) +  
  xlab("Q1 = overall appreciation") 

# plot info about subjects
g1 <- ggplot(Data,aes(x=Q50)) +
  geom_bar(position = position_dodge(width = .8), fill = "#56B4E9") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=.5))+
  ggtitle("Hours listening")
g2 <- ggplot(Data,aes(x=Q51)) +
  geom_bar(position = position_dodge(width = .8), fill = "#56B4E9") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=.5))+
  ggtitle("Hours playing")
g3 <- ggplot(Data,aes(x=Q52)) +
  geom_bar(position = position_dodge(width = .8), fill = "#56B4E9") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=.5))+
  ggtitle("Musical level")
g4 <- ggplot(Data,aes(x=Q53)) +
  geom_bar(position = position_dodge(width = .8), fill = "#56B4E9") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=.5))+
  ggtitle("Age")
g5 <- ggplot(Data,aes(x=Q54)) +
  geom_bar(position = position_dodge(width = .8), fill = "#56B4E9") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=.5))+
  ggtitle("Gender")
 chapListeningPlot2 <- (g1+g2+g5)/(g3+g4)

 
 chapListeningPlot3 <- ggplot(Data,aes(x = scale(Q1), y = scale(Quality)) )+
   geom_point(alpha = 0.5, position = position_jitter(width = .1,height = .1),shape=1,size=2)  + 
   geom_smooth(method = "lm") +
   geom_smooth(method = "gam") +
   theme_bw() +
   scale_fill_manual(values=cbPalette) +
   scale_colour_manual(values=cbPalette) +
   # scale_x_continuous(name = "Q1", 
   #                      limits = c(0.5, 10.5), 
   #                    breaks = 1:10, 
   #                    #expand = c(0,0)
   #                    ) +  #xlim(1,10) +
   ylab("Quality - scaled") + xlab("Q1 - scaled")
 
 cbPalette <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
 
 Data3 <- Data %>% mutate(Q3 = ifelse(is.na(Q3), 3, Q3), Q4 = ifelse(is.na(Q4), 3, Q4))
 chapListeningPlot4 <- ggplot(Data3,aes(y = (Q3), x = (Q4), color = factor(Q57))) +
  geom_point(position = position_jitterdodge(dodge.width = .6, jitter.height = .3, jitter.width = .5),  size = 2, shape = 1)  + 
   geom_rect(aes(xmin = 2.5, xmax = 3.5, ymin = -Inf, ymax = Inf),fill = "white", alpha = 0.01) +
   geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = 2.5, ymax = 3.5),fill = "white", alpha = 0.01) +
   
   annotate("text", label = "sad", x = 1.5, y = 1.5, size = 5, colour = "black")   + 
   annotate("text", label = "relaxing", x = 4.3, y = 1.5, size = 5, colour = "black")   + 
   annotate("text", label = "aggressive", x = 1.3, y = 4.5, size = 5, colour = "black")   + 
   annotate("text", label = "happy", x = 4.3, y = 4.5, size = 5, colour = "black")   + 
   theme_bw() +
   scale_fill_manual(values=cbPalette) +
   scale_colour_manual(values=cbPalette) +
   labs(y = "Q3 = arousal",x = "Q4 = valence", color = "Like_Dislike")
 
 
 make_fig("Figures/chapListener_Plot1",chapListeningPlot1)
 make_fig("Figures/chapListener_Plot2",chapListeningPlot2)
 make_fig("Figures/chapListener_Plot3",chapListeningPlot3)
 make_fig("Figures/chapListener_Plot4",chapListeningPlot4)

 
}
run = FALSE

#############################
#############################
# Daggity plotting via website using this model below
############################
############################
if(0){
  
mdl <- lavaanify("
Evaluation =~ X1 + X2 + X3
Immersion =~ X4 + X5 + X6 
Embodiment =~ X7 + X8 + X9
Emotion =~ X10 + X11 + X12 
Quality =~ X13 + X14 + X15 
Kind_of_experience =~ Immersion + Embodiment + Emotion
Evaluation ~ Kind_of_experience
Kind_of_experience ~  Quality 
Global_Appreciation ~ Evaluation",fixed.x=FALSE)
plot( lavaanToGraph( mdl ) )
dag <- lavaanToGraph( mdl )
impliedConditionalIndependencies( dag)

#########
########## Corrrelation analysis
########## 
d <- Data %>% select(-c(Title,Composer,Q3,Q4,Q6,Q8,Q10,Q12,Q14,Q16,
                        Q50,Q51,Q52,Q53,Q54,Q57,ArousalValence_category)) %>% cor(use = "complete.obs") 
#ggcorrplot(d)
#
#
pdf(file = "Figures/chapListener_Cor1.pdf")
corrplot::corrplot(as.matrix(d),method = 'square',type = 'lower', diag = FALSE)
dev.off()
png(file = "Figures/chapListener_Cor1.png")
corrplot::corrplot(as.matrix(d),method = 'square',type = 'lower', diag = FALSE)
dev.off()

#########
#########
w = which(abs(d) < .5)
d[w] = 0
#ggcorrplot(d)
pdf(file = "Figures/chapListener_Cor2.pdf")
corrplot::corrplot(as.matrix(d),method = 'square',type = 'lower', diag = FALSE)
dev.off()
png(file = "Figures/chapListener_Cor2.png")
corrplot::corrplot(as.matrix(d),method = 'square',type = 'lower', diag = FALSE)
dev.off()

}



