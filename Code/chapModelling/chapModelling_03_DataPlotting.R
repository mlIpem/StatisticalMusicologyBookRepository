# The palette with grey:
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
cbPalette <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
#  scale_fill_manual(values=cbPalette) +
#  scale_colour_manual(values=cbPalette) +

run = FALSE
# run = TRUE
if(run){

# #####################
# Plotting histograms
# #####################

cbPalette <- c("#999999",  "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
p1 <- ggplot(DData) + 
  geom_histogram( aes(x=dtime, color = names), alpha = 0.5, position = position_dodge(), bins = 50) +
  theme_bw() + #xlim(0.5,0.7) +
  scale_fill_manual(values=cbPalette) +
  scale_colour_manual(values=cbPalette) +
  xlab("period") + xlim(0.4,0.8)
cbPalette <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

p2 <- ggplot(RelPhase) +
  geom_histogram( aes(x=relphase), alpha = 0.2, position = "identity", bins = 50) +
  geom_vline(xintercept = -1.09, color = "#E69F00", size=1, linetype = "dashed") + # obtained from modelling
  geom_vline(xintercept =  -2.03, color = "#56B4E9", size = 1, linetype = "dashed") + # obtained from modelling
  geom_vline(xintercept =  alpha, color = "#D55E00", size = 1, linetype = "dashed") + # obtained from mean vector angle calculation
  theme_bw() + xlim(-pi,+pi) +
  scale_fill_manual(values=cbPalette) +
  scale_colour_manual(values=cbPalette) +
  xlab("relative phase") 
chapModellingTapping1 <- (p1 + p2)

# p<-ggplot(data=RelPhase,aes(x=relphase))+
#   geom_histogram(bins = 60)+
#   coord_polar() +
#   scale_x_continuous("", limits = c(-pi, pi), 
#                      breaks = c(-pi,-pi/2,0,pi/2), labels = c("-pi","-pi/2","0","pi/2"))+
#   scale_y_continuous("",limits=c(-pi,pi))
# p
make_fig("Figures/chapModelling_Tapping1",chapModellingTapping1)


# #####################
# Plotting relative phase over time
# #####################
# 
chapModellingTapping0 <-  RelPhase %>% filter(time > 160, time < 175) %>% 
ggplot() +
  geom_point( aes(x=time, y=relphase),size=3, shape=1, color = "black") +
  #geom_point( aes(x=time_metronome, y=relphase*0),shape=3,size=3,color = "#56B4E9") +
  geom_hline(aes(yintercept = 0), linetype = "dashed",color = "#D55E00") +
  #geom_vline(aes(xintercept = 0), linetype = "dashed",color = "#56B4E9") +
  geom_vline(aes(xintercept = time_metronome), linetype = "dashed",color = "#56B4E9") +
  theme_bw() + ylim(-pi,pi) +
  scale_fill_manual(values=cbPalette) +
  scale_colour_manual(values=cbPalette) +
  # geom_vline(xintercept = 39*seq(1,10) ) +
  xlab("time [s]") + ylab("relative phase [rad]") + ylim(-pi,+pi)

 make_fig("Figures/chapModelling_Tapping0",chapModellingTapping0)

}
run = FALSE