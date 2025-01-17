# chapExoskeletons_03_DataPlotting.R

run = FALSE
# run = TRUE
if(run){
cbPalette <- c("#509999","#999999", "#E69F00", "#56B4E9", "#009E73", 
               "#F0E442", "#0072B2", "#D55E00", "#CC79A7", 
               "#A072B2","#FF8442","#F02242")

#Dat_long <- Dat %>% filter(Dyad == "D19") %>% pivot_longer(cols = c(s1,s2))
Dat_long <- Dat %>% pivot_longer(cols = c(ss1,ss2))

p1 <- ggplot(Dat_long) +
  geom_line(aes(x=Time, y = value, color = name)) +
  facet_grid( Block ~ interaction(Condition,Tempo)) +
  theme_bw() + 
  scale_fill_manual(values=cbPalette) +
  scale_colour_manual(values=cbPalette) 


Dat_long <- Dat %>% dplyr::filter(Dyad == "D19") %>% pivot_longer(cols = c(s1,s2))
p1 <- ggplot(Dat_long) +
  geom_line(aes(x=Time, y = value, color = name)) +
  facet_grid( Block ~ interaction(Condition,Tempo)) +
  theme_bw() + 
  scale_fill_manual(values=cbPalette) +
  scale_colour_manual(values=cbPalette) 



  make_fig("Figures/chapExoskeletons_DataPlotting_p1",p1)

p2 <-  Dat %>% filter(Dyad == "D04") %>% 
  ggplot() +
  geom_point(aes(x=scale(s1), y = scale(s1)) ) +
  facet_grid( Block ~ interaction(Condition,Tempo))+
  theme_bw() + 
  scale_fill_manual(values=cbPalette) +
  scale_colour_manual(values=cbPalette) 


p3 <- Dat %>% filter(Dyad == "D04") %>% 
  ggplot() +
  geom_line(aes(x=Time, y = s1 - s2), color = "#E69F00")


p4 <- Dat %>% filter(Dyad == "D04", Block == "B1", Trial == "T4") %>% 
  ggplot() +
  geom_point(aes(x = s1, y = s2, color = Condition)) 

# (p1 + p2) / (p3 + p4)



v1 <- Vector %>% ggplot() +
  geom_point(aes(x=ConditionTempo, y = R,fill=ConditionTempo),
             position = position_jitterdodge(dodge.width=.3,jitter.width=.5),shape=1) +
 # facet_grid(  ~ Block) +
  theme_bw() + 
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=.5)) +
  scale_fill_manual(values=cbPalette) + scale_colour_manual(values=cbPalette) 

v2 <- ggplot(Vector) +
  geom_point(aes(x=ConditionTempo, y = abs(alpha) ),
             position = position_dodge(width=.3)) +
  facet_grid(  ~ Block) +
  theme_bw() + 
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=.5)) +
  scale_fill_manual(values=cbPalette) + scale_colour_manual(values=cbPalette) 

# ggplot(Vector1) +
#   geom_boxplot(aes(x=ConditionTempo,y=R,color = Block)) + theme_bw() 
# ggplot(Vector1) +
#   geom_boxplot(aes(x=Block,y=R,color = ConditionTempo))+theme_bw() 
# ggplot(Vector2) +
#   geom_boxplot(aes(x=ConditionTempo,y=R,color = Block)) + theme_bw() 
# ggplot(Vector2) +
#   geom_boxplot(aes(x=Block,y=R,color = ConditionTempo))+theme_bw() 

ggplot(Vector) +
  geom_boxplot(aes(x=ConditionTempo,y=R,color = Block)) + theme_bw() 
ggplot(Vector) +
  geom_boxplot(aes(x=ConditionTempo,y=abs(alpha),color = Block)) + theme_bw() 



hist(Vector$alpha,60)
hist(Vector$abs_alpha,60)
hist((Vector$R),60)
plot(Vector$R)
plot(Vector$alpha)
plot(Vector$abs_alpha)
v1
v2


v3a <- Vector %>% filter(Reference == "violin 1") %>%
  ggplot() + geom_boxplot(aes(x = ConditionTempo, y = R, color = Expertise)) 
v3b <- Vector %>% filter(Reference == "violin 2") %>%
  ggplot() + geom_boxplot(aes(x = ConditionTempo, y = R, color = Expertise)) 


v4a <- Vector %>% filter(Reference == "violin 1") %>%
  ggplot() + geom_boxplot(aes(x = ConditionTempo, y = logalpha_abs, color = Expertise)) 
v4b <- Vector %>% filter(Reference == "violin 2") %>%
  ggplot() + geom_boxplot(aes(x = ConditionTempo, y = logalpha_abs, color = Expertise)) 


v3a / v3b
v4a / v4b


Vector %>% 
  ggplot() + 
  geom_boxplot(aes(x = Condition, y = R, color = Expertise),
               position = position_dodge(width = .7) ) +
  geom_point(aes(x = Condition, y = R, color = Expertise),
             position = position_jitterdodge(dodge.width = .7, jitter.width = .15)) +
  facet_grid(~Tempo)

Vector_c %>% 
  ggplot() + 
  geom_boxplot(aes(x = Condition, y = R_c, color = Expertise),
               position = position_dodge(width = .7) ) +
  geom_point(aes(x = Condition, y = R_c, color = Expertise),
             position = position_jitterdodge(dodge.width = .7, jitter.width = .15)) +
  facet_grid(~Tempo)

Vector_c %>% 
  ggplot() + 
  geom_boxplot(aes(x = Condition, y = R_c, color = Tempo),
               position = position_dodge(width = .7) ) +
  geom_point(aes(x = Condition, y = R_c, color = Tempo),
             position = position_jitterdodge(dodge.width = .7, jitter.width = .15)) +
  theme_bw() + 
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=.5)) +
  scale_fill_manual(values=cbPalette) + scale_colour_manual(values=cbPalette) 


Vector_c %>% 
  ggplot() + 
  geom_boxplot(aes(x = Condition, y = angle_abs_c, color = Tempo),
               position = position_dodge(width = .7) ) +
  geom_point(aes(x = Condition, y = angle_abs_c, color = Tempo),
             position = position_jitterdodge(dodge.width = .7, jitter.width = .15)) +
  theme_bw() + 
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=.5)) +
  scale_fill_manual(values=cbPalette) + scale_colour_manual(values=cbPalette) 
}
run = FALSE




run = FALSE
# run = TRUE
if(run){
  cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", 
                 "#F0E442", "#0072B2", "#D55E00", "#CC79A7", 
                 "#509999","#A072B2","#FF8442","#F02242")
# Bow marker 1
# 
datnow <-  Dat %>% filter(Dyad == "D04", Block == "B1", Trial == "T4") 
  
Bp1 <-  ggplot(datnow) +
  geom_line(aes(x=Time,y=`BOW1_1 X`), color = "#D55E00") +
  geom_line(aes(x=Time,y=BX1), color = "#999999") +
  theme_bw() +
  labs(x="time [s]")
Bp2 <-  ggplot(datnow) +
  geom_line(aes(x=Time,y=`BOW1_1 Y`), color = "#D55E00") +
  geom_line(aes(x=Time,y=BY1), color = "#999999") +
  theme_bw()+
  labs(x="time [s]")
Bp3 <-  ggplot(datnow) +
  geom_line(aes(x=Time,y=`BOW1_1 Z`), color = "#D55E00") +
  geom_line(aes(x=Time,y=BZ1), color = "#999999") +
  theme_bw()+
  labs(x="time [s]")

# Violin marker 3
Vp1 <-  ggplot(datnow) +
  geom_line(aes(x=Time,y=`VIOLIN1_3 X`), color = "#D55E00") +
  geom_line(aes(x=Time,y=VX1), color = "#999999") +
  theme_bw()+
  labs(x="time [s]")
Vp2 <-  ggplot(datnow) +
  geom_line(aes(x=Time,y=`VIOLIN1_3 Y`), color = "#D55E00") +
  geom_line(aes(x=Time,y=VY1), color = "#999999") +
  theme_bw()+
  labs(x="time [s]")
Vp3 <-  ggplot(datnow) +
  geom_line(aes(x=Time,y=`VIOLIN1_3 Z`), color = "#D55E00") +
  geom_line(aes(x=Time,y=VZ1), color = "#999999") +
  theme_bw()+
  labs(x="time [s]")


chapExoskeletons_Bow_Violin_markers <- (Bp1 + Vp1) / (Bp2 + Vp2) / (Bp3 + Vp3)


p1 <-  ggplot(datnow) +
  geom_line(aes(x=Time,y=`X1`), color = "#D55E00") +
  geom_line(aes(x=Time,y=-s1), color = "#999999") +
  theme_bw() + 
  labs(x="time")
p2 <-  ggplot(datnow) +
  geom_line(aes(x=Time,y=`Y1`), color = "#D55E00") +
  geom_line(aes(x=Time,y=-s1), color = "#999999") +
  theme_bw()+ 
  labs(x="time")
p3 <-  ggplot(datnow) +
  geom_line(aes(x=Time,y=`Z1`), color = "#D55E00") +
  geom_line(aes(x=Time,y=-s1), color = "#999999") +
  theme_bw()
chapExoskeletons_PCA_extracted <- p1 / p2 / p3

chapExoskeletons_s1s2_time <-  ggplot(datnow)+
  geom_line(aes(x=Time,y=ss1), color = "#E69F00") +
  geom_line(aes(x=Time,y=ss2), color = "#56B4E9")+
  theme_bw() +
  labs(x = "time", y = "bowing s1 and s2")

chapExoskeletons_s1s2 <- ggplot(datnow)+
  geom_point(aes(x=ss2,y=ss1), color = "#E69F00") +  theme_bw()




dnow <- datnow %>% mutate(  as1 = hilbert(datnow$ss1, 120),
                    ps1 = ifreq(as1, f=120,plot=FALSE)$p,
                    as2 = hilbert(datnow$ss2, 120),
                    ps2 = ifreq(as2, f=120,plot=FALSE)$p,
                    # time <- ps1[,1]
                    phi1 = unwrap(ps1[,2]),
                    phi2 = unwrap(ps2[,2]),
                       relphase_centered = (((phi1 - phi2) +pi) %% (2*pi)) -pi,
                    relphase = phi1 - phi2,
                    V = mean(exp(1i*relphase)),
                    R = Mod(V),
                    alpha = Arg(V)
                    )

pp2 <- ggplot(dnow)+
  geom_point(aes(x=Time,y=relphase_centered), color = "#E69F00") +  theme_bw()

chapExoskeletons_subject_1_2_relphase <- pp1 / pp2

make_fig("Figures/chapExoskeletons_Bow_Violin_markers",chapExoskeletons_Bow_Violin_markers)
make_fig("Figures/chapExoskeletons_PCA_extracted",chapExoskeletons_PCA_extracted)
make_fig("Figures/chapExoskeletons_subject_1_2_relphase", chapExoskeletons_subject_1_2_relphase)

make_fig("Figures/chapExoskeletons_s1s2_time", chapExoskeletons_s1s2_time)
make_fig("Figures/chapExoskeletons_s1s2", chapExoskeletons_s1s2)

cor(datnow$ss1,datnow$ss2)
}
run = FALSE





