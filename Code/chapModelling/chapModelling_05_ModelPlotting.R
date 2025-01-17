# ModelPlotting 

run = FALSE
# run = TRUE
if(run){
# ############################################
# Plotting smooth regression on relative phase
# load B1 and B2
# Generated in source_04_Modelling
load("Fitted/chapModelling_modelB1_k50.rda")
load("Fitted/chapModelling_modelB2_k50.rda")
ce1 <- conditional_effects(B1)[[1]]
ce2 <- conditional_effects(B2)[[1]]
ce <- cbind(ce1, estimate2 = ce2$estimate__, lower2 = ce2$lower__, upper2 = ce2$upper__)
#plot2 <- plot(ce, points = TRUE,point_args = list(shape=1,alpha=.5))[[1]] + theme_bw()
chapModellingBrms1 <- ggplot(ce) +
  geom_lineribbon(aes(x = time, y = estimate__, ymin = lower__, ymax = upper__), alpha=.4, color = "#56B4E9") + 
  geom_lineribbon(aes(x = time, y = estimate2, ymin = lower2, ymax = upper2), linetype = "dotted",alpha=.3, color = "#E69F00") + 
  geom_line(aes(x=time,y=estimate2), color = "#E69F00", alpha = 1, size = 1) +
  geom_line(aes(x=time,y=estimate__), color = "#56B4E9", alpha=1, size=1) +
  ylab("relative phase") +
  geom_point(data=RelPhase,aes(x=time,y=relphase),shape=1,size=2, alpha=.3) +
  theme_bw()

make_fig("Figures/chapModelling_Brms1",chapModellingBrms1)


# ############################################
# Plotting simulated discrete taps. This can take a while
# # ############################################

# 1. Prepare a data frame with equal samples
Newdata = data.frame(time = seq(0,100, by=0.01))
# 2. draw samples from the posterior in B1 with Newdata
pred1 <- B1 %>% epred_draws(newdata = Newdata) %>% median_qi(.epred) 
# 3. calculate discrete taps
pred2 <-  pred1 %>% mutate(phase.metro = 2*pi*time/.6, # we have 2pi when time = .6
                           epsilon2pi =  2*pi*rnorm(length(time),0,.05), # add a bit of noise to the .epred
                           relphase = .epred + epsilon2pi,
                           phase.tap = phase.metro + relphase,
                           newtime = phase.tap*.6/(2*pi))
# plot.ts(pred2$phase.tap[1:1000]%% (2*pi))
p <- findpeaks(pred2$phase.tap %% (2*pi), minpeakdistance = 56)
p = p[,2]
taps <- pred2[p,] #%>% mutate(phase.tap = 2*pi) 
# 4. Only the first 100s
relPhase <- RelPhase %>% filter(time <= 100)
# 5. Plotting
chapModellingB1predicted <- ggplot(pred2) +
  geom_lineribbon(aes(x = time, y = .epred, ymin = .lower, ymax = .upper), alpha=.4, color = "#56B4E9") + 
  #geom_line(aes(x=time,y=.epred), color = "#56B4E9", alpha=1, size=1) +
  # geom_point(aes(x=time,y=relphase)) +
  ylab("relative phase") +
  geom_point(data=relPhase,aes(x=time,y=relphase),shape=1,size=2, alpha=.6) +
  geom_point(data=taps,aes(x=newtime,y= relphase), size=2, alpha=.6) +
  theme_bw() +
  xlim(0,100)

make_fig("Figures/chapModelling_B1predicted",chapModellingB1predicted)
}
run = FALSE


