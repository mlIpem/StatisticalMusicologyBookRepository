# Modelling

###########################
###########################
# run B1 B2
###########################
# This chunk of code is pre-processed hence run = FALSE
# Select run = TRUE to run the chunk. It will then create files containing the fitted models
run = FALSE
# run = TRUE
if(run){
form = bf(relphase ~ 1 + s(time, k=50))
fam = "von_mises"
priors = get_prior(formula = form,data = RelPhase, family = fam)
B1 <- run_model_cmdstanr(RelPhase, form, priors, fam)
fam = "gaussian"
priors = get_prior(formula = form,data = RelPhase, family = fam)
B2 <- run_model_cmdstanr(RelPhase, form, priors, fam)

save(file = "Fitted/chapModelling_modelB1_k50.rda",B1)
save(file = "Fitted/chapModelling_modelB2_k50.rda",B2)

#g <- gam(data = RelPhase,relphase ~ 1 + s(time, k=30))
}

###########################
###########################
# run B3 B4
###########################
run = FALSE
# run = TRUE
if(run){
  # Execute with out compiling book
  form = bf(relphase ~ 1 + s(time0c))
  fam = "von_mises"
  B3 <- run_model_cmdstanr(Dataset, form, fam)
  form = bf(relphase ~ 1 + s(time0c) + (1 | cycle))
  B4 <- run_model_cmdstanr(Dataset, form, fam)
  
  save(file = "Fitted/chapModelling_modelB3.rda",B3)
  save(file = "Fitted/chapModelling_modelB4.rda",B4)
  
  g <- gam(data = Dataset,relphase ~ 1 + s(time0c, k=10) + s(cycle, bs="re"),method = "REML")
  g <- gam(data = Dataset,relphase ~ 1 + s(time0c, k=10, by=cycle), method = "REML")
  
  plot(predict(g))
  
}
run = FALSE




###########################
###########################
# predicted B1
###########################
# Not needed ?????????
run = FALSE
# run = TRUE
if(run){
load(file = "Fitted/chapModelling_modelB1_k50.rda") # load B1
Newdata = data.frame(time = seq(0,390, by=0.6))
pred <- B1 %>% epred_draws(newdata = Newdata) %>%
  median_qi(.epred) %>% mutate(phase.metro = 2*pi*time,
                               ruis =  (2*pi*rnorm(length(time),0,.06)),
                               relphase = .epred +ruis,
                               phase.tap = phase.metro + relphase,
                               
                               #phase.tap = phase.tap + ruis,
                               #phase.tap.cumsum = cumsum(phase.tap),
                               newtime = phase.tap/(2*pi)
                               )

plot <- ggplot(pred) +
  geom_lineribbon(aes(x = time, y = .epred, ymin = .lower, ymax = .upper), alpha=.4, color = "#56B4E9") + 
  geom_line(aes(x=time,y=.epred), color = "#56B4E9", alpha=1, size=1) +
  #geom_point(aes(x=time,y=relphase)) +
  ylab("relative phase") +
  geom_point(aes(x=newtime, y= do_rotate(relphase)), size=2, alpha=.6) +
  geom_point(data=RelPhase,aes(x=time,y=relphase),shape=1,size=2, alpha=.6) +
  theme_bw() +
  xlim(0,390)
sd_time = 0.007 #in seconds
m1 <- DData %>% filter(names == "metronome 1") %>% mutate(phase = 2*pi)
s1 <- DData %>% filter(names == "subject 1") %>% mutate(time = time + (2*pi*rnorm(length(time),0,.01)), phase = 2*pi)
m2 <- DData %>% filter(names == "metronome 2") %>% mutate(phase = 2*pi)
s2 <- DData %>% filter(names == "subject 2") %>% mutate(time = time + (2*pi*rnorm(length(time),0,.01)), phase = 2*pi)
}
run = FALSE


