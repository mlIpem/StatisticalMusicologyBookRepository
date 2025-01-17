# chapTappers_06_Evaluation
#


run = FALSE
# run = TRUE
if(run){
# ####################################################################
# PosteriorSummary of state parameters
# After having done 05_Modelling.Rmd

nr = nrow(K)
filenr = 1:nr
print("calculate posterior summary of states")
PosteriorSummary_States <- data.frame() 
for(s in filenr){
  # s = 1
  fn = paste("Fitted/chapTappers_sim_StateSpaceModel_", datum, "_",s,".rds",sep="")
  if(file.exists(fn)) {
    print(paste("processing",fn))
    fit <- readRDS(file = fn)
    fit_result <- fit %>% gather_draws(ks1m1,ks2m2,ds1m1,ds1s2,ds2m2,ds2s1) %>%
      median_qi()  %>% mutate(sim = s) %>% rename(names = ".variable")
    PosteriorSummary_States <- rbind(PosteriorSummary_States,fit_result)
  }else{print("The file doesn't exist")}
}


saveRDS(file = paste("Data/chapTappers_PosteriorSummary_States.rds",sep=""),PosteriorSummary_States)



# ####################################################################
# PosteriorSummary of observation parameters
# After having done 05_Modelling.Rmd

#stan_plot(fit, pars = "observation_std")


nr = nrow(K)
filenr = 1:nr
print("calculate posterior summary of observations")
PosteriorSummary_Observations <- data.frame() 
for(s in filenr){
  # s = 1
  fn = paste("Fitted/chapTappers_sim_StateSpaceModel_", datum, "_",s,".rds",sep="")
  if(file.exists(fn)) {
    print(paste("processing",fn))
    fit <- readRDS(file = fn)
    res <- fit %>%
      spread_draws(observation_std[unit]) %>%
      median_qi(.width=c(.95))
    res$observation_std[1] = res$observation_std[1]*.6/(2*pi)
    res$.lower[1] = res$.lower[1] * .6/(2*pi)
    res$.upper[1] = res$.upper[1] * .6/(2*pi)
    res$observation_std[2] = res$observation_std[2]*.6/(2*pi)
    res$.lower[2] = res$.lower[2] * .6/(2*pi)
    res$.upper[2] = res$.upper[2] * .6/(2*pi)
    
    res$observation_std[3] = res$observation_std[3]*.61/(2*pi)
    res$.lower[3] = res$.lower[3] * .61/(2*pi)
    res$.upper[3] = res$.upper[3] * .61/(2*pi)
    res$observation_std[4] = res$observation_std[4]*.61/(2*pi)
    res$.lower[4] = res$.lower[4] * .61/(2*pi)
    res$.upper[4] = res$.upper[4] * .61/(2*pi)  
    
    res <- res %>% mutate(unit = c("m1","s1","m2","s2"))
    PosteriorSummary_Observations <- rbind(PosteriorSummary_Observations,res)
  }else{print("The file doesn't exist")}
}
saveRDS(file = paste("Data/chapTappers_PosteriorSummary_Observations.rds",sep=""),PosteriorSummary_Observations)

}
run = FALSE


#ggplot(res) + geom_pointinterval(aes(x=observation_std,xmin=.lower,xmax=.upper,y=unit))