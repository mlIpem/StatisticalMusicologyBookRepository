# Modelling 
# It can be time-consuming to run all models, even when for-loops are parallelized. 
# Alternatively it is possible to make selections, avoiding loops.
#################################################################################################
# 1. Bowing gesture events
#########################################################
# 1.1. Modelling 2 conditions per piece
#    use NewData = bowing gesture events (Adriaan)
#########################################################
# skew_normal
run = FALSE
# run = TRUE
if(run){
  cores=detectCores()
  cl <- makeCluster(cores[1]-1) #not to overload your computer
  registerDoParallel(cl)
  foreach (p = c(1:4)) %dopar% {
    library(tidyverse)
    library(brms)
    # p = 1
    print(paste("------> Processing ", Pieces[[p]]))
    fn = paste("Fitted/chapViolinist_model_", Pieces[[p]],".rds",sep="")
    Dat <- NewData %>% filter(piece == Pieces[[p]]) %>% droplevels()
    
    #prior(normal(0,1.5), class = b) + prior(cauchy(0,2), class = sd)
    print("# 1. Run the model")
    
    form = bf(logPD ~ 1 + condition +  s(time, k = 30) + 
                s(time, by = condition, k = 30) + 
                (1 | subject + trial)  )
    fam = gaussian()
    prior4 <- get_prior(data = Dat,formula = form,family = fam)
    
    fit <- brm(
      formula = form,
      data = Dat, 
      family = fam, 
      prior = prior4,
      iter = 6000, warmup = 2000, thin = 2, #refresh = 0,
      #control = list(adapt_delta = 0.998, max_treedepth = 15),
      backend = "cmdstanr",
      silent = 0,
      threads = threading(10))
    saveRDS(file = fn, fit)
  }
  stopCluster(cl)
}
run = FALSE
#########################################################
# 1.2. Modelling individual subjects
#    use NewData = bowing gesture events (Adriaan)
#########################################################
run = FALSE
# run = TRUE
if(run){
  #setup parallel backend to use many processors
  cores=detectCores()
  cl <- makeCluster(cores[1]-1) #not to overload your computer
  registerDoParallel(cl)
  # load(file = "./Data/chapViolinist_Data.RData") # Get Data
  load(file = "Data/chapViolinist_NewData.RData") # Get NewData
  Pieces <- list("F1","F2","F3","F4")
  Subjects <- unique(NewData$subject) %>% as.character()
  
  # try with : Subjects <- Subjects[c(1,2)]
  foreach(s = 1:length(Subjects)) %dopar% {
    # for (s in Subjects){
    library(tidyverse)
    library(brms)
    subj = Subjects[s]
    Dat <- NewData %>% filter(subject == subj) %>% droplevels() %>% mutate(conditiontrial = interaction(condition,trial))
    for (c in unique(Dat$condition)){
      Datcond <- Dat %>% filter(condition == c)
      form = bf(logPD ~ 1 + conditiontrial +  s(time, k = 30) +
                  s(time, by = conditiontrial, k = 30) )
      fam = skew_normal()
      prior4 <- get_prior(data = Datcond,formula = form,family = fam)
      
      fit <- brm(formula = form,
                 data = Datcond, 
                 family = fam, 
                 prior = prior4,
                 iter = 6000, warmup = 2000, thin = 2, 
                 #  control = list(adapt_delta = 0.998, max_treedepth = 15),
                 backend = "cmdstanr",
                 threads = threading(10)
      )
      fn = paste("Fitted/chapViolinist_model_subj_",subj,"_condi_", c,".rds",sep="")
      saveRDS(file = fn,fit)
    }
  }
  #stop cluster
  stopCluster(cl)
}
run = FALSE
#################################################################################################
#################################################################################################
# 2. Relative phase continuous
#########################################################
# 2.1. Parallel processing to contrast conditions per piece
###########################################
#Res <- readRDS(file = "Data/Res.rds")

run = FALSE
# run = TRUE
if(run){
  cores=detectCores()
  cl <- makeCluster(cores[1]-1) #not to overload your computer
  registerDoParallel(cl)
  Pieces <- list("F1","F2","F3","F4")
  
  foreach(p = 1:length(Pieces)) %dopar% {
    library(tidyverse)
    library(brms)
    Dat <- Res %>% filter(Piece == Pieces[[p]], ! Participant == "P006") %>% droplevels()
    print("# 1. Run the model")
    form <- bf(log_1_R ~ 1 + Condition +  s(Time, k = 30) + 
                 s(Time, by = Condition, k = 30) + 
                 (1 | Participant:Trial))
    fam = gaussian()
    prior4 <- get_prior(formula = form, data = Dat, family = fam)
    fit <- brm(formula = form,
               family = fam,
               data = Dat, 
               prior = prior4,
               iter = 6000, warmup = 2000, thin = 1, #refresh = 0,
               #control = list(adapt_delta = 0.998, max_treedepth = 15),
               backend = "cmdstanr",
               silent = 0,
               threads = threading(5),
    )
    fn = paste("Fitted/chapViolinist_GUSO2_Gaussian_model_2conditions_piece_",Pieces[[p]],".rds",sep="")
    saveRDS(file = fn,fit)
  }
  #stop cluster
  stopCluster(cl)
}
run = FALSE

#######################################
# 2.2. Parallel processing of all subjects individually
#######################################
# Res <- readRDS(file = "Data/Res.rds")

run = FALSE
#run = TRUE
if(run){
  #setup parallel backend to use many processors
  cores=detectCores()
  cl <- makeCluster(cores[1]-1) #not to overload your computer
  registerDoParallel(cl)
  Pieces <- list("F1","F2","F3","F4")
  
  # try with : Subjects <- Subjects[c(1,2)]
  foreach(p = 1:length(unique(Res$Participant))) %dopar% {
    # p = 3
    pp = unique(Res$Participant)[p]
    p = pp
    library(tidyverse)
    library(brms)
    Dat <- Res %>% filter(Participant == p) %>%
      mutate(ConditionTrial = interaction(Condition,Trial)) %>% droplevels()
    for (c in unique(Dat$Condition)){
      # c = "3D"
      Datcond <- Dat %>% filter(Condition == c)
      form = bf(log_1_R ~ 1 + ConditionTrial +  s(Time, k= 30) +
                  s(Time, by = ConditionTrial, k = 30) )
      fam = gaussian()
      prior4 <- get_prior(formula = form, data = Datcond, family = fam)
      fit <- brm(formula = form,
                 data = Datcond, 
                 family = fam,
                 prior = prior4,
                 iter = 6000, warmup = 2000, thin = 2, 
                 #refresh = 0,
                 #  control = list(adapt_delta = 0.998, max_treedepth = 15),
                 backend = "cmdstanr",
                 threads = threading(5)
      )
      fn = paste("Fitted/chapViolinist_GUSO2_Amodel_subj_",p,"_condi_", c,".RData",sep="")
      save(file = fn,fit)
    }
  }
  #stop cluster
  stopCluster(cl)
}
run = FALSE

