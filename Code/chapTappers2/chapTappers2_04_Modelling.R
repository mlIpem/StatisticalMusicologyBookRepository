# chapTappers2_04_Modelling

# ########################################################################
# Smooth regression of relative phase of tapping data
# To show the processing we delete dyads and focus on a single cycle
# Results in the text are based on a processing of all dyads and all cycles
# See chapTappers2_02_DataPreparation where DTapping_RelativePhase has been calculated
# ########################################################################
run = FALSE
# run = TRUE
if(run){
  DTapping_RelativePhase <- readRDS("Data/chapTappers2_DTapping_RelativePhase.rds")
  Data <- DTapping_RelativePhase %>% mutate(namescondition = interaction(names,condition))  %>%
    dplyr::filter(!(dyad %in% c("dyad_1", "dyad_3", "dyad_5", "dyad_6", "dyad_8", "dyad_13", "dyad_15")), 
                  cycle %in% c("cycle_2","cycle_4","cycle_6")) %>% droplevels()
  fam <- von_mises()
  model_form <- bf(relphase ~ 1 + namescondition + 
                     s(time0c, by = namescondition) +
                     (1  |  dyad:cycle), 
                   kappa ~ 1 + namescondition + 
                     s(time0c, by = namescondition) +
                     (1  |  dyad:cycle)
  )
  priors <- get_prior(formula = model_form, family = fam, data = Data)
  fit <- run_model_cmdstanr(Data,model_form,"von_mises",priors)
  saveRDS(file="Fitted/chapTappers2_SmoothRegression_RelPhase.rds",fit)
}
run = FALSE

# ########################################################################
# Regression of parameter dataset for K
# ########################################################################
run = FALSE
# run = TRUE
if(run){
  run_model_cmdstanr <- function(Data, form, fam, priors){
    print(paste(form[1],fam,sep="\n"))
    fit <- brm(data = Data,
               formula = form,
               family = fam,
               prior = priors,
               iter = 8000, 
               #warmup = 2000,
               #control = list(adapt_delta = 0.95,  max_treedepth = 10),
               init = 0,
               # thin = 2,
               chains = 4,
               backend = "cmdstanr",
               threads = threading(4),
               silent = 0,
               # cores = 4
               #save_pars = save_pars(all = TRUE),
               #sample_prior = TRUE #,
               #file = filen
    )
    return(fit)
  }
  
  Posterior_selection <- Posterior %>% 
    filter(.variable %in% c("ks1m1","ks2m2")) %>%
    #separate(id, into = c("condition", "dyad", "cycle"), sep = "\\.") %>%
    mutate(dyad = factor(dyad, levels = all_dyads), cycle = factor(cycle, labels = all_cycles_numbers),
           condition = factor(condition, levels = all_conditions),
           .variable = factor(.variable))
  # dat_selected_K
  form_K00 = bf(.value ~ condition * .variable + ( 1 | dyad:cycle),
                sigma ~ condition * .variable + ( 1  | dyad:cycle))
  prior_K00 <- get_prior(form_K00,data = Posterior_selection,family = "skew_normal")
  fit_K00 <- run_model_cmdstanr(Posterior_selection,form_K00,"skew_normal",prior_K00)
  

  saveRDS(file = "Fitted/chapTappers2_fit_K00.rds",fit_K00)
  
  # can be deleted because we don't run this model
  # form_K02 = bf(.value ~ 1,
  #               mu1 ~ condition * .variable + (1 | dyad:cycle),
  #               mu2 ~ condition * .variable + (1 | dyad:cycle),
  #               phi1 ~ condition * .variable + (1 | dyad:cycle),
  #               phi2 ~ condition * .variable + (1 | dyad:cycle))
  # prior_K02 <- get_prior(form_K02,data = Posterior_selection,family = mixture("beta","beta"))
  # fit_K02 <- run_model_cmdstanr(Posterior_selection,form_K02,mixture("beta","beta"),prior_K02)
  # saveRDS(file = "Fitted/chapTappers2_fit_K02.rds",fit_K02)
  # 
  #loo(fit_K00, fit_K02)
  
  # ########################################################################
  # modelling the parameters K per dyad
  # obtain a data frame Df with parameters K per 
  Df <- data.frame()
  for(dy in unique(dat_selected_K$dyad)){
    # dy = "dyad_5"
    print(paste("processing ",dy))
    dat_selected_K_dyad <- dat_selected_K %>% dplyr::filter(dyad == dy) %>% droplevels()
    form_K00_dyad = bf(.value ~ condition * names + ( 1 | cycle),
                       sigma ~ condition * names + ( 1  | cycle))
    prior_K00_dyad <- get_prior(form_K00_dyad,
                                data = dat_selected_K_dyad,
                                family = "skew_normal")
    fit_K00_dyad <- run_model_cmdstanr(dat_selected_K_dyad,
                                       form_K00_dyad,
                                       "skew_normal",
                                       prior_K00_dyad)
    newdat = dat_selected_K_dyad %>% data_grid(condition,names)
    df <- epred_draws(fit_K00_dyad, newdat, re_formula = NA) %>% median_qi(.epred) %>% 
      mutate(dyad = dy) %>%
      dplyr::select(dyad, condition, names, .epred, .lower, .upper) 
    
    Df <- rbind(Df,df)
  }
  saveRDS(file = "Data/chapTappers2_EstimatedParameters_perDyad",Df)
}
run = FALSE

# ########################################################################
# Regression of parameter dataset for D
# ########################################################################
run = FALSE
# run = TRUE
if(run){
  # dat_selected_D
  Posterior_selection <- Posterior %>% 
    filter(.variable %in% c("ds1m1","ds2m2","ds1s2","ds2s1")) %>%
    #separate(id, into = c("condition", "dyad", "cycle"), sep = "\\.") %>%
    mutate(dyad = factor(dyad, levels = all_dyads), cycle = factor(cycle, labels = all_cycles_numbers),
           condition = factor(condition, levels = all_conditions),
           .variable = factor(.variable))
  
  form_D00 = bf(.value ~ condition * .variable + ( 1 | dyad:cycle),
                sigma ~ condition * .variable + ( 1  | dyad:cycle))
  prior_D00 <- get_prior(form_D00,data = Posterior_selection,family = "normal")
  fit_D00 <- run_model_cmdstanr(Posterior_selection,form_D00,"normal",prior_D00)
  
  saveRDS(file = "Fitted/chapTappers2_fit_D00.rds",fit_D00)
  
  
  
  
  
  # ########################################################################
  # modelling the parameters D per dyad
  # obtain a data frame Df with parameters K per 
  # 
  # 
  Df <- data.frame()
  for(dy in unique(dat_selected_K$dyad)){
    # dy = "dyad_5"
    print(paste("processing ",dy))
    dat_selected_K_dyad <- dat_selected_K %>% dplyr::filter(dyad == dy) %>% droplevels()
    form_K00_dyad = bf(.value ~ condition * names + ( 1 | cycle),
                       sigma ~ condition * names + ( 1  | cycle))
    prior_K00_dyad <- get_prior(form_K00_dyad,
                                data = dat_selected_K_dyad,
                                family = "skew_normal")
    fit_K00_dyad <- run_model_cmdstanr(dat_selected_K_dyad,
                                       form_K00_dyad,
                                       "skew_normal",
                                       prior_K00_dyad)
    newdat = dat_selected_K_dyad %>% data_grid(condition,names)
    df <- epred_draws(fit_K00_dyad, newdat, re_formula = NA) %>% median_qi(.epred) %>% 
      mutate(dyad = dy) %>%
      dplyr::select(dyad, condition, names, .epred, .lower, .upper) 
    
    Df <- rbind(Df,df)
  }
  saveRDS(file = "Data/chapTappers2_EstimatedParameters_perDyad",Df)
}
run = FALSE


# ########################################################################
# Regression of parameter dataset for Observation_std
# ########################################################################
run = FALSE
# run = TRUE
if(run){
  
  # Observation_std
  Posterior_selection <- Posterior %>% 
    filter(.variable %in% c("observation_std[1]",
                            "observation_std[2]",
                            "observation_std[3]",
                            "observation_std[4]")) %>%
    mutate(dyad = factor(dyad, levels = all_dyads), cycle = factor(cycle, labels = all_cycles_numbers),
           condition = factor(condition, levels = all_conditions),
           .variable = factor(.variable))
  
  form_Observation_std00 = bf(.value ~ condition * .variable + ( 1 | dyad:cycle),
                sigma ~ condition * .variable + ( 1  | dyad:cycle))
  prior_Observation_std00 <- get_prior(form_Observation_std00,data = Posterior_selection,family = "normal")
  fit_Observation_std00 <- run_model_cmdstanr(Posterior_selection,form_Observation_std00,"normal",prior_Observation_std00)
  
  saveRDS(file = "Fitted/chapTappers2_fit_Observation_std00.rds",fit_Observation_std00)
}
run = FALSE 




# 
# # needed????
# # #########################################
# # 
# # modelchecking
# run = FALSE
# # run = TRUE
# if(run){
#   fit_K00 <- readRDS(file = "Fitted/chapTappers2_fit_K00.rds")
#   # fit_K0 <- readRDS(file = "Fitted/chapTappers2_fit_K0.rds")
#   #  fit_K01 <- readRDS(file = "Fitted/chapTappers2_fit_K01.rds")
#   fit_K02 <- readRDS(file = "Fitted/chapTappers2_fit_K02.rds")
#   #loo(fit_K00,fit_K0,fit_K01,fit_K02)
#   loo_out <- loo(fit_K00, fit_K02)
#   
#   
#   
#   
#   
#   # Entrainment 2
#   newdat <- dat_selected_K %>% data_grid(condition, dyad, names, cycle)
#   #pred <- epred_draws(fit_K02, newdata = newdat, allow_new_levels = TRUE)
#   pred <- dat_selected_K %>% droplevels() %>% select(condition,dyad,names, cycle) %>% 
#     add_epred_draws(fit_K02, re_formula = NULL) %>% ungroup() %>% group_by(condition,names,dyad, cycle) %>% median_qi(.epred )
#   
#   p2 <- 
#     ggplot(pred) + 
#     geom_point(aes(x=dyad,y=.epred, color = condition), 
#                position = position_jitterdodge(jitter.width = .5),
#                size = 2, shape = 1) +
#     scale_fill_manual(values=cbPalette) +
#     scale_colour_manual(values=cbPalette) +
#     theme_bw() + facet_grid(~names)
#   
#   
#   + facet_grid(~condition) + ylim(0,1)
#   
#   
#   pred <- dat_selected_K %>% droplevels() %>% select(condition,dyad,names, cycle) %>% 
#     add_epred_draws(fit_K02, re_formula = NULL) %>% ungroup() %>% group_by(condition,names,dyad, cycle) #%>% median_qi(.epred )
#   
#   
#   for(dy in all_dyads){
#     A <- pred %>% filter(dyad == dy, condition == "2PC", names == "ks2m2") #%>% 
#     #droplevels() %>% ungroup() %>% median_qi(.epred)
#     #hist(A$.epred)
#     B <- pred %>% filter(dyad == dy, condition == "1PU", names == "ks2m2")
#     #hist(B$.epred)
#     AB <-  cbind(A = A$.epred,B = B$.epred) %>% data.frame() %>% mutate(C = B-A) %>% median_qi()
#     print(paste(AB$A,AB$B,AB$C))
#   }
#   
#   
#   # dat_1PU <- dat_selected_Ks %>% filter(condition == "1PU")
#   # dat_2PC <- dat_selected_Ks %>% filter(condition == "2PC")
#   
#   
# }
# run = FALSE
