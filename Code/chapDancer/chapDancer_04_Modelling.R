# chapDancer_04_Modelling
# 

#D_I.F1 <- Data %>% filter(Fragment == "I.F1", Heel == "HeelsDown")
#D_F1 <- Data %>% filter(Fragment == "F1", Heel == "HeelsDown")
# t.test(D_I.F1$Phase1,D_F1$Phase1)


###########################
# Model1 for comparing fragments and participants among fragments (no smoothing over time)
# Be aware of the time-consuming calculation !!!

run = FALSE
# run = TRUE
if(run){
  # model_1
  form <- bf(Phase1 ~ 1 + Fragment * Participant * Heel + (1| Trialf),
                    kappa ~ 1 + Fragment * Participant * Heel + (1 | Trialf))
  fam = von_mises()
  priors <- get_prior(formula = form, family = fam, data = Data)
  fit <- run_model_cmdstanr(Data,form,fam,priors)
  saveRDS(file = "Fitted/chapDancer_Model1.rds", fit)
}
run = FALSE

###########################
# Model3d. Smooth regression, now with time included
run = FALSE
# run = TRUE
if(run){
  # model_3d (very computation intensive: only run on a server)
  model3d_form <- bf(Phase1 ~ 1 + interaction(Heel,Fragment) + 
                       s(time0, by = interaction(Heel,Fragment)) +
                       (1  |  Participant/Trialf ),
                     kappa ~ 1 + interaction(Heel,Fragment) + 
                       s(time0, by = interaction(Heel,Fragment)) +
                       (1  |  Participant/Trialf )
  )
  fit <- run_model_cmdstanr(LL_data_3,model3d_form,"von_mises",priors_sd)
  saveRDS(file="Fitted/chapDancer_Model3d.rds",fit) # model3d
}
run = FALSE


