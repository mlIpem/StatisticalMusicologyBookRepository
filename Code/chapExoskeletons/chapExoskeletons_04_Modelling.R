#chapExoskeletons_04_Modelling.R

run = FALSE
# run = TRUE
if(run){
  # ################################################################
  # Continuous relative phase: R 
  form = bf(R ~ Condition*Tempo + (1|Expertise:Dyad:Block), 
            sigma ~ Condition*Tempo + (1|Expertise:Dyad:Block)
            # phi ~ Condition*Tempo + (1|Expertise:Dyad:Block)
  )
  
  fam = "skew_normal"
  # fam = "beta"
  priors <- get_prior(form,Vector,fam)
  fit <- run_model_cmdstanr(Vector, form, fam, priors)
  
  fit_R <- fit 
  saveRDS(file = "Fitted/chapExoskeletons_fit_R.rds",fit_R)
  
  # ################################################################
  # Continuous relative phase: alpha
   form = bf(alpha ~ Condition* Tempo + (1|Expertise:Dyad:Block), 
            sigma ~  Condition* Tempo + (1|Expertise:Dyad:Block))
  fam = "gaussian"
  priors <- get_prior(form,Vector,fam)
  fit <- run_model_cmdstanr(Vector, form, fam, priors)
  plot(conditional_effects(fit), ask=FALSE, points=TRUE, point_args = list(width=.1,alpha=.5,shape=1,size=2))
  fit_alpha <- fit 
  saveRDS(file = "Fitted/chapExoskeletons_fit_alpha.rds",fit_alpha)
  
  # Continuous relative phase: log_abs_alpha
  form = bf(log_abs_alpha ~ Condition* Tempo + (1|Expertise:Dyad:Block), 
            sigma ~  Condition* Tempo + (1|Expertise:Dyad:Block))
  fam = "skew_normal"
  #fam = "beta"
  priors <- get_prior(form,Vector,fam)
  fit <- run_model_cmdstanr(Vector, form, fam, priors)
  plot(conditional_effects(fit), ask=FALSE, points=TRUE, point_args = list(width=.1,alpha=.5,shape=1,size=2))
  fit_log_abs_alpha <- fit 
  saveRDS(file = "Fitted/chapExoskeletons_fit_log_abs_alpha.rds",fit_log_abs_alpha)
  
}
run = FALSE
