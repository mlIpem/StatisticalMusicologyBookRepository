# chapTappers2_05_DataPreparation_Posterior
# # when data have been processed by state-space modelling
# we work with the posterior 
# This posterior has been either calculated in 04 or read from a pre-calculated file
# ##################################################################################################
#run = FALSE
run = TRUE
if(run){
  # ##################################################################################################
  # Get the Posterior of all models : Posterior, 

  fn = paste("Data/chapTappers2_Posterior.rds",sep="")
  Dummy <- readRDS(file = fn) # get posterior
  # expand id and get factors right. It saves work for later
  Posterior <- Dummy %>%
    separate(id, into = c("condition", "dyad", "cycle"), sep = "\\.") %>%
    mutate(dyad = factor(dyad, levels = all_dyads), cycle = factor(cycle, labels = all_cycles_numbers),
           condition = factor(condition, levels = all_conditions),
           codycy = Dummy$id)
  #fn = paste("Data/chapTappers2_PosteriorRhat.rds",sep="")
  #PosteriorRhat = readRDS(file = fn) # get: Posterior;   # it contains Rhats of all models

  
  ##################################################################################################
  # Here we select the state-space models whose Rhat < 1.05
  # because the models that have > 1.05 are not really reliable
  ##################################################################################################
  # To be checked
 #  dat <-  merge(Fit_tap_all_post_summary,Fit_Rhat, by = c("condition", "dyad", "cycle", "names"))
#  dat_selected_Rhat_clean <- dat %>% filter(Rhat < 1.05) 
#  saveRDS(file = "Data/chapTappers2_dat_selected_Rhat_clean.rds",dat_selected_Rhat_clean)
  
  ##################################################################################################
  # Here we use dat_selected_Rhat_clean to prepare the selected data properly for further usage
  ##################################################################################################
  
 
}
run = FALSE
