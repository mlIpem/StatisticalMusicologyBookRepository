# chapTappers2_06_Contrasts

run = FALSE
# run = TRUE
if(run){
  fit <- readRDS(file = "Fitted/chapTappers2_fit_K00.rds")
  Posterior_selected <- Posterior %>% 
    filter(.variable %in% c("ks1m1","ks2m2")) %>% droplevels() %>% 
    select(condition,dyad,cycle, .variable,.value)
  
  pred <- Posterior_selected %>% 
    add_epred_draws(fit, re_formula = NULL) %>% ungroup() %>% 
    group_by(condition,.variable) #%>% median_qi(.epred )
  

  do_contrast_test <- function(prediction, 
                               condi1 = all_conditions,
                               condi2 = all_conditions, 
                               dyad1 = all_dyads, 
                               names1 = all_namesk, 
                               names2 = all_namesk){
    # prediction = pred; condi1 = "U"; condi2 = "C"; dyad1 = all_dyads; names1 = all_namesk; names2 = all_namesk
    mess <- (paste("contrast of ",toString(condi1),"versus ", toString(condi2),"using ", toString(dyad1),"using ", toString(names1)))
    print(mess)
    d1 <- prediction %>% filter(condition %in% condi1, dyad %in% dyad1,.variable %in% names1) %>% ungroup()  %>% select(.epred) %>% slice_sample(n=500000)
    d2 <- prediction %>% filter(condition %in% condi2, dyad %in% dyad1,.variable %in% names2) %>% ungroup()  %>% select(.epred) %>% slice_sample(n=500000)
    # hist(d1$.epred);hist(d2$.epred)
    diff = d1$.epred-d2$.epred
    h <- hist(diff)
    print(h)
    mq <- median_qi(diff, width = .95) %>% select(y,ymin,ymax) %>% mutate(pd = mean(diff > 0)) %>% rename(d12 = y, d12min = ymin, d12max = ymax)
    d1q <- median_qi(d1$.epred, width = .95) %>% select(y) %>% rename(d1 = y)
    d2q <- median_qi(d2$.epred, width = .95) %>% select(y) %>% rename(d2 = y)
    ret = list(d1q,d2q,mq,h,mess)
    return(ret)
  }
  
  Contrast.Hypothesis1 <- list()
  Contrast.Hypothesis2 <- list()
  Contrast.Hypothesis1[[1]] <- do_contrast_test(pred, condi1 = c("U"), condi2 = c("C"))
  Contrast.Hypothesis1[[4]] <- do_contrast_test(pred, condi1 = c("U"),condi2 = c("U"), names1 = "ks1m1", names2 = "ks2m2")
 Contrast.Hypothesis1[[7]] <- do_contrast_test(pred, condi1 = c("C"),condi2 = c("C"), names1 = "ks1m1", names2 = "ks2m2")
  Contrast.h <- rbind(cbind(Contrast.Hypothesis1[[1]][[1]], Contrast.Hypothesis1[[1]][[2]],Contrast.Hypothesis1[[1]][[3]]),
                   cbind(Contrast.Hypothesis1[[4]][[1]], Contrast.Hypothesis1[[4]][[2]],Contrast.Hypothesis1[[4]][[3]]),
                   cbind(Contrast.Hypothesis1[[7]][[1]], Contrast.Hypothesis1[[7]][[2]],Contrast.Hypothesis1[[7]][[3]]))
  Contrast.H1 <- cbind(Hyp = c(
    "U > C",
    "U_ks1m1 > U_ks2m2",
    "C_ks1m1 > C_ks2m2"
  ), round(Contrast.h,2))
  
  saveRDS(file = "Data/chapTappers2_ContrastH1.rds", Contrast.H1)
  # Contrast.H1.kable <- kable(Contrast.H1, format = "pipe")
 # Contrast.H1.kable
}
run = FALSE








