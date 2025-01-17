# chapExoskeletons_06_Contrasts.R

fit_R <- readRDS(file = "Fitted/chapExoskeletons_fit_R.rds") 
fit_alpha <- readRDS(file = "Fitted/chapExoskeletons_fit_alpha.rds")


# ##############################################
# 1. Contrast fixed effects: fit_R en fit_alpha
run = FALSE
# run = TRUE
if(run){
fit <- fit_R
#em <- emmeans(fit, ~ Condition, by = "Tempo" )
#pairs(em)#,simple = "each", combine = TRUE)


post <- epred_draws(fit, newdata = Vector_Condition1234, re_formula = NA) %>% data.frame() %>% droplevels()
Contrast <- data.frame()
for(tempo in unique(post$Tempo)){
  # tempo = "72BPM"
  # for(condition in unique(post$Condition)){
  # condition = "A"
  condi.A <- post %>% dplyr::filter(Condition == "A", Tempo == tempo)
  condi.AE <- post %>% dplyr::filter(Condition == "AE", Tempo == tempo)
  condi.AV <- post %>% dplyr::filter(Condition == "AV", Tempo == tempo)
  condi.AVE <- post %>% dplyr::filter(Condition == "AVE", Tempo == tempo)
  
  condi.AAV <- post %>% dplyr::filter(Condition %in% c("A","AV"), Tempo == tempo)
  condi.AEAVE <- post %>% dplyr::filter(Condition %in% c("AE","AVE"), Tempo == tempo)
  
  result.AAV.AEAVE <- do_contrast(condi.AAV,condi.AEAVE) %>% mutate(label = paste("AAV-AEAVE_",tempo))
  
  result.A.AE <- do_contrast(condi.A,condi.AE) %>% mutate(label = paste("A-AE_",tempo))
  result.A.AV <- do_contrast(condi.A,condi.AV) %>% mutate(label = paste("A-AV_",tempo))
  result.A.AVE <- do_contrast(condi.A,condi.AVE) %>% mutate(label = paste("A-AVE_",tempo))
  result.AE.AV <- do_contrast(condi.AE,condi.AV) %>% mutate(label = paste("AE-AV_",tempo))
  result.AE.AVE <- do_contrast(condi.AE,condi.AVE) %>% mutate(label = paste("AE-AVE_",tempo))
  result.AV.AVE <- do_contrast(condi.AV,condi.AVE) %>% mutate(label = paste("AV-AVE_",tempo))
  
  result <- rbind(result.AAV.AEAVE,
                  result.A.AE,
                  result.A.AV,
                  result.A.AVE,
                  result.AE.AV,
                  result.AE.AVE,
                  result.AV.AVE) 
  
  Contrast <- rbind(Contrast,result)
}


condi.72 <- post %>% dplyr::filter(Tempo == "72BPM")
condi.100 <- post %>% dplyr::filter(Tempo == "100BPM")
# hist(condi.72$.epred)
# hist(condi.100$.epred)
result.72.100 <- do_contrast(condi.72,condi.100) %>% mutate(label = paste("72BPM-100BPM"))

Contrast <- rbind(Contrast,result.72.100)
Contrast_fe_R <- Contrast %>% rename(`pd(%)` = probability_of_direction) %>% select(label,y,ymin,ymax,`pd(%)`)
#Contrast_population <- knitr::kable(Contrast_fe_R, format = "rst")

saveRDS(file = "Data/chapExoskeletons_Contrast_fe_R.rds", Contrast_fe_R)
knitr::kable(Contrast_fe_R, format = "pipe")


#####################@
#####################
fit <- fit_alpha
#em <- emmeans(fit, ~ Condition, by = "Tempo" )
#pairs(em)#,simple = "each", combine = TRUE)


post <- epred_draws(fit, newdata = Vector_Condition1234, re_formula = NA) %>% data.frame()
Contrast <- data.frame()
for(tempo in unique(post$Tempo)){
  # tempo = "72BPM"
  for(condition in unique(post$Condition)){
    # condition = "A"
    condi.A <- post %>% dplyr::filter(Condition == "A", Tempo == tempo)
    condi.AE <- post %>% dplyr::filter(Condition == "AE", Tempo == tempo)
    condi.AV <- post %>% dplyr::filter(Condition == "AV", Tempo == tempo)
    condi.AVE <- post %>% dplyr::filter(Condition == "AVE", Tempo == tempo)
    
    condi.AAV <- post %>% dplyr::filter(Condition %in% c("A","AV"), Tempo == tempo)
    condi.AEAVE <- post %>% dplyr::filter(Condition %in% c("AE","AVE"), Tempo == tempo)
    
    result.AAV.AEAVE <- do_contrast(condi.AAV,condi.AEAVE) %>% mutate(label = paste("AAV-AEAVE_",tempo))
    
    result.A.AE <- do_contrast(condi.A,condi.AE) %>% mutate(label = paste("A-AE_",tempo))
    result.A.AV <- do_contrast(condi.A,condi.AV) %>% mutate(label = paste("A-AV_",tempo))
    result.A.AVE <- do_contrast(condi.A,condi.AVE) %>% mutate(label = paste("A-AVE_",tempo))
    result.AE.AV <- do_contrast(condi.AE,condi.AV) %>% mutate(label = paste("AE-AV_",tempo))
    result.AE.AVE <- do_contrast(condi.AE,condi.AVE) %>% mutate(label = paste("AE-AVE_",tempo))
    result.AV.AVE <- do_contrast(condi.AV,condi.AVE) %>% mutate(label = paste("AV-AVE_",tempo))
    
    result <- rbind(result.AAV.AEAVE,
                    result.A.AE,
                    result.A.AV,
                    result.A.AVE,
                    result.AE.AV,
                    result.AE.AVE,
                    result.AV.AVE) 
  }
  Contrast <- rbind(Contrast,result)
}


condi.72 <- post %>% dplyr::filter(Tempo == "72BPM")
condi.100 <- post %>% dplyr::filter(Tempo == "100BPM")
# hist(condi.72$.epred)
# hist(condi.100$.epred)
result.72.100 <- do_contrast(condi.72,condi.100) %>% mutate(label = paste("72BPM-100BPM"))

Contrast <- rbind(Contrast,result.72.100)
Contrast_fe_a <- Contrast %>% rename(`pd(%)` = probability_of_direction) %>% select(label,y,ymin,ymax,`pd(%)`)
#Contrast_population <- knitr::kable(Contrast_fe_alpha, format = "latex")

saveRDS(file = "Data/chapExoskeletons_Contrast_fe_a.rds", Contrast_fe_a)
knitr::kable(Contrast_fe_a, format = "pipe")

}
run = FALSE

# ##############################################
# 2. explore group effects: Expertise fit_R and fit_alpha
run = FALSE
# run = TRUE
if(run){
fit <- fit_R

r1 <- fit %>% 
  gather_draws(`r_Expertise:Dyad:Block.*1,Intercept]` , regex=TRUE) %>% ungroup() %>% 
  select(.value) %>% mutate(name = "E1", rn = row_number()) %>% slice_sample(n = 5000)
r2 <- fit %>% 
  gather_draws(`r_Expertise:Dyad:Block.*2,Intercept]` , regex=TRUE) %>% ungroup() %>% 
  select(.value) %>% mutate(name = "E2", rn = row_number()) %>% slice_sample(n = 5000)
r3 <- fit %>% 
  gather_draws(`r_Expertise:Dyad:Block.*3,Intercept]` , regex=TRUE) %>% ungroup() %>% 
  select(.value) %>% mutate(name = "E3", rn = row_number() )%>% slice_sample(n = 5000)
R <- rbind(r1,r2,r3) %>% ungroup() %>% data.frame() %>% mutate(name = factor(name))

# calculations of contrasts
result.r1.r2 <- do_contrast2(r1,r2) %>% mutate(label = "Amateur-Prof")
result.r1.r3 <- do_contrast2(r1,r3) %>% mutate(label = "Amateur-Semiprof")
result.r2.r3 <- do_contrast2(r2,r3) %>% mutate(label = "Prof-Semiprof")

Contrast_r <- rbind(result.r1.r2, result.r1.r3, result.r2.r3)
Contrast_re_Expertise_R <- Contrast_r %>% rename(`pd(%)` = probability_of_direction) %>% select(label,y,ymin,ymax,`pd(%)`)

saveRDS(file = "Data/chapExoskeletons_Contrast_re_Expertise_R.rds", Contrast_re_Expertise_R)
knitr::kable(Contrast_re_Expertise_R, format = "pipe")


fit <- fit_alpha

r1 <- fit %>% 
  gather_draws(`r_Expertise:Dyad:Block.*1,Intercept]` , regex=TRUE) %>% ungroup() %>% 
  select(.value) %>% mutate(name = "E1", rn = row_number()) %>% slice_sample(n = 5000)
r2 <- fit %>% 
  gather_draws(`r_Expertise:Dyad:Block.*2,Intercept]` , regex=TRUE) %>% ungroup() %>% 
  select(.value) %>% mutate(name = "E2", rn = row_number()) %>% slice_sample(n = 5000)
r3 <- fit %>% 
  gather_draws(`r_Expertise:Dyad:Block.*3,Intercept]` , regex=TRUE) %>% ungroup() %>% 
  select(.value) %>% mutate(name = "E3", rn = row_number() )%>% slice_sample(n = 5000)
R <- rbind(r1,r2,r3) %>% ungroup() %>% data.frame() %>% mutate(name = factor(name))

# calculations of contrasts
result.r1.r2 <- do_contrast2(r1,r2) %>% mutate(label = "Amateur-Prof")
result.r1.r3 <- do_contrast2(r1,r3) %>% mutate(label = "Amateur-Semiprof")
result.r2.r3 <- do_contrast2(r2,r3) %>% mutate(label = "Prof-Semiprof")

Contrast_r <- rbind(result.r1.r2, result.r1.r3, result.r2.r3)
Contrast_re_Expertise_a <- Contrast_r %>% rename(`pd(%)` = probability_of_direction) %>% select(label,y,ymin,ymax,`pd(%)`)
#Contrast_r_Expertise_Expertise_alpha <- knitr::kable(Contrast_r_Expertise_alpha, format = "latex")

saveRDS(file = "Data/chapExoskeletons_Contrast_re_Expertise_a.rds", Contrast_re_Expertise_a)
knitr::kable(Contrast_re_Expertise_a, format = "pipe")

}
run = FALSE

# ##############################################
# 3. Explore group effects: Block fit_R and fit_alpha
run = FALSE
# run = TRUE
if(run){
  fit <- fit_R
  r1 <- fit %>% 
    gather_draws(`r_Expertise:Dyad:Block\\[.*B1,Intercept]` , regex=TRUE) %>%
    select(.value) %>% mutate(name = "B1") %>% slice_sample(n = 5000)
  r2 <- fit %>% 
    gather_draws(`r_Expertise:Dyad:Block\\[.*B2,Intercept]` , regex=TRUE) %>%
    select(.value) %>% mutate(name = "B2") %>% slice_sample(n = 5000)
  r3 <- fit %>% 
    gather_draws(`r_Expertise:Dyad:Block\\[.*B3,Intercept]` , regex=TRUE) %>%
    select(.value) %>% mutate(name = "B3") %>% slice_sample(n = 5000)
  r4 <- fit %>% 
    gather_draws(`r_Expertise:Dyad:Block\\[.*B4,Intercept]` , regex=TRUE) %>%
    select(.value) %>% mutate(name = "B4") %>% slice_sample(n = 5000)
  r5 <- fit %>% 
    gather_draws(`r_Expertise:Dyad:Block\\[.*B5,Intercept]` , regex=TRUE) %>%
    select(.value) %>% mutate(name = "B5") %>% slice_sample(n = 5000)
  r6 <- fit %>% 
    gather_draws(`r_Expertise:Dyad:Block\\[.*B6,Intercept]` , regex=TRUE) %>%
    select(.value) %>% mutate(name = "B6") %>% slice_sample(n = 5000)
  R <- rbind(r1,r2,r3,r4,r5,r6) %>% ungroup()
  
  
  # calculations of contrasts
  result.r1.r2 <- do_contrast2(r1,r2) %>% mutate(label = "B1-B2")
  result.r1.r3 <- do_contrast2(r1,r3) %>% mutate(label = "B1-B3")
  result.r1.r4 <- do_contrast2(r1,r4) %>% mutate(label = "B1-B4")
  result.r1.r5 <- do_contrast2(r1,r4) %>% mutate(label = "B1-B5")
  result.r1.r6 <- do_contrast2(r1,r4) %>% mutate(label = "B1-B6")
  
  Contrast_r <- rbind(result.r1.r2, result.r1.r3, result.r1.r4, result.r1.r5,result.r1.r6)
  Contrast_re_Block_R <- Contrast_r %>% rename(`pd(%)` = probability_of_direction) %>% select(label,y,ymin,ymax,`pd(%)`)
  #Contrast_r_Block_Expertise <- knitr::kable(Contrast_r_Block_R, format = "latex")
  
  saveRDS(file = "Data/chapExoskeletons_Contrast_re_Block_R.rds", Contrast_re_Block_R)
  knitr::kable(Contrast_re_Block_R, format = "pipe")
  
  
  fit <- fit_alpha
  r1 <- fit %>% 
    gather_draws(`r_Expertise:Dyad:Block\\[.*B1,Intercept]` , regex=TRUE) %>%
    select(.value) %>% mutate(name = "B1") %>% slice_sample(n = 5000)
  r2 <- fit %>% 
    gather_draws(`r_Expertise:Dyad:Block\\[.*B2,Intercept]` , regex=TRUE) %>%
    select(.value) %>% mutate(name = "B2") %>% slice_sample(n = 5000)
  r3 <- fit %>% 
    gather_draws(`r_Expertise:Dyad:Block\\[.*B3,Intercept]` , regex=TRUE) %>%
    select(.value) %>% mutate(name = "B3") %>% slice_sample(n = 5000)
  r4 <- fit %>% 
    gather_draws(`r_Expertise:Dyad:Block\\[.*B4,Intercept]` , regex=TRUE) %>%
    select(.value) %>% mutate(name = "B4") %>% slice_sample(n = 5000)
  r5 <- fit %>% 
    gather_draws(`r_Expertise:Dyad:Block\\[.*B5,Intercept]` , regex=TRUE) %>%
    select(.value) %>% mutate(name = "B5") %>% slice_sample(n = 5000)
  r6 <- fit %>% 
    gather_draws(`r_Expertise:Dyad:Block\\[.*B6,Intercept]` , regex=TRUE) %>%
    select(.value) %>% mutate(name = "B6") %>% slice_sample(n = 5000)
  R <- rbind(r1,r2,r3,r4,r5,r6) %>% ungroup()
  
  
  # calculations of contrasts
  result.r1.r2 <- do_contrast2(r1,r2) %>% mutate(label = "B1-B2")
  result.r1.r3 <- do_contrast2(r1,r3) %>% mutate(label = "B1-B3")
  result.r1.r4 <- do_contrast2(r1,r4) %>% mutate(label = "B1-B4")
  result.r1.r5 <- do_contrast2(r1,r4) %>% mutate(label = "B1-B5")
  result.r1.r6 <- do_contrast2(r1,r4) %>% mutate(label = "B1-B6")
  
  Contrast_r <- rbind(result.r1.r2, result.r1.r3, result.r1.r4, result.r1.r5,result.r1.r6)
  Contrast_re_Block_a <- Contrast_r %>% rename(`pd(%)` = probability_of_direction) %>% select(label,y,ymin,ymax,`pd(%)`)
  #Contrast_r_Block_Expertise <- knitr::kable(Contrast_r_Block_alpha, format = "latex")
  
  
  saveRDS(file = "Data/chapExoskeletons_Contrast_re_Block_a.rds", Contrast_re_Block_a)
  knitr::kable(Contrast_re_Block_a, format = "pipe")
}
run = FALSE

# ##############################################
# 4. Calculate effect size

run = FALSE
# run = TRUE
if(run){
  post <- epred_draws(fit, newdata = Vector, re_formula = NA) %>% data.frame()
  Effectsize <- data.frame()
  for(tempo in unique(post$Tempo)){
    # tempo = "72BPM"
    for(condition in unique(post$Condition)){
      # condition = "A"
      condi.A <- post %>% dplyr::filter(Condition == "A", Tempo == tempo)
      condi.AE <- post %>% dplyr::filter(Condition == "AE", Tempo == tempo)
      condi.AV <- post %>% dplyr::filter(Condition == "AV", Tempo == tempo)
      condi.AVE <- post %>% dplyr::filter(Condition == "AVE", Tempo == tempo)
      
      condi.AAV <- post %>% dplyr::filter(Condition %in% c("A","AV"), Tempo == tempo)
      condi.AEAVE <- post %>% dplyr::filter(Condition %in% c("AE","AVE"), Tempo == tempo)
      
      result.AAV.AEAVE <- do_effectsize(condi.AAV,condi.AEAVE) %>% mutate(label = paste("AAV-AEAVE_",tempo))
      
      result.A.AE <- do_effectsize(condi.A,condi.AE) %>% mutate(label = paste("A-AE_",tempo))
      result.A.AV <- do_effectsize(condi.A,condi.AV) %>% mutate(label = paste("A-AV_",tempo))
      result.A.AVE <- do_effectsize(condi.A,condi.AVE) %>% mutate(label = paste("A-AVE_",tempo))
      result.AE.AV <- do_effectsize(condi.AE,condi.AV) %>% mutate(label = paste("AE-AV_",tempo))
      result.AE.AVE <- do_effectsize(condi.AE,condi.AVE) %>% mutate(label = paste("AE-AVE_",tempo))
      result.AV.AVE <- do_effectsize(condi.AV,condi.AVE) %>% mutate(label = paste("AV-AVE_",tempo))
      
      result <- rbind(result.AAV.AEAVE,
                      result.A.AE,
                      result.A.AV,
                      result.A.AVE,
                      result.AE.AV,
                      result.AE.AVE,
                      result.AV.AVE) 
    }
    Effectsize <- rbind(Effectsize,result)
  }
  
  
  condi.72 <- post %>% dplyr::filter(Tempo == "72BPM")
  condi.100 <- post %>% dplyr::filter(Tempo == "100BPM")
  hist(condi.72$.epred)
  hist(condi.100$.epred)
  result.72.100 <- do_effectsize(condi.72,condi.100) %>% mutate(label = paste("72BPM-100BPM"))
  
  Effectsize <- rbind(Effectsize,result.72.100)
  names(Effectsize) <- c("d","label")
  
  knitr::kable(Effectsize, format = "latex")
}
run = FALSE
