# source_chapListener_04_Modelling

# Defining and testing the model
run = FALSE
# run = TRUE
if(run){
  
  model_1 <- '
Evaluation =~ Q47 + Q48  + Q49
Immersion =~ Q33 + Q35 + Q31 
Embodiment =~ Q37 + Q39 + Q41
Emotion =~ Q43
Kind_of_experience =~ Immersion + Embodiment + Emotion
Kind_of_experience ~ Quality 
Evaluation ~ Kind_of_experience
Q1 ~ Evaluation + Q2'

  
  # model_1
  Data <- Data %>% mutate(Q57 = factor(Q57), ArousalValence_category = factor(ArousalValence_category)) %>% filter(!Q54 == "NA")
  # use ordered for model_6 with Qualities defined by Q6, Q12, Q14 # ordered = c("Q6", "Q12", "Q14"),
  semfit1_model_1 <- lavaan::sem(model_1, data = Data, group = "Q57",  meanstructure = TRUE) #se = "bootstrap", bootstrap = 1000)
  semfit2_model_1 <- lavaan::sem(model_1, data = Data, group = "Q57",  meanstructure = TRUE, group.equal = c("loadings")) #se = "bootstrap", bootstrap = 1000)
  semfit3_model_1 <- lavaan::sem(model_1, data = Data, group = "Q57", meanstructure = TRUE, group.equal = c("loadings", "intercepts")) #se = "bootstrap", bootstrap = 1000)
  summary_model_1 <- summary(semfit1_model_1, standardized = TRUE, rsquare = TRUE, ci = T)
  modelperformance_model_1 <- model_performance(semfit1_model_1,  verbose = TRUE, metrics = c( "Chi2", "AGFI", "CFI", "RMSEA", "SRMR", "AIC"))
  compareFit_model_1 <- compareFit(semfit1_model_1,semfit2_model_1,semfit3_model_1, nested = FALSE)
  Model_1_results <- list(semfit1_model_1,modelperformance_model_1, compareFit_model_1)
  save(file = "Fitted/chapListener_SEM_model_1_results.RData", Model_1_results)
  
}
run = FALSE

# Predictions
run = FALSE
# run = TRUE
if(run){
  # Prediction Bach
  Data_withoutBach <- Data %>% filter(! (Composer == "Bach, Johann Sebastian") )
  Data_onlyBach <- Data %>% filter((Composer == "Bach, Johann Sebastian") ) 
  semfit <- lavaan::sem(model_1, data = Data_withoutBach , meanstructure = TRUE) 
  Pred <- lavPredict(semfit, newdata = Data_onlyBach, type = "ov") %>% data.frame() %>% 
    rename(Q1_predict = Q1) %>% select(Q1_predict)
  PredDat <- Data_onlyBach %>%  rename(Q1_data = Q1, LD = Q57) %>% select(Q1_data, LD)
  Pred_Data <- cbind(Pred,PredDat)
  saveRDS(file = "Data/chapListener_PredictionsBach.rds", Pred_Data)
  
  # Prediction Spiegel
  Data_withoutSpiegel <- Data %>% filter(! (str_detect(Title,"Spiegel")) )
  Data_onlySpiegel <- Data %>% filter((str_detect(Title,"Spiegel"))  )
  semfit <- lavaan::sem(model_1, data = Data_withoutSpiegel, meanstructure = TRUE) # se = "bootstrap", bootstrap = 1000)
  Pred <- lavPredict(semfit, newdata = Data_onlySpiegel, type = "ov") %>% data.frame() %>%
    mutate(Q1_data = Data_onlySpiegel$Q1) %>% select(Q1) %>% rename(Q1_predict = Q1)
  PredDat <- Data_onlySpiegel %>% select(Q1, Q57) %>% rename(Q1_data = Q1, LD = Q57)
  Pred_Data <- cbind(Pred,PredDat)
  saveRDS(file = "Data/chapListener_PredictionsSpiegel.rds", Pred_Data)
  
  # Prediction Arousal Valence
  d_1 <- Data %>% filter(ArousalValence_category=="high_pos")
  d_2 <- Data %>% filter(ArousalValence_category=="high_neg")
  d_3 <- Data %>% filter(ArousalValence_category=="low_pos")
  d_4 <- Data %>% filter(ArousalValence_category=="low_neg")
  semfit <- lavaan::sem(model_1, data = Data ,  meanstructure = TRUE) #se = "bootstrap", bootstrap = 1000)
  Pred_1 <- lavPredict(semfit, newdata = d_1, type = "lv") %>% data.frame() %>% mutate(Code = factor("happy")) %>% select(Evaluation,Kind_of_experience,Code)
  Pred_2 <- lavPredict(semfit, newdata = d_2, type = "lv") %>% data.frame() %>% mutate(Code = factor("aggressive"))%>% select(Evaluation,Kind_of_experience,Code)
  Pred_3 <- lavPredict(semfit, newdata = d_3, type = "lv") %>% data.frame() %>% mutate(Code = factor("relaxing"))%>% select(Evaluation,Kind_of_experience,Code)
  Pred_4 <- lavPredict(semfit, newdata = d_4, type = "lv") %>% data.frame() %>% mutate(Code = factor("sad"))%>% select(Evaluation,Kind_of_experience,Code)
  Pred_long <- rbind(Pred_1, Pred_2, Pred_3, Pred_4) %>% pivot_longer(cols = -Code)
  saveRDS(file = "Data/chapListener_PredictionArousalValence.rds", Pred_long)
  
}
run = FALSE

