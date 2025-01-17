# chapViolinist_06_Contrasts.R

# ################################################################# 
# Calculate the 2D-3D contrasts per Block
# This is based on the processing done in 05 !!!
run = FALSE
# run = TRUE
if(run){
  Block_contrasts_PostPred <- list()
  Block_contrasts_Table <- data.frame()
  count = 0
  # p = 1
  for(p in 1:4){
  band_start = Band_start[[p]]
  band_end = Band_end[[p]]
  len = length(band_start) - 1
  print(paste("------> Processing ", Pieces[[p]]))
  fn = paste("Fitted/chapViolinist_model_", Pieces[[p]],".rds",sep="")
  fit <- readRDS(fn) #load fit

  for(l in 1:len){
    # l = 1
    count = count + 1
    print(paste("piece=",p,"len=",l,"band_end",band_end[l],"band_start",band_start[l+1]))
    # 1. make data (not including PD) with a new grid for time
    Dat <- NewData %>% filter(piece == Pieces[[p]], !subject == "P006") %>% droplevels() %>% 
      group_by(condition, trial) %>% 
      data_grid(time = seq(band_end[l],band_start[l+1],by=.5))
    # 2. generate the logPD data
    PostPred <- epred_draws(fit, Dat, ndraws = 1000, scale = "response", re_formula = NA) # %>% 
    #group_by(time,condition,trial,subject) %>% median_qi(.epred)
    # 3. split condition to contrast them
    PostPred_wide <- PostPred %>% dplyr::select(condition, .epred) %>%  group_by(condition) %>% 
      mutate(rn = row_number()) %>%
      pivot_wider(id_cols = c(rn),
                  names_from = condition, values_from = .epred) %>%
      rename(D2 = `2D` , D3 = `3D`) %>%  mutate(Diff = D2 - D3 ) 
    # 4. do_contrast
    # 
    # info <- paste(Pieces[[p]],"sec",band_end[l],"-",band_start[l+1],"\n pd>0=",ss)
    
    PostPred_2D <- PostPred %>% ungroup()%>% filter(condition == "2D")  
    PostPred_3D <- PostPred %>% ungroup()%>% filter(condition == "3D")
    contrast <- do_contrast(PostPred_2D,PostPred_3D) %>% 
      mutate(pd = probability_of_direction,
             label = paste(Pieces[[p]],"[",band_end[l],",",band_start[l+1],"]",sep="")) %>%
      dplyr::select(label, y, ymin, ymax, pd)
    
    Block_contrasts_Table <-rbind(Block_contrasts_Table,contrast)
  }
  } #p
  saveRDS(file = "Data/chapViolinist_Block_contrasts_Table.rds", Block_contrasts_Table)
  }
run = FALSE

