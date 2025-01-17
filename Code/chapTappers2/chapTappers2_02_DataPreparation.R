# chapTappers2_02_DataPreparation.R
# 

# ##################################################################################################
# Useful definitions of variables
all_dyads1 = c("dyad_1","dyad_2","dyad_3","dyad_4","dyad_5","dyad_6","dyad_8","dyad_9","dyad_10")
all_dyads2 = c("dyad_11","dyad_12","dyad_13","dyad_14","dyad_15","dyad_16","dyad_17","dyad_18","dyad_19","dyad_20")
all_dyads = c(all_dyads1,all_dyads2)
all_cycles = c("cycle_1","cycle_2","cycle_3","cycle_4","cycle_5","cycle_6","cycle_7","cycle_8","cycle_9","cycle_10")
all_cycles_numbers = as.character(seq(1:10))
all_dyads_numbers = as.character(c(seq(1:6),seq(8,20)))
all_conditions = c("U","C")
all_namesk = c("ks1m1","ks2m2")
all_namesd = c("ds1m1","ds2m2","ds1s2","ds2s1")
all_nameso = c("observation_std[1]",
               "observation_std[2]",
               "observation_std[3]",
               "observation_std[4]")

# useful definitions
filenameFitted = paste("Fitted/chapTappers2_")
filenameFigures = paste("Figures/chapTappers2_")
# ##################################################################################################
# Load data and bind them together in DTapping
load(paste("Data/chapTappers2_DTapping_1PU.rda",sep=""))
DTapping_1PU <- DTapping %>% mutate(condition = "U")
load(paste("Data/chapTappers2_DTapping_2PC.rda",sep=""))
DTapping_2PC <- DTapping %>% mutate(condition = "C")
DTapping <- rbind(DTapping_1PU,DTapping_2PC) %>% 
  mutate(codycy = interaction(condition,dyad,cycle)) %>% 
  dplyr::select(time, time0c, names_indicator, condition, dyad, cycle, codycy, phase)

# end raw data
################################@



# ################################################################################################
## calculate Relphase = Extract from the DTapping data 
## extract the phase relative to the metronomes in DTapping_RelativePhase 
# ################################################################################################
run = FALSE
#run = TRUE
if(run){
  DTapping_RelativePhase <- data.frame()
  for (ucodycy in unique(DTapping$codycy)){
    # ucodycy = "C.dyad_11.cycle_1"
    print(paste("processing",ucodycy))
    Dummy <- DTapping %>% filter(codycy == ucodycy) 
    if( length(w<-which(Dummy$time==0)) > 0) {
      Dummy <- Dummy[-w,]
      print("---> time = 0 is deleted: check")}
    m1 <- Dummy %>% filter(names_indicator == 1) 
    s1 <- Dummy %>% filter(names_indicator == 2) 
    m2 <- Dummy %>% filter(names_indicator == 3)
    s2 <- Dummy %>% filter(names_indicator == 4) 
    C1 <- do_calc_phase2(s1,m1)
    C2 <- do_calc_phase2(s2,m2)
    C <- rbind(C1,C2) %>% 
      dplyr::select(time, time0c, names_indicator,condition, dyad, cycle, codycy, relphase)
    DTapping_RelativePhase <- rbind(DTapping_RelativePhase,C) 
  }
  # make sure that we have factors
  DTapping_RelativePhase <- DTapping_RelativePhase %>% 
    mutate(dyad = factor(dyad,levels = all_dyads),
           cycle = factor(cycle,levels = all_cycles),
           condition = factor(condition,levels = all_conditions)) %>% 
    droplevels()
  saveRDS(file = "Data/chapTappers2_DTapping_RelativePhase.rds", DTapping_RelativePhase)
}
run = FALSE

DTapping_RelativePhase <- readRDS(file = "Data/chapTappers2_DTapping_RelativePhase.rds")

