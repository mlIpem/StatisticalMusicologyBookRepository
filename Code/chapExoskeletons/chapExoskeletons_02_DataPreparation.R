# chapExoskeletons_02_DataPreparation.R


Conditions <- readRDS(file = "Data/chapExoskeletons_Conditions.rds")
Dat <- readRDS(file = "Data/chapExoskeletons_Dat.rds")
Info <- readRDS(file = "Data/chapExoskeletons_Info.rds")
Vector <- readRDS(file = "Data/chapExoskeletons_Vector.rds")
fit_R <- readRDS(file = "Fitted/chapExoskeletons_fit_R.rds") 
fit_alpha <- readRDS(file = "Fitted/chapExoskeletons_fit_alpha.rds")

Vector_Condition1234 <- Vector %>% dplyr::filter(!(Condition %in% c("Pre","Post"))) %>% droplevels()
#Vector_Condition56 <- Vector %>% dplyr::filter(Condition %in% c("Pre","Post")) %>% droplevels()

run = FALSE
# run = TRUE
if(run){
  # Here is how we calculated the features 
  if(0){
    Info <- data.frame()
    R_alpha <- data.frame()
    count = 0
    
    for(placeholder in unique(Dat$Placeholder)){
      # placeholder = "D06_B3_T1_A"
      datnow <- Dat %>% dplyr::filter(Placeholder == placeholder) %>% droplevels()
      # placeholder = unique(datnow$Placeholder)
      if(!nrow(datnow)==0) { # needed to check whether subject performed the condition
        print(paste("---> Processing:: Placeholder =", placeholder) )
        count = count + 1
        ############################
        if(1){ # 1. relphase calculation on continuous signal
          c = as.numeric(cor(datnow$ss1,datnow$ss2))
          print(paste("1. correlation ss1 ss2",c))
          if (c < 0){ # if correlation is negative then reverse the signal with lowest value
            # this is only in daatnow and not in Dat !!!
            smallest = min(datnow$ss1[1],datnow$ss2[1])
            if(smallest == datnow$ss1[1]) {datnow$ss1 = -datnow$ss1}
            else{datnow$ss2 = -datnow$ss2}
          }
          c = as.numeric(cor(datnow$ss1,datnow$ss2))
          print(paste("2. correlation ss1 ss2",c))
          as1<- hilbert(datnow$ss1, 120)
          ps1 <- ifreq(as1, f=120,plot=FALSE)$p
          as2 <- hilbert(datnow$ss2, 120)
          ps2 <- ifreq(as2, f=120,plot=FALSE)$p
          # time <- ps1[,1]
          phi1 <- unwrap(ps1[,2])
          phi2 <- unwrap(ps2[,2])
          #   relphase <- (((phi1 - phi2) +pi) %% (2*pi)) -pi
          relphase <- phi1 - phi2
          V <- mean(exp(1i*relphase))
          R <- Mod(V)
          alpha <- Arg(V)
          abs_alpha <- abs(alpha)
          # Collect results
          r_alpha <- cbind(R, alpha, abs_alpha, c)
          R_alpha <- rbind(R_alpha,r_alpha)
          info <- cbind(count, placeholder, dyad = levels(datnow$Dyad), block = levels(datnow$Block), 
                        trial = levels(datnow$Trial),  condition = levels(datnow$Condition), 
                        tempo = levels(datnow$Tempo), expertise = levels(datnow$Expertise)) 
          Info <- rbind(Info, info)
        }
      } else {
        # print("       ---> datnow is empty - condition not performed")
      } # needed to check whether subject performed the condition
    }
    
    
  }
}
run = FALSE
