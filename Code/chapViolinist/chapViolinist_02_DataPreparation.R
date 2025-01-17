# chapViolinist_02_DataPreparation

# Data read in:

load(file = "Data/chapViolinist_Data.RData") # Get Data
load(file = "Data/chapViolinist_NewData.RData") # Get NewData
DatStudent <- readRDS(file = "Data/chapViolinist_DatStudent.rds")
DatTeacher <- readRDS(file = "Data/chapViolinist_DatTeacher.rds")
Vector <- readRDS(file = "Data/chapViolinist_Vector.rds")
Res <- readRDS(file = "Data/chapViolinist_Res.rds")
Block_contrasts_PostPred <- readRDS("Data/chapViolinist_Block_contrasts_PostPred.rds")
Block_contrasts_Table <- readRDS("Data/chapViolinist_Block_contrasts_Table.rds") 


#########################################################
# 1. NewData = bowing gesture events (Adriaan)
#########################################################
#########################################################
# This dataset comes from a study on violin playing
# Data have been published in Campo et al. 2023

#load(file = "Data/chapViolinist_NewData.RData") # Get NewData

# F1 regions and vertical line position
F1_band_start = c(0,   38.7, 88,   99,  145, 180.5 )
F1_band_end   = c(3.6, 53,   97.3,   127,   165,    200 )
F1_vline_full = c(38.7, 88,   99,  145, 180.5, 3.6, 53,   97.3,   127,   165 )
# F2
F2_band_start = c(0, 33.5, 93,  131, 180 )
F2_band_end   = c(23, 45,  110 , 145.7,  200)
F2_vline_full = c(33.5, 93,  131, 180,23, 45,  110 , 145.7)
# F3
F3_band_start = c(0,    32.5,   68.5, 133, 156, 188.5)
F3_band_end   = c(14.5, 39 ,    110, 145, 183, 200)
F3_vline_full = c(32.5,   68.5, 133, 156, 188.5,14.5, 39 ,    110, 145, 183)
# F4
F4_band_start = c(0, 28, 96.6, 179 )
F4_band_end   = c(20, 73, 140, 200)
F4_vline_full = c(28, 96.6, 179, 20, 73, 140)
# Make lists
Vline <- list(F1_vline_full,F2_vline_full,F3_vline_full,F4_vline_full)
Band_start <- list(F1_band_start,F2_band_start,F3_band_start,F4_band_start)
Band_end <- list(F1_band_end,F2_band_end,F3_band_end,F4_band_end)

Pieces <- list("F1","F2","F3","F4")
Subjects <- as.list(as.character(unique(NewData$subject)))
Condition <- list("2D","3D")

#########################################
#########################################
# Author's script to generate data
# This can be ignored by the reader
# Generate NewData only when needed
run = FALSE
# run = TRUE
if(run){
  # Set the working directory to the directory containing the CSV files
  pathname <- "./Data/AnalyzedData"
  csv_files <- list.files(path = pathname, pattern = "\\.csv$")
  D <- data.frame()
  for (file in csv_files) {
    # file <- csv_files[[1]]
    fn <- file.path(pathname, file)
    S <- str_split(file,"_")
    # P011_T4_3D_F3_AnalyzedData.csv
    data <- read.csv(fn, stringsAsFactors = FALSE) %>% data.frame() %>% 
      mutate(subject = S[[1]][1], trial = S[[1]][2], condition = S[[1]][3], piece = S[[1]][4])
    D <- rbind(D,data)
  }
  Data <- D
  save(file = "./Data/chapViolinist_Data.RData",Data)
  
  NewData <- Data %>% 
    mutate(startIndex = startIndex/120,
           endIndex = endIndex/120,
           time = map2(startIndex, endIndex, seq, by = .3)) %>%
    unnest(cols = time) %>% 
    arrange(time) %>% 
    mutate(scp_id = factor(interaction(subject,condition,piece)) ,
           trial = factor(trial),
           subject = factor(subject),
           condition = factor(condition),
           logPD = scale(log(PD)),
           PDi = as.integer(PD * 1000)) %>% 
    filter(! subject == "P006") 
  save(file = "./Data/chapViolinist_NewData.RData",NewData)
}
run = FALSE
###############################
# make sure we have all data
# load(file = "Data/chapViolinist_Data.RData") # Get Data
# load(file = "Data/chapViolinist_NewData.RData") # Get NewData

####################################
# AUDIO processing
run = FALSE
# run = TRUE
if(run){
  #path of file
  file_audio_path <- "Data/Avatar Data/First_Violin_F1_Audio.wav"
  audio = readWave(file_audio_path)
  aud <- mono(audio, "both") # downsample to 11025 samples/sec.:
  aud11 <- downsample(aud, 2000) 
  waveform <- as.vector(aud11@left)
  time <- seq(0, duration(aud11), length.out = length(waveform))
  Audio_data <- data.frame(time = time, amplitude = waveform)
  save(file = "./Data/chapViolinist_AudioF1.RData",Audio_data)
  
  
  file_audio_path <- "Data/Avatar Data/First_Violin_F2_Audio.wav"
  audio = readWave(file_audio_path)
  aud <- mono(audio, "both") # downsample to 11025 samples/sec.:
  aud11 <- downsample(aud, 2000) 
  waveform <- as.vector(aud11@left)
  time <- seq(0, duration(aud11), length.out = length(waveform))
  Audio_data <- data.frame(time = time, amplitude = waveform)
  save(file = "./Data/chapViolinist_AudioF2.RData",Audio_data)
  
  
  file_audio_path <- "Data/Avatar Data/Second_Violin_F3_Audio.wav"
  audio = readWave(file_audio_path)
  aud <- mono(audio, "both") # downsample to 11025 samples/sec.:
  aud11 <- downsample(aud, 2000) 
  waveform <- as.vector(aud11@left)
  time <- seq(0, duration(aud11), length.out = length(waveform))
  Audio_data <- data.frame(time = time, amplitude = waveform)
  save(file = "./Data/chapViolinist_AudioF3.RData",Audio_data)
  
  
  file_audio_path <- "Data/Avatar Data/Second_Violin_F4_Audio.wav"
  audio = readWave(file_audio_path)
  aud <- mono(audio, "both") # downsample to 11025 samples/sec.:
  aud11 <- downsample(aud, 2000) 
  waveform <- as.vector(aud11@left)
  time <- seq(0, duration(aud11), length.out = length(waveform))
  Audio_data <- data.frame(time = time, amplitude = waveform)
  save(file = "./Data/chapViolinist_AudioF4.RData",Audio_data)
}
run = FALSE


#########################################################
# 2. Continuous data based on relative phase calculations
# #############################################################

# F1 regions and vertical line position
F1_band_start_Conti = c(0,   38.7, 88,   99,  145, 180.5 )
F1_band_end_Conti   = c(3.6, 53,   97.3,   127,   165,    200 )
F1_vline_full_Conti = c(38.7, 88,   99,  145, 180.5, 3.6, 53,   97.3,   127,   165 )
# F2
F2_band_start_Conti = c(0, 33.5, 93,  131, 180 )
F2_band_end_Conti   = c(23, 45,  110 , 145.7,  200)
F2_vline_full_Conti = c(33.5, 93,  131, 180,23, 45,  110 , 145.7)
# F3
F3_band_start_Conti = c(0,    32.5,   68.5, 133, 156, 188.5)
F3_band_end_Conti   = c(14.5, 39 ,    110, 145, 183, 200)
F3_vline_full_Conti = c(32.5,   68.5, 133, 156, 188.5,14.5, 39 ,    110, 145, 183)
# F4
F4_band_start_Conti = c(0, 28, 96.6, 179 )
F4_band_end_Conti   = c(20, 73, 140, 200)
F4_vline_full_Conti = c(28, 96.6, 179, 20, 73, 140)
# Make lists
Vline_Conti <- list(F1_vline_full_Conti,F2_vline_full_Conti,F3_vline_full_Conti,F4_vline_full_Conti)
Band_start_Conti <- list(F1_band_start_Conti,F2_band_start_Conti,F3_band_start_Conti,F4_band_start_Conti)
Band_end_Conti <- list(F1_band_end_Conti,F2_band_end_Conti,F3_band_end_Conti,F4_band_end_Conti)

####################
## Create DatStudent and DatTeacher
#####################
run = FALSE
# run = TRUE
if(run){
  df_avatar_noNAbeginning <- readRDS("Data/chapViolinist_df_avatar_noNAbeginning.rds")
  df_GUSOparticipant_noNAbeginning <- readRDS("Data/chapViolinist_df_GUSOparticipant_noNAbeginning.rds")
  
  DataTeacher <- df_avatar_noNAbeginning %>%
    rename(BX1 = "X_Bow_1",  BY1 = "Y_Bow_1",  BZ1 = "Z_Bow_1", 
           VX1 ="X_Violin_2", VY1 = "Y_Violin_2", VZ1 = "Z_Violin_2"    
    ) %>% 
    mutate(X1 = VX1 - BX1, Y1 = VY1 - BY1, Z1 = VZ1 - BZ1)
  
  DataStudent <- df_GUSOparticipant_noNAbeginning %>%
    rename(BX1 = "X_Bow_1",  BY1 = "Y_Bow_1",  BZ1 = "Z_Bow_1", 
           VX1 ="X_Violin_2", VY1 = "Y_Violin_2", VZ1 = "Z_Violin_2"    
    ) %>% 
    mutate(X1 = VX1 - BX1, Y1 = VY1 - BY1, Z1 = VZ1 - BZ1)
  
  # Create DatTeacher
  DatTeacher <- DataTeacher %>% #dplyr::filter(piece == "F2") %>% droplevels() %>%
    group_by(piece) %>% 
    mutate(
      # Time = (1:length(X1))/120,
      Xs1 = do_impute_smooth1(X1,timeindex),
      Ys1 = do_impute_smooth1(Y1,timeindex),
      Zs1 = do_impute_smooth1(Z1,timeindex),
      s1 = do_pca(Xs1,Ys1,Zs1) ,
      ss1 = scale(s1,scale= FALSE),
      ss1 = ss1 * ifelse(ss1[1]>0,1,-1) # First value should be on top
    ) %>%
    ungroup()
  
  # Create DatStudent
  DatStudent <- data.frame()
  Plots <- list()
  Info <- data.frame()
  Phase1 <- data.frame()
  Phase2 <- data.frame()
  R_alpha <- data.frame()
  #R_alpha_vel <- data.frame()
  count = 0
  
  for(placeholder in unique(DataStudent$Placeholder)){
    count = count + 1
    print(paste(count, ".processing placeholder", placeholder))
    datnow <- data.frame()
    datnow <- DataStudent %>% dplyr::filter(Placeholder == placeholder) %>% droplevels() 
    # Plots[[count]] <- plot.ts(datnow$X1)
    print(head(datnow$X1))
    
    p = levels(datnow$piece) 
    print(paste("--> placeholder", placeholder, "---> piece is",p))
    datnow_teacher <- DatTeacher %>% dplyr::filter(piece == p) %>% droplevels() 
    #len = length(datnow_teacher$ss1)
    #datnow <- datnow[1:len,]
    
    datnow <- datnow %>% mutate(
      #Time = (1:length(X1))/120,
      Xs1 = do_impute_smooth1(X1,timeindex),
      Ys1 = do_impute_smooth1(Y1,timeindex),
      Zs1 = do_impute_smooth1(Z1,timeindex),
      s1 = do_pca(Xs1,Ys1,Zs1) ,
      ss1 = scale(s1,scale= FALSE),
      ss1 = ss1 * ifelse(ss1[1]>0,1,-1) # First value should be on top
    )
    DatStudent <- rbind(DatStudent,datnow) 
  }
  levels(DatStudent$condition) <- c("2D","3D")
  saveRDS(file = "Data/chapViolinist_DatStudent.rds", DatStudent)
  saveRDS(file = "Data/chapViolinist_DatTeacher.rds", DatTeacher)
}
run = FALSE


# Create Vector
run = FALSE
# run = TRUE
if(run){
  DatStudent <- readRDS(file = "Data/chapViolinist_DatStudent.rds")
  DatTeacher <- readRDS(file = "Data/chapViolinist_DatTeacher.rds")
  Plots <- list()
  Info <- data.frame()
  Phase1 <- data.frame()
  Phase2 <- data.frame()
  R_alpha <- data.frame()
  Relphase <- data.frame()
  #R_alpha_vel <- data.frame()
  count = 0
  for(placeholder in unique(DatStudent$Placeholder)){
    # placeholder = "P001_T1_3D_F1"
    print(paste("processing", placeholder))
    datnow <- data.frame()
    #  datnow <- Dat %>% dplyr::filter(Dyad == dyad, Block == block, Condition == condition, Tempo == tempo)  %>% 
    datnow <- DatStudent %>% dplyr::filter(Placeholder == placeholder) %>% droplevels() 
    p = levels(datnow$piece) 
    print(paste("placeholder is", placeholder, "---> piece is",p))
    datnow_teacher <- DatTeacher %>% dplyr::filter(piece == p) %>% droplevels() 
    # placeholder = unique(datnow$Placeholder)
    if(!nrow(datnow)==0) { # needed to check whether subject performed the condition
      print(paste("---> Processing:: Placeholder =", placeholder) )
      count = count + 1
      ############################
      if(1){ # 1. relphase calculation on continuous signal
        c = as.numeric(cor(datnow$ss1,datnow_teacher$ss1, use = "pairwise.complete.obs"))
        print(paste("1. correlation ss1 ss2",c))
        if (c < 0){ # if correlation is negative then reverse the signal with lowest value
          # this is only in daatnow and not in Dat !!!
          smallest = min(datnow$ss1[1],datnow_teacher$ss1[1])
          if(smallest == datnow$ss1[1]) {datnow$ss1 = -datnow$ss1}
          else{datnow_teacher$ss1 = -datnow_teacher$ss1}
        }
        c = as.numeric(cor(datnow$ss1,datnow_teacher$ss1, use = "pairwise.complete.obs"))
        print(paste("2. correlation ss1 ss2",c))
        as1<- hilbert(datnow$ss1, 120)
        ps1 <- ifreq(as1, f=120,plot=FALSE)$p
        as2 <- hilbert(datnow_teacher$ss1, 120)
        ps2 <- ifreq(as2, f=120,plot=FALSE)$p
        # time <- ps1[,1]
        phi1 <- unwrap(ps1[,2])
        phi2 <- unwrap(ps2[,2])
        relphaseC <- (((phi1 - phi2) +pi) %% (2*pi)) -pi
        relphase <- phi1 - phi2
        time <- (1:length(relphase))/120
        V <- mean(exp(1i*relphase))
        R <- Mod(V)
        alpha <- Arg(V)
        abs_alpha <- abs(alpha)
        r_alpha <- cbind(R, alpha, abs_alpha, c)
        
        
        R_alpha <- rbind(R_alpha,r_alpha)
        info <- cbind(count, placeholder, participant = levels(datnow$participant), 
                      trial = levels(datnow$trial),  condition = levels(datnow$condition), 
                      piece = levels(datnow$piece), stand = levels(datnow$stand)) %>% data.frame()
        Info <- rbind(Info, info)
        relphaseInfo <- data.frame(relphase,relphaseC) %>% mutate(count = info$count,
                                                                  Placeholder = info$placeholder, 
                                                                  Participant = info$participant,
                                                                  Trial = info$trial,
                                                                  Condition = info$condition,
                                                                  Piece = info$piece,
                                                                  Stand = info$stand,
                                                                  Time = datnow$timeindex)
        Relphase <- rbind(Relphase,relphaseInfo)
      }
      ############################
    } else {
      # print("       ---> datnow is empty - condition not performed")
    } # needed to check whether subject performed the condition
  }
  
  levels(Relphase$Condition) <- c("2D","3D")
  
  Info <- Info %>% rename(Placeholder = placeholder,
                          Participant = participant,
                          Trial = trial,
                          Condition = condition,
                          Piece = piece,
                          Stand = stand) %>%
    mutate(count = as.numeric(count),
           Placeholder = factor(Placeholder),
           Participant = factor(Participant),
           Trial = factor(Trial),
           Condition = factor(Condition),
           Piece = factor(Piece),
           Stand = factor(Stand)
    )
  
  Vector <- cbind(R_alpha,Info)
  
  saveRDS(file = "Data/chapViolinist_Relphase.rds",Relphase)
  saveRDS(file = "Data/chapViolinist_Info.rds",Info)
  saveRDS(file = "Data/chapViolinist_R_alpha.rds",R_alpha)
  saveRDS(file = "Data/chapViolinist_Vector.rds",Vector)
}
run = FALSE

#########################
# Create Res data containing R values reduced

run = FALSE
# run = TRUE
if(run){
  Relphase <- readRDS(file = "Data/chapViolinist_Relphase.rds")
  
  Res <- data.frame()
  breaks = seq(1,200,1)
  for(placeholder in unique(Relphase$Placeholder)){
    # placeholder = "P001_T2_2D_F2"
    print(paste("processing", placeholder))
    datnow <- data.frame()
    datnow <- Relphase %>% dplyr::filter(Placeholder == placeholder) %>% 
      droplevels() %>% mutate(cutted_relphase = factor(cut(Time,breaks)) ) # 50 cuts
    for (cr in unique(datnow$cutted_relphase)){
      # cr = "(-0.183,18.3]"
      # print(paste("--> cr= ",cr))
      rp <- datnow %>% dplyr::filter(cutted_relphase == cr) %>% select(relphase)
      V <- mean(exp(1i*rp$relphase))
      R <- Mod(V)
      alpha <- Arg(V)
      abs_alpha <- abs(alpha)
      Res <- rbind(Res, cbind(R,alpha,Placeholder = unique(datnow$Placeholder), 
                              Participant = unique(datnow$Participant),
                              Condition = unique(datnow$Condition), 
                              Piece = unique(datnow$Piece), 
                              Trial = unique(datnow$Trial), Cut = (cr)) )
    }
  }
  
  
  Res <- Res %>% mutate(R = as.numeric(R), log_1_R = scale(log(1-R)), Cut = factor(Cut, levels = unique(Cut)),
                        Time = as.numeric(Cut),
                        Piece = factor(Piece), Trial = factor(Trial), 
                        Condition = factor(Condition,levels = c("2D","3D")),
                        Placeholder = factor(Placeholder),
                        Participant = factor(Participant))
  
  saveRDS(file = "Data/chapViolinist_Res.rds",Res)
  
}
run = FALSE

# DatStudent <- readRDS(file = "Data/chapViolinist_DatStudent.rds")
# DatTeacher <- readRDS(file = "Data/chapViolinist_DatTeacher.rds")
# Vector <- readRDS(file = "Data/chapViolinist_Vector.rds")
# Res <- readRDS(file = "Data/chapViolinist_Res.rds")




