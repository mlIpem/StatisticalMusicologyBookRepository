
# 1. the dataset DData is extracted from the data of Rosso et al. (2024).
# It filters the tapping of dyad_1 in the condition where a human subject is 
# requested to tap along with a metronome
TicTap_data <- readRDS(file = "Data/chapModelling_TicTap_Data.rds")
d1 <- TicTap_data %>% filter(names == "metronome") %>% mutate(dtime = c(0.6,diff(time))) 
d2 <- TicTap_data %>% filter(names == "subject") %>% mutate(dtime = c(NA,diff(time))) 
DData <- rbind(d1,d2) %>% arrange(time)
w <- which(DData$dtime>.9)
DData <- DData[-w,]

# Needed ?
w <- which(d2$dtime>.9)
d2d <- d2[-w,]

# 2. Calculate relative phase and extraxt some parameters
RelPhase <- do_calc_phase2(d2,d1) %>% group_by(time) %>% mutate(cycle=factor(cycle))
phi <- RelPhase$relphase
V <- mean(exp(1i*phi))
R <- abs(V)
alpha <- angle(V) 

# 3. Create Dataset, which is the dataset used in regression analysis
# We calculated the RelPhase in 03
Dataset <- RelPhase %>% dplyr::select(relphase, time, cycle, time0c) %>% arrange(time) 
# remove negative values which could occur at the beginning
w <- which(Dataset$time0c < 0)
if(!is_empty(w))  Dataset <- Dataset[-w,]
#Dataset


