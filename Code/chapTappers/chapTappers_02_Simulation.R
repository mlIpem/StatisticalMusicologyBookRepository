# chapTappers_02_Simulation with differential equation

run = FALSE
# run = TRUE
if(run){
  # define differential equation dydt
  dydt <- function(t, y, parms) {
    with(as.list(c(y, parms)), {
      dm1 = 2*pi*1000/600
      ds1 = 2*pi*1000/600  + .5*(ks1m1* sin(m1 - s1) + (1-ks1m1)* sin(s2 - s1) )
      
      dm2 = 2*pi*1000/610
      ds2 = 2*pi*1000/610  + .5*(ks2m2* sin(m2 - s2) + (1-ks2m2)* sin(s1 - s2) )
      
      ds1a = ks1m1 * sin(m1 - s1)
      ds1b = (1-ks1m1) * sin(s2 - s1)
      
      return(list(c(dm1,ds1,dm2,ds2,ds1a,ds1b)))
    })
  }
  
  Solution <- data.frame()
  dt = 1/100
  time_points <- seq(0, 36.6, by = dt)
  state <- c(m1 = 0, s1 = 0, m2 = 0, s2 = 0, s1a = 0, s1b = 0)
  
  for (ks1m1 in seq(1,.2,-.2)){ # different values for coupling strength of s1
    for(ks2m2 in c(1,.75)){# different values for coupling strength of s2
      parm <- c(ks1m1 = ks1m1,ks2m2 = ks2m2)
      # solve differential equation
      solution <- ode( state, time_points, dydt, parms = parm) %>% 
        data.frame() %>% 
        dplyr::select(time,m1,s1,m2,s2,s1a,s1b) %>% 
        mutate(ks1m1 = factor(ks1m1), ks2m2=factor(ks2m2))
      Solution <- rbind(Solution, solution)
    }
  }
  
  # calculate differential to obtain instantaneous frequency from phase
  Solution <- Solution %>% mutate(
    dm1 = c(NA,diff(m1)),
    ds1 = c(NA,diff(s1)),
    dm2 = c(NA,diff(m2)),
    ds2 = c(NA,diff(s2)),
    ds1a = c(NA,diff(s1a)),
    ds1b = c(NA,diff(s1b))
  )
}
run = FALSE