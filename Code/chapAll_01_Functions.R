# The palette with grey:
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
cbPalette <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
#  scale_fill_manual(values=cbPalette) +
#  scale_colour_manual(values=cbPalette) +



do_pca <- function(D1,D2,D3){
  # D1 = datnow$Xs1; D2 = datnow$Ys1; D3 = datnow$Zs1
  # D1 = datnow$Xs2; D2 = datnow$Ys2; D3 = datnow$Zs2
  
  # D = as.matrix(cbind(D1,D2,D3))
  # d <- principal(D, covar = TRUE, nfactors = 1)$score
  #dd <- scale(d)
  # if(0){
  # D1 = datnow$Xs1; D2 = datnow$Ys1; D3 = datnow$Zs1
  # D1 = datnow$Xs2; D2 = datnow$Ys2; D3 = datnow$Zs2
  #D = as.matrix(cbind(D1,D2,D3))
  #d1 <- principal(D, covar = TRUE, nfactors = 1)$score
  #d2 <- principal(D, covar = TRUE, nfactors = 1)$score
  
  #plot.ts(D)
  #plot.ts(cbind(d1,d2))
  #plot(p$ind$coord)
  cov_matrix <- cov(cbind(D1, D2, D3), use = "pairwise.complete.obs")
  eigen_decomp <- eigen(cov_matrix)
  sorted_indices <- order(eigen_decomp$values, decreasing = TRUE)
  sorted_eigenvectors <- eigen_decomp$vectors[, sorted_indices]
  max_variance_index <- 1  # Assuming the first eigenvector has the max variance
  selected_eigenvector <- sorted_eigenvectors[, max_variance_index]
  # Project the original data onto the selected eigenvector
  S <- cbind(D1, D2, D3) %*% selected_eigenvector
  #plot.ts(S)
  # If you want to scale S to have mean 0 and unit variance
  #S <- scale(S)
  #  }
  return(S)
}


do_impute_smooth1 <- function(D,Time){
  # D = Data$X1; Time = Data$Time
  #D1 = ifelse(D==0,NA,D) 
  #D2 = impute_AR1_t(D1)
  D3 = ksmooth(Time, D, "normal", bandwidth = .3)$y
  return(D3)
}

do_contrast <- function(post1, post2){
  epred1 <- post1$.epred
  epred2 <- post2$.epred
  contrast_distribution <- epred1 - epred2
  probability_of_direction = round(100 * sum(contrast_distribution > 0, na.rm = TRUE) / sum(contrast_distribution < Inf, na.rm = TRUE),0)
  summary_of_contrast <- median_qi(contrast_distribution) %>% select(y,ymin,ymax)
  result <- round(cbind(summary_of_contrast, probability_of_direction),2)
  return <- result
}

do_effectsize <- function(post1, post2){
  m1 <- mean(post1$.epred)
  m2 <- mean(post2$.epred)
  sd1 <- sd(post1$.epred)
  sd2 <- sd(post2$.epred)
  pooled_sd <- sqrt((sd1^2 + sd2^2) / 2)
  cohen_d <- (m1 - m2) / pooled_sd %>% data.frame()
  return <- cohen_d
}


# Contrasts working with .value (directly on parameters)
do_contrast2 <- function(post1, post2){
  epred1 <- post1$.value
  epred2 <- post2$.value
  contrast_distribution <- epred1 - epred2
  probability_of_direction = round(100 * sum(contrast_distribution > 0) / sum(contrast_distribution < Inf),0)
  summary_of_contrast <- median_qi(contrast_distribution) %>% select(y,ymin,ymax)
  result <- round(cbind(summary_of_contrast, probability_of_direction),2)
  return <- result
}


do_rotate <- function(value){
  rotated_value <- ((value + pi)%%(2*pi)) -pi
  return(rotated_value)
}

# run_model_cmdstanr(Data, form, fam)
run_model_cmdstanr <- function(Data, form, fam, priors){   #  }, priors){
  print(paste(form[1],fam,sep="\n"))
  fit <- brm(data = Data,
             formula = form,
             family = fam,
             prior = priors,
             iter = 6000, warmup = 3000,
             #control = list(adapt_delta = 0.995,  max_treedepth = 12),
             init = 0,
             thin = 2,
             chains = 4,
             backend = "cmdstanr",
             threads = threading(4),
             silent = 0
             #save_pars = save_pars(all = TRUE),
             #sample_prior = TRUE #,
             #file = filen
  )
  return(fit)
}


dz_dt <- function(t, state, parameters) {
  with(as.list(c(state, parameters)), {
    d_phi_m1 <-  omega_m1
    d_phi_s1 <-  omega_s1 + .5 * (k_s1m1 *  sin(phi_m1 - phi_s1 + d_s1m1) + (1-k_s1m1)* sin(phi_s2 -phi_s1 + d_s1s2) )
    d_phi_m2 <-  omega_m2
    d_phi_s2 <-  omega_s2 + .5 * (k_s2m2 *  sin(phi_m2 - phi_s2 + d_s2m2) + (1-k_s2m2) * sin(phi_s1 - phi_s2 + d_s1s2) )
    list(c(d_phi_m1, d_phi_s1, d_phi_m2,d_phi_s2))
  })
}

dz_dt_old <- function(t, state, parameters) {
  with(as.list(c(state, parameters)), {
    d_phi_m1 <-  omega_m1
    d_phi_s1 <-  omega_s1 + .5 * (k_s1m1 *  sin(phi_m1 - phi_s1) + k_s1s2* sin(phi_s2 -phi_s1) )
    d_phi_m2 <-  omega_m2
    d_phi_s2 <-  omega_s2 + .5 * (k_s2m2 *  sin(phi_m2 - phi_s2) + k_s2s1 * sin(phi_s1 - phi_s2) )
    list(c(d_phi_m1, d_phi_s1, d_phi_m2,d_phi_s2))
  })
}


do_calc_phase2 <- function(tappings,metronome) {
  # tappings = s1
  # metronome = m1
  tappings <- tappings %>% ungroup()
  relphase <- tappings$time * NA #  max time is 39000
  metro <- tappings$time * NA #  max time is 39000
  
  #cycle <- tappings$time * NA
  count = 0 # counter goes allonw with the taps. tap starts at second tap because the fist is NA
  
  for (tap in tappings$time){ #  tijd van metronome$time reeks
    #print(tap)
    if(tap >= last(metronome$time)) {
      print(paste("We reached the end of the metronome time, so we cut off here."))
      break
    }
    if(tap >= first(metronome$time)) { 
      #metro_t1 = metronome$time[count]# twee opeenvolgende tijdstippen van de metronoom
      count = count + 1
      w1 = which(metronome$time <= tap)
      w1 = max(w1) 
      w2 = which(metronome$time > tap)
      w2 = min(w2) # select minimum index
      #print(paste(sub,dyadnr,condi,count,tap,metronome$time[w1],metronome$time[w2]))
      if(metronome$time[w2] == metronome$time[w1+1]){ 
        # dan weten we zeker dat tap tussen w1 en w2 valt en kan de fase berekend worden
        relphase[count] = 2*pi* (metronome$time[w1]-tap) / (metronome$time[w1] - metronome$time[w2]) # index van phasetime gaat samen met Metro_t1$time
        metro[count] = metronome$time[w1] #metronome reference time for phase
      } else { # dan ligt phase na w2 en berekenen we de fase dus met het volgende interval
        relphase[count] = 2*pi* (metronome$time[w2]-tap) / (metronome$time[w2] - metronome$time[w2+1]) 
        metro[count] = metronome$time[w2]
      }
    } # tap > metronome time
    if(tap < first(metronome$time)) {
      count = count + 1
      relphase[count] = NA
      metro[count] = NA
    }
  } # for all taps
  # time is the time of the tapping, time0 set the first tap relative to the time of the first metronome
  M1 <- tappings %>% mutate(relphase = relphase, #((relphase + pi)%%(2*pi)) -pi, 
                            time0 = time - metro[1], time_metronome = metro) 
  # Here we set select the total time of the Fragments
  #M1 <- M1[M1$time0 <= 11,] 
  w = which(is.na(M1$relphase))
  if(length(w) > 0) M1 <- M1[-w,]
  #print(paste("NA discovered:",w))
  return(M1)
}


make_pdf_figure <- function(fn,fig){
  fn_pdf = paste(fn,".pdf",sep="")
  print(paste("plotting", fn_pdf))
  pdf(file = fn_pdf ,width=9, height=5)  #Note that you can convert inches to centimeters dividing by 2.54.
  print(fig)
  dev.off()
}

make_png_figure <- function(fn,fig){
  fn_png = paste(fn,".png",sep="")
  print(paste("plotting", fn_png))
  png(filename=fn_png, width=9, height=5, units = "in", res = 600)
  print(fig)
  dev.off()
}

make_svg_figure <- function(fn,fig){
  fn_svg = paste(fn,".svg",sep="")
  print(paste("plotting", fn_svg))
  svg(file = fn_svg ,width=7, height=3
      #Note that you can convert inches to centimeters dividing by 2.54.
  ) 
  print(fig)
  dev.off()
}


make_eps_figure <- function(fn,fig){
  fn_eps = paste(fn,".eps",sep="")
  print(paste("plotting", fn_eps))
  postscript(file = fn_eps ,width=7, height=3
             #Note that you can convert inches to centimeters dividing by 2.54.
  ) 
  print(fig)
  dev.off()
}

make_fig <- function(fn,fig){
  make_pdf_figure(fn,fig)
  make_png_figure(fn,fig)
}

