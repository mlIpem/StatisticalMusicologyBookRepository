# chapExoskeletons_05_ModelPlotting.R

# Plotting from continuous phase features: R and abs_alpha
# Population-effects: R and abs_alpha

run = FALSE
# run = TRUE
if(run){
  cbPalette <- c("#E69F00", "#56B4E9", "#009E73", "#999999", "#F0E442", "#0072B2", "#D55E00", "#CC79A7", "#509999","#A072B2","#FF8442","#F02242")
  #  scale_fill_manual(values=cbPalette) +
  #  scale_colour_manual(values=cbPalette) +
  
  #########
  # plotting R
  fit_R <- readRDS(file = "Fitted/chapExoskeletons_fit_R.rds") 
  fit <- fit_R
  
  pp <- pp_check(fit) + theme_bw()
  CE <- conditional_effects(fit, re_formula = NA)
  CE3 <- CE[[3]] %>% mutate(Condition = factor(Condition, labels = c("audio","audio-haptic","audio-visual","audio-visual-haptic")))
  VC1234 <- Vector_Condition1234 %>% mutate(Condition = factor(Condition, labels = c("audio","audio-haptic","audio-visual","audio-visual-haptic")))
  
  
  p_R <- ggplot() +
    geom_errorbar(data = CE3, aes(y=estimate__,ymin=lower__,ymax=upper__, x=Condition, color = Tempo),
                  position = position_dodge(width = .7),linewidth = 1) +
    geom_point(data = CE3, aes(y=estimate__,x=(Condition), color = Tempo),size = 2,
               position = position_dodge(width = .7)) +
    geom_point(data = VC1234, aes(x=(Condition),y=R, color = Tempo), 
               position = position_jitterdodge(dodge.width = .5, jitter.width = .15, jitter.height = 0), 
               size = 2, shape=1,alpha = .85) +
    theme_bw() + #theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=.5)) +
    scale_fill_manual(values=cbPalette) + scale_colour_manual(values=cbPalette) +
    labs(y = "R") +
    ylim(.6,1)
  p_R
  
  make_fig(fn = "Figures/chapExoskeletons_fit_R", p_R)
  make_fig(fn = "Figures/chapExoskeletons_fit_R_pp_check", pp)
  
  #########
  # plotting alpha
  fit_alpha <- readRDS(file = "Fitted/chapExoskeletons_fit_alpha.rds")
  fit <- fit_alpha
  
  #summary(fit)
  pp <- pp_check(fit) + theme_bw()
  
  CE <- conditional_effects(fit, re_formula = NA)
  
  CE3 <- CE[[3]] %>% mutate(Condition = factor(Condition, labels = c("audio","audio-haptic","audio-visual","audio-visual-haptic")))
  VC1234 <- Vector_Condition1234 %>% mutate(Condition = factor(Condition, labels = c("audio","audio-haptic","audio-visual","audio-visual-haptic")))
  
  p_alpha <- ggplot() +
    geom_errorbar(data = CE3, aes(y=(estimate__),ymin=(lower__),ymax=(upper__), x=Condition, color = Tempo),
                  position = position_dodge(width = .7)) +
    geom_point(data = CE3, aes(y=(estimate__),x=Condition, color = Tempo),size = 2,
               position = position_dodge(width = .7)) +
    geom_point(data = VC1234, aes(x=Condition,y=alpha, color = Tempo), 
               position = position_jitterdodge(dodge.width = .5, jitter.width = .15, jitter.height = 0), size = 1, shape=1,alpha = .85) +
    theme_bw() + #theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=.5)) +
    scale_fill_manual(values=cbPalette) + scale_colour_manual(values=cbPalette) +
    labs(y = expression("\u03b1") ) +
    ylim(-pi,pi)
  p_alpha
  
  make_fig(fn = "Figures/chapExoskeletons_fit_alpha", p_alpha)
  make_fig(fn = "Figures/chapExoskeletons_fit_alpha_pp_check", pp)
  
  
  
  #########
  # plotting log_abs_alpha
  fit_log_abs_alpha <- readRDS(file = "Fitted/chapExoskeletons_fit_log_abs_alpha.rds")
  fit <- fit_log_abs_alpha
  
  #summary(fit)
  pp_check(fit)
  
  CE <- conditional_effects(fit, re_formula = NA)
  p_abs_alpha <- ggplot() +
    geom_errorbar(data = CE[[3]], aes(y=exp(estimate__),ymin=exp(lower__),ymax=exp(upper__), x=Condition, color = Tempo),
                  position = position_dodge(width = .7)) +
    geom_point(data = CE[[3]], aes(y=exp(estimate__),x=Condition, color = Tempo),size = 2,
               position = position_dodge(width = .7)) +
    geom_point(data = Vector, aes(x=Condition,y=abs_alpha, color = Tempo), position = position_jitterdodge(dodge.width = .5, jitter.width = .15), size = 1, shape=1,alpha = .85) +
    theme_bw() + theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=.5)) +
    scale_fill_manual(values=cbPalette) + scale_colour_manual(values=cbPalette) +
    labs(y = expression("|\u03b1|") ) +
    ylim(0,.5)
  p_abs_alpha
  
  make_fig(fn = "Figures/chapExoskeletons_fit_abs_alpha", p_abs_alpha)
}
run = FALSE