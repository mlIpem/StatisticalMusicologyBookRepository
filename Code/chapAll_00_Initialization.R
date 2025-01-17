#install.packages("tidyverse")
library(tidyverse)
#library(reshape2)
library(readxl)
library(readr)

library(ggplot2)
library(patchwork)
library(grid)
library(gridExtra)
library(ggExtra)
library(ggthemes)
library(ggside)
#library(ggpubr)
#library(sjPlot)

#install.packages("coda")
#install.packages("rjags") -> vergt eerst aparte installatie
#install.packages("BEST")
#library(coda)
#library(rjags)
#library(BEST)
#


########################################################################
########################################################################
######## rstan
######## https://github.com/stan-dev/rstan/wiki/RStan-Getting-Started
if(0){
# remove.packages(c("StanHeaders", "rstan"))
# 
install.packages("rstan", repos = c("https://mc-stan.org/r-packages/", getOption("repos")))
install.packages("rstan", repos = c('https://stan-dev.r-universe.dev', getOption("repos")))
# https://github.com/stan-dev/rstan/wiki/Configuring-C---Toolchain-for-Mac
# remotes::install_github("coatless-mac/macrtools")
# macrtools::macos_rtools_install()
}
options(mc.cores = parallel::detectCores())
library(rstan)
library(rstantools)
rstan_options(auto_write = TRUE)

########################################################################
########################################################################

library(StanHeaders)
library(rstan)
options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)

library(tidybayes)
library(bayesplot)

library(brms)

#library(shinystan)
library(bayestestR)
library(posterior)

#library(lme4)
#library(BListener)
#library(flexdashboard)

##library(sjPlot)
#library(kableExtra)
#library(magick)

#library(modelbased)
#library(emmeans)
#library(patchwork)

#install.packages("easystats", repos = "https://easystats.r-universe.dev")
#library("easystats")
#library(insight)
#library(see)
#library(equatiomatic)
library(performance)
library(parameters)
#library(see)

library(mgcv)
#library(smoothr)
library(gratia)


#library(foreach)
#library(doParallel)

#library(DescTools)
#install.packages("ggcorrplot")                      # Install ggcorrplot package
library("ggcorrplot") 

# install.packages("devtools")
# # remotes::install_github("stan-dev/cmdstanr")
 library(cmdstanr)
 check_cmdstan_toolchain(fix = TRUE, quiet = TRUE)
 check_cmdstan_toolchain()
# # install_cmdstan(cores = 16, overwrite=TRUE)
if(0){
        rebuild_cmdstan(cores = 16)
}
 cmdstan_path()

#library(xfun)

#library(misty)
#library(rstatix)
#library(coin)
#library(tidymv)
#library(flextable)
#library(broom.mixed)


library(tuneR)
library(seewave)
library(R.matlab)

#install_github("m-vidal/eaR")

library(deSolve)
library(shinystan)

#library(doParallel)
#library(foreach)
library(pracma)

#library(lhs)

library(lavaan)
library(blavaan)
library(lavaanPlot)
library(semPlot)
library(modelr)
library(FactoMineR)
library(ggplotify)
library(cowplot)
library(knitr)
library(kableExtra)
library(tidymv)
library(tidygam)
library(kableExtra)
library(bookdown)
library(tuneR)

 library(parallel)
 library(parallelly)
 library(doParallel)
 
 #install.packages("corrplot")
 library(corrplot)
library(semTools)
 library(sn)
 library(janitor)
 # install.packages("circular")
 library(circular)
 library(cowplot)
 
#install.packages('dagitty')
library('dagitty')
 library(miniUI)
 library(deSolve)
 
 # #set config
 # usethis::edit_r_environ() # see your .Renviron with the token
 # 
 # usethis::use_git_config(user.name = "YourName", user.email = "your@mail.com")
 # 
 # #Go to github page to generate token
 # usethis::create_github_token() 
 # 
 # #paste your PAT into pop-up that follows...
 # credentials::set_github_pat()
 # 
 # #now remotes::install_github() will work
 # remotes::install_github("username/privaterepo")
 
 #install.packages("devtools")
 #library("devtools")
 #remotes::install_github("IPEM/BListener",force = TRUE)
 #library(BListener)
