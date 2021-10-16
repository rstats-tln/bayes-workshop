library(tidyverse)
library(here)
library(brms)
library(bayesplot)
library(tidybayes)
library(modelr)
library(rstan)
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

#' 
#' Euroopa Komisjoni andmetel olid liikluses kõige ohustatumad rühmad jalakäijad, jalg- ja mootorratturid 
#' ning vanad inimesed. 
#' Arvestades demograafilisi muutusi, võivad need ohvrite grupid veelgi kasvada ja seepärast 
#' tuleb nendele suurimat tähelepanu pöörata, märkis Komisjon (allikas ERR).
#' 
#' 
#' Most vulnerable groups in traffic are pedestrians, cyclist, and elderly people.
#' 
#' 

#'
#' Importing dataset
#' 
#' Variable names in English can be found on https://github.com/rstats-tln/traffic-accidents.
#' 
#+
onnetused <- read_csv(here("data/liiklusonnetused.csv"), col_types = cols(juhtumi_nr = col_character()))

onnetused %>% 
  ggplot() +
  geom_bar(aes(hukkunuid, fill = liiklusõnnetuse_liik_1), position = "stack") +
  facet_wrap(~ ilmastik_1, scales = "free")

#'
#' Number of deaths by type and weather1
#'
#'
#+
data <- onnetused %>% 
  select(toimumisaeg, hukkunuid, liik = liiklusõnnetuse_liik_1, ilm = ilmastik_1) %>% 
  filter(ilm != "Märkimata")

#'
#' Formula and default priors
#+
f <- hukkunuid ~ liik + ilm
get_prior(formula = f, data = data, family = hurdle_poisson(link = "log")) 


#'
#' Figure out some more informative priors.
#' 
#+
range(data$hukkunuid)
table(data$hukkunuid)
mean(data$hukkunuid == 0)
hist(rbeta(1000, 12, 1), breaks = seq(0, 1, by = 0.01))


#'
#' Setting priors
#'
#+
priors <- c(
  prior("normal(-2.5, 1)", class = "Intercept"),
  prior("normal(0, 1)", class = "b"),
  prior("beta(12, 1)", class = "hu")
)


#'
#' Create models dir to store models if missing
#'
#+
if(!dir.exists(here("models"))) dir.create(here("models"))


#'
#' Initial prior-only model
#'
#+
mod1.0 <- brm(
  formula = hukkunuid ~ liik + ilm,
  data = data,
  family = hurdle_poisson(link = "log"),
  prior = priors,
  chains = 3,
  file = here("models/hukkunuid~liik+ilm_prior_only"),
  sample_prior = "only",
  file_refit = "on_change"
)

#'
#' Model summary
#' 
#+
summary(mod1.0)


#'
#'  
#+
pp_check(mod1.0)

#'
#'  
#+
mod1.1 <- brm(
  formula = hukkunuid ~ liik + ilm,
  data = data,
  family = hurdle_poisson(link = "log"),
  prior = priors,
  chains = 3,
  file = here("models/hukkunuid~liik+ilm"),
  sample_prior = "yes",
  file_refit = "on_change"
)

#'
#'  
#+
plot(mod1.1)


#'
#'  
#+
summary(mod1.1)

#'
#'  
#+
plot(conditional_effects(mod1.1))



#'
#'  
#+
mod1.2 <- brm(
  formula = hukkunuid ~ liik + ilm + liik:ilm,
  data = data,
  family = hurdle_poisson(link = "log"),
  prior = priors,
  chains = 3,
  file = here("models/hukkunuid~liik+ilm+liik:ilm"),
  sample_prior = "yes",
  file_refit = "on_change"
)

#'
#'  
#+
plot(mod1.2)

#'
#'  
#+
summary(mod1.2)

#'
#'  
#+
plot(conditional_effects(mod1.2))

#'
#'  
#+
get_prior(bf(hukkunuid ~ liik + ilm + liik:ilm, hu ~ liik + ilm), data = data, hurdle_poisson())

priors <- c(
  prior("normal(-2.5, 1)", class = "Intercept"),
  prior("normal(0, 1)", class = "b"),
  prior("beta(12, 1)", class = "Intercept", dpar = "hu")
)

mod1.3 <- brm(
  formula = bf(hukkunuid ~ liik + ilm + liik:ilm, hu ~ liik),
  data = data,
  family = hurdle_poisson(link = "log"),
  prior = priors,
  chains = 3,
  file = here("models/hukkunuid~liik+ilm+liik:ilm_hu~liik+ilm"),
  sample_prior = "yes",
  file_refit = "on_change"
)

plot(mod1.3)

summary(mod1.2)

plot(conditional_effects(mod1.2))


