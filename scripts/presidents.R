#' ---
#' title: "Starting with Bayes statistics by estimating mean"
#' subtitle: "Simple intercept-only model"
#' author: "Taavi Päll and Ülo Maiväli"
#' date: "2021-10-02"
#' output: github_document
#' ---


#'
#' ## Loading some required libraries
#' 
#+
library(tidyverse)
library(here)
library(brms)
library(bayesplot)

#' 
#' ## Getting data
#' 
#' We will use USA president heights dataset.
#' 
#' ### Downloading US president heights data
#' 
#' President heights were copy-pasted from [potus.com](https://www.potus.com/presidential-facts/presidential-heights/) 
#' and after preprocessing (keeping only names and height in cm) saved to 
#' `data` subfolder in our project folder. 
#' 

#'
#' ### Import president heights data
#' 
#' Practice safe paths.
#' Use projects and the **here** package. 
#' How can you avoid `setwd()` at the top of every script? 
#' Organize each logical project into a folder on your computer.
#' 
#+
(president_heights <- read_csv(here("data/president_heights.csv"), col_types = "cd"))


#'
#' We have two columns in our tibble -- presidents names ("name") and height in 
#' cm ("height_cm").
#'


#'
#' ## Visualization
#' 
#' As always, any analysis should start with data visualization to avoid [Datasaurus](https://itsalocke.com/datasaurus/) 
#' appearing in the end.
#' 
#' Simple scatter plot, starting with tallest presidents.
#'    
#' - Abraham Lincoln was the tallest president at 193 cm.
#' - James Madison was the shortest president at 163 cm.
#' - The average height of the presidents is 180 cm.
#' 
#+
ggplot(data = president_heights) +
  geom_point(aes(x = height_cm, y = fct_reorder(name, height_cm))) +
  theme(axis.title.y = element_blank())

#'
#' Histogram shows that most frequently US presidents have been 183 cm tall.
#+
ggplot(data = president_heights) +
  geom_histogram(aes(x = height_cm), binwidth = 1) +
  scale_y_continuous(breaks = scales::pretty_breaks())



#'
#' ## Modeling
#' 
#' ### Simple intercept-only model
#'
#' We denote our intercept-only model like so:
#'
#' $$\text{height_cm} \sim \text{Normal}(\text{mu}, \text{sigma})$$
#'
#'
#' As for R model formula, on left side we define "height_cm" as our response 
#' variable (must be in data) and on the right side we define that we are interested only in 
#' modeling "Intercept".
#' 
#' Here, for the sake of our demonstrations, we define that intercept is also 
#' part of all regression coefficients, so that sampling from its prior 
#' becomes possible, thats why use ... ~ 0 + Intercept + ... in the formulas.
#' 
#+
f <- height_cm ~ 0 + Intercept # height_cm ~ 1


#'
#' Let's have a look at the parameters in our model for which we can specify 
#' priors and default priors.
#' 
#+
get_prior(formula = f, data = president_heights, family = gaussian())

#'
#' To fit a **brms** model, we need to specify minimally:     
#' 1. **model formula** in lme4 syntax,    
#' 2. **data** as data.frame and      
#' 3. **family** to specify response distribution and **link function**.    
#' 
#' Additionally, we want to run three chains and save fitted model to a file in 
#' `models` subfolder (next line creates this folder if missing) to avoid 
#' always refitting when updating and rerunning the script. 
#' 
#' If you need to refit the model, then go to models folder and delete the model file (.rds format).
#'
#+
if (!dir.exists(here("models"))) dir.create(here("models")) # we keep models only locally

#' 
#' 
#' #### Testing different priors for Intercept
#' 
#' Here we fit intercept-only model using president heights data and 
#' **uninformative** priors: "let data speak"
#'
#'
#+
priors <- prior("normal(0, 200)", class = "b", coef = "Intercept")
mod1 <- brm(
  formula = f, 
  data = president_heights, 
  family = gaussian(), 
  prior = priors,
  chains = 3, 
  file = here("models/height_cm~0+Intercept_uninformative"),
  sample_prior = "yes"
  )

#' 
#' #### Model diagnostics parameters
#' 
#' Rhat function produces R-hat convergence diagnostic, which compares the 
#' between- and within-chain estimates for model parameters. 
#' If chains have not mixed well (ie, the between- and within-chain estimates 
#' don't agree), Rhat is larger than 1. 
#' It's recommend to run at least four chains by default and only using the 
#' sample if Rhat is less than 1.05. 
#' 
#' Both bulk-ESS and tail-ESS should be at least ~100 per Markov Chain in order 
#' to be reliable and indicate that estimates of respective posterior 
#' quantiles are reliable.
#' 
#'
#+
summary(mod1)

#'
#'
#+
plot(mod1)


#'
#' Let's check our priors from model:
#' 
#+
mod1$prior

#'
#'
#+
post_sam1 <- posterior_samples(mod1)
post_sam1 <- as_tibble(post_sam1)

#'
#'
#+
post_sam1 %>% 
  select(matches("Intercept")) %>% 
  pivot_longer(cols = matches("Intercept")) %>% 
  ggplot() +
  geom_density(aes(value, linetype = name)) +
  labs(title = str_c("Default prior: ", mod1$prior[2, 1])) +
  facet_wrap(~name, ncol = 1, scales = "free_y")


#'
#' **Weakly informative prior**
#' 
#' Specifying good informative prior for Intercept term.
#' We have found information that average male height in USA is 178 cm 
#' (measured 2015-2018).
#' 
#' Weakly informative priors are designed to provide moderate regularization 
#' and help stabilize computation
#'
#'
#' Here we fit intercept-only model using president heights data and default priors:
#+
priors <- prior("normal(178, 10)", class = "b", coef = "Intercept")
mod3 <- brm(
  formula = f, 
  data = president_heights, 
  family = gaussian(), 
  prior = priors,
  chains = 3, 
  file = here("models/height_cm~0+Intercept_weakly_informative"),
  sample_prior = "yes"
)


#'
#'
#'
#+
summary(mod3)

#'
#'
#+
plot(mod3)


#'
#'
#+
post_sam3 <- posterior_samples(mod3)
post_sam3 <- as_tibble(post_sam3)

#'
#'
#+
post_sam3 %>% 
  select(matches("Intercept")) %>% 
  pivot_longer(cols = matches("Intercept")) %>% 
  ggplot() +
  geom_density(aes(value, linetype = name)) +
  labs(title = str_c("Good informative prior: ", mod3$prior[2,1])) +
  facet_wrap(~name, ncol = 1, scales = "free_y")

#'
#' **Bad informative prior**
#' 
#' Specifying good informative prior for Intercept term.
#' We have found information that average male height in USA is 178 cm (measured ).
#'
#+
priors <- prior("normal(225, 2.5)", class = "b", coef = "Intercept")

#'
#' Here we fit intercept-only model using president heights data and default priors:
#+
mod4 <- brm(
  formula = f, 
  data = president_heights, 
  family = gaussian(), 
  prior = priors,
  chains = 3, 
  file = here("models/height_cm~1_bad_informative_prior"),
  sample_prior = "yes"
)


#'
#'
#'
#+
summary(mod4)

#'
#'
#+
plot(mod4)


#'
#'
#+
post_sam4 <- posterior_samples(mod4)
post_sam4 <- as_tibble(post_sam4)

#'
#'
#+
post_sam4 %>% 
  select(matches("Intercept")) %>% 
  pivot_longer(cols = matches("Intercept")) %>% 
  ggplot() +
  geom_density(aes(value, linetype = name)) +
  labs(title = str_c("Bad informative prior: ", mod4$prior[1,1])) +
  facet_wrap(~name, ncol = 1, scales = "free_y")

#' 
#' We can see that in our models, posterior displays less variation than prior, 
#' that's because posterior distribution incorporates information from data, we can 
#' expect that it's less variable than prior distribution. 
#' 
#' 
#' 
#' There are several reasons for using non-informative priors, including:    
#' - Not having any useful prior information or strong personal 
#' opinion upon which to base an informative prior.    
#' - a non-informative prior gives a result numerically similar to a frequentist 
#' approach when little or no prior information is provided, 
#' while allowing for use of prior information when it exists.    
#' - *ad hoc* sensitivity analysis to see how much influence a strong prior 
#' has had on the results of a Bayesian analysis.    
#' 
#'
#'
#' Let's compare, how much different priors have on the results 
#' of a Bayesian analysis.
#' 
#' We test hypothesis that USA presidents are above average tall, 
#' with contemporary average male height 178 cm.
#'
#'
#' Uninformative prior: "let data speak"
plot(hypothesis(mod1, "Intercept > 178"), plot = FALSE)[[1]] + 
  labs(title = str_c("Uninformative prior: ", mod1$prior[2,1])) +
  scale_x_continuous(limits = c(-50, 50))

#' Good informative prior (?should we  use weakly informative prior instead?)
plot(hypothesis(mod3, "Intercept > 178"), plot = FALSE)[[1]] +
  labs(title = str_c("Good informative prior: ", mod3$prior[2,1]))

#' Bad informative prior
plot(hypothesis(mod4, "Intercept > 178"), plot = FALSE)[[1]] +
  labs(title = str_c("Bad informative prior: ", mod4$prior[2,1]))



#'
#' Let's fit the model w.o. data (prior predictive check)
#' 
#' We need to set samples are drawn solely from the priors ignoring the likelihood, 
#' which allows among others to generate samples from the prior 
#' predictive distribution. 
#'

mod4 <- brm(
  formula = f, 
  data = president_heights,
  family = gaussian(), 
  prior = prior("normal(178, 10)", class = "Intercept"),
  chains = 3, 
  file = here("models/height_cm~1_prior_only"),
  sample_prior = "only"
)

#'
#'
#'
#+
summary(mod4)


#'
#'
#'
#+
set.seed(12)
pp_check(mod4)

#'
#'
#'
#+
y <- president_heights$height_cm
yrep <- posterior_predict(mod4)


# ppc_dens_overlay(y, yrep) + xlim(50, 300)
ppc_stat(y, yrep, stat = "mean", binwidth = 5)
ppc_stat(y, yrep, stat = "max", binwidth = 5)


mcmc_areas(mod4, pars = "b_Intercept", prob = 0.8, prob_outer = 0.99)


mod5 <- brm(
  formula = f, 
  data = president_heights,
  family = gaussian(), 
  prior = prior("student_t(3, 178, 7.5)", class = "Intercept"),
  chains = 3, 
  file = here("models/height_cm~1_prior_only2"),
  sample_prior = "only"
)

#'
#' Generating posterior predictive samples.
#' 
#+
yrep <- posterior_predict(mod5)


#'
#' We can check mean and max values in replications:
#'
#+
ppc_stat(y, yrep, stat = "median", binwidth = 5)
ppc_stat(y, yrep, stat = "max", binwidth = 5)

#'
#' Or look at custom summary statistics, e.g. quantiles
#' 
#' 25% quantile
#+
q25 <- function(x) quantile(x, 0.25)
ppc_stat(y, yrep, stat = "q25", binwidth = 5)

q75 <- function(x) quantile(x, 0.75)
ppc_stat(y, yrep, stat = "q75", binwidth = 5)

#'
#' or plot central quantile posterior interval estimates
#'
#+
mcmc_areas(mod5, pars = "b_Intercept", prob = 0.50, prob_outer = 0.9)







