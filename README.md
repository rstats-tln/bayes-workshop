
# Bayesian statistics workshop Oct. 2021

<!-- badges: start -->
<!-- badges: end -->


Thank you for your registration to the Bayesian statistics workshop.    

We start at 10 AM each morning and finish roughly at 17:45 PM.   

Location: Akadeemia tee 3, rooms SOC-413 (Oct 2, 3, 17) and SOC-418 (Oct 16). The rooms should be easy to find.    

Bring your laptop. We will use RStudio Cloud for the hands-on sessions, so there is no need to install any software beforehand. However, as RStudio Cloud runs in your browser it can sometimes be slow. So, if you wish, you can download R (https://www.r-project.org/), RStudio (https://www.rstudio.com/products/rstudio/download/), and Stan (https://mc-stan.org/users/interfaces/rstan) into your laptop. But if you have any problems with installations, then don’t worry – you can use RStudio Cloud.    

Coffee breaks and lunches will be provided.   

Don’t forget to bring your digital COVID certificate or a recent negative test.   


# Course materials

If you are not friends with git, whole repo (all materials included) can be downloaded as a zip file using "Download ZIP" button under green "Code" button.

- Newer materials on Bayesian modeling by Ülo are available in this repo in docs folder [Bayes_eng.pdf](docs/Bayes_eng.pdf) (in English) and [Bayes_est.pdf](docs/Bayes_est.pdf) (in Estonian). Rest of pdfs in the docs folder are Ülo slides. 

- Older materials (last updated Oct-2019) of Bayesian data analysis are available [here](https://rstats-tartu.github.io/bayesiraamat/) as an *ebook* (in Estonian).

- Scripts (and data) is stored in [scripts](scripts/) and [data](data/) folder, respectively. Rmd files in scripts folder need to have paths wrapped into `here` function for these paths to work.

# Software

Taavis course materials were developed using [rocker/verse:4.1.0](https://hub.docker.com/r/rocker/verse/tags) Docker image, followed by installation of following CRAN and non-cran libraries -- tidyverse, lubridate, here, brms, bayesplot, tidybayes, modelr, rstan, rethinking, loo, rstanarm, mice, naniar, qgcomp.

If you want to give a try to this setup, first you need to [get and install Docker](https://docs.docker.com/get-docker/).

Pull rocker/verse image
```bash
docker pull rocker/verse:4.1.0
```

Start local RStudio server and log in with user "rstudio" and password "yourpassword" (for pw, choose what ever you want) 
```bash
docker run -e PASSWORD=yourpassword --rm -p 8787:8787 -v /path/to/your/R/projects/folder:/home/rstudio rocker/verse:4.1.0
```

Then required additional R libraries can be installed using regular `install.packages()` command. 





