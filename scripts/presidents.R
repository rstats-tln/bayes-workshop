#' ---
#' title: "Starting with Bayes statistics by estimating mean"
#' subtitle: "Simple intercept-only model"
#' author: "Taavi Päll and Ülo Maiväli"
#' date: "2021-10-02"
#' output: html_document
#' ---


#'
#' ## Loading required libraries
#' 
#+
library(tidyverse)
library(here)

#' 
#' ## Getting data
#' 
#' ### Downloading US president heights data
#'  

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
#' We have two columns in our tibble -- presidents names "name" and height in 
#' cm "height_cm".
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
#' - The average height of the first 44 presidents is 180 cm.
#' 
#+
ggplot(data = president_heights) +
  geom_point(aes(x = height_cm, y = fct_reorder(name, height_cm))) +
  theme(axis.title.y = element_blank())

#'
#' Histogram shows that most frequently US presidents have been 72in tall.
#+
ggplot(data = president_heights) +
  geom_histogram(aes(x = height_cm), binwidth = 1) +
  scale_y_continuous(breaks = scales::pretty_breaks())
