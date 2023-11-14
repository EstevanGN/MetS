# Installing necessary libraries -----------------------------------------------
if(!require(shiny)){
  install.packages("shiny")
}
if(!require(DT)){
  install.packages("DT")
}
if(!require(readr)){
  install.packages("readr")
}
if(!require(tidyverse)){
  install.packages("tidyverse")
}
if(!require(shinyvalidate)){
  install.packages("shinyvalidate")
}
if(!require(shinybusy)){
  install.packages("shinybusy")
}
if(!require(MSbox)){
  install.packages("MSbox")
}



# Libraries --------------------------------------------------------------------
library(shiny)          # basic for shiny apps
library(DT)             # Output tables
library(readr)          # Read big size files
library(tidyverse)      # %>% function
library(shinyvalidate)  # Input validation 
library(shinybusy)      # App indicators
library(MSbox)          # Calculate mz



# Local files ------------------------------------------------------------------
source("app/functions.R")



# Shiny options ----------------------------------------------------------------
options(shiny.maxRequestSize=3000*1024^2)



# Execute App -------------------------------------------------------
runApp("app")