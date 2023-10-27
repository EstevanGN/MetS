# ___________________________________________________________________
# Installing necessary libraries ------------------------------------
# ___________________________________________________________________
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



# ___________________________________________________________________
# Libraries ---------------------------------------------------------
# ___________________________________________________________________
library(shiny)          # basic for shiny apps
library(DT)             # Output tables
library(readr)          # Read big size files
library(tidyverse)      # %>% function
library(shinyvalidate)  # Input validation 



# ___________________________________________________________________
# Execute App -------------------------------------------------------
# ___________________________________________________________________
runApp('app')