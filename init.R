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



# ___________________________________________________________________
# Libraries ---------------------------------------------------------
# ___________________________________________________________________
library(shiny)      # basic for shiny apps
library(DT)         # Output tables
library(readr)      # Read big size files
library(tidyverse)  # %>% function



# ___________________________________________________________________
# Execute App -------------------------------------------------------
# ___________________________________________________________________
runApp('app')