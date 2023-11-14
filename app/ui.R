# Local files ------------------------------------------------------------------
source("tab_metabolomicData.R")
source("tab_compoundData.R")
source("tab_onlyMZ.R")
source("tab_mzAndRT.R")
source("tab_calculateMZ.R")
source("tab_info.R")
source("tab_compoundSearch.R")



# User interface ---------------------------------------------------------------
ui <- navbarPage(
  "Metabolomic Search",
  
  
  
  ## Upload data section -------------------------------------------------------
  navbarMenu(
    "Upload Data",
    
    ### Upload metabolomic data section ----------------------------------------
    tab_metabolomicData,
      
    ### Upload compound data section -------------------------------------------
    tab_compoundData
  ),
  
  
  
  ## Search for molecules section ----------------------------------------------
  navbarMenu(
    "Search",
             
    ### Only with m/z value ----------------------------------------------------
    tab_onlyMZ,
    
    ### With m/z and RT values -------------------------------------------------
    tab_mzAndRT,
    
    ### Compound list ----------------------------------------------------------
    tab_compoundSearch
  ),
  
  
  
  ## More tools section --------------------------------------------------------
  navbarMenu(
    "More tools",
             
    ### Calculate mz section ---------------------------------------------------
    tab_calculateMZ,
             
    ### Information section ----------------------------------------------------
    tab_info
  )
)