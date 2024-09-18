# Local files ------------------------------------------------------------------
source("tab_metabolomicData.R")
source("tab_compoundData.R")
source("tab_onlyMZ.R")
source("tab_mzAndRT.R")
source("tab_compoundSearch.R")
source("tab_GNPSconnection.R")
source("tab_calculateMZ.R")
source("tab_info.R")




# User interface ---------------------------------------------------------------
ui <- navbarPage(
  "MetS (Metabolomic Search)",
  
  # Habilita shinyjs
  useShinyjs(),
  
  
  ## Upload data section -------------------------------------------------------
  navbarMenu(
    "Upload data",
    
    ### Upload metabolomic data section ----------------------------------------
    tab_metabolomicData,
      
    ### Upload compound data section -------------------------------------------
    tab_compoundData
  ),
  
  
  
  ## Search for a specific molecule section ------------------------------------
  navbarMenu(
    "Search compound",
             
    ### Only with m/z value ----------------------------------------------------
    tab_onlyMZ,
    
    ### With m/z and RT values -------------------------------------------------
    tab_mzAndRT
  ),
  
  
  
  ## Search for a list of compounds --------------------------------------------
  navbarMenu(
    "Search many compounds",
    
    ### Compound list ----------------------------------------------------------
    tab_compoundSearch
  ),
  
  
  
  ## Connect with GNPS section -------------------------------------------------
  navbarMenu(
    "GNPS connection",
    
    ### Connect with GNPS section ----------------------------------------------
    tab_GNPSconnection
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