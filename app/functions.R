# Possible RTs from a specific mzValue -----------------------------------------
possibleRT <- function(dataSamples, mzColumn, rtColumn, mz, interval){
  mzTarget <- dataSamples %>%
    filter(dataSamples[,mzColumn] >= mz-interval, 
           dataSamples[,mzColumn] <= mz+interval)
  return(data.frame(mzTarget)[,rtColumn])
}



# With a specific RT searching all mz near (with correlation) ------------------
mzNear <- function(
    dataSamples, 
    rowColumn, 
    rt, 
    rtColumn, 
    interval, 
    mz, 
    mzColumn, 
    cor, 
    sampColumn
){
  
  samColumns <- as.numeric(
    strsplit(sampColumn, "-")[[1]]
  )
  
  rtTarget <- dataSamples %>%
    filter(dataSamples[,rtColumn] >= rt-interval, 
           dataSamples[,rtColumn] <= rt+interval )
  
  indColumns <- samColumns[1]:samColumns[2]
  matrixRT <- as.matrix(t(rtTarget[,indColumns]))
  
  idMzValue <- which.min(
    abs(rtTarget[,mzColumn] - mz)
  )
  
  idCorrelation <- cor(matrixRT, method="pearson")[idMzValue,] > cor
  columnsNames <- names(dataSamples)[indColumns]
  sampMaxIntensity <- NULL
  
  focusTable <- rtTarget[idCorrelation,]
  
  for(i in 1:nrow(focusTable)){
    sampMaxIntensity[i] <- paste0(
      columnsNames[which.max(focusTable[i,indColumns])],
      " (",
      max(focusTable[i,indColumns]),
      ")"
    )
  }
  
  result <- cbind(
    data.frame(
      focusTable
    )[,c(rowColumn, mzColumn, rtColumn)],
    "Corr"=cor(matrixRT, method="pearson")[idMzValue,][idCorrelation],
    "Samp.max.int"=sampMaxIntensity
  )
  
  return(result)
}



# With a table from mzNear calculate distance to fragments ---------------------
fragDistance <- function(result, fragment){
  distanceToFragments <- NULL
  distance <- NULL
  
  for(j in 1:length(fragment)){
    for(k in 1:dim(result)[1]){
      distance[k] <- 
        abs(result[k,2]-fragment[j])
    }
    distanceToFragments[j] <- min(distance)
    distance <- NULL
  }
  return( sum(distanceToFragments) )
}



# With a set of RTs, searching for the closer to mz fragments ------------------
closerFragmentation <- function(
    dataSamples,
    rowColumn,
    rtSet,
    rtColumn,
    interval, 
    mz, 
    mzColumn,
    cor, 
    sampColumn,
    fragment
){
  distanceTable <- matrix(0, length(rtSet), 3)
  
  if("" %in% fragment){
    # If there are no fragments
    
    for(i in 1:length(rtSet)){
      result <- mzNear(
        dataSamples,
        rowColumn,
        rtSet[i],
        rtColumn,
        interval, 
        mz,
        mzColumn,
        cor,
        sampColumn
      )
      
      totalSum <- min(abs(result[,2] - mz))
      distanceTable[i,] <- c(i, rtSet[i], totalSum)
    }
    
  }else{
    # If there are fragments
    
    for(i in 1:length(rtSet)){
      result <- mzNear(
        dataSamples,
        rowColumn,
        rtSet[i],
        rtColumn,
        interval, 
        mz,
        mzColumn,
        cor,
        sampColumn
      )
      
      totalSum <- fragDistance(result, fragment)
      
      distanceTable[i,] <- c(i, rtSet[i], totalSum)
    }
  }
  
  return(
    data.frame("RT_id"=distanceTable[which.min(distanceTable[,3]),1],
               "RT"=distanceTable[which.min(distanceTable[,3]),2],
               "distance"=distanceTable[which.min(distanceTable[,3]),3])
  )
}



# Print all possible tables with Rt near ---------------------------------------
printResults <- function(
    dataSamples,
    rowColumn,
    rtSet,
    rtColumn,
    interval, 
    mz, 
    mzColumn,
    cor,
    sampColumn,
    fragment
){
  
  completeResults <- data.frame(
    "Group_id" = character(0),
    "Score" = numeric(0),
    "Parental_RT" = numeric(0),
    "Row_names" = character(0),
    "mz" = numeric(0),
    "RT" = numeric(0),
    "Correlation" = numeric(0)
  )
  
  for(i in 1:length(rtSet)){
    partialResult <- mzNear(dataSamples, rowColumn, rtSet[i], rtColumn, 
                            interval, mz, mzColumn, cor, sampColumn)
    group <- rep(i, nrow(partialResult))
    if("" %in% fragment){
      sco <- rep( 
        min(abs(partialResult[,2] - mz)), 
        nrow(partialResult)
      )
    }else{
      sco <- rep( 
        fragDistance(partialResult, fragment), 
        nrow(partialResult)
      )
    }
    parental <- rep(rtSet[i], nrow(partialResult))
    
    completeResults <- rbind(
      completeResults,
      cbind(group, sco, parental, partialResult)
    )
  }
  
  return(completeResults)
}



# Create a data frame with empty entries for Best Results ----------------------
empty_bestResult <- function(){
  df <- data.frame(
    "Row.names"   = c("No data found. Please modify the parameters."),
    "mz"         = c(NA),
    "RT"          = c(NA),
    "Correlation" = c(NA),
    "Sample.max.intensity" = c(NA)
  )
  
  return(df)
}



# Create a data frame with empty entries for Complete Results ------------------
empty_completeResult <- function(){
  df <- data.frame(
    "Group.id"    = c("No data found. Please modify the parameters."),
    "Score"       = c(NA),
    "Parental.RT" = c(NA),
    "Row.names"   = c(NA),
    "mz"         = c(NA),
    "RT"          = c(NA),
    "Correlation" = c(NA),
    "Sample.max.intensity" = c(NA)
  )
  
  return(df)
}



# Create a data frame for Best Results -----------------------------------------
bestResultTable <- function(
  dataSamples, 
  rowColumn, 
  rt, 
  rtColumn, 
  interval, 
  mz, 
  mzColumn, 
  cor, 
  sampColumn
){
  df <- mzNear(
    dataSamples, 
    rowColumn, 
    rt, 
    rtColumn, 
    interval, 
    mz, 
    mzColumn, 
    cor, 
    sampColumn
  )
  
  rownames(df) <- NULL
  colnames(df) <- c("Row.names", "mz", "RT", 
                    "Correlation", "Sample.max.intensity")
  numeric_cols <- sapply(df, is.numeric)
  df[numeric_cols] <- lapply(
    df[numeric_cols], function(x) round(x, 5)
  )
  
  return(df)
}



# Create a data frame for Complete Results -------------------------------------
completeResultTable <- function(
  dataSamples,
  rowColumn,
  rtSet,
  rtColumn,
  interval, 
  mz, 
  mzColumn,
  cor,
  sampColumn,
  fragment
){
  df <- printResults(
    dataSamples,
    rowColumn,
    rtSet,
    rtColumn,
    interval, 
    mz, 
    mzColumn,
    cor,
    sampColumn,
    fragment
  )
  
  rownames(df) <- NULL
  colnames(df) <- c("Group.id", "Score", "Parental.RT", 
                    "Row.names", "mz", "RT", "Correlation",
                    "Sample.max.intensity")
  numeric_cols <- sapply(df, is.numeric)
  df[numeric_cols] <- lapply(
    df[numeric_cols], function(x) round(x, 5)
  ) 
  
  return(df)
}