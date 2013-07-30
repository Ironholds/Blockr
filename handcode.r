handcode.fun <- function(){
  
  #Load in query-dependent config variables.
  source(file = file.path(getwd(),"config.r"))
  
  #Load in global functions.
  source(file = file.path(getwd(),"functions.r"))
  
  #Grab data to be randomly tested.
  source(file = file.path(getwd(),"TestComponents","Retrieve.r"))
  
  #Analyse hand-coding output.
  source(file = file.path(getwd(),"TestComponents","HandCodingAnalysis.r"))
  
}

#Run
handcode.fun()