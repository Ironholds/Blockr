additional.fun <- function(){
  
  #Load in query-dependent config variables 
  source(file = file.path(getwd(),"config.r"))
  
  #Load in global functions
  source(file = file.path(getwd(),"functions.r"))
  
  #Piecewise analysis
  source(file = file.path(getwd(),"AdditionalComponents","piecewise.r"))
  
  #Check AbuseFilter/EditFilter data.
  source(file = file.path(getwd(),"AdditionalComponents","editfilters.r"))

}

#Run
additional.fun()