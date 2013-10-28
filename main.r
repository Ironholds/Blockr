#Blockr - a project to accurately triage data on blocked Wikipedia users, identify
#the underlying rationales and test various hypotheses as to any outcome
#
# @Year = 2013
# @Copyright: Oliver Keyes
# @License = MIT (http://opensource.org/licenses/MIT)

#Load in query-dependent config variables 
source(file = file.path(getwd(),"config.r"))

#Load in global functions
source(file = file.path(getwd(),"functions.r"))

#Create output folders, if they don't already exist.
dir.create(file.path(getwd(), "Data"), showWarnings = FALSE)
dir.create(file.path(getwd(), "Graphs"), showWarnings = FALSE)

run.fun <- function(){
  
  runtype <- readline(prompt = "Type 'full' for a full run, or 'limited' for a limited run\n")
  
  if(runtype %in% c("full","limited")){
    
    cat("Thank you. Beginning run.\n")
    
    source(file.path(getwd(),"Core Scripts","retrieve.r"))
    
    cat("initial data retrieved.\n")
    
    source(file.path(getwd(),"Core Scripts","initial_graphing.r"))
    
    cat("initial graphing complete.\n")
    
    if(runtype == "full"){
            
      source(file.path(getwd(),"Optional Scripts","additional_graphing.r"))
      
      cat("additional retrieval and graphing complete.\n")
    }
    
    cat("Thank you. Run complete.\n")

  }
  
  else{
    
    cat("this is not a recognised run type\n")
  }
}

#Run
run.fun()