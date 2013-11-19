#Blockr - a project to accurately triage data on blocked Wikipedia users, identify
#the underlying rationales and test various hypotheses as to any outcome
#
# @Year = 2013
# @Copyright: Oliver Keyes
# @License = MIT (http://opensource.org/licenses/MIT)

#Load
source(file = file.path(getwd(),"config.r")) #Config variables and packages
source(file = file.path(getwd(),"functions.r")) #Global functions
source(file = file.path(getwd(),"globalstrings.r")) #Global strings
source(file = file.path(getwd(),"classes.r")) #Classes

#Create output folders, if they don't already exist.
dir.create(file.path(getwd(), "Data"), showWarnings = FALSE)
dir.create(file.path(getwd(), "Graphs"), showWarnings = FALSE)

run.fun <- function(){
  
  runtype <- readline(prompt = "Type 'full' for a full run, or 'limited' for a limited run\n")
  
  if(runtype %in% c("full","limited")){
    
    source(file.path(getwd(),"exploratory_analysis.r"))
    
    if(runtype == "full"){
            
    }
    
    cat("Thank you. Run complete.\n")

  }
  
  else{
    
    cat("this is not a recognised run type\n")
  }
}

#Run
run.fun()