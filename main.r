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

#Create output folder, if it doesn't already exist.
dir.create(file.path(getwd(), "Output"), showWarnings = FALSE)

#Load in the enclosing functions for basic analysis, and run
source(file = file.path(getwd(), "ComponentFiles", "registration_data.r"))

print("Run 33 percent complete")

source(file = file.path(getwd(), "ComponentFiles", "ipblock.r"))

print("Run 66 percent complete")

#Load in the enclosing function for logging table analysis, and run
source(file = file.path(getwd(), "ComponentFiles", "logging.r"))

print("Run 100 percent complete")

#quit
q(save = "no")