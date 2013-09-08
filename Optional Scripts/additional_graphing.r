#Blockr - a project to accurately triage data on blocked Wikipedia users, identify
#the underlying rationales and test various hypotheses as to any outcome
#
#additional_graphing.r extracts information from the Wikimedia db to test the various different hypotheses.
#
# @Year = 2013
# @Copyright: Oliver Keyes
# @License = MIT (http://opensource.org/licenses/MIT)

#Wrapping function
additional_graph.fun <- function(){
  
  #Read non-context-specific data in
  registrations.df <- read.delim(file = file.path(getwd(),"Data","registrations.tsv"), as.is = TRUE, header = TRUE)
  registrations_with_edits.df <- read.delim(file = file.path(getwd(),"Data","registrations_with_edits.tsv"), as.is = TRUE, header = TRUE)
  
  #Take filter hits and subset
  filter.fun <- function(){
    
    #Take filter hits
    editfilters.df <- read.delim(file = file.path(getwd(),"Data","editfilter_hits.tsv"), as.is = TRUE, header = TRUE)

  }
  
  #Run
  filter_data.ls <- filter.fun()
  
  #Function to iterate on for comparisons
  iterate.fun <- function(table,usergroup){
    
    #Read in pertinent data
    block_data.df <- read.delim(file = file.path(getwd(),"Data",paste(table,usergroup,"regex_matches.tsv",sep = "_")), as.is = TRUE, header = TRUE)
    
    #Subset
    block_data.df <- block_data.df[block_data.df$variable = "bad.edit",]
    
  }
  
}

#Run
additional_graphs.fun()