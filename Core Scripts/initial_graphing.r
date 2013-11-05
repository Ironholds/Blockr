#Blockr - a project to accurately triage data on blocked Wikipedia users, identify
#the underlying rationales and test various hypotheses as to any outcome
#
#initial_graphing.r takes the data extracted by retrieve.r and maps it.
#
# @Year = 2013
# @Copyright: Oliver Keyes
# @License = MIT (http://opensource.org/licenses/MIT)

initial_graphing.fun <- function(){
  
  #Grab data
  data.ls <- retrieve_enclose.fun()
  
  #Report as such
  print("initial data retrieval complete")
  
  #Metadata for elements of data.ls
  #@1 Temporary object name to use
  #@2 Usergroup
  #@3 Type of data
  graphing_data.ls <- list(
    c("anonymous.df","anonymous","raw"),
    c("registered.df","registered","raw"),
    c("anonymous_norm.df","anonymous","normalised"),
    c("registered_norm.df","registered","normalised")
  )
  
  #Check for user error in expanding/modifying the Blockr codebase
  if(length(graphing_data.ls) != length(data.ls)){
    stop("There is an inconsistency between the graphing data and the number of dataframes to analyse")
    
  }else{
    
    #Iterate over the non-normalised data to produce juicy, juicy graphs
    for(i in 1:length(graphing_data.ls)){
      
      #Handle different datatypes
      if(graphing_data.ls[[i]][3] == "raw"){
        
        #Split the pertinent dataframe out of data.ls and add to a newly-created Blockr_vis object
        assign(graphing_data.ls[[i]][1],
          value = new("Blockr_vis",
            data = as.data.frame(data.ls[[i]]),
            yearly_data = data_aggregation.fun(x = as.data.frame(data.ls[[i]])),
            data_type = graphing_data.ls[[i]][3],
            user_group = graphing_data.ls[[i]][2]
          )
        )
     
        #Graph
        get(graphing_data.ls[[i]][1])$initial_graph.fun()

      } else{ #And for normalised data...
        
        assign(graphing_data.ls[[i]][1],
         value = new("Blockr_vis_proportion",
           data = as.data.frame(data.ls[[i]]),
           yearly_data = data_aggregation.fun(x = as.data.frame(data.ls[[i]])),
           data_type = graphing_data.ls[[i]][3]
         )
        )
        
        #Graph
        get(graphing_data.ls[[i]][1])$initial_graph.fun()
        
        #Do some time-series analysis, to boot
        get(graphing_data.ls[[i]][1])$timeseries.fun()
        
      }
    }
  }
  
  #Report
  print("initial graphing complete")
}
