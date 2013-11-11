#Blockr - a project to accurately triage data on blocked Wikipedia users, identify
#the underlying rationales and test various hypotheses as to any outcome
#
#initial_graphing.r takes the data extracted by retrieve.r and maps it, performing simple
#graphing and linear regression for non-proportionate data, and adding nonparametric regression
#in for the proportionate data
# 
# Copyright (c) 2013 Oliver Keyes
#   
# Permission is hereby granted, free of charge, to any person obtaining a copy
# of this software and associated documentation files (the "Software"), to deal
# in the Software without restriction, including without limitation the rights
# to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
# copies of the Software, and to permit persons to whom the Software is
# furnished to do so, subject to the following conditions:
#   
# The above copyright notice and this permission notice shall be included in
# all copies or substantial portions of the Software.
# 
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
# AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
# OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
# THE SOFTWARE.

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
      
      #Time-series analysis
      if(get(graphing_data.ls[[i]][1]$data_type) == "raw"){
        
        get(graphing_data.ls[[i]][1])$timeseries.fun()
        
      }
    }
  }
  
  #Report
  print("initial graphing complete")
}
