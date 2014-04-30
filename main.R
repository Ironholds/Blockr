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

#Load config files and functions
source(file = file.path(getwd(),"config.R")) #Config variables and packages
ignore <- lapply(list.files(file.path(getwd(),"Functions"), full.names = TRUE), source)

blockr_initial <- function(){
  
  #Read in data
  input_data <- data_reader()
  
  #For each type of data...
  for(i in seq_along(input_data)){
    
    #If it's anonymous...
    if(input_data[[i]]$userid[1] == 0){
      
      #Set anonymous as a user type
      usergroup <- "anonymous"
      
    } else {
      
      #Otherwise, registered
      usergroup <- "registered"
      
    }
    
    #Run the regular expressions over the data
    regex_results <- regexer(input_data[[i]])
    
    #Expand to allow for null entries
    regex_results <- fitvals(regex_results)
    
    #Save to file
    write.table(x = regex_results,
                file = file.path(getwd(),"Data",paste(usergroup,"regex_hits.tsv", sep = "_")),
                quote = TRUE,
                sep = "\t",
                row.names = FALSE)
    
    #Graph
    grapher(x = regex_results, usergroup = usertype)
  }
  
}


#Run
blockr_initial()

#Quit
q(save = "no")