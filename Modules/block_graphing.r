# block_graphing.r is a module that generates basic graphing of the data outputted by exploratory_analysis.r
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
block_graphing.fun <- function(){

  #Message
  cat("Starting initial graphing\n")
  
  #Grab dataset combinations
  datanames.ls <- list(c("anonymous","proportionate"),
                       c("registered","proportionate"),
                       c("anonymous","disproportionate"),
                       c("registered","disproportionate"))
  
  #Loop-de-loop
  to_dispose <- lapply(datanames.ls, function(x){
    
    #Read in the file.
    data.df <- read.delim(file.path(
      getwd(),"Data",paste(as.character(x[1]),"_",as.character(x[2]),".tsv", sep = "")),
      as.is = TRUE, header = TRUE)
    
    #Check if it's a 
    if(as.character(x[2]) == "disproportionate"){
      
      data.df <- ddply(.data = data.df,
                   .var = "timestamp",
                   .fun = function(x){
                     
                     #Grab new line to insert
                     value_sum.vec <- c(x[1,1],"sum",sum(x$value))
                     
                     #Insert line and return
                     x[nrow(x)+1, ] <- value_sum.vec
                     return(x)
                   }
      )
      
      #Factor
      data.df$variable <- as.factor(data.df$variable)
      
      #Throw into class
      graphing_data.obj <- Blockr_vis$new(data = data.df,
                                          data_type = as.character(x[2]),
                                          user_group = as.character(x[1]))
    } else {
      
      #Factor
      data.df$variable <- as.factor(data.df$variable)
      
      #Throw into class
      graphing_data.obj <- Blockr_vis_proportionate$new(data = data.df,
                                          data_type = as.character(x[2]),
                                          user_group = as.character(x[1]))
      
    }
    
    #Either way, run graphing function
    graphing_data.obj$graphing.fun()
    
  })
  
  #Send the user a lovely message.
  cat("Initial graphing complete \n")
}