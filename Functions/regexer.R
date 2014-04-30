# Copyright (c) 2014 Oliver Keyes
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

regexer <- function(input_data){
  
  #For each regex...
  regex_results <- lapply(regex.ls, function(x){
    
    #If there are more than 0 rows in the dataset...
    if(nrow(input_data.df) > 0){
      
      #Run the regex that serves as x[2] over the input data
      grepvec <- grepl(pattern = x[2],
                       x = input_data$reason,
                       perl = TRUE,
                       ignore.case = TRUE)
      
      #If there are any hits...
      if(sum(grepvec) > 0){
        
        #Create an aggregate table of entries with hits
        to_return <- as.data.frame(table(input_data$timestamp[grepvec]), stringsAsFactors = FALSE)
        
        #Export non-matched rows back into the parent environment for use with the next regex
        assign(x = "input_data",
               value = input_data[!grepvec,],
               envir = parent.env(environment()))
        
        #Add regex name to the aggregate table
        to_return$regex <- x[1]
        
        #Rename object
        names(to_return) <- c("month","hits","regex")
        
        #Return it
        return(to_return)
      }
    }
    
    
  })
  
  #Aggregate remainder, adding "misc"
  regex_nonhits <- as.data.frame(table(input_data$timestamp))
  regex_nonhits$regex <- "misc"
  names(regex_nonhits) <- c("month","hits","regex")
  regex_results[[length(regex_results)+1]] <- regex_nonhits
  
  #Bind into a single df and return
  return(do.call("rbind",regex_results))
}