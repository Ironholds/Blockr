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

# Fit values
fitvals <- function(df){
  
  #Generate list of unique regexes
  regex_uniques <- unique(df$regex)
    
  #Generate list of unique timestamps, repeating by the number of uniques
  uniques <- rep(x = unique(df$month),
                 times = length(regex_uniques))
  
  #Format as data frame
  template.df <- as.data.frame(uniques)
  template.df$uniques <- template.df$uniques[order(template.df$uniques)]
  template.df <- cbind(template.df,regex_uniques)
  names(template.df) <- c("month","regex")
  template.df$hits <- NA
  
  #Merge
  results.df <- ddply(.data = template.df,
                      .variables = c("month","regex"),
                      .fun = function(x){
                        
                        if(length(df$hits[df$month == x$month & df$regex == x$regex]) == 0){
                          
                          x$hits <- 0
                          
                        } else {
                          
                          x$hits <- df$hits[df$month == x$month & df$regex == x$regex]
                        }
                        
                        return(x)
                      }
  )
  
  #Return
  return(results.df)
}