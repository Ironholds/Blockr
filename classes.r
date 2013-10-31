# classes.r contains custom ReferenceClasses applied to objects in Blockr
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

#Base, parent class for Blockr objects.
#This contains the basic methods for parsing block entries,
#and applies to data.frames quite happily.
Blockr_base <- setRefClass("Blockr_base",
  fields = list(data = "data.frame"), #Includes generic data.frame functions.
  methods = list(
    
    #Initial contents of regex_loop.fun
    #Obviously for the parent class we want the most common use case, which is returning aggregate numbers
    #@x = input data
    ddply_loop.fun = function(x){
      
      #Fix input data
      input_data.df <- x
      
      #Initialise export object
      to_return.vec <- vector()
      
      #For loop
      for(i in 1:length(regex.vec)){
        
        #Run regexes
        grepvec <- grepl(pattern = regex.vec[i],
                         x = input_data.df$reason,
                         perl = TRUE,
                         ignore.case = TRUE)
        
        #Number of rows that match
        to_return.vec[i] <- sum(grepvec)
        
        #Non-matches
        input_data.df <- input_data.df[!grepvec,]
      }
      
      #Include non-matches
      to_return.vec[length(to_return.vec)+1] <- nrow(input_data.df)
      
      #Return
      return(to_return.vec)
    },

    #Function for looping regexes - uses loop_contents.fun, allowing for modification of the actual
    #nuts and bolts in child classes without reinventing the /entire/ wheel
    #@data = the input dataframe
    #@var = the variable(s) to loop over in ddply
    #@rename = the list of vectors/new names for those vectors - see rename() in the plyr documentation for examples
    regex_loop.fun = function(data,var,rename){
      
      #Use ddply to iterate over each time period
      to_output <- ddply(.data = data,
        .variables = var,
        .fun = .self$ddply_loop.fun
      )
      
      #Do we need to rename?
      if(!missing(rename)){#Yep?
        
        names(to_output) <- replace.vec
        
      }
      
      #Either way, return
      return(to_output)
    }
  )
)