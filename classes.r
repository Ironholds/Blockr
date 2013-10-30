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
    loop_contents.fun = function(data){
      
      
      
      
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
        .fun = function(x){
          
          #Fix input data in place
          input_data.df <- x
          
          #Create empty dataframe to be exported
          loop_output_data.df <- data.frame()
          
           for(i in 1:length(regex.vec)){ #Loop over each set of regular expressions
             
             output.list <- loop_contents.fun(data = input_data.df) #Run loop_contents.fun, exporting a list
             
             input_data.df <- as.data.frame(output.list[1]) #Fling the first list element back in as input data for the next run
             
             loop_output_data.df <- cbind(loop_output_data.df,) #Fling the second list element out, binding it into the newly initialised object for exporting
           }
           
           
           
         }
      )
      
      #Do we need to rename?
      if(!missing(rename)){#Yep?
        
        to_output <- rename(x = to_output, replace = rename)
        
      }
        
      #Either way, return
      return(to_output)
    },
    
    #Quick aggregation function, to deal with year/month conversion
    #@x = input dataframe
    data_aggregation.fun = function(x){
      
      #Substring
      x$timestamp <- substring(x,1,6)
      
      #Aggregate
      to_output <- ddply(.data = x,
                         .var = "timestamp",
                         .fun = function(x))
      
      
    }
  )
)