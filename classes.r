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
      for(i in 1:length(regex.ls)){
        
        #Run regexes
        grepvec <- grepl(pattern = regex.ls[[i]][2],
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
    regex_container.fun = function(data,var,rename_strings){
      
      #Use ddply to iterate over each time period
      to_output <- ddply(.data = data,
        .variables = var,
        .fun = .self$ddply_loop.fun
      )
      
      #Do we need to rename?
      if(!missing(rename_strings)){#Yep?
        
        to_output <- rename(to_output, replace = rename_strings)
        
      }
      
      #Either way, factorise and return
      to_output$timestamp <- as.factor(to_output$timestamp)
      
      return(to_output)
    }
  )
)

#Hand-coding class - child of Blockr_base, overwrites ddply_loop.fun
#This is used both to produce a hand-coding sample, and to generate data that can be used for a proportion analysis
Blockr_base_handcode <- setRefClass("Blockr_base_handcode",
  fields = list(data = "data.frame", sample_size = "numeric"), #Note that it requires a set sample size to be valid
  contains = "Blockr_base",
  methods = list(
    
    ddply_loop.fun = function(x){
      
      #Fix input data, and sample as appropriate
      input_data.df <- trickstr::dfsample(df = x, size = .self$sample_size) #Sample is based on the object's sample_size value
      
      #Initialise export object
      to_return.df <- data.frame()
      
      #For loop
      for(i in 1:length(regex.ls)){
        
        #Check contents of input_data.df in case all possible matches have already been found
        if(nrow(input_data.df) > 0){
          
          #Run regexes
          grepvec <- grepl(pattern = regex.ls[[i]][2],
                           x = input_data.df$reason,
                           perl = TRUE,
                           ignore.case = TRUE)
          
          #Fix matches
          matches.df <- input_data.df[grepvec,]
          
          #Add match number
          if(nrow(matches.df) > 0){
            matches.df$matched_regex <- regex.ls[[i]][1]
            
            #Grab and return matches
            to_return.df <- rbind(to_return.df,matches.df)
            
          }
          #Non-matches
          input_data.df <- input_data.df[!grepvec,]
        }
      }
      
      #Mark non-matches
      if(nrow(input_data.df) > 0){
        
        input_data.df$matched_regex <- "Misc"
        
        #Add to exporting object
        to_return.df <- rbind(to_return.df,input_data.df)
        
      }
      
      #Return
      return(to_return.df)
      
    }
  )
)

#Base visualisation class
Blockr_vis <- setRefClass("Blockr_vis",
  fields = list(data = "data.frame", yearly_data = "data.frame", data_type = "character", user_group = "character"), #Includes generic data.frame functions.
  methods = list(
    
    #Initial graphing function
    initial_graph.fun = function(){
    
      #Simple line graph of monthly data
      monthly_line_graph <- ggplot(.self$data, aes(timestamp, value)) + 
        geom_freqpoly(aes(group = variable, colour = variable), stat = "identity") +
        labs(x = "Month", y = "Number of blocks") +
        ggtitle(paste("Block rationales on the English-language Wikipedia by month\n",sql_start.str,"-",sql_end.str,.self$user_group,"users", sep = " ")) +
        scale_x_discrete(breaks = seq(from = as.numeric(sql_start.str), to = as.numeric(sql_end.str), by = 100), expand = c(0,0)) +
        scale_y_continuous(expand = c(0, 0)) +
        theme(axis.text.x = element_text(angle = 90, hjust = 1))
      
      #Print
      ggsave(filename = file.path(getwd(),"Graphs",paste(.self$usergroup,.self$data_type,"monthly_line_graph.png",sep = "_")),
             plot = monthly_line_graph,
             width = 8,
             height = 8,
             units = "in")
      
      #Monthly, with points and simple linear regression.
      monthly_regression_graph <- ggplot(.self$data,aes(x = timestamp,y = value, colour = variable))+
        geom_point(shape=3) +
        geom_smooth(method = lm, se = TRUE, aes(group= variable)) +
        labs(x = "Month", y = "Number of blocks") +
        ggtitle(paste("Block rationales on the English-language Wikipedia by month\n",sql_start.str,"-",sql_end.str,.self$user_group,"users", sep = " ")) +
        scale_x_discrete(breaks = seq(from = as.numeric(sql_start.str), to = as.numeric(sql_end.str), by = 100), expand = c(0,0)) +
        scale_y_continuous(expand = c(0,0)) +
        theme(axis.text.x = element_text(angle = 90, hjust = 1))
      
      #Print
      ggsave(filename = file.path(getwd(),"Graphs",paste(.self$usergroup,.self$data_type,"monthly_linear_regression_graph.png",sep = "_")),
             plot = monthly_regression_graph,
             width = 8,
             height = 8,
             units = "in")
      
      #Yearly summary
      year_line_graph <- ggplot(.self$yearly_data, aes(timestamp, value)) + 
        geom_freqpoly(aes(group = variable, colour = variable), stat = "identity") +
        labs(x = "Year", y = "Number of blocks") +
        ggtitle(paste("Block rationales on the English-language Wikipedia by year\n (2006-2012)\n",table,"table,",usergroup,"users", sep = " ")) +
        scale_x_discrete(breaks = seq(from = as.numeric(sql_year_start.str), to = as.numeric(sql_year_end.str), by = 1), expand = c(0,0)) +
        scale_y_continuous(expand = c(0, 0)) +
        theme(axis.text.x = element_text(angle = 90, hjust = 1))
      
      #Print
      ggsave(filename = file.path(getwd(),"Graphs",paste(.self$usergroup,.self$data_type,"yearly_line_graph.png",sep = "_")),
             plot = year_line_graph,
             width = 8,
             height = 8,
             units = "in")
    }
  }
}

Blockr_vis_proportion <- setRefClass("Blockr_vis_proportion",
  contains = "Blockr_vis",
  fields = list(data = "data.frame"), #Includes generic data.frame functions.
  methods = list(
    
    
    #Time-series analysis for the normalised data
    timeseries.fun = function(){
      
      
      
    }
    
    
    ))