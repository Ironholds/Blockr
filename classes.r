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
  fields = list(data = "data.frame", user_type = "character", sample_size = "character"), #Includes generic data.frame functions.
  methods = list(
    
    #Raw data processing method
    data_process.fun = function(){
      
      #Retrieve data
      retrieved_data.df <- ddply(.data = .self$data,
                                 .var = "timestamp",
                                 .fun = function(x){
                                   
                                   #Fix the raw data as input_data.df, and create output object
                                   input_data.df <- x
                                   output_data.df <- data.frame()
                                   
                                   #Use lapply rather than a for loop. Microbenchmarks show a substantial performance improvement.
                                   lapply_output <- lapply(regex.ls,function(x){
                                     
                                     if(nrow(input_data.df) > 0){
                                       
                                       #Run regexes in regex.ls over input data, one by one
                                       grepvec <- grepl(pattern = x[2],
                                                        x = input_data.df$reason,
                                                        perl = TRUE,
                                                        ignore.case = TRUE)
                                       
                                       if(sum(grepvec) > 0){
                                         #Output refined input data, using assign() 
                                         #due to lapply's distinct environment for 
                                         #function calls
                                         assign(x = "input_data.df",
                                                value = input_data.df[!grepvec,],
                                                envir = parent.env(environment()))
                                         
                                         #Generate data to output, and output
                                         lapply_output.df <- input_data.df[grepvec,]
                                         lapply_output.df$regex <- as.character(x[1])
                                         
                                         assign("output_data.df",
                                                value = rbind(output_data.df,lapply_output.df),
                                                envir = parent.env(environment()))
                                       }
                                     }
                                   }
                                   )
                                   
                                   
                                   #Add in remaining input data
                                   input_data.df$regex <- "misc"
                                   to_output <- rbind(output_data.df,input_data.df)
                                   
                                   #Return
                                   return(to_output)
                                 }
      )
      
      #Return
      .self$data <- retrieved_data.df
      
    },
    
    #Aggregate data
    aggregation.fun = function(){
      
      #Aggregate
      aggregated.df <- ddply(.data = .self$data,
                             .var = "timestamp",
                             .fun = function(x){
                               
                               return(as.data.frame(table(x$regex)))
                             }
      )
      
      #Rename
      names(aggregated.df)[2:3] <- c("variable","value")
      
      #Defactor
      aggregated.df$variable <- as.character(aggregated.df$variable)
      
      #Return
      return(aggregated.df)
    },
    
    #Sampling function
    sample.fun = function(){
            
      #Sample
      sampled_data.df <- ddply(.data = .self$data,
                               .var = "timestamp",
                               .fun = function(x){
                                 
                                 sample.df <- trickstr::dfsample(x,as.numeric(.self$sample_size))
                                 
                                 return(sample.df)
                               }
                               
      )
      
      #Write into the raw_data field of the parent object
      return(sampled_data.df)
      
    },
    
    #Function to tie them all together
    grouping.fun = function(){
      
      #Process data
      data_process.fun()
      
      #Retrieve aggregates and save
      aggregated.df <- .self$aggregation.fun()
      export_file_path <- file.path(getwd(),"Data",paste(as.character(.self$user_type),"_disproportionate",".tsv",sep = ""))
      write.table(aggregated.df, file = export_file_path, col.names = TRUE,
                  row.names = FALSE, sep = "\t", quote = FALSE)
      
      #Sample original data
      sample.df <- .self$sample.fun()
      
      #Return
      return(sample.df)
      
    }
      
  )
)

#Hand-coding class - child of Blockr_base, overwrites ddply_loop.fun
#This is used both to produce a hand-coding sample, and to generate data that can be used for a proportion analysis
Blockr_base_handcode <- setRefClass("Blockr_base_handcode",
  contains = "Blockr_base", #Inherit Blockr_base, and thus those fields and methods.
  methods = list(
    
    #Function to tie them all together
    grouping.fun = function(){
            
      #Retrieve aggregates and save
      aggregated.df <- .self$aggregation.fun()
      export_file_path <- file.path(getwd(),"Data",paste(as.character(.self$user_type),"_proportionate",".tsv",sep = ""))
      write.table(aggregated.df, file = export_file_path, col.names = TRUE,
                  row.names = FALSE, sep = "\t", quote = FALSE)
        
    }
      
  )
)

#Base visualisation class
Blockr_vis <- setRefClass("Blockr_vis",
  fields = list(data = "data.frame", data_type = "character", user_group = "character"), #Includes generic data.frame functions.
  methods = list(
    
    #Monthly graphs
    monthly_graph.fun = function(x,
                                 y,
                                 variable,
                                 title,
                                 y_lab,
                                 x_lab){#Abstract strings away
      
      line_graph <- ggplot(.self$data, aes(x,y)) +
        geom_freqpoly(aes(group = variable, colour = variable), stat = "identity") +
        labs(x = x_lab, Y = y_lab) +
        ggtitle(title) +
        scale_x_discrete(breaks = seq(from = as.numeric(sql_start.str), to = as.numeric(sql_end.str), by = 100)) +
        scale_y_continuous(expand = c(0,0)) +
        theme(axis.text.x = element_text(angle = 90, hjust = 1))
      
      #Save
      ggsave(filename = file.path(getwd(),"Graphs",paste(.self$user_group,.self$data_type,"monthly_line_graph.png",sep = "_")),
             plot = line_graph,
             width = 8,
             height = 8,
             units = "in")
      
      #Monthly, with points and simple linear regression.
      regression_graph <- ggplot(.self$data,aes(x = x, y = y, colour = variable))+
        geom_point(shape=3) +
        geom_smooth(method = lm, se = TRUE, aes(group= variable)) +
        labs(x = x_lab, Y = y_lab) +
        ggtitle(title) +
        scale_x_discrete(breaks = seq(from = as.numeric(sql_start.str), to = as.numeric(sql_end.str), by = 100), expand = c(0,0)) +
        scale_y_continuous(expand = c(0,0)) +
        theme(axis.text.x = element_text(angle = 90, hjust = 1))
      
      #Print
      ggsave(filename = file.path(getwd(),"Graphs",paste(.self$user_group,.self$data_type,"monthly_linear_regression_graph.png",sep = "_")),
             plot = monthly_regression_graph,
             width = 8,
             height = 8,
             units = "in")
    },
    
    yearly_graph.fun = function(x,
                                y,
                                title,
                                labels){ #Abstract variables away
      
      #Fix data
      data.df <- .self$data
      
      #Aggregate
      
      #Substring and temporarily defactor
      data.df$timestamp <- substring(data.df$timestamp,1,4)
      
      #Aggregate
      data.df <- ddply(.data = data.df,
                         .var = c("timestamp","variable"),
                         .fun = function(x){
                           
                           return(sum(x[,3]))
                         }
      )
      
      #Renumber, refactorise, rename!
      data.df$timestamp <- as.factor(data.df$timestamp)
      data.df <- rename(data.df, replace = c("V1" = "value"))
      
      #Yearly summary
      year_line_graph <- ggplot(data.df, aes(timestamp, value)) + 
        geom_freqpoly(aes(group = variable, colour = variable), stat = "identity") +
        labs(x = "Year", y = "Number of blocks") +
        ggtitle(paste("Block rationales on the English-language Wikipedia by year\n (",sql_year_start.str,"-",sql_year_end.str,")",.self$user_group,"users,",.self$data_type,"data",sep = " ")) +
        scale_x_discrete(breaks = seq(from = as.numeric(sql_year_start.str), to = as.numeric(sql_year_end.str), by = 1), expand = c(0,0)) +
        scale_y_continuous(expand = c(0, 0)) +
        theme(axis.text.x = element_text(angle = 90, hjust = 1))
      
      #Print
      ggsave(filename = file.path(getwd(),"Graphs",paste(.self$user_group,.self$data_type,"yearly_line_graph.png",sep = "_")),
             plot = year_line_graph,
             width = 8,
             height = 8,
             units = "in")      
    },
      
    #Grouping function
    grouping.fun = function(){
      
      #Run monthly graphing
      monthly_graph.fun(x = "timestamp", y = "value", variable = "variable",
                        title = paste("Block rationales on the English-language Wikipedia by month\n(",sql_start.str,"-",sql_end.str,")", .self$user_group,"users\n",.self$data_type,"data", sep = " "),
                        y_lab = "Number of blocks", x_lab = "Month")
      
      
    }
  )
)