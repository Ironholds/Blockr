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
                                         #Generate data to output, and output
                                         lapply_output.df <- input_data.df[grepvec,]
                                         lapply_output.df$regex <- as.character(x[1])
                                         
                                         #Output refined input data, using assign() 
                                         #due to lapply's distinct environment for 
                                         #function calls
                                         assign(x = "input_data.df",
                                                value = input_data.df[!grepvec,],
                                                envir = parent.env(environment()))
                                         
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
    
    #Monthly regression graph
    monthly_regression.fun = function(data,
                              x,
                              y,
                              variable,
                              title,
                              y_lab,
                              layer,
                              x_lab){#Abstract strings away
      
      #Rename variables, as specified, to ensure consistency within the plotting environment
      #Fix data
      data.df <- data
      
      #Rename variables, as specified, to ensure consistency within the plotting environment.
      data.df <- data.df[,c(x,y,variable)]
      names(data.df) <- c("time","value","variable")
      
      #Monthly, with points and simple linear regression.
      regression_graph <- ggplot(data.df, aes(time, value))+
        geom_point(shape=3) +
        geom_smooth(method = lm, se = TRUE, aes(group = 1)) +
        labs(x = x_lab, Y = y_lab) +
        ggtitle(title) +
        scale_x_discrete(breaks = seq(from = as.numeric(sql_start.str), to = as.numeric(sql_end.str), by = 100), expand = c(0,0)) +
        scale_y_continuous() +
        expand_limits(x = 0, y = 0) +
        theme(axis.text.x = element_text(angle = 90, hjust = 1))
      
      #Return
      return(regression_graph)
    },
    
    yearly_line_graph.fun = function(data,
                                x,
                                y,
                                variable,
                                y_lab,
                                title,
                                layer,
                                x_lab){ #Abstract variables away
      
      data.df <- data
      
      #Rename variables, as specified, to ensure consistency within the plotting environment.
      data.df <- data.df[,c(x,y,variable)]
      names(data.df) <- c("time","value","variable")
      
      if(nchar(as.character(data.df$time)[1]) > 4){
        
        #Substring and temporarily defactor
        data.df$time <- substring(data.df$time,1,4)
        
        #Aggregate
        data.df <- ddply(.data = data.df,
                         .var = c("time","variable"),
                         .fun = function(x){
                           
                           return(sum(x$value))
                         }
        )
        
        #Renumber, refactorise, rename!
        data.df$time <- as.factor(data.df$time)
        data.df <- rename(data.df, replace = c("V1" = "value"))
        data.df$value <- as.numeric(data.df$value)
      }
      
      #Yearly summary
      year_line_graph <- ggplot(data.df, aes(time, value)) + 
        geom_freqpoly(aes(group = variable, colour = variable), stat = "identity") +
        labs(x = "x_lab", y = "y_lab") +
        ggtitle("title") +
        scale_x_discrete(breaks = seq(from = as.numeric(sql_year_start.str), to = as.numeric(sql_year_end.str), by = 1), expand = c(0,0)) +
        scale_y_continuous(expand = c(0, 0)) +
        theme(axis.text.x = element_text(angle = 90, hjust = 1))
      
      #Return
      return(year_line_graph)
    },
      
    #Grouping function
    graphing.fun = function(){
      
      #Regression graphs
      #Grab unique variable values
      unique_vars <- unique(.self$data$variable[.self$data$variable != "misc"])
      
      #Initialise holding list
      holding.ls <- list()
      
      #Graph
      for (i in 1:length(unique_vars)){
        
        #Generate regression graph
        regression_graph <- monthly_regression.fun(data = .self$data[.self$data$variable == unique_vars[i],], x = "timestamp", y = "value", variable = "variable",
                                                    title = paste(unique_vars[i],"blocks on the English-language Wikipedia by month\n(",sql_start.str,"-",sql_end.str,")", .self$user_group,"users\n",.self$data_type,"data", sep = " "),
                                                    y_lab = "Number of blocks", x_lab = "Month")
        #Throw over to the holding list
        holding.ls[[length(holding.ls)+1]] <- regression_graph
        
      }
      
      
      #Save
      png(file.path(getwd(),"Graphs",paste(.self$user_group,.self$data_type,"block_graphs.png", sep = "_")), width = 1020, height= 1020)
      do.call(grid.arrange, holding.ls)
      dev.off()
      
      #Yearly summary
      yearly_graph <- yearly_line_graph.fun(data = .self$data[.self$data$variable != "misc",], x = "timestamp", y = "value", variable = "variable",
                             title = paste("Blocks on the English-language Wikipedia by year\n(",sql_year_start.str,"-",sql_year_end.str,")", .self$user_group,"users\n",.self$data_type,"data", sep = " "),
                             y_lab = "Number of blocks", x_lab = "Year")
      
      #Print
      ggsave(filename = file.path(getwd(),"Graphs",paste(.self$user_group,.self$data_type,"yearly_line_graph.png",sep = "_")),
             plot = yearly_graph,
             width = 8,
             height = 8,
             units = "in")
      
      #Yearly summary
      total_yearly <- yearly_line_graph.fun(data = .self$data[.self$data$variable == "total",], x = "timestamp", y = "value", variable = "variable",
                                            title = paste("Total blocks on the English-language Wikipedia by year\n(",sql_year_start.str,"-",sql_year_end.str,")", .self$user_group,"users\n",.self$data_type,"data", sep = " "),
                                            y_lab = "Number of blocks", x_lab = "Year")
      
      #Print
      ggsave(filename = file.path(getwd(),"Graphs",paste(.self$user_group,.self$data_type,"total_yearly_blocks.png",sep = "_")),
             plot = total_yearly,
             width = 8,
             height = 8,
             units = "in")
    }
  )
)