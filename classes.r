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
                                     
                                     #Run regexes in regex.ls over input data, one by one
                                     grepvec <- grepl(pattern = x[2],
                                                      x = input_data.df$reason,
                                                      perl = TRUE,
                                                      ignore.case = TRUE)
                                     
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
                                 
                                 sample.df <- trickstr::dfsample(x,.self$sample_size)
                                 
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
      export_file_path <- file.path(getwd(),"Data",paste(as.character(.self$user_type),"_disproportionate_",".tsv",sep = ""))
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
      
      #Save data to file
      
      #Retrieve aggregates and save
      aggregated.df <- .self$aggregation.fun()
      export_file_path <- file.path(getwd(),"Data",paste(as.character(.self$user_type),"_disproportionate_",".tsv",sep = ""))
      write.table(aggregated.df, file = export_file_path, col.names = TRUE,
                  row.names = FALSE, sep = "\t", quote = FALSE)
        
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
        ggtitle(paste("Block rationales on the English-language Wikipedia by month\n(",sql_start.str,"-",sql_end.str,"),",.self$user_group,"users", sep = " ")) +
        scale_x_discrete(breaks = seq(from = as.numeric(sql_start.str), to = as.numeric(sql_end.str), by = 100), expand = c(0,0)) +
        scale_y_continuous(expand = c(0, 0)) +
        theme(axis.text.x = element_text(angle = 90, hjust = 1))
      
      #Print
      ggsave(filename = file.path(getwd(),"Graphs",paste(.self$user_group,.self$data_type,"monthly_line_graph.png",sep = "_")),
             plot = monthly_line_graph,
             width = 8,
             height = 8,
             units = "in")
      
      #Monthly, with points and simple linear regression.
      monthly_regression_graph <- ggplot(.self$data,aes(x = timestamp,y = value, colour = variable))+
        geom_point(shape=3) +
        geom_smooth(method = lm, se = TRUE, aes(group= variable)) +
        labs(x = "Month", y = "Number of blocks") +
        ggtitle(paste("Block rationales on the English-language Wikipedia by month\n(",sql_start.str,"-",sql_end.str,"),",.self$user_group,"users", sep = " ")) +
        scale_x_discrete(breaks = seq(from = as.numeric(sql_start.str), to = as.numeric(sql_end.str), by = 100), expand = c(0,0)) +
        scale_y_continuous(expand = c(0,0)) +
        theme(axis.text.x = element_text(angle = 90, hjust = 1))
      
      #Print
      ggsave(filename = file.path(getwd(),"Graphs",paste(.self$user_group,.self$data_type,"monthly_linear_regression_graph.png",sep = "_")),
             plot = monthly_regression_graph,
             width = 8,
             height = 8,
             units = "in")
      
      #Yearly summary
      year_line_graph <- ggplot(.self$yearly_data, aes(timestamp, value)) + 
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
    
    timeseries.fun = function(){
      
      #Filter
      x <- .self$data[.self$data$variable != "misc",]
      
      #Convert timestamps into character representations, and thence into a zoo yearmon object.
      x$timestamp <- as.character(x$timestamp)
      x$timestamp <- as.yearmon(x$timestamp, "%Y%m")
      
      #Identify unique variables
      unique_vars <- unique(x$variable)
      
      consistent_length <- nrow(x[x$variable == "disruption",])
      
      #For each unique variable, generate and plot stl data.
      for(i in 1:length(unique_vars)){
        
        #Grab the data for the pertinent variable, removing, well, the variable.
        input_data <- x[x$variable == unique_vars[i],c(1,3)]
        
        if(length(input_data) == consistent_length){
          
          #Generate stl data
          data.stl <- stl(x = zoo(x = input_data$value,
                                  order.by = input_data$timestamp),
                          s.window = "periodic"
          )
          
          #Plot it and return
          graph_path <- file.path(getwd(),"Graphs",paste(.self$user_group,.self$data_type,unique_vars[i],"timeseries_analysis.png", sep = "_"))
          png(filename = graph_path)
          plot(data.stl)
          title(main = "Seasonal decomposition of block data",
                sub = paste(.self$data_type,"data,",.self$user_group,"users,",unique_vars[i],"blocks", sep = " "))
          dev.off()
          
          #Return to file, too, using a roundabout method due to cat()'s inability to appreciate lists.
          sink(file.path(getwd(),"Metadata",paste(.self$user_group,.self$data_type,unique_vars[i],"timeseries_analysis.txt", sep = "_")))
          lapply(data.stl$time.series, print)
          sink()
        }
      }
      
      
    }
  )
)

Blockr_vis_nonraw <- setRefClass("Blockr_vis_nonraw",
                          fields = list(data = "data.frame", data_type = "character", user_group = "character"), #Includes generic data.frame functions.
                          contains = "Blockr_vis",
                          methods = list(
                            initial_graph.fun = function(){
                              
                              #Monthly data
                              monthly_bar_graph <- ggplot(.self$data, aes(timestamp, value, fill = variable)) + 
                                geom_bar(aes(group = variable, colour = variable), stat = "identity") +
                                labs(x = "Month", y = "Number of blocks") +
                                ggtitle(paste("Block rationales on the English-language Wikipedia by month\n(",sql_start.str," - ",sql_end.str,"), ",.self$user_group," users",.self$data_type," data", sep = "")) +
                                scale_x_discrete(breaks = seq(from = as.numeric(sql_start.str), to = as.numeric(sql_end.str), by = 100), expand = c(0,0)) +
                                scale_y_continuous(expand = c(0, 0)) +
                                theme(axis.text.x = element_text(angle = 90, hjust = 1))
                              
                              #Print
                              ggsave(filename = file.path(getwd(),"Graphs",paste(.self$user_group,.self$data_type,"monthly_bar_graph.png",sep = "_")),
                                     plot = monthly_bar_graph,
                                     width = 8,
                                     height = 8,
                                     units = "in")
                            }
                          )
)