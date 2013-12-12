# correlations.r checks for any correlations between block rate for a group and external factors.
# Examples would be registration rate and abusefilter hits
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

#Enclosure
correlations_enclosure.fun <- function(){
  
  #Data retrieval grouping function
  retrieval.fun <- function(){
    
    #Read in disproportionate data.
    generated_data.ls <- lapply(list("anonymous","registered"), function(x){
      
      #Read in data
      input.df <- read.delim(file.path(getwd(),"Data",paste(as.character(x),"_disproportionate.tsv",sep = "")), as.is = TRUE, header = TRUE)
      
      #Subset data
      input.df <- input.df[! input.df$variable %in% c("misc","proxy"),]
      
      #Add distinguishing characteristic
      input.df$data_type <- "blocks"
      input.df$variable <- paste(as.character(x),input.df$variable)
      
      return(input.df)
    })
    
    filter.fun <- function(){
      
      #Grab edit filter data
      filter.df <- sql.fun(paste("SELECT SUBSTRING(afl_timestamp,1,6) AS timestamp,
                            afl_user AS user,
                            afl_actions AS action
                            FROM abuse_filter_log INNER JOIN abuse_filter
                            ON afl_filter = af_id
                            WHERE af_public_comments NOT LIKE '%test%'
                            AND afl_action = 'edit'
                            AND SUBSTRING(afl_timestamp,1,6) BETWEEN ",sql_start.str," AND ",sql_end.str,
                            " AND afl_actions NOT IN ('','tag')",sep = ""))
      
      #Subset and bind together
      filter_subset.df <- cbind(as.data.frame(table(filter.df$timestamp)), #All hits
                                as.data.frame(table(filter.df[filter.df$user > 0,]$timestamp))[,2], #Registered users
                                as.data.frame(table(filter.df[filter.df$user == 0,]$timestamp))[,2], #Anonymous users
                                as.data.frame(table(filter.df[grepl(pattern = "disallow", x = filter.df$action),]$timestamp))[,2], #Disallowed edits
                                as.data.frame(table(filter.df[grepl(pattern = "disallow", x = filter.df$action) & filter.df$user > 0,]$timestamp))[,2], #Disallowed edits, registered users
                                as.data.frame(table(filter.df[grepl(pattern = "disallow", x = filter.df$action) & filter.df$user == 0,]$timestamp))[,2]) #Disallowed edits, anonymous users
      
      #Rename
      names(filter_subset.df) <- c("timestamp","all","all registered","all anonymous","disallowed","anonymous disallowed","registered disallowed")
      
      #Add columns and reshape
      filter_subset.df$data_type <- "edit filter hits"
      filter_subset.df <- melt(filter_subset.df, id.vars = c(1,8), measure.vars = 2:7)
      
      #Defactor
      filter_subset.df$timestamp <- as.numeric(as.character(filter_subset.df$timestamp))
      filter_subset.df$variable <- as.character(filter_subset.df$variable)
      
      #Return
      return(filter_subset.df)
    }
    
    registration.fun <- function(){
      
      #Query
      registration.df <- sql.fun(paste("
                            SELECT substring(log_timestamp,1,6) AS timestamp,
                            user_editcount AS edits
                            FROM
                            logging INNER JOIN user
                            ON log_title = user_name
                            WHERE
                            log_type = 'newusers'
                            AND log_action NOT IN ('autocreate')
                            AND substring(log_timestamp,1,6) BETWEEN ",sql_start.str," AND ",sql_end.str,sep = ""))
      
      #Aggregate and bind
      registration_subset.df <- cbind(as.data.frame(table(registration.df$timestamp)),
                                      as.data.frame(table(registration.df[registration.df$edits > 0,]$timestamp))[,2])

      #Rename and add columns
      names(registration_subset.df) <- c("timestamp","all",">1 edit")
      registration_subset.df$data_type <- "registrations"
      
      #Melt, normalise and return
      registration_subset.df <- melt(registration_subset.df, id.vars = c(1,4), measure.vars = 2:3)
      registration_subset.df$timestamp <- as.numeric(as.character(registration_subset.df$timestamp))
      registration_subset.df$variable <- as.character(registration_subset.df$variable)
      return(registration_subset.df)
    }
    
    #Bind
    resulting_data.df <- rbind(filter.fun(),
                               registration.fun(),
                               as.data.frame(generated_data.ls[[1]]),
                               as.data.frame(generated_data.ls[[2]]))
    
    #Return
    return(resulting_data.df)
  }

  #Graphing function
  graphing.fun <- function(x,y){
     
    #Standardise data
    if(nrow(x) > nrow(y)){
        
      x <- order(x[x$timestamp %in% y$timestamp,]$timestamp)
        
    } else if(nrow(x) < nrow(y)){
        
      y <- order(y[y$timestamp %in% y$timestamp,]$timestamp)
        
    }
      
    
    data.df <- cbind(x$value,y$value)
    names(data.df) <- c("column1","column2")
     
    #Plot
    correlation_plot <- ggplot(data.df, aes(x = column1,y = column2))+
      geom_point(shape=3) +
      geom_smooth(method = "lm", se=TRUE, formula = y ~ x) +
      labs(x = x$data_type[1], y = y$data_type[2]) +
      ggtitle(paste("Relationship between",x$variable[i],x$data_type[1],"and",y$variable[1],y$data_type[1]                      +
      scale_x_continuous(expand = c(0,0)) +
      scale_y_continuous(expand = c(0,0)) +
      geom_text(aes(x = max(column1), y = min(column2), label = trickstr::r2_display(lm = lm(column1 ~ column2, data.df))), parse = TRUE)     
  }
  
  #Retrieve data
  data.df <- retrieval.fun()
  
  #Identify uniques, pull out value
  unique_vars <- unique(data.df$variable)
  while_val <- 1
  
  while(while_val <= length(unique_vars)){
    
    #Grab dataset 1
    first_dataset <- data.df[data.df$variable == unique_vars[while_val],]
    
    #Start iteration loop
    for(i in 1:length(unique_vars)){
      
      if(!unique_vars[i] %in% unique_vars[1:while_val]){
        
        second_dataset <- data.df[data.df$variable == unique_vars[i],]
        
        graphing.fun(x = first_dataset, y = second_dataset)
      }
      
    }
    
    #Increment while_val
    while_val <- while_val+1
  }
  
  d_ply(.data = data.df,
        .variables = "variable",
        .fun = function(x){
          
          for(i in 1:length(unique_vars)){
              
            if(unique_vars[i] != x$variable[1]){
              
              #Grab data
              data <- rbind(x$value,data.df[data.df$timestamp %in% x$timestamp & data.df$variable == unique_vars[i],]$value)
              
              #Graph
              graphing.fun(x = data[,1],
                           y = data[,2],
                           type1 <- x$variable[1],
                           type2 <- unique_vars[i])
              
            }
          }

          
          
          
        })
}

#Run
correlations_enclosure.fun()