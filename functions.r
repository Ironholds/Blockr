# functions.r centralises miscellaneous functions used throughout the Blockr project
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

#Function to enclose RMySQL's querying abilities, given the commonality of names here.
querySQL <- function(query_statement){
  
  #Open a connection
  con <- dbConnect(drv = "MySQL",
                   username = analytics_user,
                   password = analytics_pass,
                   host = analytics_server,
                   dbname = analytics_database)
  
  #Send the query
  QuerySend <- dbSendQuery(con, statement = query_statement)
  
  #Retrieve output of query
  output <- fetch(QuerySend, n = -1)
  
  #Kill connection
  dbDisconnect(con)
  
  #Return output
  return(output)
}

#Function for retrieving logging entries specifically
data_reader <- function(){
  
  #Query database to retrieve data from the logging table
  query.df <- querySQL(query_statement = paste("
                                              SELECT
                                              substring(logging.log_timestamp,1,6) AS timestamp,
                                              logging.log_comment AS reason,
                                              user.user_id AS userid
                                              FROM logging LEFT JOIN user ON logging.log_title = user.user_name
                                              WHERE substring(logging.log_timestamp,1,6) BETWEEN",sql_start.str,"AND",sql_end.str,"
                                              AND logging.log_type = 'block'
                                              AND logging.log_action = 'block';")
  )
  
  #Normalise and make identifiable
  query.df$userid[is.na(query.df$userid)] <- 0
  query.df$timestamp <- as.numeric(query.df$timestamp)
  
  #List
  input.ls <- list(query.df[query.df$userid == 0,],
                   query.df[query.df$userid > 0,])
  
  #Return
  return(input.ls)
  
}
#Actual regexing function
regexer <- function(input_data.df){
  
  #For each regex...
  regex_results.ls <- lapply(regex.ls, function(x){
    
    #If there are more than 0 rows in the dataset...
    if(nrow(input_data.df) > 0){
      
      #Run the regex that serves as x[2] over the input data
      grepvec <- grepl(pattern = x[2],
                       x = input_data.df$reason,
                       perl = TRUE,
                       ignore.case = TRUE)
      
      #If there are any hits...
      if(sum(grepvec) > 0){
        
        #Create an aggregate table of entries with hits
        to_return.df <- as.data.frame(table(input_data.df$timestamp[grepvec]))
        
        #Export non-matched rows back into the parent environment for use with the next regex
        assign(x = "input_data.df",
               value = input_data.df[!grepvec,],
               envir = parent.env(environment()))
        
        #Add regex name to the aggregate table
        to_return.df$regex <- x[1]
        
        #Rename object
        names(to_return.df) <- c("month","hits","regex")
        
        #Return it
        return(to_return.df)
      }
    }
    
    
  })
  
  #Aggregate remainder, adding "misc"
  regex_nonhits.df <- as.data.frame(table(input_data.df$timestamp))
  regex_nonhits.df$regex <- "misc"
  names(regex_nonhits.df) <- c("month","hits","regex")
  regex_results.ls[[length(regex_results.ls)+1]] <- regex_nonhits.df
  
  #Bind into a single df and return
  return(do.call("rbind",regex_results.ls))
}