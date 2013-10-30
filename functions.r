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
sql.fun <- function(query_statement){
  
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

parse_data.fun <- function(x, tablename, usergroup){
    
  #Regex function
  regex.fun <- function(input_data){
    
    output_data.df <- ddply(.data = input_data,
                            .var = "block_timestamp",
                            .fun = function(x){
                              
                              #Pin
                              to_run.df <- x
                              
                              #Create empty vector.
                              to_return.vec <- vector()
                              
                              #And now we loop.
                              for(i in (1:length(regex.vec))){
                                
                                #Run regexes
                                grepvec <- grepl(pattern = regex.vec[i],
                                x = to_run.df$reason,
                                perl = TRUE,
                                ignore.case = TRUE)
                                
                                #Number of rows that match
                                to_return.vec[i] <- sum(grepvec)
                                
                                #Non-matches
                                to_run.df <- to_run.df[!grepvec,]
                              }
                              
                              #Include non-matches
                              to_return.vec[length(to_return.vec)+1] <- nrow(to_run.df)
                                                            
                              #Return
                              return(to_return.vec)}
                            )
    
    #Rename
    output_data.df <- rename(output_data.df,c("V1" = "spam","V2" = "bad.edit","V3" = "Sockpuppetry", "V4" = "Username.problems", "V5" = "Proxies", "V6" = "Misc"))
  
    #Return
    return(output_data.df)
  }
    
  #Run regexes across rationales
  parsed_data.df <- regex.fun(input_data = x)
  
  #Melt datasets
  melted_data.df <- melt(parsed_data.df, id.vars = 1, measure.vars = 2:7)
  
  #Export it
  melted_file_path <- file.path(getwd(),"Data",paste(tablename,usergroup,"regex_matches.tsv",sep = "_"))
  write.table(melted_data.df, file = melted_file_path, col.names = TRUE,
              row.names = FALSE, sep = "\t", quote = FALSE)
}