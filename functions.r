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

#Quick aggregation function, to deal with year/month conversion
#@x = input dataframe
data_aggregation.fun = function(x){
  
  #Substring and temporarily defactor
  x$timestamp <- substring(x$timestamp,1,4)
  
  #Aggregate
  to_output <- ddply(.data = x,
   .var = c("timestamp","variable"),
   .fun = function(x){
          
     return(sum(x[,3]))
   }
  )
  
  #Renumber, refactorise, rename!
  to_output$timestamp <- as.factor(to_output$timestamp)
  to_output <- rename(to_output, replace = c("V1" = "value"))
  
  #return
  return(to_output)
}

