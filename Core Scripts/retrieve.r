# retrieve.r retrieves data from the logging table and processes it through the regex functions in Blockr_base
# (contained in classes.r)
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

#Enclosing function
enclose.fun <- function(){
  
  #Query database to retrieve data from the logging table
  query.df <- sql.fun(query_statement = paste("
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

  
  #Split
  anonusers.df <- query.df[query.df$userid == 0,]
  registered.df <- query.df[query.df$userid > 0,]
  
  #Run
  parse_data.fun(x = anonusers.df, tablename = "logging", usergroup = "anonymous")
  parse_data.fun(x = registered.df, tablename = "logging", usergroup = "registered")
    
}
  
#Run
enclose.fun()
