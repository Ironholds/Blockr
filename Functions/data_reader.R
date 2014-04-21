# Copyright (c) 2014 Oliver Keyes
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

#Function for retrieving logging entries specifically
data_reader <- function(){
  
  #Query database to retrieve data from the logging table
  query_results <- querySQL(query_statement = paste("
                                              SELECT
                                              LEFT(logging.log_timestamp,6) AS timestamp,
                                              logging.log_comment AS reason,
                                              user.user_id AS userid
                                              FROM logging LEFT JOIN user ON logging.log_title = user.user_name
                                              WHERE LEFT(logging.log_timestamp,6) BETWEEN",sql_start,"AND",sql_end,"
                                              AND logging.log_type = 'block'
                                              AND logging.log_action = 'block';")
  )
  
  #Replace NAs with 0s to distinguish anonymous/registered users
  query_results$userid[is.na(query_results$userid)] <- 0
  
  #Format into POSIXct timestamps
  query_results$timestamp <- paste(query_results$timestamp, "01", sep = "")
  query_results$timestamp <- as.date(query_results$timestamp, format = "%Y%m%d")
  
  #List
  to_output <- list(query_results[query_results$userid == 0,],
                   query_results[query_results$userid > 0,])
  
  #Return
  return(to_output)
  
}