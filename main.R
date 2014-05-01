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

#Load config files and functions
source(file = file.path(getwd(),"config.R")) #Config variables and packages
ignore <- lapply(list.files(file.path(getwd(),"Functions"), full.names = TRUE), source)

#Initial data retrieval and analysis
blockr_initial <- function(){
  
  #Read in block data
  input_data <- data_reader(paste("SELECT
                                  LEFT(logging.log_timestamp,6) AS month,
                                  logging.log_comment AS reason,
                                  user.user_id AS userid,
                                  logging.log_user AS performer
                                  FROM logging LEFT JOIN user ON logging.log_title = user.user_name
                                  WHERE LEFT(logging.log_timestamp,6) BETWEEN",sql_start,"AND",sql_end,"
                                  AND logging.log_type = 'block'
                                  AND logging.log_action = 'block';"))
  
  #For each type of data...
  ignore <- lapply(input_data,function(x){
    
    #If it's anonymous...
    if(x$userid[1] == 0){
      
      #Set anonymous as a user type
      usergroup <- "anonymous"
      
    } else {
      
      #Otherwise, registered
      usergroup <- "registered"
      
    }
    
    #Run the regular expressions over the data
    regex_results <- regexer(x)
    
    #Expand to allow for null entries
    regex_results <- fitvals(regex_results)
    
    #Save to file
    write.table(x = regex_results,
                file = file.path(getwd(),"Data",paste(usergroup,"regex_hits.tsv", sep = "_")),
                quote = TRUE,
                sep = "\t",
                row.names = FALSE)
    
    #Blank return
    return(invisible())
  })
  
  #Read in registration data
  registrations <- querySQL(paste("SELECT LEFT(log_timestamp,6) AS month, 
                                  COUNT(*) AS registrations
                                  FROM logging
                                  WHERE log_action != 'autocreate'
                                  AND LEFT(log_timestamp,6) BETWEEN",sql_start,"AND",sql_end,"
                                  GROUP BY month;"))
  
  #Save to file
  write.table(x = regex_results,
              file = file.path(getwd(),"Data","registration_data.tsv"),
              quote = TRUE,
              sep = "\t",
              row.names = FALSE)
  
  #Grab abusefilter data
  abusefilter <- data_reader(paste("SELECT LEFT(afl_timestamp,6) AS month,
                                   afl_user AS userid,
                                   afl_actions AS tag
                                   FROM abuse_filter_log
                                   WHERE LEFT(afl_timestamp,6) BETWEEN",sql_start,"AND",sql_end,"
                                   AND afl_action = 'edit'"))
  
  #Parse it
  ignore <- lapply(abusefilter, function(x){
    
    #If it's anonymous...
    if(x$userid[1] == 0){
      
      #Set anonymous as a user type
      usergroup <- "anonymous"
      
    } else {
      
      #Otherwise, registered
      usergroup <- "registered"
      
    }
    
    #Filter
    x <- x[grepl(x = x$tag, pattern = "(warn|disallow)", perl = TRUE),]
    
    #Write to file
    write.table(x = regex_results,
                file = file.path(getwd(),"Data",paste(usergroup,"abusefilter_hits.tsv", sep = "_")),
                quote = TRUE,
                sep = "\t",
                row.names = FALSE)
    
  })
}

#Run
blockr_initial()

#Quit
q(save = "no")