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
      input.df$data_type <- as.character(x)
      input.df$user_type <- "proportionate"
      
      return(input.df)
    })
    
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
    
  }
  
  
}

#Run
correlations_enclosure.fun()