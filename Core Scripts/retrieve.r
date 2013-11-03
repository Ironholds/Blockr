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

#Enclosing function, to be source()d and executed by visualise.r
retrieve_enclose.fun <- function(){
  
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
  
  #Loop to format, run and export
  #List necessary params.
  #@1 resulting object name
  #@2 source object name
  #@3 resulting file name
  data_loop.ls <-list(c("anon.base","anonusers.df","anonymous_aggregate","anon.base_hand"),
                      c("registered.base","registered.df","registered_aggregate","registered.base_hand"))
  
  #Rename vector
  rename.vec <- c("timestamp" = "timestamp", "V1" = "spam", "V2" = "disruption","V3" = "sockpuppetry",
                  "V4" = "username", "V5" = "proxy", "V6" = "misc")
  
  #Output object
  output.ls <- list()
  
  #Loop
  for(i in 1:length(data_loop.ls)){
    
    #Create in the Blockr_base class, with the input data as, well, $data
    assign(x = data_loop.ls[[i]][1],
           value = new("Blockr_base", data = get(data_loop.ls[[i]][2]))
      )
    
    #Run function and retrieve
    holding.df <- get(data_loop.ls[[i]][1])$regex_container.fun(
      data = get(data_loop.ls[[i]][1])$data,
      var = "timestamp",
      rename_strings = rename.vec)
      
    #add to returning list
    output.ls[[i]] <- holding.df
    
    #Melt
    to_output.df <- melt(holding.df, id.vars = 1, measure.vars = 2:7)

    #Export
    export_file_path <- file.path(getwd(),"Data",paste(data_loop.ls[[i]][3],".tsv",sep = ""))
    write.table(to_output.df, file = export_file_path, col.names = TRUE,
                row.names = FALSE, sep = "\t", quote = FALSE)
    
    #And now we play the hand-coding game
    assign(x = data_loop.ls[[i]][4],
     value = new("Blockr_base_handcode", data = get(data_loop.ls[[i]][2]),
                 sample_size = trickstr::sample_size(x = get(data_loop.ls[[i]][2]), variable = "timestamp", percentage = 0.20))
    )
    
    holding.df <- get(data_loop.ls[[i]][4])$regex_container.fun(
      data = get(data_loop.ls[[i]][4])$data,
      var = "timestamp")
  }
  
  #Return
  return(output.ls)
}
