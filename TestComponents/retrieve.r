retrieve.fun <- function(){
  
  #Retrieve blocks of logged-in users.
  query.df <- sql.fun(query_statement = "
        SELECT
          substring(logging.log_timestamp,1,6) AS block_timestamp,
          logging.log_comment AS reason,
          user.user_id AS userid
        FROM logging INNER JOIN user ON logging.log_title = user.user_name
        WHERE logging.log_timestamp BETWEEN 20060101010101 AND 20121231235959
          AND logging.log_type = 'block'
          AND logging.log_action = 'block'
          AND logging.log_params NOT LIKE '%indefinite%';"
  )
  
  #Function to output data for hand-coding
  usertest.fun <- function(x){
    
    #Pin input data
    input_data.df <- x
    
    #Create an empty object to fill with output
    output.df <- data.frame()
    
    #Run regular expressions over input
    for(i in 1:length(regex_list)){
      
      #Run regex
      grepvec <- grepl(pattern = regex_list[i],
                       x = input_data.df$reason,
                       perl = TRUE,
                       ignore.case = TRUE)
      
      #Extract rows that match
      matches <- input_data.df[grepvec,]
      
      #Add regex value, to later identify /what/ it matched.
      matches$matched <- i
      
      #bind to output object
      output.df <- rbind(output.df,matches)
      
      #replace input with non-matches from the last run
      input_data.df <- input_data.df[!grepvec,]

    }
    
    #Add non-matches to the file we're exporting
    input_data.df$matched <- 0
    output.df <- rbind(output.df,input_data.df)    
    
    #Save non-matches and matches both to file.
    usertest_file_path <- file.path(getwd(),"Output","blocks_with_match_status.tsv")
    write.table(output.df, file = usertest_file_path, col.names = TRUE,
                row.names = FALSE, sep = "\t", quote = TRUE, qmethod = "double")
  }
  
  #Random sampling
  sample.df <- ddply(.data = query.df,
                     .var = "block_timestamp",
                     .fun = function(x){
                       
                       #Randomly sample
                       to_output.df <- dfsample(x, 250)
                       
                       #Return
                       return(to_output.df)
                     }
  )
  
  #Run regexes and export
  usertest.fun(sample.df)
}

#Run
retrieve.fun()