#UserSurvival - a project to accurately triage data on blocked Wikipedia users, identify
#the underlying rationales and test various hypotheses as to any outcome
#
#logging.r outputs similar results to the basic analysis, but for the logging rather than ipblocks table
#
# @Year = 2013
# @Copyright: Oliver Keyes
# @License = MIT (http://opensource.org/licenses/MIT)

#Enclosing function
logging.fun <- function(){
  
  #Query database
  query.df <- sql.fun(query_statement = "
        SELECT
          substring(logging.log_timestamp,1,6) AS block_timestamp,
          logging.log_comment AS reason,
          user.user_editcount AS editcount,
          user.user_id AS user_id
        FROM logging LEFT JOIN user ON logging.log_title = user.user_name
        WHERE logging.log_timestamp BETWEEN 20060101010101 AND 20121231235959
          AND logging.log_type = 'block'
          AND logging.log_action = 'block';"
  )
  
  #Interpret the outcome
  parse_data.fun <- function(x){
    
    #How many items shall we sample from each month?
    samplesize <- sample_size(x = query.df,
                              variable = "block_timestamp",
                              percentage = 0.20)
    
    #Run regexes across rationales
    parsed_data.df <- regex.fun(input_data = x)
    
    #Melt dataset
    melted_data.df <- melt(parsed_data.df, id.vars = 1, measure.vars = 2:6)

    #Export it
    melted_file_path <- file.path(getwd(),"Output","logging_regex_matches.tsv")
    write.table(melted_data.df, file = melted_file_path, col.names = TRUE,
                row.names = FALSE, sep = "\t", quote = FALSE)
    
    #Return it
    return(melted_data.df)
  }
  
  #Run
  regex_matches.df <- parse_data.fun(x = query.df)
  
  #Graph the outcome
  
}

#Run
logging.fun()