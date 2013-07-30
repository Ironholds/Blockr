#Wrapper function
editfilters.r <- function(){
  
  #Retrieve filter data, and parse it for stupid
  filterparse.fun <- function(){
    
    #SQL query to retrieve the data
    query.df <- sql.fun(query_statement = "
      SELECT afl_id AS id,
        substring(afl_timestamp,1,6) AS timestamp,
        afl_action AS user_action,
        afl_actions AS filter_action,
        afl_user AS user_id
      FROM abuse_filter_log INNER JOIN abuse_filter
        ON afl_filter = af_id
      WHERE af_public_comments NOT LIKE '%test%';"
    )
    
    #Filter out hits where no action was taken
    query.df <- query.df[!query.df$filter_action == "",]
    
    #Filter out non-edits
    query.df <- query.df[query.df$user_action == "edit",]
    
    #Split and format as a list
    anonymous_hits.df <- query.df[query.df$user_id == 0,]
    registered_hits.df <- query.df[query.df$user_id > 0,]
    output.list <- list(anonymous_hits.df,registered_hits.df)
    
    #Return
    return(output.list)
  }
  
  #Retrieve the data
  filters.list <- filterparse.fun()
  
  #Grab in anonymous and registered blocking data for vandalism-related blocks
  anonymous_blocks.df <- subset(
    read.delim(file.path(getwd(),"Output",""))
    )
  registered_blocks.df <- 
  
  output.fun(x){
    
    
    
    
    
    
  }
  
  
  
  
}