#Blockr - a project to accurately triage data on blocked Wikipedia users, identify
#the underlying rationales and test various hypotheses as to any outcome
#
#functions.r contains the project's global, reused functions
#
# @Year = 2013
# @Copyright: Oliver Keyes
# @License = MIT (http://opensource.org/licenses/MIThttp://opensource.org/licenses/MIT)

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
      
      #How many items shall we sample?
      samplesize <- sample_size(x = input_data,
                                  variable = "block_timestamp",
                                  percentage = 0.20)
      
      output_data.df <- ddply(.data = input_data,
                              .var = "block_timestamp",
                              .fun = function(x, samplesize){
                                
                                #sample from the subset to produce normalised data, operating off the value of samplesize
                                x <- dfsample(df = x, size = samplesize)
                                
                                #Pin input data
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
    
    #Take the input, generate yearly totals too.
    data_by_year.df <- parsed_data.df
    data_by_year.df$block_timestamp <- substring(data_by_year.df$block_timestamp,1,4)
    data_by_year.df <- ddply(.data = data_by_year.df,
                             .var = "block_timestamp",
                             .fun = function(x){
                                return(colSums(x[,2:7]))
                             }
    )
    
    #Melt datasets
    melted_data_month.df <- melt(parsed_data.df, id.vars = 1, measure.vars = 2:7)
    melted_data_year.df <- melt(data_by_year.df, id.vars = 1, measure.vars = 2:7)
    
    #Export it
    melted_file_path <- file.path(getwd(),"Output",paste(usergroup),"ComponentFiles",paste(tablename,"regex_matches_monthly.tsv",sep = "_"))
    write.table(melted_data_month.df, file = melted_file_path, col.names = TRUE,
                row.names = FALSE, sep = "\t", quote = FALSE)
    melted_file_path <- file.path(getwd(),"Output",paste(usergroup),"ComponentFiles",paste(tablename,"regex_matches_yearly.tsv",sep = "_"))
    write.table(melted_data_year.df, file = melted_file_path, col.names = TRUE,
                row.names = FALSE, sep = "\t", quote = FALSE)
    
    #List
    to.return <- list(melted_data_month.df,melted_data_year.df)
    
    #Return it
    return(to.return)
  }