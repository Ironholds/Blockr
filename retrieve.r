#Blockr - a project to accurately triage data on blocked Wikipedia users, identify
#the underlying rationales and test various hypotheses as to any outcome
#
#retrieve.r grabs the raw data from the Wikimedia analytics slaves and parses it into aggregates, exporting raw data for hand-coding.
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
          user.user_id AS userid
        FROM logging LEFT JOIN user ON logging.log_title = user.user_name
        WHERE logging.log_timestamp BETWEEN 20060101010101 AND 20121231235959
          AND logging.log_type = 'block'
          AND logging.log_action = 'block'
          AND logging.log_params NOT LIKE '%indefinite%';"
  )
  
  #Make anon users identifiable
  query.df$userid[is.na(query.df$userid)] <- 0
  
  #Split
  anonusers.df <- query.df[query.df$userid == 0,]
  registered.df <- query.df[query.df$userid > 0,]
  
  #Run
  parse_data.fun(x = anonusers.df, tablename = "logging", usergroup = "anonymous")
  parse_data.fun(x = registered.df, tablename = "logging", usergroup = "registered")
  
  #Hand-coding
  
  #Sample
  to_code.df <- dfsample(registered.df, size = 2000)
  
  #Create exportable object
  hand_code.df <- data.frame()

  #Run the regexes
  for(i in (1:length(regex.vec))){
    
    #Run regexes
    grepvec <- grepl(pattern = regex.vec[i],
    x = to_code.df$reason,
    perl = TRUE,
    ignore.case = TRUE)
    
    #Extract rows that match
    matches <- to_code.df[grepvec,]
    
    #Add regex value, to later identify /what/ it matched.
    matches$matched <- i
    
    #bind to output object
    output.df <- rbind(hand_code.df,matches)
    
    #replace input with non-matches from the last run
    input_data.df <- to_code.df[!grepvec,]
  }
  
  #Add non-matches to the file we're exporting
  to_code.df$matched <- 0
  hand_code.df <- rbind(hand_code.df,to_code.df)
  
  #Export codable results
  blockr_file_path <- file.path(getwd(),"Data","hand_codable.tsv")
  write.table(hand_code.df, file = blockr_file_path, col.names = TRUE,
              row.names = FALSE, sep = "\t", quote = TRUE, qmethod = "double")
}

ipblock.fun <- function(){
  
  #Grab dataset
  query.df <- sql.fun(query_statement = "
          SELECT ipb_reason AS reason,
            substring(ipb_timestamp,1,6) AS block_timestamp,
            ipb_user
          FROM ipblocks
          WHERE ipb_timestamp BETWEEN 20060101010101 AND 20121231235959
          AND ipb_expiry = 'infinity';"
  )
  
  #Split
  anons.df <- query.df[query.df$ipb_user == 0,]
  registered.df <- query.df[query.df$ipb_user > 0,]
  
  #Run
  parse_data.fun(x = registered.df, tablename = "ipblocks", usergroup = "registered")
  parse_data.fun(x = anons.df, tablename = "ipblocks", usergroup = "anonymous")
}

#Run
logging.fun()
ipblock.fun()
  