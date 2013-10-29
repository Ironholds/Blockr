#Blockr - a project to accurately triage data on blocked Wikipedia users, identify
#the underlying rationales and test various hypotheses as to any outcome
#
#retrieve.r grabs the raw data from the Wikimedia analytics slaves and parses it into aggregates, exporting raw data for hand-coding.
#
# @Year = 2013
# @Copyright: Oliver Keyes
# @License = MIT (http://opensource.org/licenses/MIT)

#Enclosing function
enclose.fun <- function(){
  
  #Logging data 
  logging.fun <- function(){
    
    #Query database
    query.df <- sql.fun(query_statement = "
          SELECT
            substring(logging.log_timestamp,1,6) AS block_timestamp,
            logging.log_comment AS reason,
            user.user_id AS userid
          FROM logging LEFT JOIN user ON logging.log_title = user.user_name
          WHERE substring(logging.log_timestamp,1,6) BETWEEN 200601 AND 201309
            AND logging.log_type = 'block'
            AND logging.log_action = 'block';"
         )
    
    #Make anon users identifiable
    query.df$userid[is.na(query.df$userid)] <- 0
    
    #Split
    anonusers.df <- query.df[query.df$userid == 0,]
    registered.df <- query.df[query.df$userid > 0,]
    
    #Run
    parse_data.fun(x = anonusers.df, tablename = "logging", usergroup = "anonymous")
    parse_data.fun(x = registered.df, tablename = "logging", usergroup = "registered")
    
  }
  
  #Ipblocks table data
  ipblock.fun <- function(){
    
    #Grab dataset
    query.df <- sql.fun(query_statement = "
            SELECT ipb_reason AS reason,
              substring(ipb_timestamp,1,6) AS block_timestamp,
              ipb_user
            FROM ipblocks
            WHERE substring(ipb_timestamp,1,6) BETWEEN 200601 AND 201309
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
}
  
#Run
enclose.fun()
