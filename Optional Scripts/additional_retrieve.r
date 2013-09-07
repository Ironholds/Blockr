#Blockr - a project to accurately triage data on blocked Wikipedia users, identify
#the underlying rationales and test various hypotheses as to any outcome
#
#additional_retrieve.r extracts information from the Wikimedia db to test the various different hypotheses.
#
# @Year = 2013
# @Copyright: Oliver Keyes
# @License = MIT (http://opensource.org/licenses/MIT)

#Wrapper function
additional_retrieve.fun <- function(){
  
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
      WHERE af_public_comments NOT LIKE '%test%'
        AND substring(afl_timestamp,1,6) <= '201308';"
    )
    
    #Filter out hits where no action was taken, or the action was to tag
    query.df <- query.df[!query.df$filter_action %in% c("","tag"),]
    
    #Filter out non-edits
    query.df <- query.df[query.df$user_action == "edit",]
    
    #Split and export
    anonymous_hits.df <- query.df[query.df$user_id == 0,]
    blockr_file_path <- file.path(getwd(),"Data","anonymous_editfilter_hits.tsv")
    write.table(anonymous_hits.df, file = blockr_file_path, col.names = TRUE,
                row.names = FALSE, sep = "\t", quote = TRUE, qmethod = "double")
    registered_hits.df <- query.df[query.df$user_id > 0,]
    blockr_file_path <- file.path(getwd(),"Data","registered_editfilter_hits.tsv")
    write.table(registered_hits.df, file = blockr_file_path, col.names = TRUE,
                row.names = FALSE, sep = "\t", quote = TRUE, qmethod = "double")
  }
  
  #Registration data
  registration.fun <- function(){
    
    #Query
    query.df <- sql.fun(query_statement = "
            SELECT user.user_id AS id,
              substring(logging.log_timestamp,1,4) AS registration_date,
              user.user_editcount AS edit_count
            FROM
              logging INNER JOIN user
                ON logging.log_title = user.user_name
            WHERE
              logging.log_type = 'newusers'
              AND logging.log_action NOT IN ('autocreate')
              AND substring(logging.log_timestamp,1,4) BETWEEN '200601' AND '201308';"
    )
    
    #Export
    registration_file_path <- file.path(getwd(),"Data","registrations.tsv")
    write.table(aggregate.data, file = aggregate_file_path, col.names = TRUE,
                row.names = FALSE, sep = "\t", quote = FALSE)
  }
  
  #Run
  filterparse.fun()
  registration.fun()
}

#Run
additional_retrieve.fun()