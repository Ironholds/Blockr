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
          user.user_id AS userid
        FROM logging LEFT JOIN user ON logging.log_title = user.user_name
        WHERE logging.log_timestamp BETWEEN 20060101010101 AND 20121231235959
          AND logging.log_type = 'block'
          AND logging.log_action = 'block'
          AND logging.log_params NOT LIKE '%indefinite%';"
  )
  
  #Make anon users identifiable
  query.df$userid[is.na(query.df$userid)] <- 0

  #Graph the outcome
  graphing.fun <- function(x, usergroup){
    
    #Split up the list
    monthly_data.df <- as.data.frame(x[1])
    yearly_data.df <- as.data.frame(x[2])
    
    #A simple line-and-point graph.
    line_graph_yearly <- ggplot(yearly_data.df, aes(block_timestamp, value)) + 
      geom_freqpoly(aes(group = variable, colour = variable), stat = "identity") +
      labs(x = "Year", y = "Number of users") +
      ggtitle(paste("Block rationales on the English-language Wikipedia\nby year (2006-2012) - logging table,",usergroup, sep = " ")) +
      scale_x_discrete(breaks = 2006:2012, expand = c(0,0)) +
      scale_y_continuous(expand = c(0, 0)) +
      theme(axis.text.x = element_text(angle = 90, hjust = 1))
    
    #print
    ggsave(filename = file.path(getwd(),"Output", paste(usergroup),"ComponentFiles","logging_regex_matches_by_year.png"),
      plot = line_graph_yearly,
      width = 8,
      height = 8,
      units = "in")
    
    #Some regression, sir?
    regression_graph_monthly <- ggplot(monthly_data.df,aes(x = block_timestamp,y = value, colour = variable))+
      geom_point(shape=3) +
      geom_smooth(method = lm, se = TRUE, aes(group= variable)) +
      labs(x = "Year", y = "Number of users") +
      ggtitle(paste("Block rationales on the English-language Wikipedia\nby month (2006-2012) - logging table,",usergroup, sep = " ")) +
      scale_x_discrete(expand = c(0,0)) +
      scale_y_continuous(expand = c(0,0)) +
      theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())
    
    #Print
    ggsave(filename = file.path(getwd(),"Output", paste(usergroup), "ComponentFiles", "logging_regex_matches_linear_regression.png"),
      plot = regression_graph_monthly,
      width = 8,
      height = 8,
      units = "in")
  }
  
  #Split
  anonusers.df <- query.df[query.df$userid == 0,]
  registered.df <- query.df[query.df$userid > 0,]
  
  #Run
  regex_matches.list <- parse_data.fun(x = anonusers.df, tablename = "logging", usergroup = "anonymous")
  graphing.fun(x = regex_matches.list, usergroup = "anonymous")
  regex_matches.list <- parse_data.fun(x = registered.df, tablename = "logging", usergroup = "registered")
  graphing.fun(x = regex_matches.list, usergroup = "registered")
  
  #Function to output data for hand-coding
  usertest.fun <- function(x){
    
    #Pin input data
    input_data.df <- x
    
    #Run regular expressions over
    for(i in length(regex_list)){
      
      #Run regex
      grepvec <- grepl(pattern = regex_list[i],
                       x = input_data.df$reason,
                       perl = TRUE,
                       ignore.case = TRUE)
      
      #Throw non-matches to the source
      input_data.df <- input_data.df[!grepvec,]
    }
    
    #Throw things that don't match any regex to file.
    usertest_file_path <- file.path(getwd(),"Output","non_match.tsv")
    write.table(input_data.df, file = usertest_file_path, col.names = TRUE,
                row.names = FALSE, sep = "\t", quote = FALSE)
  }
}

#Run
logging.fun()
