#Blockr - a project to accurately triage data on blocked Wikipedia users, identify
#the underlying rationales and test various hypotheses as to any outcome
#
#editfilters.r extracts information on EditFilter hits and performs simple linear regression against the blocking data from logging.r
#
# @Year = 2013
# @Copyright: Oliver Keyes
# @License = MIT (http://opensource.org/licenses/MIT)

#Wrapper function
editfilters.fun <- function(){
  
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
        AND substring(afl_timestamp,1,6) <= '201212';"
    )
    
    #Filter out hits where no action was taken, or the action was to tag
    query.df <- query.df[!query.df$filter_action %in% c("","tag"),]
    
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
    read.delim(file.path(getwd(),"Output","anonymous","ComponentFiles","logging_regex_matches_monthly.tsv"), header = TRUE, as.is = TRUE), variable == "bad.faith" & block_timestamp >= 200903
    )
  registered_blocks.df <- subset(
    read.delim(file.path(getwd(),"Output","registered","ComponentsFiles","logging_regex_matches_monthly.tsv"), header = TRUE, as.is = TRUE), variable == "bad.faith" & block_timestamp >= 200903
  )

  #Bring in interesting data and bind
  bind.fun <- function(hit_data, block_data){
    
    #Grab filter data
    hits.df <- as.data.frame(filters.list[hit_data])
    
    #Aggregate - totals (warn, strip and disallow) and a more strict metric (disallows only)
    total.df <- as.data.frame(table(hits.df$timestamp))
    nonwarn.df <- hits.df[!hits.df$filter_action == "warn",]
    grepvec <- grepl(pattern = "blockautopromote", x = nonwarn.df$filter_action, perl = TRUE, ignore.case = TRUE)
    strict.df <- as.data.frame(table(nonwarn.df[!grepvec,]$timestamp))
    
    #Bind to block data
    to_rename.df <- cbind(total.df,strict.df[,2],block_data[,3])
    
    #Rename, defactor and output
    to_output.df <- rename(to_rename.df, replace = c(
      "Var1" = "timestamp",
      "Freq" = "total.hits",
      "strict.df[, 2]" = "strict.hits",
      "block_data[, 3]" = "blocks"))
    
    to_output.df$timestamp <- as.character(to_output.df$timestamp)
    return(to_output.df)
  }
  
  #Run
  anonymous_data.df <- bind.fun(1, anonymous_blocks.df)
  registered_data.df <- bind.fun(2, registered_blocks.df)
  
  #Function to generate /tres interessant/ data. Well, interessant to me.
  output.fun <- function(data, name){
    
    #Save dataframes for any later usage, and for transparency
    aggregate_file_path <- file.path(getwd(),"Output",paste(name),"AdditionalComponents", "AbuseFilter_hits.tsv", sep = "_"))
    write.table(data, file = aggregate_file_path, col.names = TRUE,
                row.names = FALSE, sep = "\t", quote = FALSE)
    
    #Linear regression; what's the relationship between AV blocks and edit filter hits overall?
    regression_graph_totals <- ggplot(data,aes(x = total.hits,y = blocks))+
      geom_point(shape=3) +
      geom_smooth(method = "lm", se=TRUE, formula = y ~ x) +
      labs(x = "AbuseFilter hits (monthly)", y = "bad-faith blocks (monthly)") +
      ggtitle(paste("Relationship between abusefilter hits and user blocks\n, by month (2009-2012)",name,"users",sep = " ")) +
      scale_x_continuous(expand = c(0.01,0.01)) +
      scale_y_continuous(expand = c(0.01,0.01))
    
    #Strict hits?
    regression_graph_prohibits <- ggplot(data,aes(x = strict.hits, y = blocks))+
      geom_point(shape=3) +
      geom_smooth(method = "lm", se=TRUE, formula = y ~ x) +
      labs(x = "AbuseFilter hits (monthly)", y = "bad-faith blocks (monthly)") +
      ggtitle(paste("Relationship between abusefilter prohibitions and user blocks\n, by month (2009-2012)",name,"users",sep = " ")) +
      scale_x_continuous(expand = c(0.01,0.01)) +
      scale_y_continuous(expand = c(0.01,0.01))
    
    #Print
    ggsave(filename = file.path(getwd(),"Output", paste(name),"AdditionalComponents", "AbuseFilter_hits.png"),
           plot = regression_graph_totals,
           width = 8,
           height = 8,
           units = "in")
    
    ggsave(filename = file.path(getwd(),"Output", paste(name),"AdditionalComponents", "AbuseFilter_prohibitions.png"),
           plot = regression_graph_prohibits,
           width = 8,
           height = 8,
           units = "in")
    
    #Generate and throw out data on the linear regression
    sink(file = file.path(getwd(),"Output", paste(name),"AdditionalComponents", "AbuseFilter_hits.txt"))
    print(summary(lm(formula = blocks ~  total.hits, data = data,)))
    sink()
    sink(file = file.path(getwd(),"Output", paste(name),"AdditionalComponents", "AbuseFilter_prohibitions.txt"))
    print(summary(lm(formula = blocks ~  strict.hits, data = data,)))
    sink()
  }
  
  #Run
  output.fun(data = anonymous_data.df, name = "anonymous")
  output.fun(data = registered_data.df, name = "registered")
  
}

#Run
editfilters.fun()