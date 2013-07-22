#UserSurvival - a project to accurately triage data on blocked Wikipedia users, identify
#the underlying rationales and test various hypotheses as to any outcome
#
#ipblock.r outputs the basic analysis documented at http://blog.ironholds.org/?p=31
#
# @Year = 2013
# @Copyright: Oliver Keyes
# @License = MIT (http://opensource.org/licenses/MIT)

#Enclosing function
ipblock.fun <- function(){
  
  #Grab dataset
  query.df <- sql.fun(query_statement = "
          SELECT ipb_reason AS reason,
            substring(ipb_timestamp,1,6) AS block_timestamp
          FROM ipblocks
          WHERE ipb_timestamp BETWEEN 20060101010101 AND 20121231235959
          AND ipb_expiry = 'infinity'
          AND ipb_user > 0;"
  )
  
  #Run parse_data.fun
  regex_matches.list <- parse_data.fun(x = query.df, tablename = "ipblocks")
    
  graphing.fun <- function(x){
    
    #Split up the list
    monthly_data.df <- as.data.frame(x[1])
    yearly_data.df <- as.data.frame(x[2])
    
    #Line-and-point graphs for yearly and monthly data.
    line_graph_yearly <- ggplot(yearly_data.df, aes(block_timestamp, value)) + 
      geom_freqpoly(aes(group = variable, colour = variable), stat = "identity") +
      labs(x = "Year", y = "Number of users") +
      ggtitle("Block rationales on the English-language Wikipedia\nby year (2006-2012) - ipblocks table") +
      scale_x_discrete(breaks = 2006:2012, expand = c(0,0)) +
      scale_y_continuous(expand = c(0, 0)) +
      theme(axis.text.x = element_text(angle = 90, hjust = 1))

    line_graph_monthly <- ggplot(monthly_data.df, aes(block_timestamp, value)) +
      geom_freqpoly(aes(group = variable, colour = variable), stat = "identity") +
      labs(x = "Year", y = "Number of users") +
      ggtitle("Block rationales on the English-language Wikipedia\nby month (2006-2012) - ipblocks table") +
      scale_x_discrete(expand = c(0,0)) +
      scale_y_continuous(expand = c(0,0)) +
      theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())
    
    #print
    ggsave(filename = file.path(getwd(),"Output", "ipblocks_regex_matches_by_year.png"),
      plot = line_graph_yearly,
      width = 8,
      height = 8,
      units = "in")
    ggsave(filename = file.path(getwd(),"Output", "ipblocks_regex_matches_by_month.png"),
      plot = line_graph_monthly,
      width = 8,
      height = 8,
      units = "in")
    
    #Regression
    regression_graph_monthly <- ggplot(monthly_data.df,aes(x = block_timestamp,y = value, colour = variable))+
      geom_point(shape=3) +
      geom_smooth(method = lm, se = TRUE, aes(group= variable)) +
      labs(x = "Year", y = "Number of users") +
      ggtitle("Block rationales on the English-language Wikipedia\nby month (2006-2012) - ipblocks table") +
      scale_x_discrete(expand = c(0,0)) +
      scale_y_continuous(expand = c(0,0)) +
      theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())
    
    #Print
    ggsave(filename = file.path(getwd(),"Output", "ipblocks_regex_matches_linear_regression.png"),
      plot = regression_graph_monthly,
      width = 8,
      height = 8,
      units = "in")
    
    #The dataset makes it difficult to properly show everything. Let's graph some subsets
    regression_graph_monthly_badfaith <- ggplot(monthly_data.df[monthly_data.df$variable %in% c("bad.faith","spam"),],
      aes(x = block_timestamp,y = value, colour = variable))+
      geom_point(shape=3) +
      geom_smooth(method = lm, se = TRUE, aes(group= variable)) +
      labs(x = "Year", y = "Number of users") +
      ggtitle("Spam and bad-faith blocks on the English-language Wikipedia\nby month (2006-2012) - ipblocks table") +
      scale_x_discrete(expand = c(0,0)) +
      scale_y_continuous(expand = c(0,0)) +
      theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())
      
    regression_graph_monthly_other <- ggplot(monthly_data.df[!monthly_data.df$variable %in% c("bad.faith","spam"),],
      aes(x = block_timestamp,y = value, colour = variable))+
      geom_point(shape=3) +
      geom_smooth(method = lm, se = TRUE, aes(group= variable)) +
      labs(x = "Year", y = "Number of users") +
      ggtitle("Other blocks on the English-language Wikipedia\nby month (2006-2012) - ipblocks table") +
      scale_x_discrete(expand = c(0,0)) +
      scale_y_continuous(expand = c(0,0)) +
      theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())
    
    ggsave(filename = file.path(getwd(),"Output", "ipblocks_linear_regression_badfaith.png"),
      plot = regression_graph_monthly_badfaith,
      width = 8,
      height = 8,
      units = "in")
    ggsave(filename = file.path(getwd(),"Output", "ipblocks_linear_regression_other.png"),
      plot = regression_graph_monthly_other,
      width = 8,
      height = 8,
      units = "in")
  }
  
  #Run
  graphing.fun(x = regex_matches.list)
    
}

#Run
ipblock.fun()
