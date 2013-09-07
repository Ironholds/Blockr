#Blockr - a project to accurately triage data on blocked Wikipedia users, identify
#the underlying rationales and test various hypotheses as to any outcome
#
#initial_graphing.r takes the data extracted by retrieve.r and maps it.
#
# @Year = 2013
# @Copyright: Oliver Keyes
# @License = MIT (http://opensource.org/licenses/MIT)

initial_graphing.fun <- function(){
  
  #Grouped function
  graphing.fun <- function(table,usergroup){
    
    #Read in
    input.df <- read.delim(file.path(getwd(),"Data",paste(table,usergroup,"regex_matches.tsv",sep = "_")), as.is = TRUE, header = TRUE)
    
    #Factorise, to allow for a continuous scale
    input.df$block_timestamp <- as.factor(input.df$block_timestamp)
    
    #A simple line-and-point graph.
    line_graph <- ggplot(input.df, aes(block_timestamp, value)) + 
      geom_freqpoly(aes(group = variable, colour = variable), stat = "identity") +
      labs(x = "Month", y = "Number of users") +
      ggtitle(paste("Block rationales on the English-language Wikipedia by month\n (2006-2012)\n",table,"table,",usergroup,"users", sep = " ")) +
      scale_x_discrete(breaks = seq(from = 200601, to = 201201, by = 100), expand = c(0,0)) +
      scale_y_continuous(expand = c(0, 0)) +
      theme(axis.text.x = element_text(angle = 90, hjust = 1))
    
    #print
    ggsave(filename = file.path(getwd(),"Graphs",paste(table,usergroup,"line_by_month.png",sep = "_")),
      plot = line_graph,
      width = 8,
      height = 8,
      units = "in")
    
    #Monthly, with points and simple linear regression.
    regression_graph <- ggplot(input.df,aes(x = block_timestamp,y = value, colour = variable))+
      geom_point(shape=3) +
      geom_smooth(method = lm, se = TRUE, aes(group= variable)) +
      labs(x = "Year", y = "Number of users") +
      ggtitle(paste("Block rationales on the English-language Wikipedia by month\n (2006-2012)\n",table,"table,",usergroup,"users", sep = " ")) +
      scale_x_discrete(breaks = seq(from = 200601, to = 201201, by = 100), expand = c(0,0)) +
      scale_y_continuous(expand = c(0,0)) +
      theme(axis.text.x = element_text(angle = 90, hjust = 1))
      
    #print
    ggsave(filename = file.path(getwd(),"Graphs",paste(table,usergroup,"regression_by_month.png",sep = "_")),
      plot = regression_graph,
      width = 8,
      height = 8,
      units = "in")
    
    #Create yearly data totals
    yearly.df <- input.df
    yearly.df$block_timestamp <- substring(yearly.df$block_timestamp,1,4)
    yearly.df <- ddply(.data = yearly.df[yearly.df$block_timestamp <=2012,],
                             .var = c("block_timestamp","variable"),
                             .fun = function(x){
                                return(sum(x[,3]))
                             }
    )
    
    #Factorise
    yearly.df$block_timestamp <- as.factor(yearly.df$block_timestamp)
    
    #Graph
    year_graph <- ggplot(yearly.df, aes(block_timestamp, V1)) + 
      geom_freqpoly(aes(group = variable, colour = variable), stat = "identity") +
      labs(x = "Month", y = "Number of users") +
      ggtitle(paste("Block rationales on the English-language Wikipedia by year\n (2006-2012)\n",table,"table,",usergroup,"users", sep = " ")) +
      scale_x_discrete(breaks = seq(from = 200601, to = 201201, by = 100), expand = c(0,0)) +
      scale_y_continuous(expand = c(0, 0)) +
      theme(axis.text.x = element_text(angle = 90, hjust = 1))
    
    #print
    ggsave(filename = file.path(getwd(),"Graphs",paste(table,usergroup,"line_by_year.png",sep = "_")),
      plot = year_graph,
      width = 8,
      height = 8,
      units = "in")
  }
  
  #Runs
  graphing.fun(table = "ipblocks", usergroup = "registered")
  graphing.fun(table = "ipblocks", usergroup = "anonymous")
  graphing.fun(table = "logging", usergroup = "anonymous")
  graphing.fun(table = "logging", usergroup = "registered")
}

#Run
initial_graphing.fun()