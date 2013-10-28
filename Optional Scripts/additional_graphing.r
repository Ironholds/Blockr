#Blockr - a project to accurately triage data on blocked Wikipedia users, identify
#the underlying rationales and test various hypotheses as to any outcome
#
#additional_graphing.r takes the data from additional_retrieve.r, formats and processes it, and graphs it against blocking data to test the original hypothesis
#
# @Year = 2013
# @Copyright: Oliver Keyes
# @License = MIT (http://opensource.org/licenses/MIT)

#Read in data
data_read.fun <- function(){

  #Grab editfilter data and generate other
  source(file.path(getwd(),"Optional Scripts","additional_retrieve.r"))
  
  #Read non-context-specific data in
  registrations.df <- read.delim(file = file.path(getwd(),"Data","registrations.tsv"), as.is = TRUE, header = TRUE)
  registrations_with_edits.df <- read.delim(file = file.path(getwd(),"Data","registrations_with_edits.tsv"), as.is = TRUE, header = TRUE)
  block_data.df <- subset(read.delim(file = file.path(getwd(),"Data","logging_registered_regex_matches.tsv"), as.is = TRUE, header = TRUE), variable == "bad.edit")
  
  #Parse edit-filter data
  anonymous_hits.df <- editfilter.df[editfilter.df$user_id == 0,]
  registered_hits.df <- editfilter.df[editfilter.df$user_id > 0,]
  
  #Process
  filter_process.fun <- function(x){
    
    #Grab totals.
    loose.df <- as.data.frame(table(x$timestamp))
    nonwarn.df <- x[!x$filter_action == "warn",]
    grepvec <- grepl(pattern = "blockautopromote", x = nonwarn.df$filter_action, perl = TRUE, ignore.case = TRUE)
    strict.df <- as.data.frame(table(nonwarn.df[!grepvec,]$timestamp))
    
    #Insert NAs
    bind_NA <- rep(NA,(nrow(block_data.df) - nrow(loose.df)))
    to_return_loose <- c(bind_NA,loose.df$Freq)
    to_return_strict <- c(bind_NA,strict.df$Freq)
    
    #Return
    return(data.frame(to_return_loose,to_return_strict))
  }
  
  #Run
  anonymous_hits.df <- filter_process.fun(anonymous_hits.df)
  registered_hits.df <- filter_process.fun(registered_hits.df)
  
  #Bind it all together
  bound.df <- cbind(block_data.df[,c(1,3)],registrations.df$Freq,registrations_with_edits.df$Freq,anonymous_hits.df,registered_hits.df)
  
  #Rename
  bound.df <- rename(bound.df, replace = c("block_timestamp" = "timestamp", "value" = "blocks", "registrations.df$Freq" = "registrations",
                                           "registrations_with_edits.df$Freq" = "registrations_with_edits", "to_return_loose" = "anonymous_editfilter_hits",
                                           "to_return_strict" = "anonymous_editfilter_prohibits"))
  names(bound.df)[7] <- "registered_editfilter_hits"
  names(bound.df)[8] <- "registered_editfilter_prohibits"
  
  #Save
  blockr_file_path <- file.path(getwd(),"Data","additional_graphing.tsv")
  write.table(bound.df, file = blockr_file_path, col.names = TRUE,
              row.names = FALSE, sep = "\t")
  
  #Return
  return(bound.df)
}

data.df <- data_read.fun()

graph.fun <- function(){
  
  #Linear regression; what's the relationship between AV blocks and registrations?
  regression_graph_registrations <- ggplot(data.df, aes(x = blocks,y = registrations))+
    geom_point(shape=3) +
    geom_smooth(method = "lm", se=TRUE, formula = y ~ x) +
    geom_text(aes(x = 500, y = 250000, label = trickstr::r2_display(lm = lm(registrations ~ blocks, data.df))), parse = TRUE) +
    labs(x = "bad-faith blocks (monthly)", y = "Registrations") +
    ggtitle("Relationship between blocks and registrations, by month \n(2006-2013)") +
    scale_x_continuous(expand = c(0.01,0.01)) +
    scale_y_continuous(expand = c(0.01,0.01))
    
  #Print
  ggsave(filename = file.path(getwd(),"Graphs","registrations_versus_blocks.png"),
         plot = regression_graph_registrations,
         width = 8,
         height = 8,
         units = "in")
  
  #R2 and other data
  sink(file = file.path(getwd(),"Metadata", "registrations_versus_blocks.txt"))
  print(summary(lm(formula = registrations ~  blocks, data = data.df,)))
  sink()
  
  #Regression
  regression_graph_registrations_w_edits <- ggplot(data.df, aes(x = blocks,y = registrations_with_edits))+
    geom_point(shape=3) +
    geom_smooth(method = "lm", se=TRUE, formula = y ~ x) +
    geom_text(aes(x = 750, y = 120000, label = trickstr::r2_display(lm = lm(registrations_with_edits ~ blocks, data.df))), parse = TRUE) +
    labs(x = "bad-faith blocks (monthly)", y = "Registrations") +
    ggtitle("Relationship between blocks and registrations of accounts that edit\n by month (2006-2013)") +
    scale_x_continuous(expand = c(0.01,0.01)) +
    scale_y_continuous(expand = c(0.01,0.01))
  
  #Print
  ggsave(filename = file.path(getwd(),"Graphs","registrations_w_edits_versus_blocks.png"),
         plot = regression_graph_registrations_w_edits,
         width = 8,
         height = 8,
         units = "in")
  
  #R2 and other data
  sink(file = file.path(getwd(),"Metadata", "registrations_w_edits_versus_blocks.txt"))
  print(summary(lm(formula = registrations_with_edits ~  blocks, data = data.df,)))
  sink()
  
  #Subset data
  data.df <- data.df[data.df$timestamp >= 200903,]
  
  #Regression
  regression_graph_anonymous_editfilter_hits <- ggplot(data.df, aes(x = blocks,y = anonymous_editfilter_hits))+
    geom_point(shape=3) +
    geom_smooth(method = "lm", se=TRUE, formula = y ~ x) +
    geom_text(aes(x = 400, y = 100000, label = trickstr::r2_display(lm = lm(anonymous_editfilter_hits ~ blocks, data.df))), parse = TRUE) +
    labs(x = "bad-faith blocks (monthly)", y = "EditFilter hits on anonymous edits") +
    ggtitle("Relationship between blocks and editfilter hits \n anonymous hits, by month (2009-2013)") +
    scale_x_continuous(expand = c(0.01,0.01)) +
    scale_y_continuous(expand = c(0.01,0.01))
  
  #Print
  ggsave(filename = file.path(getwd(),"Graphs","anon_abusefilter_hits_versus_blocks.png"),
         plot = regression_graph_anonymous_editfilter_hits,
         width = 8,
         height = 8,
         units = "in")
  
  #R2 and other data
  sink(file = file.path(getwd(),"Metadata", "anon_abusefilter_hits_versus_blocks.txt"))
  print(summary(lm(formula = anonymous_editfilter_hits ~  blocks, data = data.df,)))
  sink()  
  
  #Regression
  regression_graph_anonymous_editfilter_prohibits <- ggplot(data.df, aes(x = blocks,y = anonymous_editfilter_prohibits))+
    geom_point(shape=3) +
    geom_smooth(method = "lm", se=TRUE, formula = y ~ x) +
    geom_text(aes(x = 500, y = 40000, label = trickstr::r2_display(lm = lm(anonymous_editfilter_prohibits ~ blocks, data.df))), parse = TRUE) +
    labs(x = "bad-faith blocks (monthly)", y = "EditFilter prohibits on anonymous edits") +
    ggtitle("Relationship between blocks and editfilter prohibits \n anonymous prohibits, by month (2009-2013)") +
    scale_x_continuous(expand = c(0.01,0.01)) +
    scale_y_continuous(expand = c(0.01,0.01))

  #Print
  ggsave(filename = file.path(getwd(),"Graphs","anon_abusefilter_prohibits_versus_blocks.png"),
         plot = regression_graph_anonymous_editfilter_prohibits,
         width = 8,
         height = 8,
         units = "in")
  
  #R2 and other data
  sink(file = file.path(getwd(),"Metadata", "anon_abusefilter_prohibits_versus_blocks.txt"))
  print(summary(lm(formula = anonymous_editfilter_prohibits ~  blocks, data = data.df,)))
  sink()
  
  #Regression
  regression_graph_registered_editfilter_hits <- ggplot(data.df, aes(x = blocks,y = registered_editfilter_hits))+
    geom_point(shape=3) +
    geom_smooth(method = "lm", se=TRUE, formula = y ~ x) +
    geom_text(aes(x = 300, y = 24000, label = trickstr::r2_display(lm = lm(registered_editfilter_hits ~ blocks, data.df))), parse = TRUE) +
    labs(x = "bad-faith blocks (monthly)", y = "EditFilter hits on registered edits") +
    ggtitle("Relationship between blocks and editfilter hits \n registered hits, by month (2009-2013)") +
    scale_x_continuous(expand = c(0.01,0.01)) +
    scale_y_continuous(expand = c(0.01,0.01))

  #Print
  ggsave(filename = file.path(getwd(),"Graphs","registered_abusefilter_hits_versus_blocks.png"),
         plot = regression_graph_registered_editfilter_hits,
         width = 8,
         height = 8,
         units = "in")
  
  #R2 and other data
  sink(file = file.path(getwd(),"Metadata", "registered_abusefilter_hits_versus_blocks.txt"))
  print(summary(lm(formula = registered_editfilter_hits ~  blocks, data = data.df,)))
  sink()
  
  #Regression
  regression_graph_registered_editfilter_prohibits <- ggplot(data.df, aes(x = blocks,y = registered_editfilter_prohibits))+
    geom_point(shape=3) +
    geom_smooth(method = "lm", se=TRUE, formula = y ~ x) +
    geom_text(aes(x = 400, y = 6000, label = trickstr::r2_display(lm = lm(registered_editfilter_prohibits ~ blocks, data.df))), parse = TRUE) +
    labs(x = "bad-faith blocks (monthly)", y = "EditFilter prohibits on registered edits") +
    ggtitle("Relationship between blocks and editfilter prohibits \n registered prohibits, by month (2009-2013)") +
    scale_x_continuous(expand = c(0.01,0.01)) +
    scale_y_continuous(expand = c(0.01,0.01))
  
  #Print
  ggsave(filename = file.path(getwd(),"Graphs","registered_abusefilter_prohibits_versus_blocks.png"),
         plot = regression_graph_registered_editfilter_prohibits,
         width = 8,
         height = 8,
         units = "in")
  
  #R2 and other data
  sink(file = file.path(getwd(),"Metadata", "registered_abusefilter_prohibits_versus_blocks.txt"))
  print(summary(lm(formula = registered_editfilter_hits ~  blocks, data = data.df,)))
  sink()
}

#Run
graph.fun()