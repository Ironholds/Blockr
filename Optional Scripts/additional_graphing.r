#Blockr - a project to accurately triage data on blocked Wikipedia users, identify
#the underlying rationales and test various hypotheses as to any outcome
#
#additional_graphing.r extracts information from the Wikimedia db to test the various different hypotheses.
#
# @Year = 2013
# @Copyright: Oliver Keyes
# @License = MIT (http://opensource.org/licenses/MIT)

#Wrapping function
additional_graph.fun <- function(){
  
  data_read.fun <- function(){
  
    #Read non-context-specific data in
    registrations.df <- read.delim(file = file.path(getwd(),"Data","registrations.tsv"), as.is = TRUE, header = TRUE)
    registrations_with_edits.df <- read.delim(file = file.path(getwd(),"Data","registrations_with_edits.tsv"), as.is = TRUE, header = TRUE)
    block_data.df <- subset(read.delim(file = file.path(getwd(),"Data","logging_registered_regex_matches.tsv"), as.is = TRUE, header = TRUE), variable == "bad.edit")
    
    #Read edit-filter data in
    editfilter.df <- read.delim(file = file.path(getwd(),"Data","editfilter_hits.tsv"), as.is = TRUE, header = TRUE)
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
      bind_NA <- rep(NA,(nrow(block_data.df)-nrow(loose.df)))
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
  
}

#Run
additional_graphs.fun()