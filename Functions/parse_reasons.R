parse_reasons <- function(x){
  
  #Split x up by date...
  x <- x[, j = {
    
    #Copy .SD
    sd_copy <- copy(.SD)
    
    #Construct output vector
    reg_match_result <- character(.N)
    
    #For each regex
    for(i in seq_along(regex_list)){
      
      #Write the category to matches of that regex
      reg_match_result[grepl(x = .SD$reason, pattern = regex_list[[i]][2],
                             perl = TRUE, useBytes = TRUE, ignore.case = TRUE)] <- regex_list[[i]][1]
      
    }
    
    #Handle unmatched blocks
    reg_match_result[reg_match_result == ""] <- "misc"
    
    #Write and return
    sd_copy$reason_category <- reg_match_result
    sd_copy
  }, by = "timestamp"]
  
  #Write handcoded dataset to file
  write.table(x = data.table(x$reason, x$reason_category), file = file.path(getwd(),"Datasets",HANDCODE_SAVE),
              row.names = FALSE, quote = TRUE, sep = "\t")
  
  #Limit to things we care about
  x <- x[,c("reason","registration") := NULL]
  
  #Return
  return(x)
  
}