# Fit values
fitvals <- function(df){
  
  #Generate list of unique regexes
  regex_uniques <- unique(x$regex)
    
  #Generate list of unique timestamps, repeating by the number of uniques
  uniques <- rep(x = unique(x$month),
                 times = length(regex_uniques))
  
  #Format as data frame
  template.df <- as.data.frame(uniques)
  template.df$uniques <- as.character(template.df$uniques)
  template.df$uniques <- template.df$uniques[order(template.df$uniques)]
  template.df <- cbind(template.df,regex_uniques)
  template.df$hits <- NA
  names(template.df) <- c("month","regex")
  
  #Merge
  results.df <- ddply(.data = template.df,
                      .variables = c("month","regex"),
                      .fun = function(x){
                        
                        if(length(df$hits[df$month == x$month & df$regex == x$regex]) == 0){
                          
                          x$hits <- 0
                          
                        } else {
                          
                          x$hits <- df$hits[df$month == x$month & df$regex == x$regex]
                        }
                        
                        return(x)
                      }
  )
  
  #Return
  return(results.df)
}