#UserSurvival - a project to accurately triage data on blocked Wikipedia users, identify
#the underlying rationales and test various hypotheses as to any outcome
#
#ipblock.r outputs some of basic analysis documented at http://blog.ironholds.org/?p=31 - specifically,
#information around registration rates and block ratios
#
# @Year = 2013
# @Copyright: Oliver Keyes
# @License = MIT (http://opensource.org/licenses/MIT)

#Take the retrieved dataset, graph registrations by month and by year,
#output a randomised, normalised sample from each year
registration_data.fun <- function(){
  
  #Grab dataset
  query.df <- sql.fun(query_statement = "
          SELECT user.user_id AS id,
            substring(logging.log_timestamp,1,4) AS registration_date,
            user.user_editcount AS edit_count,
            ipblocks.ipb_expiry AS expiry
          FROM
            logging INNER JOIN user
              ON logging.log_title = user.user_name
            LEFT JOIN ipblocks
              ON ipblocks.ipb_user = user.user_id
          WHERE
            logging.log_type = 'newusers'
            AND logging.log_action NOT IN ('autocreate');"
  )
  
  #Make non-blocked users identifiable
  query.df$expiry[ is.na(query.df$expiry) ] <- as.numeric(0)  
  
  #Data on registrations
  registrations.fun <- function(x, graphname, filename){
    
    #Aggregate
    aggregate.data <- as.data.frame(
      table(x$registration_date
      )
    )
    
    #Write
    aggregate_file_path <- file.path(getwd(),"Output",paste("account_registrations_by_",filename,".png", sep = ""))
    write.table(aggregate.data, file = aggregate_file_path, col.names = TRUE,
                row.names = FALSE, sep = "\t", quote = FALSE)
    
    #Plot registrations
    registration.plot <- ggplot(data = aggregate.data,
      aes(x=Var1, y = Freq)) +
      geom_bar(width=.5, fill = "red", stat = "identity", position = "dodge") +
      labs(x = "Year", y = "Number of registrations") +
      theme(axis.text.x=element_text(angle = -90, hjust = 1)) +
      ggtitle(paste("English-language Wikipedia account registrations by",graphname, sep = " "))
    
    #Print
    ggsave(filename = file.path(getwd(),"Output", paste("account_registrations_by_",filename,".png", sep = "")),
      plot = registration.plot,
      width = 8,
      height = 8,
      units = "in")
  }

  #Basic analysis of how likely users are to get blocked, using a randomised sample
  block.proportion.fun <- function(input_data, filename){
    
    #For each registration year, work out those not-blocked, those indefinitely blocked, and those blocked for other dates 
    #Retrieve an appropriate sample size
    samplesize <- sample_size(x = input_data,
      variable = "registration_date",
    percentage = 0.20)
    
    sample.df <- rename(ddply(.data = input_data,
      .variables = "registration_date",
      .fun = function(x){
        
        #Sample
        df <- dfsample(df = x, size = samplesize)
        
        #Calculate indef, not-blocked and other numbers for each year
          results <- c(
            nrow(df[df$expiry == "0",]),
            nrow(df[df$expiry == "infinity",]),
            nrow(df[!df$expiry %in% c("0","infinity"),]))
            
          #Smush into a vector
          results.vector <- as.vector(results)
          
          #Add percentage
          results.vector[4] <- sum(results.vector[2:3])/sum(results.vector[1:3])
        
          #Return
          return(results.vector)
        }
      #Replace column names with something sensical
      ), replace = c(
          "V1" = "unblocked", "V2" = "indefinite", "V3" = "other", "V4" = "Percentage blocked"
      )
    )
    
    #Write
    aggregate_file_path <- file.path(getwd(),"Output", paste(filename,"_is_blocked.tsv", sep = ""))
    write.table(sample.df, file = aggregate_file_path, col.names = TRUE,
                row.names = FALSE, sep = "\t", quote = FALSE)    
  }
  
  #Run by year
  registrations.fun(x = query.df, filename = "year", graphname = "year")
  block.proportion.fun(input_data = query.df[query.df$registration_date >= 2006 & query.df$registration_date <= 2012,], filename = "year")
  
  #Subset to exclude non-editors and run again
  registrations.fun(x = query.df[query.df$edit_count >= 1,], filename = "year_with_edits", graphname = "year (>0 edits)")
  block.proportion.fun(input_data = query.df[query.df$edit_count >= 1 & query.df$registration_date >= 2006 & query.df$registration_date <= 2012,], filename = "year_with_edits")

}
registration_data.fun()
