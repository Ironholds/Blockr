block_edits.fun <- function(){
  
  #Include config variables and sql.fun
  source("config.r")
  source("functions.r")
  
  #Retrieve initial data
  query.df <- sql.fun("SELECT
                      logging.log_timestamp AS timestamp,
                      user.user_id AS userid
                      FROM logging INNER JOIN user ON logging.log_title = user.user_name
                      WHERE logging.log_timestamp BETWEEN 20060101010001 AND 20130831235959
                      AND log_type = 'block'
                      AND log_action = 'block';")
  
  #Remove duplicates, so we only have the first blocks for each user
  query.df <- query.df[!duplicated(query.df$userid),]
  
  #Create null vectors
  query.df$revision <- NA
  query.df$archive <- NA
  query.df$edits <- NA
  
  #Retrieve number of edits
  for(i in 1:nrow(query.df)){
    
    #Query to retrieve revision table entries
    revision <- as.numeric(sql.fun(paste("SELECT COUNT(*) FROM revision
                              WHERE rev_user = ",query.df[i,2],
                              " AND rev_timestamp <= '",query.df[i,1],"';",sep = "")))
    #Add revision hits
    query.df[i,3] <- revision
    
    archive <- as.numeric(sql.fun(paste("SELECT COUNT(*) FROM archive
                              WHERE ar_user = ",query.df[i,2],
                                        " AND ar_timestamp <= '",query.df[i,1],"';",sep = "")))
    #Add archive hits
    query.df[i,4] <- archive
    
    #Sum and add
    query.df[i,5] <- sum(revision, archive)
    
    if(i %in% seq(1,nrow(query.df),2000)){
      
      print(i)
    }
  
  }
  
  #Aggregate and save
  blockr_file_path <- file.path(getwd(),"Data","blocked_users.tsv")
  write.table(as.data.frame(table(query.df$edits), file = blockr_file_path, col.names = TRUE,
              row.names = FALSE, sep = "\t")
  
  #Plot
  density_plot <- ggplot(query.df, aes(x = edits))+
    geom_density() +
    scale_x_log10() +
    ggtitle("Density estimate of edits before first blocks (all blocked users, 2006-13, log10)")
  
  ggsave(filename = file.path(getwd(),"Graphs","density_estimate.png"),
         plot = density_plot,
         width = 8,
         height = 8,
         units = "in")
}

#Run
block_edits.fun()