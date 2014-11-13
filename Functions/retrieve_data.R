retrieve_data <- function(){
  
  #Grab block logs
  input_data <- mysql_query("SELECT
                            logging.log_timestamp AS timestamp,
                            logging.log_comment AS reason,
                            user.user_id AS subject,
                            user.user_registration AS registration,
                            logging.log_user AS performer
                            FROM logging LEFT JOIN user ON logging.log_title = user.user_name
                            WHERE logging.log_user NOT IN (SELECT user_id FROM user INNER JOIN user_groups ON user_id = ug_user AND ug_group = 'bot')
                            AND logging.log_type = 'block'
                            AND logging.log_action = 'block'
                            AND logging.log_deleted = 0;",
                            "enwiki")
  
  #Identify anons
  input_data$type[is.na(input_data$subject)] <- "Anonymous"
  input_data$type[is.na(input_data$type)] <- "Registered"
  input_data$subject[is.na(input_data$subject)] <- 0
  
  #Remove blocks that were clearly tests - i.e., made by user1 against user1
  input_data <- input_data[!input_data$subject == input_data$performer,]
  
  #Calculate elapsed time
  input_data$time_from_reg <- as.numeric(mw_strptime(input_data$timestamp)) - as.numeric(mw_strptime(input_data$registration))
  
  #Convert timestamps into year/months
  timestamps <- as.Date(mw_strptime(input_data$timestamp))
  day(timestamps) <- 1
  input_data$timestamp <- timestamps
  
  #Return
  return(input_data)
}