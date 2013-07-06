#UserSurvival - a project to accurately triage data on blocked Wikipedia users, identify
#the underlying rationales and test various hypotheses as to any outcome
#
#US_functions.r contains the project's global, reused functions
#
# @Year = 2013
# @Copyright: Oliver Keyes
# @License = MIT (http://opensource.org/licenses/MIThttp://opensource.org/licenses/MIT)

#Function to enclose RMySQL's querying abilities, given the commonality of names here.
sql.fun <- function(query_statement){
  
  #Open a connection
  con <- dbConnect(drv = "MySQL",
                   username = analytics_user,
                   password = analytics_pass,
                   host = analytics_pass,
                   dbname = analytics_pass)
  
  #Send the query
  QuerySend <- dbSendQuery(con, statement = query_statement_
  
  #Retrieve output of query
  output <- fetch(QuerySend, n = -1)
  
  #Kill connection
  dbDisconnect(con)
  
  #Return output
  return(output)
}

#Function to take a dataset and run the regexes over it.

regex.fun <- function(input_data){
  
  #Create list of regexes
  regex_list <- list(c("(spam|advertis(ement|ing)|promot(e|ion))"),
                     c("(arb(itration|com)|defamat(ion|ory)|vandal(|ism)|disrupti(ve|on)|WP:BLP|troll|attack|bad faith|harr?ass?(ment)|abus(e|ive)|vau(|block)|suppress|stalk|phish(ing|)|(death|legal) threat|biographies of living|hoax|rac(ist|ial)|copy(right|vio)|threatban|nonsense|(in|un|)civil(ity)|WP:VAND|WP:LEGAL|edit( |-)war(ring)|schoolblock|revert rule|\\drr|deliberately triggering|outing|anonblock|battle(|field|ground)|topic ban)"),
                     c("((?-i)LTA(?i)|sock|eva(de|sion|ding)|JarlaxleArtemis|grawp|WP:LTA|term abuse|multiple accounts|checkuser|MascotGuy|Jessica Liao|Grundle2600|Swale|sleeper|willy|puppet|\\{\\{WoW\\}\\}|reincarnaton|ararat|CU block|banned)"),
                     c("(username|uw-(uhblock|softestblock)|(softer|cause|u)block|impost(or|er)|too similar|similar to existing user or recent meme|\\{\\{unb|contact an administrator for verification|impersonat(or|ion|ing))"),
                     c("(torblock|blocked proxy|webhostblock|\\{\\{tor)"))
  
  output_data.df <- ddply(.data = input_data,
                          .var = "block_timestamp",
                          .fun = function(x){
                            
                            #Pin input data
                            to_run.df <- input_data
                            
                            #Create empty vector.
                            to_return.vec <- vector()
                            
                            #Grab length
                            reg_length <- length(regex_list)
                            
                            #And now we loop.
                            for(i in (1:reg_length){
                              
                              #Run regexes
                              grepvec <- grepl(pattern = regex_list[i],
                              x = to_run.df$reason,
                              perl = TRUE,
                              ignore.case = TRUE)
                              
                              #Number of rows that match
                              to_return.vec[i] <- length(grepvec[TRUE])
                              
                              #Non-matches
                              to_run.df <- to_run.df[!grepvec,]
                            }
                            
                            #Include non-matches
                            to_return.vec[reg_length+1] <- nrow(to_run.df)
                            
                            #Return
                            return(to_return.vec)}
                          )
  #Return
  return(output_data.df)
}

#Function to take the dataset and run the regexes over it.
regex.fun <- function(x, graphname, fileprefix, is.testing){
  
  #Retrieve an appropriate sample size
  samplesize <- sample_size(x = x,
    variable = "block_timestamp",
    percentage = 0.20)
  
  #Retrieve sample of users indefinitely blocked, pivoting on block date. 
  blocked.df <- ddply(.data = x,
    .variables = "block_timestamp",
    .fun = function(x){
      
      #sample from the subset to produce normalised data, operating off the value of samplesize
      sample.df <- dfsample(df = x, size = samplesize)
      
      return(sample.df)
      #wrapping function to iterate over
      metafun.fun <- function(item, reg_pattern){
        
        #Run regexes
        grepvec <- grepl(pattern = reg_pattern,
        x = item$reason,
        perl = TRUE,
        ignore.case = TRUE)
        
        #Number of rows that match
        nummatch <- length(grepvec[TRUE])
        
        #Grab matches
        match <- item[grepvec,]
        
        #Grab non-matches
        nonmatch <- item[!grepvec,]
        
        #Include regex name in matches
        match$regex_matched <- as.character(substitute(reg_pattern))
        
        #Bind as a list
        to.return <- list(nummatch,match,nonmatch)
        
        #Return
        return(to.return)
      }

      #If we're testing, export matches/non-matches for handcoding
      if(is.testing == TRUE){
        
        #Stitch
        matches.df <- rbind(
          as.data.frame(spam[2]),
          as.data.frame(disruption[2]),
          as.data.frame(socks[2]),
          as.data.frame(usernames[2]),
          as.data.frame(proxies[2])
        )
      
        #Write matches
        tsv_wrapper(x = matches.df,
          file = file.path(
            getwd(), "Output", paste(fileprefix,"_matches.tsv", sep = "")
          )
        )
        
        #Write non-matches
        tsv_wrapper(x = as.data.frame(proxies[3]),
          file = file.path(
            getwd(), "Output", paste(fileprefix,"_non_matches.tsv", sep = "")
          )
        )
      }
        
      #Whether we're testing or not, bind the results together and return them.
      results <- c(as.numeric(spam[1]),as.numeric(disruption[1]),as.numeric(socks[1]),as.numeric(usernames[1]),as.numeric(proxies[1]),as.numeric(nrow(as.data.frame(proxies[3]))))
      
      #Bind them all together in a vector
      output <- as.vector(results)
          
      #Return
      return(output)
    }
  )

  #Make non-hideous
  blocked.df <- rename(blocked.df,
    replace = c("V1" = "spam",
      "V2" = "disruption",
      "V3" = "sockpuppet",
      "V4" = "username",
      "V5" = "proxies",
      "V6" = "misc"
    )
  )

  #Melt it so that ggplot2 can accept it as a faceted/stacked dataset
  melted.df <- melt(blocked.df, id.vars = 1, measure.vars = 2:6)
  
  #Write dataset
  tsv_wrapper(x = melted.df,
    file = file.path(
      getwd(), "Output", paste(fileprefix,"_blocked.tsv", sep = "")
    )
  )
  
  #A line graph of blocking rationales over time
  line.graph.blocked <- ggplot(melted.df, aes(block_timestamp, value)) + 
    geom_freqpoly(aes(group = variable, colour = variable), stat = "identity") +
    labs(x = "Year", y = "Number of users") +
    ggtitle(graphname) +
    scale_x_discrete(breaks = 2006:2012) +
    scale_y_continuous(expand = c(0, 0)) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1))

  #Print
  ggsave(filename = file.path(getwd(),"Output", paste(fileprefix,"_blocked_line.png", sep = "")),
    plot = line.graph.blocked,
    width = 8,
    height = 8,
    units = "in")
}
 