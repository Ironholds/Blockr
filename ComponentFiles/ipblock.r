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
  query.df <- retrieve.fun(statement = "
          SELECT ipblocks.ipb_reason AS reason,
            ipblocks.ipb_expiry AS expiry,
            user.user_editcount,
            substring(ipblocks.ipb_timestamp,1,4) AS block_timestamp
          FROM ipblocks LEFT JOIN user ON ipblocks.ipb_user = user.user_id
          WHERE ipblocks.ipb_timestamp BETWEEN 20060101010101 AND 20121231235959;"
  )
  
  #Take the data and run it through the regexes, outputting the appropriate results.
  regex.fun(x = query.df, graphname = "Extant blocks, by year", is.testing = FALSE, fileprefix = "ipblocks")
}

ipblock.fun()