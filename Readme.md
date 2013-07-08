#Blockr
_Blockr_ is a set of R scripts that analyses blocking rationales on the English-language Wikipedia. In doing so, it attempts to divine data about proportionate increases or decreases in types of misbehaviour over time.

Interesting things found using the Blockr scripts will usually be posted on [my blog](https://blog.ironholds.org) ; if you spot a bug or have any suggestions, drop me a line.

__Author:__ Oliver Keyes<br>
__License:__ [MIT](http://opensource.org/licenses/MIT)<br>
__Status:__ In development

##Contents
At the moment, MetaAnalysis contains three central components, in the _ComponentFiles_ folder. In order, these are:

* _ipblock.r_: runs the analysis over the _ipblocks_ table, which contains all currently extant blocks - in other words, the fleeting temporary blocks given out in the last N days, along with all indefinites.
* _logging.r_: runs the analysis over the _logging_ table, which contains all block actions taken over all time - or for our purposes, since 2006. 
* _registration\_data.r_: provides details on comparative block and registration numbers, to (amongst other things) identify the probability of a user being indefinitely blocked, and how this has changed over time.

##Dependencies
The biggest one is access to the Wikimedia analytics slaves - if you don't have that, this project is unlikely to be helpful to you unless you have a particular fondness for stealing my terrible, terrible code. MetaAnalysis scripts are also dependent on several R packages, specifically:

* [Plyr](https://plyr.had.co.nz/)
* [ggplot2](https://ggplot2.org/)
* [RMySQL](https://cran.r-project.org/web/packages/RMySQL/index.html)
* [Trickstr](https://github.com/Ironholds/trickstr)

In addition, the scripts depend on a config file. This takes the structure of:

	analytics_server <- "foo" #the server that contains the db you're running this on - presumably s1-analytics-slave.eqiad.wmnet
	analytics_user <- "bar" # the username for that server
	analytics_database <- "baz" the database, presumably enwiki
	analytics_pass <- "quz" #password for the user...you get the picture
	
	#And then you read in the libraries specified above.```
	
##To-do
* Explore several hypotheses around the dramatic shift in vandalism-related blocks.
* Have registration\_data.r run off the logging table for blocks, too, allowing for an estimation of whether a user would be blocked at all, not just indefinitely blocked.
* Generate the same data for anonymous contributors.
