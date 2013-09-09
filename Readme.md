#Blockr
_Blockr_ is a set of R scripts that analyses blocking rationales on the English-language Wikipedia. In doing so, it attempts to divine data about increases or decreases in types of misbehaviour over time.

Interesting things found using the Blockr scripts will usually be posted on [my blog](https://blog.ironholds.org); if you spot a bug or have any suggestions, drop me a line.

__Author:__ Oliver Keyes<br>
__License:__ [MIT](http://opensource.org/licenses/MIT)<br>
__Status:__ Stable

##Contents
At the moment, Blockr contains two types of analysis:

* _Core Scripts_: runs a basic analysis over the _ipblocks_ and _logging_ tables, categorising blocks, returning and saving the anonymised, aggregate data (in the _Data_ folder) and graphing the output (in the _Graphs_ folder).
* _Optional Scripts_: some optional data-retrieval and analysis that takes the output of the Core Scripts and returns, for example, more nuanced regression graphs, and comparisons with edit filter hits and registration times.

##Dependencies
The biggest one is access to the Wikimedia analytics slaves - if you don't have that, this project is unlikely to be helpful to you unless you have a particular fondness for stealing my terrible, terrible code. Blockr scripts are also dependent on several R packages, specifically:

* [Plyr](https://plyr.had.co.nz/)
* [ggplot2](https://ggplot2.org/)
* [RMySQL](https://cran.r-project.org/web/packages/RMySQL/index.html)
* [Trickstr](https://github.com/Ironholds/trickstr)

In addition, the scripts depend on a config file. This takes the structure of:

	#the server that contains the db you're running this on - presumably s1-analytics-slave.eqiad.wmnet
	analytics_server <- "foo"
	#the username for that server
	analytics_user <- "bar"
	#the database, presumably enwiki
	analytics_database <- "baz"
	#password for the user...you get the picture
	analytics_pass <- "quz"
	
	#And then you read in the libraries specified above.
	
##To-do
* Explore several hypotheses around the dramatic shift in vandalism-related blocks.
* Refactor the edit-filters script