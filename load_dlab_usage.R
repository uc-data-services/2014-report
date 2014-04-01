require(RCurl)
#Need to load two usage csv files. these come from different time frames and different collection methonds --
#grab 2005-2012 from gdocs & read as dframe - these data were from paper sign in sheets that were  manually keyed
csv05_12 <- getURL("https://docs.google.com/spreadsheet/pub?key=0Aom2VaaJk0aodGh5Sy1SUmZWQXZFV19YNWdkbUZyLXc&single=true&gid=0&output=csv")
st05_12 <- read.csv(textConnection(csv05_12), header=TRUE, as.is=TRUE)
#grab 2012-14; a google csv stored in my drive.
st12_14<-read.csv( "/home/tim/tdennis@berkeley.edu/datalab/archived_signin_stats/stats_june2012_mar2014.csv",
                               head=TRUE,
                               as.is=TRUE
                              )
