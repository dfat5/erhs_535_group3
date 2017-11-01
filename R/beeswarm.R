install.packages("beeswarm")
library(beeswarm)
library(dplyr)
?beeswarm

str(cmax)
cmax

beeswarm(cmax$PLA)


# good link to display the options of beeswarm as well as code chunks to make the graphs
# package does not come with any demo data sets. Need more data in order to generate meaningful plots  
#https://flowingdata.com/2016/09/08/beeswarm-plot-in-r-to-show-distributions/
