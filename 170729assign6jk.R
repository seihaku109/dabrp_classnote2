if (!require(tidyverse)) install.packages("tidyverse") 
if (!require(data.table)) install.packages("data.table") 
if (!require(arules)) install.packages("arules") 
library(tidyverse)
library(data.table)
library(arules)

wifiseoul<-fread("./data/wifi.csv",  encoding = "UTF-8")
wifiseoul
