library(scales)
library(dplyr)

data = read.csv("census_2022_2002.csv")
data = data[ - c(429,430),] # USA entfernen da NA

data22 = data[data$Year == 2022,]
# Daten zu 2022 (fuer 1 bis 3)

data02_clean = na.omit(data02) 
data22_clean = semi_join(data22, data02_clean, by= "Country")
# Daten wenn beide Jahre existieren (fuer 4)

