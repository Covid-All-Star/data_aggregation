# data_aggregation
The R-codes to aggregate the data from the different sources

# First data manipulation using the covid data from the European Center for Diseases Prevention and Control
# importing the data 
library(readxl)
ECDPC_corona <- read_excel("Bureau/Coronavirus et commerce international/Données/Coronavirus/ECDPC_corona.xlsx")
View(ECDPC_corona)
 
# ordering the data by date and country 
ECDPC_bis <- ECDPC_corona[order(ECDPC_corona$dateRep, ECDPC_corona$countryterritoryCode),]
View(ECDPC_bis)

# creating the cumulative sum of cases and deaths 
library(dplyr)
ECDPC_bis <- ECDPC_bis %>% group_by(countryterritoryCode) %>% mutate(cum_cases = cumsum(cases))
ECDPC_bis <- ECDPC_bis %>% group_by(countryterritoryCode) %>% mutate(cum_death = cumsum(deaths))

# re-ordering the data and creating a new base 
ECD <- ECDPC_bis[order(ECDPC_bis$countryterritoryCode),]
View(ECD)

# Using the Oxford political response dataset 
# importing the data 
library(readxl)
political_response <- read_excel("OxCGRT_Download_070420_154408_Full.xlsx")
View(political_response

# deleting the columns containing notes and confirmed deaths plus confirmed cases 
political_response <- political_response[,c(-6,-9,-12,-15,-18,-21,-23,-25,-27,-29,-31,-33,-35,-36,-37)]



