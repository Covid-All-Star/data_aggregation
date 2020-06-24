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

# re-ord ering the data and creating a new base 
ECD <- ECDPC_bis[order(ECDPC_bis$countryterritoryCode),]
View(ECD)

# Using the Oxford political response dataset 
# importing the data 
library(readxl)
political_response <- read_excel("OxCGRT_Download_070420_154408_Full.xlsx")
View(political_response)

# deleting the columns containing notes and confirmed deaths plus confirmed cases 
political_response <- political_response[,c(-1, -6, -9, -12, -15, -18, -21, -24, -26, -29, -31, -33, -35, -38, -40, -42, -44, -46, -48, -49, -50, -51, -52)]
political_response <- political_response[,c(-1,-31,-32,-33,-34)]
political_response <- political_response[,c(-1,-31,-32,-33,-34,-37,-38,-39,-40,-41,-42)]
# renaming the column on which the database will be merged 
names(political_response)[1] <- "countryterritoryCode"
names(political_response)[2] <- "dateRep"

# converting the dateRep variable in a date format 
political_response$dateRep <- as.Date(as.character(political_response$dateRep), format = "%Y%m%d")

# merging those two firsts datasets 

ECD <- as.data.frame(ECD)
political_response <- as.data.frame(political_response)

panelcov <- plyr::join(ECD, political_response, type = "left")
View(panelcov)

# working on the covid test database
Test_covid <- read_excel("Bureau/Coronavirus et commerce international/Données/Coronavirus/Test covid/Test_covid.xlsx")
View(Test_covid)
# removing non-essential columns
Test_covid <- Test_covid[, c(-1,-2,-6,-7,-8,-9,-10,-12,-13,-14,-15,-16,-17,-18)]
Test_covid <- Test_covid[,c(-5,-7)]
# renaming the merging variables 
names(Test_covid)[1] <- "dateRep"
names(Test_covid)[4] <- "countryterritoryCode"
# merging the datasets 
panelcov <- plyr::join(panelcov, Test_covid, type = "left")

# alternative version with owid database
test_owid <- read_excel("Data source/test_owid.xlsx")
View(test_owid)
test_owid <- test_owid[,c(-2,-3,-5,-6,-7,-8,-9,-10,-11,-12,-17,-18,-19,-20)]

names(test_owid)[1] <- "countryterritoryCode"
names(test_owid)[2] <- "dateRep"
panelcov <- plyr::join(panelcov, test_owid, type = "left")

# google mobility report data
Global_Mobility_Report.1 <- read.csv("~/Bureau/Coronavirus et commerce international/Données/Coronavirus/Data source/Global_Mobility_Report-1.csv")
View(Global_Mobility_Report.1)

Global_Mobility_Report$sub_region_1 <- as.character(Global_Mobility_Report$sub_region_1)
Global_Mobility_Report$sub_region_2 <- as.character(Global_Mobility_Report$sub_region_2)

mobility_report <- Global_Mobility_Report %>% filter(sub_region_1 == sub_region_2)
View(mobility_report)

mobility_report <- mobility_report[,c(-2,-3,-4)]
names(mobility_report)[1] <- "geoId"
names(mobility_report)[2] <- "dateRep"
names(mobility_report)[3] <- "mobility_retail_recreation"
names(mobility_report)[4] <- "mobility_grocery_pharmacy"
names(mobility_report)[5] <- "mobility_parks"
names(mobility_report)[6] <- "mobility_transportation"
names(mobility_report)[7] <-"mobility_workplaces"
names(mobility_report)[8] <- "mobility_residentials"

mobility_report$geoId <- as.character(mobility_report$geoId)
mobility_report$geoId[mobility_report$geoId == "GB"] <- "UK"
mobility_report$geoId[mobility_report$geoId == "GR"] <- "EL"


panelcov1 <- plyr::join(panelcov, mobility_report, type = "left")

# gini index work 
gini1 <- aggregate(gini$Year, by = list(gini$Code), max)
View(gini1)
names(gini1)[1] <- "Code"
View(gini1)
gini2 <- plyr::join(gini, gini1, type = "full")
View(gini2)
names(gini2)[5] <- "Yearmax"
gini3 <- gini2 %>% group_by(Code) %>% filter(Year == Yearmax)
View(gini3)

# merging
gini3 <- gini3[,c(-1,-3)]
names(gini3)[3] <- "G_measurment_year"
names(gini3)[1] <- "countryterritoryCode"
panelcov <- plyr::join(panelcov, gini3, type = "left")

# self reported trust
library(readxl)
trust <- read_excel("self-reported-trust-attitudes.xlsx")
View(trust)

trust1 <- aggregate(trust$Year, by = list(trust$Code), max)
View(trust1)
names(trust1)[1] <- "Code"
trust2 <- plyr::join(trust, trust1, type = "full")
View(trust2)
names(trust2)[5] <- "Yearmax"
trust3 <- trust2 %>% group_by(Code) %>% filter(Year == Yearmax)
View(trust3)

# merging
trust3 <- trust3[,c(-1,-3)]
names(trust3)[1] <- "countryterritoryCode"
names(trust3)[2] <- "Trust"
names(trust3)[3] <- "Trust_measurement_year"
panelcov <- plyr::join(panelcov, trust3, type = "left")
# adding gdp forecasts 
gdp_forecast <- read_excel("Data source/gdp_forecast.xlsx")
View(gdp_forecast)
gdp_forecast <- gdp_forecast[,c(-1)]
names(gdp_forecast)[1] <- "countryterritoryCode"
names(gdp_forecast)[2] <- "gdp_2020"
panelcov2 <- plyr::join(panelcov2, gdp_forecast, type = "left")

# working on the countries variable (not time dependent, i.e : beds, median age, density)
library(readxl)
Country_variable <- read_excel("Bureau/Coronavirus et commerce international/Données/Coronavirus/Country variable.xlsx")
View(Country_variable)
Country_variable <- Country_variable[,c(-1,-3,-4,-5,-7)]

names(Country_variable)[1] <- "countryterritoryCode"
panelcov <- plyr::join(panelcov, Country_variable, type = "left")

# adding the global health security index
GHS_index <- read_excel("Bureau/Coronavirus et commerce international/Données/Coronavirus/Data source/GHS index.xlsx")
View(GHS_index)
GHS_index <- GHS_index[,-2]
names(GHS_index)[1] <- "countryterritoryCode"
panelcov <- plyr::join(panelcov, GHS_index, type = "left")

# Removing some little countries 
panelcov1 <- panelcov1 %>% filter(geoId != "EH")
panelcov1 <- panelcov1 %>% filter(geoId != "FK")
panelcov1 <- panelcov1 %>% filter(geoId != "BQ")
panelcov1 <- panelcov1 %>% filter(geoId != "AI")

# Finally, adding the gtrends data 
library(readxl)
gtrends <- read_excel("Bureau/Coronavirus et commerce international/Données/Coronavirus/Google trend/gtrends.xlsx")
View(gtrends)
gtrends <- gtrends[,c(-2,-3,-4)]
names(gtrends)[2] <- "countryterritoryCode"
panelcov <- plyr::join(panelcov, gtrends,  type = "full")

# alternative version with gtrends data coming from the R package "gtrendsR"
library(gtrendsR)
library(dplyr)

# importing a country list in order to loop 
c_list = readLines("geo.csv")
 
# looping 
resultslist <- list()
for (country in c_list){
tryCatch({
keywords = c("Coronavirus")
time=("today 3-m")
channel='news'
trends = gtrends(keywords, gprop =channel,geo=country, time = time )
resultslist[[country]] <- trends$interest_over_time
}, error=function(e){})
}

# converting the result list in a proper dataframe 
library(plyr)
gtrends <- plyr::ldply(resultslist, rbind)
View(gtrends)

# arranging and aggregating
gtrends <- gtrends %>% filter(.id == geo)
gtrends <- gtrends[,c(-1,-4,-6,-7,-8)]
names(gtrends)[3] <- "geoId"
names(gtrends)[1] <- "dateRep"
names(gtrends)[2] <- "gtrends"
gtrends$geoId[gtrends$geoId == "GB"] <- "UK"
gtrends$geoId[gtrends$geoId == "GR"] <- "EL"
panelcov2 <- plyr::join(panelcov1, gtrends, type = "full")

# Exporting the database panelcov 
write.csv(panelcov, file = "panelcov.csv")

# The gtrends database with US as base comparison 
resultslist <- list()
for (country in c_list){
tryCatch({
keywords = c("Coronavirus")
combin= c("US", country)
time=("2020-01-01 2020-04-21")
channel='news'
trends = gtrends(keywords, gprop =channel,geo=combin, time = time )
resultslist[[country]] <- trends$interest_over_time
}, error=function(e){})
}

# the gtrends database with China as a base comparison 
resultslist <- list()
for (country in c_list){
tryCatch({
keywords = c("Coronavirus")
combin= c("CN", country)
time=("2020-01-01 2020-04-27")
channel='web'
trends = gtrends(keywords, gprop =channel,geo=combin, time = time )
resultslist[[country]] <- trends$interest_over_time
}, error=function(e){})
}

# mémo pratique (pour Alexis) 
fusionner les bases avec la manière suivant quand possible : 
panelcov <- plyr::join(a,b, type = "left")


