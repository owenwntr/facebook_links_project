# NUTS level data

# 1. Distance X
# 2. EU Referendum Result x
# 3. EU born population X
# 4. EU migration 2015-16 
# 5. Change in EU migration rate
# 6. Social Connectedness Index X

library(tidyverse)
library(readxl)
if("plyr" %in% (.packages())){
  try(detach("package:ggbiplot", unload=TRUE))
  try(detach("package:plyr", unload=TRUE))
}

#2. EU Referendum Result

euref <- read_csv("data/EU-referendum-result-data.csv")

la_lookup <- read_csv("Local_Authority_District_(December_2018)_to_NUTS3_to_NUTS2_to_NUTS1_(January_2018)_Lookup_in_United_Kingdom.csv")

euref$nuts3 <- la_lookup$NUTS318CD[match(euref$Area_Code, la_lookup$LAD18CD)]
euref$nuts3[which(euref$Area_Code=="S12000015")] <- "UKM72"
euref$nuts3[which(euref$Area_Code=="S12000024")] <- "UKM77"

nuts3_data <- euref %>% group_by(nuts3) %>% summarise(valid = sum(Valid_Votes),
                                                       remain = sum(Remain),
                                                       leave = sum(Leave))

nuts3_data$pct_leave <- nuts3_data$leave/nuts3_data$valid

#3. EU Born Population

countryofbirth <- read_excel("data/populationbycountryofbirthandnationalityjul15tojun16.xls", 
                             sheet = "1.1", skip = 5)

countryofbirth <- countryofbirth[7:nrow(countryofbirth),]
countryofbirth$`European Union`[which(countryofbirth$`European Union` %in% c(":","c"))] <- 0

colnames(countryofbirth)[1] <- "area"

countryofbirth$nuts3 <- la_lookup$NUTS318CD[match(countryofbirth$area, la_lookup$LAD18CD)]

countryofbirth$All <- as.numeric(countryofbirth$All)
countryofbirth$`European Union` <- as.numeric(countryofbirth$`European Union`)

nuts3_cob <- countryofbirth %>% group_by(nuts3) %>% summarise(all = sum(All,na.rm=TRUE),
                                                              eu_born = sum(`European Union`,na.rm=TRUE))

nuts3_cob$eu_pct <- nuts3_cob$eu_born/nuts3_cob$all

nuts3_data$eu_cob_pct <- nuts3_cob$eu_pct[match(nuts3_data$nuts3, nuts3_cob$nuts3)]

# Scotland

scot_cob <- read_csv("data/pop-count-birth-tab3-counc-16-cob-cor.csv",skip=3)
scot_cob <- scot_cob[5:nrow(scot_cob),]
scot_cob <- scot_cob[which(!is.na(scot_cob$EU)),]
scot_cob$EU <- as.numeric(scot_cob$EU)
scot_cob$`Total UK Born` <- as.numeric(scot_cob$`Total UK Born`)
scot_cob$`Total Non-UK Born` <- as.numeric(scot_cob$`Total Non-UK Born`)
scot_cob$total <- scot_cob$`Total UK Born` + scot_cob$`Total Non-UK Born`
scot_cob$total[which(scot_cob$nuts3=="UKM66")] <- 23
colnames(scot_cob)[1] <- "council"
scot_cob$nuts3 <- la_lookup$NUTS318CD[match(scot_cob$council, la_lookup$LAD18NM)]


scot_nuts3_cob <- scot_cob %>% group_by(nuts3) %>% summarise(all = sum(total,na.rm=TRUE),
                                                             eu_born = sum(EU,na.rm=TRUE))

scot_nuts3_cob$eu_pct <- scot_nuts3_cob$eu_born/scot_nuts3_cob$all

nuts3_data$eu_cob_pct[which(is.na(nuts3_data$eu_cob_pct))] <- scot_nuts3_cob$eu_pct[match(nuts3_data$nuts3[which(is.na(nuts3_data$eu_cob_pct))], scot_nuts3_cob$nuts3)]

# 4. EU migration 2015-16 + 5. Change in EU migration rate

migration <- read_excel("data/2021lamistables.xlsx", 
                        sheet = "Migration Flows", skip = 1)

migration <- migration[,c(1,2,3,4,28,29)]
colnames(migration) <- c("code","name","pop11","inflow11","pop16","inflow16")
migration <- migration[2:nrow(migration),]

migration$inflow11_pct <- as.numeric(migration$inflow11)/as.numeric(migration$pop11)
migration$inflow16_pct <- as.numeric(migration$inflow16)/as.numeric(migration$pop16)

migration$inflow_change <-   migration$inflow16_pct - migration$inflow11_pct

migration$nuts3 <- la_lookup$NUTS318CD[match(migration$code, la_lookup$LAD18CD)]
migration$nuts3[which(migration$code=="E06000058")] <- "UKK21"
migration$nuts3[which(migration$code=="E06000060")] <- "UKJ13"
migration$nuts3[which(migration$code=="E06000059")] <- "UKK22"
migration$nuts3[which(migration$code=="E07000244")] <- "UKH14"
migration$nuts3[which(migration$code=="S12000049")] <- "UKM82"
migration$nuts3[which(migration$code=="S12000050")] <- "UKM84"
migration$nuts3[which(migration$code=="E06000061")] <- "UKF25"
migration$nuts3[which(migration$code=="E07000246")] <- "UKK23"
migration$nuts3[which(migration$code=="E06000062")] <- "UKF24"
migration$nuts3[which(migration$code=="E07000245")] <- "UKH14"

nuts3_migration <- migration %>% group_by(nuts3) %>% summarise(pop11 = sum(as.numeric(pop11)),
                                                               pop16 = sum(as.numeric(pop16)),
                                                               inflow11 = sum(as.numeric(inflow11)),
                                                               inflow16 = sum(as.numeric(inflow16)))

nuts3_migration$inflow11_pct <- as.numeric(nuts3_migration$inflow11)/as.numeric(nuts3_migration$pop11)
nuts3_migration$inflow16_pct <- as.numeric(nuts3_migration$inflow16)/as.numeric(nuts3_migration$pop16)

nuts3_migration$inflow_change <- nuts3_migration$inflow16_pct - nuts3_migration$inflow11_pct

nuts3_data$inflow16_pct <- nuts3_migration$inflow16_pct[match(nuts3_data$nuts3,nuts3_migration$nuts3)]
nuts3_data$inflow_change <- nuts3_migration$inflow_change[match(nuts3_data$nuts3,nuts3_migration$nuts3)]

nuts3_data <- nuts3_data[which(!is.na(nuts3_data$nuts3)),]
