## Supporting Information  - R-script S1
## Script updated in 01 August 2022 by A.J. Turbelin and E. J. Hudgins

library(dplyr)
library(ggplot2)
library(invacost)
library(magrittr)
library(readxl)


rm(list=ls())
theme_set(theme_bw())

options(stringsAsFactors=FALSE)


data("invacost")

folder <- "ActivitySector" # change to path on local computer
## load species broad category file
spCat <- as.data.frame(read_xlsx(paste0(folder,"/D1_taxonomic_group_categories.xlsx"),
                                 na = c("NA", "#N/A", "#DIV/0!", "#VALEUR!",
                                        "Unspecified", "Unknown"),
                                 guess_max = 10000))


################################
## Data clean
## remove costs with na values
if(any(is.na(invacost$Cost_estimate_per_year_2017_USD_exchange_rate)))
{
  invacost <- invacost[-which(is.na(invacost$Cost_estimate_per_year_2017_USD_exchange_rate)), ]
}

invacost$Publication_year <- as.numeric(invacost$Publication_year)

## group fields with multiple Sectors, countries, or regions into "DiverseUnspecified"
invacost$Impacted_sector_2 <- ifelse(grepl('/', invacost$Impacted_sector, ignore.case = T), "DiverseUnspecified", invacost$Impacted_sector)
invacost$Official_country2 <- ifelse(grepl('/', invacost$Official_country, ignore.case = T), "DiverseUnspecified", invacost$Official_country)
invacost$Geographic_region2 <- ifelse(grepl('/', invacost$Geographic_region, ignore.case = T), "DiverseUnspecified", invacost$Geographic_region)

invacost$Impacted_sector_2[which(is.na(invacost$Impacted_sector_2))] <- "DiverseUnspecified" ## replace na values by mixed
invacost$Official_country2[is.na(invacost$Official_country2)] <- "DiverseUnspecified" ## replace na values by mixed

#unique(invacost$Impacted_sector_2)

## Keep only highly reliable observation costs
invacost <- invacost[which(invacost$Method_reliability == "High"), ]
invacost <- invacost[which(invacost$Implementation == "Observed"), ]

## Remove Diverse/Unspecified regional costs
invacost <- subset(invacost, grepl("Unit|Site|Country|SIte|unit|Regional|site|country|Continental", invacost$Spatial_scale))
invacost <- subset(invacost, Geographic_region!="Diverse/Unspecified")


## follow methods from Leroy, Boris, Diagne, Christophe, & Vaissière, Anne-Charlotte. 2020. INVACOST Database With Methods To Analyse Invasion Costs, Version: 0.2-4. R package.
uncertain.starts <- invacost[which(invacost$Time_range == "Period" &
                                     is.na(invacost$Probable_starting_year)), ]

unknown.periods <- invacost[which(is.na(invacost$Time_range)), ]

# Applying the filter
invacost <- invacost[-which(invacost$Cost_ID %in% c(uncertain.starts$Cost_ID,
                                                    unknown.periods$Cost_ID)), ]


if(any(is.na(invacost$Cost_ID)))
{
  invacost <- invacost[-which(is.na(invacost2$Cost_ID)), ]
}

invacost<-invacost[which(is.na(invacost$Probable_starting_year_adjusted)==F),]
invacost<-invacost[which(is.na(invacost$Probable_ending_year_adjusted)==F),]

# Expanding and formatting the database
db.over.time <- expandYearlyCosts(invacost,
                                  startcolumn = "Probable_starting_year_adjusted",
                                  endcolumn = "Probable_ending_year_adjusted")
db.over.time <- dplyr::filter(db.over.time, Impact_year <= "2020")
db.over.time <- dplyr::filter(db.over.time, Impact_year >= "1970")


invacost_cln <- db.over.time
invacost_cln$Cost_estimate_per_year_2017_USD_exchange_rate <- as.numeric(invacost_cln$Cost_estimate_per_year_2017_USD_exchange_rate)
invacost_cln$Type_of_cost_merged<-gsub('Unspecified', "Mixed", invacost_cln$Type_of_cost_merged)
invacost_cln$Type_of_cost_merged[which(is.na(invacost_cln$Type_of_cost_merged))] <- "Mixed"


invacost_cln$cost <- invacost_cln$Cost_estimate_per_year_2017_USD_exchange_rate
invacost_cln$cost_bil <- (invacost_cln$cost/1000000000)
invacost_cln$cost_mil <- (invacost_cln$cost/1000000)

invacost_cat <- left_join(invacost_cln, spCat, by = c("Environment_IAS","Kingdom", "Phylum", "Class", "Order", "Family", "Genus", "Species") )

invacost_cat$Group02 <- ifelse(grepl('Spartina', invacost_cat$Genus, ignore.case = T), "Plant", invacost_cat$Group02)
invacost_cat$Group02 <- ifelse(grepl('Candidatus', invacost_cat$Genus, ignore.case = T), "OtherOrganisms", invacost_cat$Group02)

names(invacost_cat)[names(invacost_cat) == 'Group02'] <- 'coarse_group'

write.csv(invacost_cat, paste0(folder,"/D2_ActivitySector_data_IC4_1.csv"))





