### Script for visualization of invacost data by activity sector
## written by Emma J. Hudgins, January 22, 2019
## emma.hudgins@mail.mcgill.ca

rm(list = ls())
library(rstan)
library(shinystan)
library(VGAM)
library(ggplot2)
library(viridis)
library(invacost)
library(readxl)
require(dplyr)
require(countrycode)
library(htmlwidgets)
library(htmltools)
library(wbstats)

data(invacost)

theme_set(theme_bw())

options(stringsAsFactors=FALSE)



# #### Data clean
# 
# if(any(is.na(invacost$Cost_estimate_per_year_2017_USD_exchange_rate)))
# {
#   invacost <- invacost[-which(is.na(invacost$Cost_estimate_per_year_2017_USD_exchange_rate)), ]
# }
# 
# invacost$Publication_year <- as.numeric(invacost$Publication_year)
# 
# 
# invacost$Impacted_sector_2 <- ifelse(grepl('/', invacost$Impacted_sector, ignore.case = T), "DiverseUnspecified", invacost$Impacted_sector)
# invacost$Official_country <- ifelse(grepl('/', invacost$Official_country, ignore.case = T), "DiverseUnspecified", invacost$Official_country)
# 
# invacost$Impacted_sector_2[is.na(invacost$Impacted_sector2)] <- "DiverseUnspecified" ## replace na values by mixed
# invacost$Official_country[is.na(invacost$Official_country)] <- "DiverseUnspecified"
# 
# unique(invacost$Impacted_sector_2)
# 
# #### Keep only highly reliable observation costs
# invacost <- invacost[which(invacost$Method_reliability == "High"), ]
# invacost <- invacost[which(invacost$Implementation == "Observed"), ]
# invacost<-subset(invacost, grepl("Unit|Site|Country|SIte|unit|Regional|site|country|Continental", invacost$Spatial_scale))
# invacost<-subset(invacost, Geographic_region!="Diverse/Unspecified")
# 
# uncertain.starts <- invacost[which(invacost$Time_range == "Period" &
#                                      is.na(invacost$Probable_starting_year)), ]
# 
# unknown.periods <- invacost[which(is.na(invacost$Time_range)), ]
# 
# # Applying the filter
# invacost <- invacost[-which(invacost$Cost_ID %in% c(uncertain.starts$Cost_ID,
#                                                     unknown.periods$Cost_ID)), ]
# 
# 
# # Number of rows after filtering
# nrow(invacost)
# 
# if(any(is.na(invacost$Cost_ID)))
# {
#   invacost <- invacost[-which(is.na(invacost2$Cost_ID)), ]
# }
# 
# invacost<-invacost[which(is.na(invacost$Probable_starting_year_adjusted)==F),]
# invacost<-invacost[which(is.na(invacost$Probable_ending_year_adjusted)==F),]
# 
# # Expanding and formating the database
# db.over.time <- expandYearlyCosts(invacost,
#                                   startcolumn = "Probable_starting_year_adjusted",
#                                   endcolumn = "Probable_ending_year_adjusted")
# db.over.time <- dplyr::filter(db.over.time, Impact_year <= "2020")
# db.over.time <- dplyr::filter(db.over.time, Impact_year >= "1970")
# 
# invacost_cln <- db.over.time
# invacost_cln$Cost_estimate_per_year_2017_USD_exchange_rate<-as.numeric(invacost_cln$Cost_estimate_per_year_2017_USD_exchange_rate)
# invacost_cln$Type_of_cost_merged<-gsub('Unspecified', "Mixed", invacost_cln$Type_of_cost_merged)
# invacost_cln$Type_of_cost_merged[which(is.na(invacost_cln$Type_of_cost_merged))]<-"Mixed"
# 
# 
# invacost_agg<-invacost_cln %>% group_by(Species, Kingdom, Phylum, Class, Order, Family, Genus, Impacted_sector_2,Type_of_cost_merged, Environment) %>% summarise_if(is.numeric, sum)
# 
# invacost_agg<-invacost_agg[,c(1:10,22)]
# 
# colnames(invacost_agg)[11]<-"Cum.Cost"
# invacost_agg$Cum.Cost<-as.numeric(as.character(invacost_agg$Cum.Cost))
# sumcost<-invacost_agg%>%group_by(Impacted_sector_2)%>%summarise_at('Cum.Cost',sum)
# 
# 
# ### Import sTwist ###
# stwist<-read.table('../Givers_takers/data/sTwist_database.csv', header=T)
# colnames(stwist)[3]<-'Species'
# colnames(stwist)[1]<-"Official_country"
# stwist$code<-countrycode(stwist$Official_country, 'country.name', 'iso3c')
# invacost_cln$code<-countrycode(invacost_cln$Official_country, 'country.name', 'iso3c')
# clip_spp<-merge(stwist, invacost_cln, by=c("Species", "code"), all=T)
# 
# cabi_categories<-read.csv('cabi_by_sector_v2.csv')
# 
# cabi_Agriculture<-subset(cabi_categories,Agriculture=="Negative")
# cabi_Fisheries<-subset(cabi_categories,Fisheries%in%c(NA, "exclude", "None, None", "None, None, None, None", "Positive, Positive")==F)
# cabi_Health<-subset(cabi_categories,Health%in%c(NA, "exclude", "None, None", "None", "Positive")==F)
# cabi_Health<-subset(cabi_Health, ispathogen=="N")
# cabi_Forestry<-subset(cabi_categories, Forestry%in%c(NA, "exclude", "None, None", "None", "Positive")==F)
# 
# 
# invacost_sub<-invacost_cln
# colnames(invacost_sub)[48]<-"Cost"
# GDP<-wb_data('NY.GDP.MKTP.CD', unique(invacost_sub$code), start_date = 2017, end_date = 2017)
# invacost_sub$GDP<-GDP$NY.GDP.MKTP.CD[match(invacost_sub$code,GDP$iso3c)]
# invacost_sub$GDP[which(is.na(invacost_sub$GDP))]<-mean(invacost_sub$GDP) #VEN and TWN
# invacost_sub$Cost_GDP<-0
# invacost_sub$Cost_GDP<-invacost_sub$Cost/invacost_sub$GDP
# 
# pathways<-read.csv('scrap/intro_pathways_cabi_v3.csv')[,1:25]
# colnames(pathways)[1]<-"Species"
# pathways$food[which(pathways$Species=="Bubalus bubalis")]<-1
# invacost_sub<-merge(invacost_sub, pathways, by="Species")
# colSums(invacost_sub[,73:95]) # pathways of introduction for these species
# 
# #subset by cost type, differentiate plant vs. animal in pet and agri pathways
# invacost_sub$all_other<-rep(0, nrow(invacost_sub))
# invacost_sub$all_other[which(rowSums(invacost_sub[,c(91:93,95)])==0)]<-1
# 
# 
# pet_path<-c('pet',  'aquarium', 'botanical_zoo', 'horticulture', 'ornamental',  'cut_flower', 'nursery')
# agri_path<-c('animal', 'seed', 'live_food', 'food') 
# for_path<-c('timber', 'forestry')
# fish_path<-c('stocking', 'hunt_fish', 'fisheries')
# #health_path<-c('medicinal')
# 
# 
# density<-invacost_sub %>% group_by(Kingdom,Phylum,Class, Order, Species, Environment, Geographic_region, Impacted_sector_2, Type_of_cost_merged) %>% summarise_if(is.numeric, sum)
# 
#   density<-as.data.frame(density)
#   net<-(density[1,-47])
#   net[1,]<-NA
#   net$pathway<-NA
#   for (i in 1:nrow(density))
#   {
#     names<-colnames(density[i,c(44:46,48:49)])[which(density[i,c(44:46,48:49)]>0)]
#     for (j in 1:length(names))
#     {
#       net<-rbind(net, setNames(cbind(density[i,-47], names[j]),c(colnames(density)[-47], "pathway")))
#        net[i,"Cost"]<-(density[i,"Cost"]*density[i, names[j]])/sum(density[i,c(44:46,48:49)])
#      net[i,"Cost_GDP"]<-(density[i,"Cost_GDP"]*density[i, names[j]])/sum(density[i, c(44:46,48:49)])
# 
#     }
#   }
# 
# net<-net[2:nrow(net),]
# library(networkD3)
# 
# net<-subset(net, Impacted_sector_2%in%c("Agriculture", "Fishery", "Forestry"))


#write.csv(net, file="net_activitysector_v3.csv", row.names=F)
net<-read.csv('net_activitysector_v3.csv')

links = data.frame(source=c(net$pathway),target=c(net$Impacted_sector_2),value=net$Cost, value2=1,value3=net$Cost_GDP, pathway=net$pathway)

nodes=data.frame(name=c(as.character(links$source), as.character(links$target)) %>% unique())
links$IDsource=match(links$source, nodes$name)-1 
links$IDtarget=match(links$target, nodes$name)-1
links<-links %>% group_by(IDsource, IDtarget, pathway) %>% summarise_if(is.numeric, sum, na.rm=T)


#my_color <- 'd3.scaleOrdinal(d3.schemeCategory20)'
my_color<-'d3.scaleOrdinal() .domain(["all_agri", "all_pet","all_other","all_for","all_fish","Fishery","Agriculture","Forestry"]) .range(["#FFAD00","#CC79A7","#D9D9D9", "#009E73","#56B4E9","#56B4E9","#FFAD00", "#009E73"])'

#Breaks colour matching but gives nicer names
#nodes[1:5,1]<-c("Other", "Agriculture", "Culture", "Forestry", "Fishery")


spp_viz<-sankeyNetwork(Links = links, Nodes = nodes, Source = "IDsource", Target = "IDtarget", Value = "value2", NodeID = "name", LinkGroup="pathway", colourScale = my_color, fontSize = 16) #plot by species
spp_viz<-prependContent(spp_viz, tags$div("Species Flows", style=("font-family: Helvetica; font-size:12; text-align: center")) )
spp_viz

gdp_viz<-sankeyNetwork(Links = links, Nodes = nodes, Source = "IDsource", Target = "IDtarget", Value = "value", NodeID = "name",LinkGroup="pathway", colourScale = my_color, fontSize = 16, iterations=0) #plot by cost, scaled by country gdp
gdp_viz<-prependContent(gdp_viz, tags$div("Cost Flows", style=("font-family: Helvetica; font-size:12; text-align: center")) )
gdp_viz

cost_gdp_viz<-sankeyNetwork(Links = links, Nodes = nodes, Source = "IDsource", Target = "IDtarget", Value = "value3", NodeID = "name",LinkGroup="pathway", colourScale = my_color, fontSize = 16, iterations=0) #plot by cost, scaled by country gdp
cost_gdp_viz<-prependContent(cost_gdp_viz, tags$div("Cost Flows (Scaled by 2017 GDP)", style=("font-family: Helvetica; font-size:12; text-align: center")) )
cost_gdp_viz


                     
                     
                     
                     