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
require(plyr)
require(countrycode)
library(htmlwidgets)
library(htmltools)
setwd('~/Dropbox/InvaCost Workshop France/Projects/Activity Sector/')

data(invacost)

theme_set(theme_bw())

options(stringsAsFactors=FALSE)


#### Data clean

if(any(is.na(invacost$Cost_estimate_per_year_2017_USD_exchange_rate)))
{
  invacost <- invacost[-which(is.na(invacost$Cost_estimate_per_year_2017_USD_exchange_rate)), ]
}

invacost$Publication_year <- as.numeric(invacost$Publication_year)


invacost$Impacted_sector_2 <- ifelse(grepl('/', invacost$Impacted_sector, ignore.case = T), "DiverseUnspecified", invacost$Impacted_sector)


invacost$Impacted_sector_2[is.na(invacost$Impacted_sector)] <- "DiverseUnspecified" ## replace na values by mixed

unique(invacost$Impacted_sector_2)

#### Keep only highly reliable observation costs
invacost <- invacost[which(invacost$Method_reliability == "High"), ]
invacost <- invacost[which(invacost$Implementation == "Observed"), ]
invacost<-subset(invacost, grepl("Unit", invacost$Spatial_scale)==F)
invacost<-subset(invacost, Geographic_region!="Diverse/Unspecified")

# Number of rows after filtering
nrow(invacost)

# Expanding and formating the database
db.over.time <- expandYearlyCosts(invacost,
                                  startcolumn = "Probable_starting_year_adjusted",
                                  endcolumn = "Probable_ending_year_adjusted")
db.over.time <- dplyr::filter(db.over.time, Impact_year <= "2020")
db.over.time <- dplyr::filter(db.over.time, Impact_year >= "1970")


invacost_cln <- db.over.time[db.over.time$Implementation == "Observed", ]
invacost_cln$Cost_estimate_per_year_2017_USD_exchange_rate<-as.numeric(invacost_cln$Cost_estimate_per_year_2017_USD_exchange_rate)
invacost_cln<-subset(invacost_cln, grepl("Unit", invacost_cln$Spatial_scale)==F)
invacost_cln<-subset(invacost_cln, Geographic_region!="Diverse/Unspecified")
invacost_cln$Type_of_cost_merged<-gsub('Unspecified', "Mixed_costs", invacost_cln$Type_of_cost_merged)

origins<-read.csv('../../../../Desktop/OneDrive - McGill University/GitHub/Givers_takers/invacost_origin_expanded_DN.csv')
origins$origin<-NA
for (i in 1:nrow(origins))
{
  origins$origin[i]<-paste(colnames(origins)[32:37][which(origins[i,32:37]==1)], collapse=";")
}

origins$origin[which(origins$Species=='Ephestia kuehniella')]<-"AS"
origins$origin[which(origins$Species=='Rumex lunaria')]<-"EUR"
origins$origin[which(origins$Species=='Thaumetopoea processionea')]<-"EUR"

invacost_cln$origin<-origins$origin[match( invacost_cln$Species,origins$Species)]
invacost_cln$origin[grepl(";", invacost_cln$origin)]<-"Diverse"

invacost_agg<-invacost_cln %>% group_by(Kingdom, Phylum, Order, Class, Genus, Species) %>% distinct(Species, Type_of_cost_merged, .keep_all=TRUE)
invacost_agg<-invacost_agg[,c(13:19,22,28,29,53,55)]

invacost_agg2<-invacost_cln %>% group_by(Species, Type_of_cost_merged) %>% summarise_if(is.numeric, sum)
invacost_agg<-merge(invacost_agg, invacost_agg2, by=c("Species", "Type_of_cost_merged"))
invacost_agg<-invacost_agg[,c(1:12,22)]

colnames(invacost_agg)[13]<-"Avg.Cost"
invacost_agg$Avg.Cost<-as.numeric(as.character(invacost_agg$Avg.Cost))

### Import sTwist ###
stwist<-read.table('../../../../Desktop/OneDrive - McGill University/Grad/scripts/extrapolation_scripts/sTwist_Database_2.3/AlienSpecies_MultipleDBs_Masterfile_vs2.3.csv', header=T)
colnames(stwist)[3]<-'Species'
colnames(stwist)[1]<-"Official_country"
stwist$Official_country<-gsub("United States of America", "USA", stwist$Official_country)
stwist$Official_country<-gsub("Viet Nam", "Vietnam", stwist$Official_country)
stwist$Official_country<-gsub("United Kingdom of Great Britain and Northern Ireland", "United Kingdom", stwist$Official_country) #also add uk to range of costs of GB and England spp
stwist$Official_country<-gsub("Tanzania, United Republic of", "Tanzania", stwist$Official_country)
stwist$Official_country<-gsub("Lao People's Democratic Republic", "Laos", stwist$Official_country)
stwist$Official_country<-gsub("Korea, Republic of" , "South Korea", stwist$Official_country)
stwist$Official_country<-gsub("El Salvador", "Salvador", stwist$Official_country)
stwist$Official_country<-gsub("Brunei Darussalam" , "Brunei", stwist$Official_country)
stwist$Official_country<-gsub("Venezuela, Bolivarian Republic of", "Venezuela", stwist$Official_country)
invacost_cln$Official_country<-gsub("\\s$", "", invacost_cln$Official_country)
#Scotland, england, northern ireland, great britain?
stwist$code<-countrycode(stwist$Official_country, 'country.name', 'iso3c')
invacost_cln$code<-countrycode(invacost_cln$Official_country, 'country.name', 'iso3c')
clip_spp<-merge(stwist, invacost_cln, by=c("Species", "code"), all=T)
countrydat<-readRDS('../../R Scripts/CountriesDataPop.rds')
countrydat$NAME<-gsub("\\sIs\\.", " Islands", countrydat$NAME)
countrydat$NAME<-gsub("Rep\\.", "Republic", countrydat$NAME)
codes2<-countrycode(countrydat$NAME, 'country.name', 'iso3c')
clip_spp$Area<-countrydat$area_sqkm[match(clip_spp$code,codes2)]
clip_spp$Area[which(clip_spp$Official_country=="Timor-Leste")]<-countrydat$area_sqkm[which(countrydat$NAME=="East Timor")] # a few buggy countries
clip_spp$Area[which(clip_spp$Official_country=="Salvador")]<-countrydat$area_sqkm[which(countrydat$NAME=="El Salvador")]
clip_spp$Area[which(clip_spp$Official_country=="England")]<-130395
clip_spp$Area[which(clip_spp$Official_country=="Great Britain")]<-209331
clip_spp$Area[which(clip_spp$Official_country=="Scotland")]<-80077
clip_spp$Area[which(clip_spp$Official_country=="Northern Ireland")]<-14130
clip_spp$Area[which(clip_spp$Official_country=="Palestine, State of")]<-6220
  clip_spp$Area[which(clip_spp$Official_country=="Korea, Democratic People's Republic of")]<-countrydat$area_sqkm[which(countrydat$NAME=="N. Korea")]
clip_spp$Area[which(clip_spp$Official_country=="Democratic Republic of the Congo (Kinhassa)")]<-countrydat$area_sqkm[which(countrydat$NAME=="Congo")]
clip_spp$Area[which(clip_spp$Official_country=="Saint Martin")]<-countrydat$area_sqkm[which(countrydat$NAME=="St. Martin")]
clip_spp$Area[which(clip_spp$Official_country=="Micronesia, Federated States of")]<-countrydat$area_sqkm[which(countrydat$NAME=="Micronesia")]
clip_spp$Area[which(clip_spp$Official_country=="Saint Vincent and the Grenadines")]<-countrydat$area_sqkm[which(countrydat$NAME=="St. Vin. and Gren.")]
clip_spp$Area[which(clip_spp$Official_country=="French Polynesia")]<-countrydat$area_sqkm[which(countrydat$NAME=="Fr. Polynesia")]
#remaining NAs are small and/or not official countries
clip_spp2<-subset(clip_spp, Species%in%invacost_agg$Species)
clip_spp4<- clip_spp2%>%group_by(Species, code)%>%summarise_if(is.numeric, sum)
range_prop<-range_size<-cost_range_size<-0
for (i in 1:length(unique(clip_spp4$Species)))
{
  sub<-subset(clip_spp4, Species==unique(clip_spp4$Species)[i])
  range_prop[i]<-sum(sub$Area[which(is.na(sub$invacost_cln)==F)], na.rm=T)/sum(sub$Area, na.rm=T)
  cost_range_size[i]<-sum(sub$Area[which(is.na(sub$invacost_cln)==F)], na.rm=T)
  range_size[i]<-sum(sub$Area, na.rm=T)
  
}

invacost_agg$range_size<-range_size[match(invacost_agg$Species, unique(clip_spp4$Species))]
invacost_agg$cost_range_size<-cost_range_size[match(invacost_agg$Species, unique(clip_spp4$Species))]
invacost_agg$range_prop<-range_prop[match(invacost_agg$Species, unique(clip_spp4$Species))]

cabi_categories<-read.csv('cabi_by_sector_v2.csv')
cabi_categories$Cost<-0
cabi_categories$Cost<-invacost_cln$Cost_estimate_per_year_2017_USD_exchange_rate[match(cabi_categories$Species, invacost_cln$Species)]
cabi_categories$Area<-invacost_agg$Area[match(cabi_categories$Species, invacost_agg$Species)]

cabi_Agriculture<-subset(cabi_categories,Agriculture=="Negative")
cabi_Fisheries<-subset(cabi_categories,Fisheries%in%c(NA, "exclude", "None, None", "None, None, None, None", "Positive, Positive")==F)
cabi_Health<-subset(cabi_categories,Health%in%c(NA, "exclude", "None, None", "None", "Positive")==F)
cabi_Health<-subset(cabi_Health, ispathogen=="N")
cabi_Forestry<-subset(cabi_categories, Forestry%in%c(NA, "exclude", "None, None", "None", "Positive")==F)


#invacost_sub<-subset(invacost_cln, Species%in%data$Species)
invacost_sub<-invacost_cln
colnames(invacost_sub)[46]<-"Cost"
table(invacost_sub$Kingdom)
table(invacost_sub$Environment)
table(invacost_sub$Geographic_region)
table(invacost_sub$Official_country)
table(invacost_sub$Type_of_cost_merged)

invacost_sub$Official_country<-gsub("\\s$","",invacost_sub$Official_country)
invacost_sub$Cost_GDP<-0
invacost_sub$Cost_GDP<-invacost_sub$Cost/countrydat$GDP_MD_EST[match(invacost_sub$Official_country, countrydat$NAME)]
invacost_sub$Cost_GDP[which(invacost_sub$Official_country=="Portugal/Spain")]<-invacost_sub$Cost[which(invacost_sub$Official_country=="Portugal/Spain")]/(countrydat$GDP_MD_EST[which(countrydat$NAME=="Spain")]+countrydat$GDP_MD_EST[which(countrydat$NAME=="Portugal")])
invacost_sub$Cost_GDP[which(invacost_sub$Official_country=="Great Britain")]<-invacost_sub$Cost[which(invacost_sub$Official_country=="Great Britain")]/2622000
invacost_sub$Cost_GDP[which(invacost_sub$Official_country=="South Korea")]<-invacost_sub$Cost[which(invacost_sub$Official_country=="South Korea")]/countrydat$GDP_MD_EST[which(countrydat$NAME=="S. Korea")]
invacost_sub$Cost_GDP[which(invacost_sub$Official_country=="England")]<-invacost_sub$Cost[which(invacost_sub$Official_country=="England")]/2622000 # can't find independent estimate of england
invacost_sub$Cost_GDP[which(invacost_sub$Official_country=="USA")]<-invacost_sub$Cost[which(invacost_sub$Official_country=="USA")]/countrydat$GDP_MD_EST[which(countrydat$NAME=="United States")]
invacost_sub$Cost_GDP[which(invacost_sub$Official_country=="Diverse/Unspecified")]<-invacost_sub$Cost[which(invacost_sub$Official_country=="Diverse/Unspecified")]/sum(countrydat$GDP_MD_EST[which(countrydat$REGION=="Europe")])
invacost_sub$Cost_GDP[which(invacost_sub$Official_country=="USA")]<-invacost_sub$Cost[which(invacost_sub$Official_country=="USA")]/countrydat$GDP_MD_EST[which(countrydat$NAME=="United States")]
invacost_sub$Cost_GDP[which(invacost_sub$Official_country=="Timor-Leste")]<-invacost_sub$Cost[which(invacost_sub$Official_country=="Timor-Leste")]/countrydat$GDP_MD_EST[which(countrydat$NAME=="East Timor")]
invacost_sub$Cost_GDP[which(invacost_sub$Official_country=="Salvador")]<-invacost_sub$Cost[which(invacost_sub$Official_country=="Salvador")]/countrydat$GDP_MD_EST[which(countrydat$NAME=="El Salvador")]
invacost_sub$Cost_GDP[which(invacost_sub$Official_country=="Korea, Democratic People's Republic of")]<-invacost_sub$Cost[which(invacost_sub$Official_country=="Korea, Democratic People's Republic of")]/countrydat$GDP_MD_EST[which(countrydat$NAME=="N. Korea")]
invacost_sub$Cost_GDP[which(invacost_sub$Official_country=="Democratic Republic of the Congo (Kinhassa)")]<-invacost_sub$Cost[which(invacost_sub$Official_country=="Democratic Republic of the Congo (Kinhassa)")]/countrydat$GDP_MD_EST[which(countrydat$NAME=="Congo")]
invacost_sub$Cost_GDP[which(invacost_sub$Official_country=="Saint Martin")]<-invacost_sub$Cost[which(invacost_sub$Official_country=="Saint Martin")]/countrydat$GDP_MD_EST[which(countrydat$NAME=="St. Martin")]
invacost_sub$Cost_GDP[which(invacost_sub$Official_country=="Micronesia, Federated States of")]<-invacost_sub$Cost[which(invacost_sub$Official_country=="Micronesia, Federated States of")]/countrydat$GDP_MD_EST[which(countrydat$NAME=="Micronesia")]
invacost_sub$Cost_GDP[which(invacost_sub$Official_country=="Saint Vincent and the Grenadines")]<-invacost_sub$Cost[which(invacost_sub$Official_country=="Saint Vincent and the Grenadines")]/countrydat$GDP_MD_EST[which(countrydat$NAME=="St. Vin. and Gren.")]
invacost_sub$Cost_GDP[which(invacost_sub$Official_country=="French Polynesia")]<-invacost_sub$Cost[which(invacost_sub$Official_country=="French Polynesia")]/countrydat$GDP_MD_EST[which(countrydat$NAME=="Fr. Polynesia")]

require(viridis)
par(mar=c(4,4,2,2))
par(oma=c(0,0,0,0))

## Simplify sectors examined
invacost_sub<-subset(invacost_sub, Species!="Diverse/Unspecified")
plot(log(invacost_sub$Cost_GDP)~as.factor(invacost_sub$Impacted_sector_2), col=viridis(length(unique(invacost_sub$Impacted_sector_2))), outline=F, xlab=NULL, ylab="logCost (2017 USD scaled by GDP)", cex.axis=0.6)
plot(log(invacost_sub$Cost)~as.factor(invacost_sub$Impacted_sector_2), col=viridis(length(unique(invacost_sub$Impacted_sector_2))), outline=F, xlab=NULL, ylab="log (Cost (2017 USD))",cex.axis=0.6)

#pie charts
invacost_sub<-as.data.frame(invacost_sub)
pie_factors<-c("Kingdom", "Phylum", "Class","Order", "Environment", "Geographic_region", "Official_country", "Impacted_sector", "Type_of_cost_merged")
factor<-6 # cycle through to plot each factor
par(mar=c(0,0,2,2))
pie(aggregate(invacost_sub$Cost, list(invacost_sub[,pie_factors[factor]]), sum)[,2], aggregate(invacost_sub$Cost, list(invacost_sub[,pie_factors[factor]]), sum)[,1], col=viridis(length(unique(invacost_sub[,pie_factors[factor]]))), main=paste("Cost by", pie_factors[factor], "(USD)"), init.angle=90, cex=0.75)
pie(aggregate(invacost_sub$Cost_GDP, list(invacost_sub[,pie_factors[factor]]), sum, na.rm=T)[,2], aggregate(invacost_sub$Cost_GDP, list(invacost_sub[,pie_factors[factor]]), sum, na.rm=T)[,1], col=viridis(length(unique(invacost_sub[,pie_factors[factor]]))), main=paste("Cost by", pie_factors[factor], "(% GDP)"), init.angle=90, cex=0.75)


pathways<-read.csv('intro_pathways_cabi_v3.csv')[,1:25]
colnames(pathways)[1]<-"Species"
pathways$food[which(pathways$Species=="Bubalus bubalis")]<-1
invacost_sub<-merge(invacost_sub, pathways, by="Species")
invacost_sub<-invacost_sub[,-19]
colSums(invacost_sub[,70:91]) # pathways of introduction for these species

#subset by cost type, differentiate plant vs. animal in pet and agri pathways
invacost_sub$all_other<-rep(0, nrow(invacost_sub))
invacost_sub$all_other[which(rowSums(invacost_sub[,87:91])==0)]<-1
#colnames(invacost_sub)[37:42]<-c("Agriculture/Livestock", "Forestry", "Hunting/Fishing", "Health", "Culture", "Other") # for some reason this breaks the diagram, i think due to reuse of names

pet_path<-c('pet',  'aquarium', 'botanical_zoo', 'horticulture', 'ornamental',  'cut_flower', 'nursery')
agri_path<-c('animal', 'seed', 'live_food', 'food') 
for_path<-c('timber', 'forestry')
fish_path<-c('stocking', 'hunt_fish', 'fisheries')
health_path<-c('medicinal')

health_spend<-read.csv('./WHO health financing data/CHE by GDP_by country.csv')
health_spend<-health_spend[2:nrow(health_spend),]
library(countrycode)
health_spend$codes2<-countrycode(health_spend$X, 'country.name', 'iso3c')
invacost_sub$codes2<-countrycode(invacost_sub$Official_country, 'country.name', 'iso3c')
invacost_sub<-merge(invacost_sub,health_spend[,c(2,20)], "codes2", )
colnames(invacost_sub)[95]<-"health_spend"

ind_spend<-read.csv('./IndustryData/WorldBank_Agriculture_Forestry_Fish_API_NV.AGR.TOTL.CD_DS2_en_csv_v2_1121017/API_NV.AGR.TOTL.CD_DS2_en_csv_v2_1121017.csv')
ind_spend$codes2<-countrycode(ind_spend$Country.Name, 'country.name', 'iso3c')
invacost_sub$codes2<-countrycode(invacost_sub$Official_country, 'country.name', 'iso3c')
invacost_sub<-merge(invacost_sub,ind_spend[,c(62,65)], "codes2", )
colnames(invacost_sub)[96]<-"ind_spend"
invacost_sub2<-subset(invacost_sub, Type_of_cost_merged=="Management_costs")

invacost_sub2$ind_spend[which(invacost_sub2$ind_spend==NA)]<-0
par(mar=c(4,4,2,2))
plot(log(invacost_sub2$Cost_GDP[which(invacost_sub2$Species%in%cabi_Agriculture$Species|invacost_sub2$Species%in%cabi_Forestry$Species|invacost_sub2$Species%in%cabi_Fisheries$Species)])~asin(sqrt(invacost_sub2$ind_spend[which(invacost_sub2$Species%in%cabi_Agriculture$Species|invacost_sub2$Species%in%cabi_Forestry$Species|invacost_sub2$Species%in%cabi_Fisheries$Species)]/1e+06/(invacost_sub2$Cost[which(invacost_sub2$Species%in%cabi_Agriculture$Species|invacost_sub2$Species%in%cabi_Forestry$Species|invacost_sub2$Species%in%cabi_Fisheries$Species)]/invacost_sub2$Cost_GDP[which(invacost_sub2$Species%in%cabi_Agriculture$Species|invacost_sub2$Species%in%cabi_Forestry$Species|invacost_sub2$Species%in%cabi_Fisheries$Species)]))),ylab='log(Management Costs) (scaled by GDP)', xlab="Arcsine((% GDP spent on Industry)^0.5)")

plot(log(invacost_sub2$Cost_GDP[which(invacost_sub2$Species%in%cabi_Health$Species)])~asin(sqrt(invacost_sub2$health_spend[which(invacost_sub2$Species%in%cabi_Health$Species)]/100)),ylab='log(Cost) (scaled by GDP)', xlab="Arcsine((% GDP spent on Health)^0.5)")

plot(log(invacost_sub2$Cost_GDP[which(invacost_sub2$Impacted_sector=="Health")])~asin(sqrt(invacost_sub2$health_spend[which(invacost_sub2$Impacted_sector=="Health")]/100)),ylab='log(Cost) (scaled by GDP)', xlab="Arcsine((% GDP spent on Health)^0.5)")

plot(log(invacost_sub2$Cost_GDP[which(invacost_sub2$Impacted_sector%in%c("Fisheries", "Forestry", "Agriculture"))])~asin(sqrt(invacost_sub2$ind_spend[which(invacost_sub2$Impacted_sector%in%c("Fisheries", "Forestry", "Agriculture"))]/1e+06/(invacost_sub2$Cost[which(invacost_sub2$Impacted_sector%in%c("Fisheries", "Forestry", "Agriculture"))]/invacost_sub2$Cost_GDP[which(invacost_sub2$Impacted_sector%in%c("Fisheries", "Forestry", "Agriculture"))]))),ylab='log(Damage Costs) (scaled by GDP)', xlab="Arcsine((% GDP spent on Industry)^0.5)")

density<-invacost_sub %>% group_by(Kingdom,Phylum,Class, Order, Species, Environment, Geographic_region, Impacted_sector_2, Type_of_cost_merged) %>% summarise_if(is.numeric, sum)
pathway<-"all" #toggle between 'for', 'fish','agri' (health has no within-sector breakdown)

if (pathway=="all")
{
  density<-as.data.frame(density)
  net<-(density[1,])
  net[1,]<-NA
  net$pathway<-NA
  net$origin<-NA
  for (i in 1:nrow(density))
  {
    names<-colnames(density[i,40:45])[which(density[i,40:45]>0)]
    for (j in 1:length(names))
    {
      net<-rbind(net, setNames(cbind(density[i,], names[j], NA),c(colnames(density), "pathway", "origin")))
       net[i,"Cost"]<-(density[i,"Cost"]*density[i, names[j]])/sum(density[i,40:45])
       net[i,"Cost_GDP"]<-(density[i,"Cost_GDP"]*density[i, names[j]])/sum(density[i, 40:45])
       net[i,"origin"]<-invacost_sub$origin[match(density$Species[i],invacost_sub$Species)]
    }
  }
}
if (pathway!="all")
{
  
density2<-invacost_sub2 %>% group_by(Kingdom,Species, Impacted_sector_2) %>% summarise_at(vars(one_of(get(paste0(pathway, "_path")))), max, na.rm=T)
density3<-invacost_sub2 %>% group_by(Kingdom,Species,Impacted_sector_2) %>% summarise_if(is.numeric, sum, na.rm=T)
density2<-as.data.frame(cbind(density2, density3[,4:5]))
net<-(density2[1,])
net[1,]<-NA
net$pathway<-NA
for(i in 1:nrow(density2))
{
names<-colnames(density2[i,4:(3+length(get(paste0(pathway, "_path"))))])[which(density2[i,4:(3+length(get(paste0(pathway, "_path"))))]>0)]

for (j in 1:length(names))
{
   net<-rbind(net, setNames(cbind(density2[i,], names[j]),c(colnames(density2), "pathway")))
  net[i,"Cost"]<-(density2[i,"Cost"]*density2[i, names[j]])/sum(density2[i, 4:(3+length(get(paste0(pathway, "_path"))))])
                                                                         net[i,"Cost_GDP"]<-(density2[i,"Cost_GDP"]*density2[i, names[j]])/sum(density2[i, 4:(3+length(get(paste0(pathway, "_path"))))])
}
}
}
net<-net[2:nrow(net),]
library(networkD3)

net$origin[which(is.na(net$origin))]<-"UNK"
net$Geographic_region<-gsub("Oceania.*", "Oceania", net$Geographic_region)
net$origin<-gsub("UNK", "Diverse", net$origin)
net<-subset(net, Impacted_sector_2%in%c("Agriculture", "Fishery", "Health", "Forestry"))
write.csv(net, file="net_activitysector_v3.csv", row.names=F)

links = data.frame(source=c(net$Kingdom, net$origin),target=c(net$origin,net$Impacted_sector_2),value=net$Cost_GDP, value2=1,Kingdom=net$Kingdom, origin=net$origin)

nodes=data.frame(name=c(as.character(links$source), as.character(links$target)) %>% unique())
#links<-links[order(links$Sector),]
links$IDsource=match(links$source, nodes$name)-1 
links$IDtarget=match(links$target, nodes$name)-1

 links$group<-as.numeric(factor(paste0(links$source, links$target, links$origin)))
 links<-links[order(links$origin),]
   links<-links %>% group_by(IDsource, IDtarget, group, Kingdom) %>% summarise_if(is.numeric, sum, na.rm=T)


my_color <- 'd3.scaleOrdinal(d3.schemeCategory20)'



nodes[5:11,1]<-c('North America', "Diverse", "South America", "Africa", "Asia", "Oceania", "Europe")

spp_viz<-sankeyNetwork(Links = links, Nodes = nodes, Source = "IDsource", Target = "IDtarget", Value = "value2", NodeID = "name", LinkGroup="Kingdom", colourScale = my_color, fontSize = 16) #plot by species
spp_viz<-prependContent(spp_viz, tags$div("Species Flows", style=("font-family: Helvetica; font-size:12; text-align: center")) )
spp_viz

gdp_viz<-sankeyNetwork(Links = links, Nodes = nodes, Source = "IDsource", Target = "IDtarget", Value = "value", NodeID = "name",LinkGroup="Kingdom", colourScale = my_color, fontSize = 16, iterations=0) #plot by cost, scaled by country gdp
gdp_viz<-prependContent(gdp_viz, tags$div("Cost Flows (rescaled by country GDP)", style=("font-family: Helvetica; font-size:12; text-align: center")) )
gdp_viz



links = data.frame(source=c(net$origin,net$pathway,net$Geographic_region),target=c(net$pathway,net$Geographic_region,net$Impacted_sector),value=net$Cost_GDP, value2=1,pathway=net$pathway, origin=net$origin)

# From these flows we need to create a node data frame: it lists every entities involved in the flow
nodes=data.frame(name=c(as.character(links$source), as.character(links$target)) %>% unique())
links$IDsource=match(links$source, nodes$name)-1 
links$IDtarget=match(links$target, nodes$name)-1

links<-links %>% group_by(IDsource, IDtarget, pathway) %>% summarise_if(is.numeric, sum, na.rm=T)
links$group<-as.numeric(factor(paste0(links$pathway)))
links<-links[order(links$pathway),]

 nodes[8:13,1]<-c("Agriculture", "Fishery",  "Forestry",  "Health","Other","Culture")
 nodes[1:7,1]<-c("North America", "Diverse","South America", "Africa", "Asia","Oceania", "Europe")

links$pathway<-mgsub(links$pathway,c("all_agri",   "all_pet" ,   "all_for",    "all_other",  "all_fish" , "all_health"),c("Agriculture", "Culture", "Forestry", "Other", "Fishery", "Health"))
 gdp_viz<-sankeyNetwork(Links = links, Nodes = nodes, Source = "IDsource", Target = "IDtarget", Value = "value", NodeID = "name",LinkGroup="pathway", colourScale = my_color, fontSize = 16, iterations=0) #plot by cost, scaled by country gdp
  gdp_viz<-prependContent(gdp_viz, tags$div("Cost Flows (rescaled by country GDP)", style=("font-family: Helvetica; font-size:12; text-align: center")) )
 gdp_viz

 spp_viz<-sankeyNetwork(Links = links, Nodes = nodes, Source = "IDsource", Target = "IDtarget", Value = "value2", NodeID = "name", LinkGroup="pathway", colourScale = my_color, fontSize = 16) #plot by species
 spp_viz<-prependContent(spp_viz, tags$div("Species Flows", style=("font-family: Helvetica; font-size:12; text-align: center")) )
 spp_viz

                     