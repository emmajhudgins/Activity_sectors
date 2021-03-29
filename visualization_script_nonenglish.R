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

setwd('~/Dropbox/InvaCost Workshop France/Projects/Activity Sector/')

#data(invacost)
invacost<-read.csv('INVACOST_v2.csv')
invacost <- invacost[-which(is.na(invacost$Cost_estimate_per_year_2017_USD_exchange_rate)), ]
db.over.time <- expandYearlyCosts(invacost,
                                  startcolumn = "Probable_starting_year_low_margin",
                                  endcolumn = "Probable_ending_year_low_margin") 
costdb <- db.over.time[db.over.time$Implementation == "Observed", ]
costdb <- costdb[which(costdb$Method_reliability == "High"), ]

invacost_cln<-0
for (i in 1:nrow(costdb)){
  invacost_cln[i]<- rawAvgCost(costdb[i,], min.year=2000, max.year=2009, year.column="Impact_year")$annual_cost# mean annual costs 2000-2009 at global scale in millions
}
invacost_cln<-cbind(costdb, invacost_cln)
invacost_cln<-invacost_cln[,c(11:23,58,59,62)]

# invacost_cum<-0
# for (i in 1:nrow(costdb)){
#   invacost_cum[i]<- calculateRawAvgCosts(costdb[i,], minimum.year=1960, maximum.year=2017, year.column="Impact_year")# totalcosts at global scale in millions
# }
# invacost_cln<-cbind(invacost_cln, invacost_cum) # these give the same values?

#Use Type2 column rather than old modifications. Same goes for sector
# types<-unique(invacost_cln$Type_of_cost)
# newtype<-read.csv('../../InvaCost Database/Outdated files/InvacostType2.csv')
# newtype$Type<-gsub(" $", "", newtype$Type)
# matchtype<-match(types, as.character(newtype$Type))
# invacost_cln$Type_of_cost<-as.character(invacost_cln$Type_of_cost)
# for (i in 1:length(types))
# {
#   invacost_cln$Type_of_cost[which(as.character(invacost_cln$Type_of_cost)==as.character(types[i]))]<-as.character(newtype$Type2[matchtype[i]])
# }

library(dplyr)
invacost_agg<-invacost_cln %>% group_by(Species) %>% distinct(Species, Type_2, .keep_all=TRUE)
invacost_agg<-invacost_agg[,1:15]
invacost_agg2<-invacost_cln %>% group_by(Species, Type_2) %>% summarise_if(is.numeric, sum)
invacost_agg<-merge(invacost_agg, invacost_agg2, by=c("Species", "Type_2"))

colnames(invacost_agg)[16]<-"Avg.Cost"
invacost_agg$Avg.Cost<-as.numeric(as.character(invacost_agg$Avg.Cost))
#colnames(invacost_agg)[3]<-"Cum.Cost"


### Import sTwist ###
stwist<-read.table('../../R Scripts/extrapolation_scripts/sTwist_Database_2.3/AlienSpecies_MultipleDBs_Masterfile_vs2.3.csv', header=T)
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

clip_spp<-merge(stwist, invacost_cln, by=c("Species", "Official_country"), all=T)
codes<-countrycode(clip_spp$Official_country, 'country.name', 'iso3c')
codes<-countrycode(clip_spp$Official_country, 'country.name', 'iso3c')
countrydat<-readRDS('../../R Scripts/CountriesDataPop.rds')
countrydat$NAME<-gsub("\\sIs.", "\\sIslands", countrydat$NAME)
countrydat$NAME<-gsub("Rep.", "Republic", countrydat$NAME)
codes2<-countrycode(countrydat$NAME, 'country.name', 'iso3c')
clip_spp$Area<-countrydat$area_sqkm[match(codes,codes2)]
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
clip_spp4<- clip_spp2%>%group_by(Species, Official_country)%>%summarise_if(is.numeric, mean)
range_prop<-range_size<-cost_range_size<-0
for (i in 1:length(unique(clip_spp4$Species)))
{
  sub<-subset(clip_spp4, Species==unique(clip_spp4$Species)[i])
  range_prop[i]<-sum(sub$Area[which(is.na(sub$invacost_cln)==F)], na.rm=T)/sum(sub$Area, na.rm=T)
  cost_range_size[i]<-sum(sub$Area[which(is.na(sub$invacost_cln)==F)], na.rm=T)
  range_size[i]<-sum(sub$Area, na.rm=T)
  
}
range_size<-data.frame(range_size, Species=unlist(unique(clip_spp2$Species)))
cost_range_size<-data.frame(cost_range_size, Species=unlist(unique(clip_spp2$Species)))
range_prop<-data.frame(range_prop, Species=unlist(unique(clip_spp2$Species)))

invacost_agg<-merge(invacost_agg, range_size)
invacost_agg<-merge(invacost_agg, cost_range_size)
invacost_agg<-merge(invacost_agg, range_prop)

cabi_categories<-read.csv('cabi_by_sector_v2.csv')
cabi_categories$Cost<-0
cabi_categories$Cost<-invacost_cln$Cost[match(cabi_categories$Species, invacost_cln$Species)]
cabi_categories$Area<-invacost_agg$Area[match(cabi_categories$Species, invacost_agg$Species)]

cabi_Agriculture<-subset(cabi_categories,Agriculture=="Negative")
cabi_Fisheries<-subset(cabi_categories,Fisheries%in%c(NA, "exclude", "None, None", "None, None, None, None", "Positive, Positive")==F)
cabi_Health<-subset(cabi_categories,Health%in%c(NA, "exclude", "None, None", "None", "Positive")==F)
cabi_Health<-subset(cabi_Health, ispathogen=="N")
cabi_Forestry<-subset(cabi_categories, Forestry%in%c(NA, "exclude", "None, None", "None", "Positive")==F)


#invacost_sub<-subset(invacost_cln, Species%in%data$Species)
invacost_sub<-invacost_cln
colnames(invacost_sub)[16]<-"Cost"
table(invacost_sub$Kingdom)
table(invacost_sub$Environment)
table(invacost_sub$Geographic_region)
table(invacost_sub$Official_country)
table(invacost_sub$Type_2)

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
plot(invacost_sub$Cost_GDP~as.factor(invacost_sub$Impacted_sector_2), col=viridis(length(unique(invacost_sub$Impacted_sector_2))), outline=F, xlab=NULL, ylab="Cost (2017 USD scaled by GDP)", cex.axis=0.8)
plot(invacost_sub$Cost~as.factor(invacost_sub$Impacted_sector_2), col=viridis(length(unique(invacost_sub$Impacted_sector_2))), outline=F, xlab=NULL, ylab="Cost (2017 USD)",cex.axis=0.8)

#pie charts
invacost_sub<-as.data.frame(invacost_sub)
pie_factors<-c("Kingdom", "Phylum", "Class","Order", "Environment", "Geographic_region", "Official_country", "Impacted_sector_2", "Type_2")
factor<-6 # cycle through to plot each factor
par(mar=c(0,0,2,2))
pie(aggregate(invacost_sub$Cost, list(invacost_sub[,pie_factors[factor]]), sum)[,2], aggregate(invacost_sub$Cost, list(invacost_sub[,pie_factors[factor]]), sum)[,1], col=viridis(length(unique(invacost_sub[,pie_factors[factor]]))), main=paste("Cost by", pie_factors[factor], "(USD)"), init.angle=90, cex=0.75)
pie(aggregate(invacost_sub$Cost_GDP, list(invacost_sub[,pie_factors[factor]]), sum, na.rm=T)[,2], aggregate(invacost_sub$Cost_GDP, list(invacost_sub[,pie_factors[factor]]), sum, na.rm=T)[,1], col=viridis(length(unique(invacost_sub[,pie_factors[factor]]))), main=paste("Cost by", pie_factors[factor], "(% GDP)"), init.angle=90, cex=0.75)


pathways<-read.csv('intro_pathways_cabi_v3.csv')[,1:25]
colnames(pathways)[1]<-"Species"
pathways$food[which(pathways$Species=="Bubalus bubalis")]<-1
invacost_sub<-merge(invacost_sub, pathways, by="Species")
colSums(invacost_sub[,18:41]) # pathways of introduction for these species

#subset by cost type, differentiate plant vs. animal in pet and agri pathways
invacost_sub$all_other<-rep(0, nrow(invacost_sub))
invacost_sub$all_other[which(rowSums(invacost_sub[,37:41])==0)]<-1
#colnames(invacost_sub)[37:42]<-c("Agriculture/Livestock", "Forestry", "Hunting/Fishing", "Health", "Culture", "Other") # for some reason this breaks the diagram, i think due to reuse of names
#invacost_sub2<-subset(invacost_sub, all_health==1)
#invacost_sub2<-subset(invacost_sub, Species%in%cabi_Health$Species)
invacost_sub2<-subset(invacost_sub, Impacted_sector_2=="Health")

pet_path<-c('pet',  'aquarium', 'botanical_zoo', 'horticulture', 'ornamental',  'cut_flower', 'nursery')
agri_path<-c('animal', 'seed', 'live_food', 'food') 
for_path<-c('timber', 'forestry')
fish_path<-c('stocking', 'hunt_fish', 'fisheries')
health_path<-c('medicinal')

health_spend<-read.csv('./WHO health financing data/CHE by GDP_by country.csv')
health_spend$codes2<-countrycode(health_spend$X, 'country.name', 'iso3c')
invacost_sub$codes2<-countrycode(invacost_sub$Official_country, 'country.name', 'iso3c')
invacost_sub<-merge(invacost_sub,health_spend[,c(2,20)], "codes2", )
colnames(invacost_sub)[44]<-"health_spend"

ind_spend<-read.csv('./IndustryData/WorldBank_Agriculture_Forestry_Fish_API_NV.AGR.TOTL.CD_DS2_en_csv_v2_1121017/API_NV.AGR.TOTL.CD_DS2_en_csv_v2_1121017.csv')
ind_spend$codes2<-countrycode(ind_spend$Country.Name, 'country.name', 'iso3c')
invacost_sub$codes2<-countrycode(invacost_sub$Official_country, 'country.name', 'iso3c')
invacost_sub<-merge(invacost_sub,ind_spend[,c(62,65)], "codes2", )
colnames(invacost_sub)[45]<-"ind_spend"

invacost_sub$ind_spend[which(invacost_sub$ind_spend==NA)]<-0
plot(log(invacost_sub$Cost_GDP)~asin(sqrt(invacost_sub$ind_spend/1e+06/(invacost_sub$Cost/invacost_sub$Cost_GDP))),ylab='log(Cost) (scaled by GDP)', xlab="Arcsine((% GDP spent on Industry)^0.5)")


density<-invacost_sub %>% group_by(Kingdom,Species, Environment, Official_country, Impacted_sector_2, Type_2) %>% summarise_if(is.numeric, sum)


density2<-invacost_sub2 %>% group_by(Kingdom,Species, Impacted_sector_2) %>% summarise_at(vars(one_of(health_path)), max)
density3<-invacost_sub2 %>% group_by(Kingdom,Species,Impacted_sector_2) %>% summarise_if(is.numeric, sum)
density2<-as.data.frame(cbind(density2, density3[,4:5]))
density<-as.data.frame(density)

net<-(density[1,])
net[1,]<-NA
net$pathway<-NA
for (i in 1:nrow(density))
{
  # names<-colnames(density2[i,4:(3+length(health_path))])[which(density2[i,4:(3+length(health_path))]>0)]
  
  #names<-"health"
    names<-colnames(density[i,28:33])[which(density[i,28:33]>0)]
  for (j in 1:length(names))
  {
  net<-rbind(net, setNames(cbind(density[i,], names[j]),c(colnames(density), "pathway")))
   net[i,"Cost"]<-(density[i,"Cost"]*density[i, names[j]])/sum(density[i, 28:33])
   net[i,"Cost_GDP"]<-(density[i,"Cost_GDP"]*density[i, names[j]])/sum(density[i, 28:33])
    # net<-rbind(net, setNames(cbind(density2[i,], names[j]),c(colnames(density2), "pathway")))
    # net[i,"Cost"]<-(density2[i,"Cost"]*density2[i, names[j]])/sum(density2[i, 4:(3+length(health_path))])
    # net[i,"Cost_GDP"]<-(density2[i,"Cost_GDP"]*density2[i, names[j]])/sum(density2[i, 4:(3+length(health_path))])
  }
}
net<-net[2:nrow(net),]
net$pathway2<-net$pathway
net$pathway2[which(rowSums(net[,28:33])>1)]<-"diverse"
library(networkD3)
net$health_spend[which(net$health_spend==NA)]<-0
links = data.frame(source=net$pathway,target=net$Impacted_sector_2,value=net$Cost_GDP, value2=1, value3=net$Cost,Kingdom=net$Kingdom, value4=net$Cost_GDP/(net$health_spend+0.0001))

# From these flows we need to create a node data frame: it lists every entities involved in the flow
nodes=data.frame(name=c(as.character(links$source), as.character(links$target)) %>% unique())
links$IDsource=match(links$source, nodes$name)-1 
links$IDtarget=match(links$target, nodes$name)-1
links$group<-as.numeric(factor(paste0(links$source, links$target, links$Kingdom)))
links<-links[order(links$Kingdom),]
links<-links %>% group_by(IDsource, IDtarget, group, Kingdom) %>% summarise_if(is.numeric, sum, na.rm=T)

my_color2c <- 'd3.scaleOrdinal() .domain(["Plantae", "Animalia", "Chromista",  "Authorities-Stakeholders" , "Environment" ,"Diverse/Unspecified", "Forestry" ,  "Agriculture","Fishery" , "Public and social welfare", "Health", "all_agri", "all_other", "all_pet", "all_for" , "all_fish"]) .range(["#b5bca0",  "#aaaaaa","#377eb8", "#e41a1c","#045a8d", "#262f09","#b4b4b4","#006d2c", "#8c8c8c", "#969696","#6effa0","#ad7274", "#a50f15", "#ff5b86", "#01dda5","#ffeb92"])' #grey animals, #green/beige plants, #blue chromista

library(htmlwidgets)
library(htmltools)

gdp_viz<-sankeyNetwork(Links = links, Nodes = nodes, Source = "IDsource", Target = "IDtarget", Value = "value", NodeID = "name", LinkGroup='Kingdom', colourScale = my_color2c, fontSize = 12, ) #plot by cost, scaled by country gdp
gdp_viz<-prependContent(gdp_viz, tags$div("Cost Flows (rescaled by country GDP)", style=("font-family: Helvetica; font-size:12; text-align: center")) )
gdp_viz$sizingPolicy$viewer$fill <- FALSE
gdp_viz


spp_viz<-sankeyNetwork(Links = links, Nodes = nodes, Source = "IDsource", Target = "IDtarget", Value = "value2", NodeID = "name", LinkGroup="Kingdom", colourScale = my_color2c, fontSize = 12) #plot by species
spp_viz<-prependContent(spp_viz, tags$div("Species Flows", style=("font-family: Helvetica; font-size:12; text-align: center")) )
spp_viz$sizingPolicy$viewer$fill <- FALSE
spp_viz

cost_viz<-sankeyNetwork(Links = links, Nodes = nodes, Source = "IDsource", Target = "IDtarget", Value = "value3", NodeID = "name", LinkGroup="Kingdom", colourScale = my_color2c, fontSize = 12) #plot by species
cost_viz<-prependContent(cost_viz, tags$div("Cost Flows (2017 USD)", style=("font-family: Helvetica; font-size:12; text-align: center")) )
cost_viz$sizingPolicy$viewer$fill <- FALSE
cost_viz

health_viz<-sankeyNetwork(Links = links, Nodes = nodes, Source = "IDsource", Target = "IDtarget", Value = "value4", NodeID = "name", LinkGroup='Kingdom', colourScale = my_color2c, fontSize = 12, ) #plot by cost, scaled by country gdp
health_viz<-prependContent(health_viz, tags$div("Cost (Scaled by Health Spending)", style=("font-family: Helvetica; font-size:12; text-align: center")) )
health_viz$sizingPolicy$viewer$fill <- FALSE
health_viz

## add in data from Anna and Jane about health and timber as % gdp      
