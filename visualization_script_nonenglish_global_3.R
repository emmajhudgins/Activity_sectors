### Script for visualization of invacost data by activity sector
## written by Emma J. Hudgins, January 22, 2019
## emma.hudgins@mail.mcgill.ca

#rm(list = ls())
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

invacost_agg<-invacost_cln %>% group_by(Species, Type_of_cost_merged, Impacted_sector) %>% distinct(Species, Type_of_cost_merged, Impacted_sector, .keep_all=TRUE)

invacost_agg<-invacost_cln %>% group_by(Kingdom, Phylum, Order, Class, Genus, Species) %>% distinct(Species, Type_of_cost_merged, .keep_all=TRUE)
invacost_agg<-invacost_agg[,c(13:19,22,28,29,55,66,46)]

invacost_agg2<-invacost_cln %>% group_by(Species, Type_of_cost_merged) %>% summarise_if(is.numeric, sum)
invacost_agg<-merge(invacost_agg, invacost_agg2, by=c("Species", "Type_of_cost_merged"))
invacost_agg<-invacost_agg[,c(1:12,23)]

colnames(invacost_agg)[13]<-"Cum.Cost"
invacost_agg$Cum.Cost<-as.numeric(as.character(invacost_agg$Cum.Cost))
sumcost<-invacost_agg%>%group_by(Impacted_sector_2)%>%summarise_at('Cum.Cost',sum)
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
  range_prop[i]<-sum(sub$Area[which(is.na(sub$Cost_estimate_per_year_2017_USD_exchange_rate)==F)], na.rm=T)/sum(sub$Area, na.rm=T)
  cost_range_size[i]<-sum(sub$Area[which(is.na(sub$Cost_estimate_per_year_2017_USD_exchange_rate)==F)], na.rm=T)
  range_size[i]<-sum(sub$Area, na.rm=T)
  
}

invacost_agg$range_size<-range_size[match(invacost_agg$Species, unique(clip_spp4$Species))]
invacost_agg$cost_range_size<-cost_range_size[match(invacost_agg$Species, unique(clip_spp4$Species))]
invacost_agg$range_prop<-range_prop[match(invacost_agg$Species, unique(clip_spp4$Species))]

plot(log(invacost_agg$Cum.Cost)~log(invacost_agg$range_size))
plot(log(invacost_agg$Cum.Cost)~log(invacost_agg$cost_range_size))

cabi_categories<-read.csv('cabi_by_sector_v2.csv')
cabi_categories$Cum.Cost<-cabi_categories$Cum.Cost<-0
invacost_spp<-invacost_agg%>%group_by(Species)%>%summarize_at('Cum.Cost', sum, na.rm=T)
cabi_categories$Cum.Cost<-invacost_spp$Cum.Cost[match(cabi_categories$Species, invacost_spp$Species)]

cabi_Agriculture<-subset(cabi_categories,Agriculture=="Negative")
cabi_Fishery<-subset(cabi_categories,Fisheries%in%c(NA, "exclude", "None, None", "None, None, None, None", "Positive, Positive")==F)
cabi_Health<-subset(cabi_categories,Health%in%c(NA, "exclude", "None, None", "None", "Positive")==F)
cabi_Health<-subset(cabi_Health, ispathogen=="N")
cabi_Forestry<-subset(cabi_categories, Forestry%in%c(NA, "exclude", "None, None", "None", "Positive")==F)
taxonomy<-read.csv('invacostSpList_AJT_20200929.csv')
invacost_agg$tax<-taxonomy$Group02[match(invacost_agg$Class, taxonomy$Class)]
invacost_agg$tax[which(is.na(invacost_agg$tax))]<-"DiverseUnspecified"
bygroup<-aggregate(invacost_agg$Cum.Cost, by=list(invacost_agg$tax, invacost_agg$Impacted_sector), sum)
colnames(bygroup)<-c("Group","Sector" ,"Cost")

invacost_cln2<-subset(invacost_cln, Impacted_sector_2%in%c("Fishery", "Forestry", "Agriculture", "Health"))

invacost_cln2<-invacost_cln2%>%group_by(Species, Type_of_cost_merged, Impacted_sector_2, Geographic_region) %>% distinct(Species, Type_of_cost_merged, Impacted_sector_2,Geographic_region, .keep_all=TRUE)
invacost_cln2$Geographic_region<-revalue(invacost_cln2$Geographic_region, c( "Oceania/Pacific Islands"= "Oceania","Africa/Asia/Europe"= "Diverse", "Europe/Asia"= "Diverse"))

invacost_cln2$tax<-taxonomy$Group02[match(invacost_cln2$Class, taxonomy$Class)]
bygroup2<-aggregate(invacost_cln2$Cost_estimate_per_year_2017_USD_exchange_rate, by=list(invacost_cln2$tax, invacost_cln2$Impacted_sector_2,invacost_cln2$Geographic_region), sum)
colnames(bygroup2)<-c('Group', 'Impacted_sector', 'Geographic_region', "Cost")
tot2<-bygroup2%>%group_by(Impacted_sector, Geographic_region)%>%mutate(costT=sum(Cost))
tot2<-tot2%>%group_by(Group,Impacted_sector, Geographic_region)%>%mutate(prop=Cost/costT)
saveRDS(tot2, file="cost_by_continent_sector.rds")
pie(subset(tot2, Geographic_region=="Africa")$prop, labels=subset(tot2, Geographic_region=="Africa")$Impacted_sector, main="Africa")
pie(subset(tot2, Geographic_region=="Europe")$prop, labels=subset(tot2, Geographic_region=="Europe")$Impacted_sector, main="Europe")
pie(subset(tot2, Geographic_region=="Africa")$prop, labels=subset(tot2, Geographic_region=="Africa")$Impacted_sector, main="Africa")

library(ggplot2)
# Barplot
bp<- ggplot(tot2, aes(x=Geographic_region, y=Cost, fill=Impacted_sector))+
  geom_bar(width = 1, stat = "identity")
bp

bygroup<-aggregate(invacost_agg$Cum.Cost, by=list(invacost_agg$tax, invacost_agg$Impacted_sector_2), sum)
colnames(bygroup)<-c('Group', 'Impacted_sector', "Cost")
tot<-bygroup%>%group_by(Impacted_sector)%>%summarize_at('Cost', sum)
tot<-bygroup%>%group_by(Group, Impacted_sector)%>%mutate(prop=Cost/tot$Cost[match(bygroup$Impacted_sector,tot$Impacted_sector)])

tot<-subset(tot,Impacted_sector%in%c("Fishery", "Forestry", "Agriculture", "Health"))
sum(cabi_Agriculture$Cum.Cost, na.rm=T)
invacost_agg$Impacted_sector<-invacost_agg$Impacted_sector_2
sum(subset(invacost_agg, Species%in%cabi_Agriculture$Species& Impacted_sector!="Agriculture")$Cum.Cost, na.rm=T)/sum(cabi_Agriculture$Cum.Cost, na.rm=T)
sum(cabi_Fishery$Cum.Cost, na.rm=T)
sum(subset(invacost_agg, Species%in%cabi_Fisheries$Species& Impacted_sector!="Fisheries")$Cum.Cost, na.rm=T)/sum(cabi_Fishery$Cum.Cost, na.rm=T)
sum(cabi_Forestry$Cum.Cost, na.rm=T)
sum(subset(invacost_agg, Species%in%cabi_Forestry$Species& Impacted_sector!="Forestry")$Cum.Cost, na.rm=T)/sum(cabi_Forestry$Cum.Cost, na.rm=T)
sum(cabi_Health$Cum.Cost, na.rm=T)
sum(subset(invacost_agg, Species%in%cabi_Health$Species& Impacted_sector!="Health")$Cum.Cost, na.rm=T)/sum(cabi_Health$Cum.Cost, na.rm=T)

invacost_agri<-subset(invacost_agg, Impacted_sector=="Agriculture")
sum(invacost_agri$Cum.Cost)

invacost_fore<-subset(invacost_agg, Impacted_sector=="Forestry")
sum(invacost_fore$Cum.Cost)

invacost_health<-subset(invacost_agg, Impacted_sector=="Health")
sum(invacost_health$Cum.Cost)

invacost_fish<-subset(invacost_agg, Impacted_sector=="Fishery")
sum(invacost_fish$Cum.Cost)

boxplot(Cum.Cost~Kingdom,data=invacost_agg)

length(unique(subset(invacost_cln, Impacted_sector_2=="Agriculture")$Official_country))/length(unique(invacost_cln$Official_country))

#Scenarios of missing species - 1) All missing species have lower costs than any reported, 2) All species follow same distribution, 3) GAM predictions
invacost_agg$Kingdom[grep("\\/",invacost_agg$Kingdom)]<-"Diverse/Unspecified"
invacost_agg$Kingdom<-gsub('plantae', 'Plantae',invacost_agg$Kingdom)
for (i in c(1:11))
{
  invacost_agg[,i]<-as.factor(invacost_agg[,i])
}
clip_spp2$eventDate<-as.numeric(gsub(";.*", "",clip_spp2$eventDate))
stwist_agg<-clip_spp2%>%group_by(Species)%>%summarise_if(is.numeric, min, na.rm=T)
stwist_agg$eventDate[which(is.infinite(stwist_agg$eventDate))]<-NA
stwist_agg$eventDate[which(is.na(stwist_agg$eventDate))]<-mean(stwist_agg$eventDate, na.rm=T)
stwist_intros<-subset(clip_spp2,establishmentMeans%in%c("introduced" ,   "introduced; uncertain"  ,"introduced; vagrant" ,"introduced; uncertain; vagrant", "introduced; NA","introduced; ; NA"  ,"NA; introduced"))
n_intro<-stwist_intros %>%group_by(Species)%>%summarise_all(length)
eventdate<-data.frame(eventDate=stwist_agg$eventDate, Species=unlist(unique(clip_spp2$Species)))
nin<-data.frame(n_intros=n_intro[,2], Species=n_intro[,1])
colnames(nin)[1]<-"n_intros"
invacost_agg<-merge(invacost_agg, nin, all.x=T)
invacost_agg<-merge(invacost_agg, eventdate, all.x=T)
invacost_agg$n_intros[which(is.na(invacost_agg$n_intros))]<-0
invacost_agg$eventDate[which(is.na(invacost_agg$eventDate))]<-mean(invacost_agg$eventDate, na.rm=T)

invacost_agg$Impacted_sector_2<-as.factor(invacost_agg$Impacted_sector_2)
library(mgcv)
library(VGAM)
library(gbm)
library(dismo)
invacost_agg$logcost<-log(as.numeric(as.character(invacost_agg$Cum.Cost)))
invacost_agg$logrange<-log(as.numeric(as.character(invacost_agg$range_size)))
invacost_agg$logcostrange<-log(as.numeric(as.character((invacost_agg$cost_range_size))))
invacost_agg$logn<-log(invacost_agg$n_intros+1)
invacost_agg$Phylum[which(invacost_agg$Phylum=="")]<-"Diverse/Unspecified"
invacost_agg$Phylum[which(invacost_agg$Phylum=="Unknown")]<-"Diverse/Unspecified"
invacost_agg$Phylum[grepl("/",invacost_agg$Phylum)]<-"Diverse/Unspecified"
invacost_agg$Kingdom[grepl("/",invacost_agg$Kingdom)]<-"Diverse/Unspecified"
invacost_agg<- subset(invacost_agg, logcost!=-Inf)
invacost_agg<-subset(invacost_agg, is.na(Species)==F)

#m<-gam(logcost~Type_of_cost_merged+Phylum+Impacted_sector+Environment+s(logn)+s(logrange)+s(logcostrange)+s(eventDate), data=invacost_agg, drop.unused.levels = F)
brt_glob<-gbm.step(gbm.x =c(2,3,4,9,12,22:24) ,gbm.y =21, data=invacost_agg,learning.rate=0.03, n.trees=100, family='gaussian', tree.complexity=2)
quant_glob<-gbm(logcost~Type_of_cost_merged+Kingdom+Phylum+Impacted_sector_2+logrange+logcostrange+Environment+logn+eventDate, distribution=list(name = "quantile", alpha = 0.025), data=invacost_agg, interaction.depth = 2, n.trees=brt_glob$n.trees, bag.fraction=0.75, cv.folds=10, shrinkage=brt_glob$shrinkage)
quant2_glob<-gbm(logcost~Type_of_cost_merged+Kingdom+Phylum+Impacted_sector_2+logrange+logcostrange+Environment+logn+eventDate, distribution=list(name = "quantile", alpha = 0.975), data=invacost_agg, interaction.depth = 2, n.trees=brt_glob$n.trees, bag.fraction=0.75, cv.folds=10, shrinkage=brt_glob$shrinkage)


sector="Agriculture" #change this to sector of interest
data<-get(paste0('cabi_', sector))
#data<-subset(data, ispathogen=='N') # pathogens?
length(which(is.na(data$Cum.Cost)==F))/nrow(data) #completeness based on CABI
mean(invacost_agg$range_prop[which(invacost_agg$Species%in%data$Species)]) # completeness of ranges


invacost_sub_agg<-subset(invacost_agg, Impacted_sector_2==sector)
invacost_sub_agg<-subset(invacost_sub_agg, Cum.Cost>0)


require(viridis)

stwist_spp<-stwist %>%group_by(Species) %>% summarise_if(is.character,first)

clip_spp3<-merge(clip_spp, data, by="Species", all.y=T)


range_size<-cost_range_size<-0
for (i in 1:length(unique(clip_spp3$Species)))
{
  sub<-subset(clip_spp3, Species==unique(clip_spp3$Species)[i])
  range_size[i]<-sum(sub$Area)
  cost_range_size[i]<-sum(sub$Area[which(is.na(sub$invacost_cln)==F)])
}

data$logrange<-log(range_size)
data$logcostrange<-log(cost_range_size)
data$logcostrange[data$logcostrange==-Inf]<-0

library(taxize)
data_hier<-tapply(1:nrow(data),1:nrow(data),function(x){classification(get_gbifid(data$Species[x], ask=F,rows=1))}) #slow

data$Kingdom<-as.factor(unlist(lapply(data_hier,function(x){ifelse(length(x)>1,unique(x$name[1]),no=NA)})))
data$Phylum<-as.factor(unlist(lapply(data_hier,function(x){ifelse(length(x)>1,unique(x$name[2]),no=NA)})))
data$Order<-as.factor(unlist(lapply(data_hier,function(x){ifelse(length(x)>1,unique(x$name[4]),no=NA)})))
data$Class<-as.factor(unlist(lapply(data_hier,function(x){ifelse(length(x)>1,unique(x$name[3]),no=NA)})))
data$Impacted_sector<-as.factor(sector)
data$Type_of_cost_merged<-("Mixed_costs")# predict mixed costs for missing set
clip_spp3$eventDate<-as.numeric(gsub(";.*", "",clip_spp3$eventDate))
stwist_agg2<-clip_spp3%>%group_by(Species)%>%summarise_if(is.numeric, min, na.rm=T)
stwist_agg2$eventDate[which(is.infinite(stwist_agg2$eventDate))]<-NA
stwist_agg2$eventDate[which(is.na(stwist_agg2$eventDate))]<-mean(stwist_agg$eventDate, na.rm=T)
data$eventDate<-stwist_agg2$eventDate
stwist_intros2<-subset(clip_spp3,establishmentMeans%in%c("introduced" ,   "introduced; uncertain"  ,"introduced; vagrant" ,"introduced; uncertain; vagrant", "introduced; NA","introduced; ; NA"  ,"NA; introduced"))
n_intro2<-stwist_intros2 %>%group_by(Species)%>%summarise_all(length)
data$n_intros<-0
data$n_intros[match(n_intro2$Species, data$Species)]<-n_intro2$Official_country
data$logn<-log(data$n_intros+1)
data_pred<-merge(data, stwist_spp[,c("Species","habitat")], by="Species", all.x=T, all.y=F)
data_pred$Environment<-data_pred$habitat
CapStr <- function(y) {
  c <- strsplit(y, " ")[[1]]
  paste(toupper(substring(c, 1,1)), substring(c, 2),
        sep="", collapse=" ")
}
capitalize_str <- function(charcter_string){
  sapply(charcter_string, CapStr)
}
data_pred$Environment<-capitalize_str(data_pred$Environment)
data_pred$Environment<-gsub("Freshwater; Terrestrial", "Aquatic/Terrestrial",data_pred$Environment)
data_pred$Environment<-gsub(";.*", "",data_pred$Environment)
data_pred$Environment<-gsub("NANA", "",data_pred$Environment)
data_pred$Environment<-gsub("Freshwater", "Aquatic",data_pred$Environment)
data_pred$Environment<-gsub("Marine", "Aquatic",data_pred$Environment)
data_pred$Environment<-gsub("Brackish", "Aquatic",data_pred$Environment)
data_pred$Environment[data_pred$Environment==""]<-NA
data_pred<-rbind(data_pred, data_pred)
data_pred<-rbind(data_pred, data_pred)
data_pred$Type_of_cost_merged[(nrow(data)+1):(2*nrow(data))]<-("Damage_costs")
data_pred$Type_of_cost_merged[(2*nrow(data)+1):(3*nrow(data))]<-("Management_costs")
data_pred$Type_of_cost_merged<-as.factor(data_pred$Type_of_cost_merged)
data_pred$logrange[which(is.na(data_pred$logrange))]<-mean(invacost_agg$logrange)
data_pred$logcostrange[which(is.na(data_pred$logcostrange))]<-0

data_pred$Environment[which(is.na(data_pred$Environment))]<-"Diverse/Unspecified"
data_pred$Environment[which(data_pred$Environment=="Endoparasitic")]<-"Diverse/Unspecified"

data_pred$Environment<-as.factor(data_pred$Environment)
invacost_sub_agg$Type_of_cost_merged[which(is.na(invacost_sub_agg$Type_of_cost_merged))]<-"Mixed_costs"
types<-c("Damage_costs", "Management_costs", "Mixed_costs")
write.csv(data_pred, file=paste0("data_pred_3", sector, ".csv"), row.names=F)

continent_cost<-invacost_cln%>%group_by(Species, Type_of_cost_merged, Geographic_region)%>%summarise_if(is.numeric, sum, na.rm=T)
invacost_cont<-merge(invacost_agg,continent_cost, all.y=T)
invacost_cont$logcost<-log(invacost_cont$invacost_cln)
invacost_cont<-subset(invacost_cont, logcost!=-Inf)
invacost_cont$Geographic_region[grep("\\/", invacost_cont$Geographic_region)]<-"Diverse/Unspecified"
brt<-gbm.step(gbm.x =c(2,3,4,5,10,12,20,22:24) ,gbm.y =21, data=invacost_cont,learning.rate=0.03, n.trees=100, family='gaussian', tree.complexity=2)
 quant<-gbm(logcost~Type_of_cost_merged+Kingdom+Phylum+Impacted_sector_2+logrange+logcostrange+Environment+logn+eventDate+Geographic_region, distribution=list(name = "quantile", alpha = 0.025), data=invacost_cont, interaction.depth = 2, n.trees=brt$n.trees, shrinkage=brt$shrinkage, bag.fraction=0.75,cv.folds=10)
 quant2<-gbm(logcost~Type_of_cost_merged+Kingdom+Phylum+Impacted_sector_2+logrange+logcostrange+Environment+logn+eventDate+Geographic_region, distribution=list(name = "quantile", alpha = 0.975), data=invacost_cont, interaction.depth = 2, n.trees=brt$n.trees, shrinkage=brt$shrinkage, verbose=T, bag.fraction=0.75, cv.folds=10)
 continents=unique(invacost_cont$Geographic_region)
 continents=continents[-7]
 allcosts<-matrix(0, length(continents),9)
 for (j in 1:length(continents))
 {
   continent=as.character(continents[j])
 countries_continent<-unique(invacost_cln$Official_country[which(invacost_cln$Geographic_region==continent)])
 spp_continent=unique(clip_spp3$Species[which(clip_spp3$Official_country%in%countries_continent)])
 data_pred2<-subset(data_pred, Species%in%spp_continent)
 data_pred2$Geographic_region<-continent
for (i in 1:3)
{
allcosts[j,(i-1)*3+1]<-sum(exp(predict.gbm(brt,newdata=subset(data_pred2, Species%in%invacost_agg$Species==F & Type_of_cost_merged==as.character(types[i])), n.trees = brt$n.trees))/1000000)+sum(subset(invacost_cont,((Type_of_cost_merged==as.character(types[i]))& Geographic_region==continent& Impacted_sector==sector))$invacost_cln/1000000) # predicted total cost from gam in millions
allcosts[j, (i-1)*3+2]<- sum(exp(predict.gbm(quant,newdata=subset(data_pred2, Species%in%invacost_agg$Species==F & Type_of_cost_merged==as.character(types[i])), n.trees = brt$n.trees))/1000000)+sum(subset(invacost_cont,((Type_of_cost_merged==as.character(types[i]))& Geographic_region==continent& Impacted_sector==sector))$invacost_cln/1000000)
allcosts[j, (i-1)*3+3]<-sum(exp(predict.gbm(quant2,newdata=subset(data_pred2, Species%in%invacost_agg$Species==F & Type_of_cost_merged==as.character(types[i])), n.trees = brt$n.trees))/1000000)+sum(subset(invacost_cont,((Type_of_cost_merged==as.character(types[i]))& Geographic_region==continent& Impacted_sector==sector))$invacost_cln/1000000)
}
 }
 allcosts<-allcosts/1000
colnames(allcosts)<-c("Damage", "LCI", "UCI", "Management","LCI", "UCI","Mixed","LCI", "UCI")
rownames(allcosts)<-continents
write.csv(allcosts, file=paste0("predicted_costs_3", sector,".csv"), row.names=F) 
invacost$Impacted_sector_2 <-revalue(invacost$Impacted_sector_2, c("Indirect costs (not detailed)" = "DiverseUnspecified",
                                                                   "Auhtorities and stakeholders" = "Authorities-Stakeholders",
                                                                   "Authorities" = "Authorities-Stakeholders"
))


invacost$Impacted_sector_2 <- ifelse(grepl('/', invacost$Impacted_sector, ignore.case = T), "DiverseUnspecified", invacost$Impacted_sector)

sum(exp(predict.gbm(brt_glob,newdata=subset(data_pred, Species%in%invacost_agg$Species==F), n.trees = brt_glob$n.trees))/1000000)+sum(invacost_sub_agg$Cum.Cost/1000000)

sum(exp(predict.gbm(quant_glob,newdata=subset(data_pred, Species%in%invacost_agg$Species==F), n.trees = brt_glob$n.trees))/1000000)+sum(invacost_sub_agg$Cum.Cost/1000000)
sum(exp(predict.gbm(quant2_glob,newdata=subset(data_pred, Species%in%invacost_agg$Species==F), n.trees = brt_glob$n.trees))/1000000)+sum(invacost_sub_agg$Cum.Cost/1000000)

costbkdown<-matrix(0, 3,3)
for (i in 1:3)
{
 costbkdown[i,1]<- sum(exp(predict.gbm(brt_glob,newdata=subset(data_pred, Species%in%invacost_agg$Species==F&Type_of_cost_merged==types[i]), n.trees = brt_glob$n.trees))/1000000)+sum(subset(invacost_sub_agg,Type_of_cost_merged==types[i])$Cum.Cost/1000000)
  
 costbkdown[i,2]<-sum(exp(predict.gbm(quant_glob,newdata=subset(data_pred, Species%in%invacost_agg$Species==F& Type_of_cost_merged==types[i]), n.trees = brt_glob$n.trees))/1000000)+sum(subset(invacost_sub_agg,Type_of_cost_merged==types[i])$Cum.Cost/1000000)
 costbkdown[i,3]<-  sum(exp(predict.gbm(quant2_glob,newdata=subset(data_pred, Species%in%invacost_agg$Species==F&Type_of_cost_merged==types[i]), n.trees = brt_glob$n.trees))/1000000)+sum(subset(invacost_sub_agg,Type_of_cost_merged==types[i])$Cum.Cost/1000000)
 }
colnames(costbkdown)<-c("Estimate", "Lower CI", "Upper CI")
row.names(costbkdown)<-c(types)

spp_sub_agg<-invacost_sub_agg%>%group_by(Species)%>%summarise_if(is.numeric, sum)
y<-spp_sub_agg$Cum.Cost/1000000
Scenario=1 #toggle between 1 and 3

LT<-min(spp_sub_agg$Cum.Cost)/1000000
MT<-quantile(spp_sub_agg$Cum.Cost, 0.75)/1000000
HT<-max(spp_sub_agg$Cum.Cost)/1000000
if (Scenario==1)
{
  L_spp<-length(unique(data$Species))-nrow(invacost_sub_agg)
  M_spp<-0
  H_spp<-0
}
if (Scenario==3)
{  
L_spp<-0
M_spp<-length(unique(data$Species))-nrow(invacost_sub_agg)
H_spp<-0
}
m_gamma<-stan(file="gamma_damage_invacost.stan",data = list(y=y,LT=LT, HT=HT, L_spp=L_spp,M_spp=M_spp, H_spp=H_spp, MT=MT, N=length(y)),pars=c("shape","scale", "log_lik"),iter=10000, control=list(adapt_delta=0.999999), cores=1)
m_weibull<-stan(file="weibull_damage_invacost.stan",data = list(y=y,LT=LT, HT=HT, L_spp=L_spp,M_spp=M_spp, H_spp=H_spp, MT=MT, N=length(y)),pars=c("shape","scale", "log_lik"),iter=10000, control=list(adapt_delta=0.999999), cores=1)
m_lognormal<-stan(file="lognormal_damage_invacost.stan",data = list(y=y,LT=LT, HT=HT, L_spp=L_spp,M_spp=M_spp, H_spp=H_spp, MT=MT, N=length(y)),pars=c("mu","sigma", "log_lik"),iter=10000, control=list(adapt_delta=0.999999), cores=1)
m_pareto<-stan(file="pareto_damage_invacost.stan",data = list(y=y,LT=LT, HT=HT, L_spp=L_spp,M_spp=M_spp, H_spp=H_spp, MT=MT, N=length(y)),pars=c("ymin", "alpha", "log_lik"),iter=10000, control=list(adapt_delta=0.999999), cores=1)
best_mod<-which.max(c(sum((rstan::extract(m_gamma)$log_lik)), sum((rstan::extract(m_weibull)$log_lik)), sum((rstan::extract(m_lognormal)$log_lik)), sum((rstan::extract(m_pareto)$log_lik))))
#launch_shinystan(m_pareto)#visualize bayesian results
cost_est<-0

weights<-c(sum(exp(rstan::extract(m_gamma)$log_lik)), sum(exp(rstan::extract(m_weibull)$log_lik)), sum(exp(rstan::extract(m_lognormal)$log_lik)), sum(exp(rstan::extract(m_pareto)$log_lik)))/sum(sum(exp(rstan::extract(m_gamma)$log_lik)), sum(exp(rstan::extract(m_weibull)$log_lik)), sum(exp(rstan::extract(m_lognormal)$log_lik)), sum(exp(rstan::extract(m_pareto)$log_lik)))
if (any(is.nan(weights)==T))
    {weights<-c(0,0,0,0)
  weights[best_mod]=1}
cost_gamma<-cost_weibull<-cost_lnorm<-cost_par<-cost_ave<-matrix(0, 20000,101)
x<-c(-10,seq(0, log10(HT), length.out=100))
x<-10^x
for (i in 1:length(x))
{
  cost_gamma[,i]<-pgamma(x[i], shape=rstan::extract(m_gamma)$shape[10001:20000], rate=rstan::extract(m_gamma)$scale[10001:20000], lower.tail=F)
  cost_weibull[,i]<-pweibull(x[i], shape=rstan::extract(m_weibull)$shape[10001:20000], scale=rstan::extract(m_weibull)$scale[10001:20000], lower.tail=F)
  cost_lnorm[,i]<-plnorm(x[i], meanlog = rstan::extract(m_lognormal)$mu[10001:20000], sdlog=rstan::extract(m_lognormal)$sigma[10001:20000], lower.tail=F)
  cost_par[,i]<-ppareto(x[i], scale=rstan::extract(m_pareto)$ymin[10001:20000], shape=rstan::extract(m_pareto)$alpha[10001:20000], lower.tail=F)
  cost_ave[,i]<-rowMeans(cbind(cost_gamma[,i],cost_weibull[,i], cost_lnorm[,i], cost_par[,i])*weights)
}
colmax<-colmin<-colmid<-0
for (i in 1:101)
{
  colmax[i]<-quantile(cost_ave[,i], 0.95)
  colmin[i]<-quantile(cost_ave[,i],0.05)
  colmid[i]<-quantile(cost_ave[,i], 0.5)
}
points<-0
for (i in 1:length(y))
{
  points[i]<-which.min(abs(x-y[i]))
}


data2<-data.frame(cbind(x,colmax, colmin, colmid))
ggplot(data) + geom_ribbon(data=data2, aes(x=x, ymin=colmin, ymax=colmax),fill=viridis(5)[2], alpha=0.5)+ scale_x_continuous(name="Annual Cost",labels = function(x) format(x, scientific = TRUE))+scale_y_continuous(name="Probability Density")+ geom_line(data=data2,aes(y=colmid,x=x))+theme_classic()+geom_point(data=data.frame(y),aes(x=y), y=colmid[points], colour=viridis(1)[1])+theme(axis.text=element_text(size=11))+theme(plot.margin=unit(c(0.5,1,0.5,0.5), "cm")) # plots distribution of annual costs across species in millions USD, observations as points


total_cost<-0
for (i in 1:100)# should be 10000, but slow to run
{
  total_cost[i]<-sum(y)+L_spp*LT*sum(weights*c(pgamma(LT, shape=rstan::extract(m_gamma)$shape[i], rate=rstan::extract(m_gamma)$scale[i], lower.tail=T), pweibull(LT,shape=rstan::extract(m_weibull)$shape[i], scale=rstan::extract(m_weibull)$scale[i], lower.tail=T), plnorm(LT,meanlog = rstan::extract(m_lognormal)$mu[i], sdlog=rstan::extract(m_lognormal)$sigma[i], lower.tail=T), ppareto(LT,scale=rstan::extract(m_pareto)$ymin[i], shape=rstan::extract(m_pareto)$alpha[i], lower.tail=T)))

  if (Scenario>1)
  {
  total_cost[i]=total_cost[i]+M_spp*MT*(sum(weights*c(pgamma(MT, shape=rstan::extract(m_gamma)$shape[i], rate=rstan::extract(m_gamma)$scale[i], lower.tail=T), pweibull(MT,shape=rstan::extract(m_weibull)$shape[i], scale=rstan::extract(m_weibull)$scale[i], lower.tail=T), plnorm(MT,meanlog = rstan::extract(m_lognormal)$mu[i], sdlog=rstan::extract(m_lognormal)$sigma[i], lower.tail=T), ppareto(MT,scale=rstan::extract(m_pareto)$ymin[i], shape=rstan::extract(m_pareto)$alpha[i], lower.tail=T)))-sum(weights*c(pgamma(LT, shape=rstan::extract(m_gamma)$shape[i], rate=rstan::extract(m_gamma)$scale[i], lower.tail=T), pweibull(LT,shape=rstan::extract(m_weibull)$shape[i], scale=rstan::extract(m_weibull)$scale[i], lower.tail=T), plnorm(LT,meanlog = rstan::extract(m_lognormal)$mu[i], sdlog=rstan::extract(m_lognormal)$sigma[i], lower.tail=T), ppareto(LT,scale=rstan::extract(m_pareto)$ymin[i], shape=rstan::extract(m_pareto)$alpha[i], lower.tail=T))))
  }
}
#in millions
par(mar=c(4,4,2,2))
hist(total_cost)
mean(total_cost)
quantile(total_cost, 0.025)
quantile(total_cost, 0.975)
hist(sum(invacost_sub_agg$Cum.Cost)/(total_cost*1e+06)) 
weights
invacost_sub_agg%>%group_by(Type_of_cost_merged)%>%summarise_if(is.numeric, sum)
type_cont<-subset(invacost_cln, (Species%in%cabi_Health$Species&Geographic_region%in%c("Europe", "North America", "South America", "Asia", "Africa", "Oceania")))%>%group_by(Type_of_cost_merged, Geographic_region)%>%summarise_if(is.numeric, sum, na.rm=T)  

length(which(invacost_agg$Species%in%data$Species))
sum(invacost_sub_agg$Cum.Cost)
