# CABI scraping for impact script - Shared with invacost workshop group by Emma Hudgins,Dec 1 2019
#emma.hudgins@mail.mcgill.ca
# extracts impact info from cabi url using search terms for each major activity sector
# currently only examines impact summary table and page sections with impact in the title

require(rvest)

Forestry_terms<-c("negatively impacts forestry")
Ag_terms<-c("negatively impacts animal health","negatively impacts agriculture") # animal health may be fish..potentially remove those appearing as both ag and fisheries pests
Fish_terms<-c("negatively impacts aquaculture/fisheries")
Health_terms<-c("negatively impacts human health")

exclusions<-c("phylum: nematoda", "domain: virus", "domain: bacteria", "kingdom: protista")
pathogens<-c("fungi", "chromista")

summary_ag<-c("agriculture", "animal health")
summary_forest<-c("forestry")
summary_fish<-c("fisheries", "aquaculture")
summary_health<-c("human health")

getImpact = function(url){
Impacts<-rep(NA,5)
names(Impacts)<-c("Agriculture", "Forestry", "Fisheries", "Health", "ispathogen")
Impacts<-as.data.frame(as.matrix(t(Impacts)))
Sys.sleep(runif(1,0,10))
  cabipage = read_html(url)
  txt = html_text(cabipage)
  
  if (any(sapply(exclusions,FUN=function(x)grepl(x, tolower(txt)))))
  {
    Impacts<-rep("exclude", 5)
    return(Impacts)
  }
  tables = html_table(cabipage, fill = T)
  if (any(sapply(pathogens,FUN=function(x)grepl(x, tolower(txt)))))
  {
    Impacts$ispathogen<-"Y"
  }else{
    Impacts$ispathogen<-"N"
  }
  
  Impact_section<-html_text(html_nodes(cabipage,xpath="//div[@class='Product_data-item']"))
  Impact_section<-Impact_section[grepl("Risk and Impact Factors",Impact_section)]
  if (length(Impact_section)>0)
  { 
    if (any(lapply(Forestry_terms,FUN=function(x){sum(grepl(x,tolower(Impact_section)))>0})==T))
    {
      Impacts$Forestry<-"Negative"
    }
    
    if (any(lapply(Ag_terms,FUN=function(x){sum(grepl(x,tolower(Impact_section)))>0})==T))
    {
      Impacts$Agriculture<-"Negative"
    }
    
    if (any(lapply(Fish_terms,FUN=function(x){sum(grepl(x,tolower(Impact_section)))>0})==T))
    {
      Impacts$Fisheries<-"Negative"
    }
    
    if (any(lapply(Health_terms,FUN=function(x){sum(grepl(x,tolower(Impact_section)))>0})==T))
    {
      Impacts$Health<-"Negative"
    }
   # return(Impacts)
  }

  
  table = which(unlist(lapply(tables, function(x){
    if(any(grepl("Impact",colnames(x)))){
      return(T)
    }else{
      return(F)
    }
  }))==T)  
  
  if (length(table)>0)
  {
    table = tables[[table]]
    if (any(lapply(summary_forest,FUN=function(x){sum(grepl(x,tolower(table$Category)))>0})==T))
    {
     if (is.na(Impacts$Forestry)){
       Impacts$Forestry<-paste(table$Impact[sapply(summary_forest, FUN=function(x)grep(x,  tolower(table$Category)))], collapse=", ")}
    }
    
    if (any(lapply(summary_ag,FUN=function(x){sum(grepl(x,tolower(table$Category)))>0})==T))
    {
     if (is.na(Impacts$Agriculture)){ Impacts$Agriculture<-paste(table$Impact[sapply(summary_ag, FUN=function(x)grep(x,  tolower(table$Category)))],collapse=", ")}
      
    }
    
    if (any(lapply(summary_fish,FUN=function(x){sum(grepl(x,tolower(table$Category)))>0})==T))
    {
     if (is.na(Impacts$Fisheries)) {Impacts$Fisheries<-paste(table$Impact[sapply(summary_fish, FUN=function(x)grep(x,  tolower(table$Category)))], collapse=", ")}  
    }
    
    if (any(lapply(summary_health,FUN=function(x){sum(grepl(x,tolower(table$Category)))>0})==T))
    {
     if(is.na(Impacts$Health)) {Impacts$Health<-paste(table$Impact[sapply(summary_health, FUN=function(x)grep(x,  tolower(table$Category)))], collapse=", ")}
    }
  }
  return(Impacts)
}
