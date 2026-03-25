#==================================================================================================
# Title:                 Solar Farms and Crop Prices Results
# Author:                Jerome Dumortier
# Date:                  21 March 2026
#==================================================================================================
rm(list=ls())
library(ggplot2)
library(ggspatial)
library(openxlsx)
library(sf)
library(terra)
root                     = "D:/Research/Live Manuscripts/Solar Farms and Crop Prices"
setwd(paste(root,"/Analysis",sep=""))
load("SolarFarmsCropPricesResults.RData")
commodities              = data.frame(commodity=c("CO","SB","WH"),
                                      commodityname=c("Maize","Soybeans","Wheat"))
scenarios                = readWorkbook("SolarFarmsCropPrices.xlsx",sheet="SCENARIOS")
scenarios                = scenarios[c("scenario","countycap","oncropland","pv","powerline",
                                       "land")]
scenarios$powerline      = ordered(scenarios$powerline,levels=c("PL1000m","PL2500m","PL5000m",
                                                                "PL No Constraint"))
#--------------------------------------------------------------------------------------------------
# Changes in Commodity Prices
#--------------------------------------------------------------------------------------------------
prices                   = subset(prices,commodity %in% commodities$commodity)
dfbase                   = subset(prices,scenario=="Baseline",select=c("commodity","value"))
dfscen                   = subset(prices,scenario!="Baseline")
dfscen                   = merge(dfbase,dfscen,by=c("commodity"))
dfscen$change            = dfscen$value.y/dfscen$value.x-1
dfscen                   = dfscen[c("commodity","scenario","change","land")]
dfscen                   = merge(dfscen,commodities,by=c("commodity"))
dfscen                   = merge(dfscen,scenarios,by=c("scenario","land"))
#--------------------------------------------------------------------------------------------------
dfscen26                 = subset(dfscen,commodity %in% c("CO","SB","WH") & land=="2.6 ha")
ggplot(dfscen26,aes(x=oncropland,y=change*100,color=paste(countycap*100,"% Area Cap",sep=""),
                  shape=powerline))+
     geom_point(position=position_dodge(width=0.75),size=2)+
     facet_grid(vars(commodityname),vars(pv))+theme_bw()+
     theme(legend.title=element_blank(),axis.title.x=element_blank(),legend.position="bottom",
           panel.grid.minor=element_blank())+ylab("% Change from No Solar")+
     scale_color_manual(values=c("10% Area Cap"="#0072B2","25% Area Cap"="#E69F00"))
ggsave(paste(root,"/Manuscript/pricechange.pdf",sep=""),width=9,height=9)
#--------------------------------------------------------------------------------------------------
dfscen40                 = subset(dfscen,commodity %in% c("CO","SB","WH") & land=="4.0 ha")
ggplot(dfscen40,aes(x=oncropland,y=change*100,color=paste(countycap*100,"% Area Cap",sep=""),
                  shape=powerline))+
     geom_point(position=position_dodge(width=0.75),size=2)+
     facet_grid(vars(commodityname),vars(pv))+theme_bw()+
     theme(legend.title=element_blank(),axis.title.x=element_blank(),legend.position="bottom",
           panel.grid.minor=element_blank())+ylab("% Change from No Solar")+
     scale_color_manual(values=c("10% Area Cap"="#0072B2","25% Area Cap"="#E69F00"))
ggsave(paste(root,"/Manuscript/pricechange40.pdf",sep=""),width=9,height=9)
#--------------------------------------------------------------------------------------------------
# Area Requirement and Area Use by State
#--------------------------------------------------------------------------------------------------
areareq                  = aggregate(value~scenario,FUN=sum,data=area)
areareq$value            = areareq$value[which(areareq$scenario=="Baseline")]-areareq$value
#--------------------------------------------------------------------------------------------------
areabase                 = subset(area,scenario=="Baseline",select=c("fips","value"))
areascen                 = subset(area,scenario!="Baseline")
areascen                 = merge(areascen,areabase,by=c("fips"))
areascen$value           = areascen$value.y-areascen$value.x
states                   = readWorkbook("SolarFarmsCropPrices.xlsx",sheet="COUNTY")
areascen                 = merge(areascen,states,by=c("fips"))
areascen                 = aggregate(value~statename+scenario+land,FUN=sum,data=areascen)
colnames(areascen)       = c("state","scenario","land","value")
#--------------------------------------------------------------------------------------------------
soi                      = subset(areascen,scenario=="Baseline (25%) - 40% Crop - PL2500m")
soi                      = soi[order(-soi$value),]
row.names(soi)           = NULL
soi                      = as.character(soi$state[1:5])
#--------------------------------------------------------------------------------------------------
df                       = subset(areascen,state %in% soi & scenario %in% scenarios$scenario &
                                       land=="2.6 ha")
df                       = merge(df,scenarios,by=c("scenario"))
ggplot(df,aes(x=oncropland,y=value/1000000,color=paste(countycap*100,"% Area Cap",sep=""),
                  shape=powerline))+
     geom_point(position=position_dodge(width=0.75),size=2)+
     facet_grid(vars(state),vars(pv))+theme_bw()+
     theme(legend.title=element_blank(),axis.title.x=element_blank(),legend.position="bottom",
           panel.grid.minor=element_blank())+ylab("in Million Ha")+
     scale_color_manual(values=c("10% Area Cap"="#0072B2","25% Area Cap"="#E69F00"))
ggsave(paste(root,"/Manuscript/areadifferencestate.pdf",sep=""),width=9,height=10)
#--------------------------------------------------------------------------------------------------
df                       = subset(areascen,state %in% soi & scenario %in% scenarios$scenario &
                                       land=="4.0 ha")
df                       = merge(df,scenarios,by=c("scenario"))
ggplot(df,aes(x=oncropland,y=value/1000000,color=paste(countycap*100,"% Area Cap",sep=""),
              shape=powerline))+
     geom_point(position=position_dodge(width=0.75),size=2)+
     facet_grid(vars(state),vars(pv))+theme_bw()+
     theme(legend.title=element_blank(),axis.title.x=element_blank(),legend.position="bottom",
           panel.grid.minor=element_blank())+ylab("in Million Ha")+
     scale_color_manual(values=c("10% Area Cap"="#0072B2","25% Area Cap"="#E69F00"))
ggsave(paste(root,"/Manuscript/areadifferencestate40.pdf",sep=""),width=9,height=10)
#--------------------------------------------------------------------------------------------------
# Changes in Production
#--------------------------------------------------------------------------------------------------
dfbase                   = subset(production,scenario=="Baseline",select=c("commodity","value"))
dfscen                   = subset(production,scenario!="Baseline")
dfscen                   = merge(dfbase,dfscen,by=c("commodity"))
dfscen$change            = dfscen$value.y/dfscen$value.x-1
dfscen                   = dfscen[c("commodity","scenario","land","change")]
dfscen                   = merge(dfscen,commodities,by=c("commodity"))
dfscen                   = merge(dfscen,scenarios,by=c("scenario","land"))
#--------------------------------------------------------------------------------------------------
df                       = subset(dfscen,land=="2.6 ha")
ggplot(df,aes(x=oncropland,y=change*100,color=paste(countycap*100,"% Area Cap",sep=""),
                  shape=powerline))+
     geom_point(position=position_dodge(width=0.75),size=2)+
     facet_grid(vars(commodityname),vars(pv))+theme_bw()+
     theme(legend.title=element_blank(),axis.title.x=element_blank(),legend.position="bottom",
           panel.grid.minor=element_blank())+ylab("% Change from No Solar")+
     scale_color_manual(values=c("10% Area Cap"="#0072B2","25% Area Cap"="#E69F00"))
ggsave(paste(root,"/Manuscript/productionchange.pdf",sep=""),width=9,height=9)
#--------------------------------------------------------------------------------------------------
df                       = subset(dfscen,land=="4.0 ha")
ggplot(df,aes(x=oncropland,y=change*100,color=paste(countycap*100,"% Area Cap",sep=""),
              shape=powerline))+
     geom_point(position=position_dodge(width=0.75),size=2)+
     facet_grid(vars(commodityname),vars(pv))+theme_bw()+
     theme(legend.title=element_blank(),axis.title.x=element_blank(),legend.position="bottom",
           panel.grid.minor=element_blank())+ylab("% Change from No Solar")+
     scale_color_manual(values=c("10% Area Cap"="#0072B2","25% Area Cap"="#E69F00"))
ggsave(paste(root,"/Manuscript/productionchange40.pdf",sep=""),width=9,height=9)
#--------------------------------------------------------------------------------------------------
rm(list=setdiff(ls(),c("farmrevenue","core","root")))
#--------------------------------------------------------------------------------------------------
# Net Return Change
#--------------------------------------------------------------------------------------------------
farmrevenue              = farmrevenue[!is.nan(farmrevenue$value),]
revbase                  = subset(farmrevenue,scenario=="Baseline",select=c("fips","value"))
revscen                  = subset(farmrevenue,scenario!="Baseline")
revscen                  = merge(revscen,revbase,by=c("fips"))
revscen$value            = revscen$value.x/revscen$value.y-1
df                       = revscen[c("fips","scenario","value")]
dfbase                   = subset(df,scenario=="Baseline (25%) - 40% Crop - PL2500m")
dfbase$value             = dfbase$value*100
dfhipv                   = subset(df,scenario=="High PV (25%) - 40% Crop - PL2500m")
dfhipv$value             = dfhipv$value*100
#--------------------------------------------------------------------------------------------------
soi                      = c("24","19","10","39","42","31","01","05","48","21","13","55","29","51",
                             "47","22","36","26","12","17","27","18","25","20","50","09","34","11",
                             "37","38","45","28","46","40","54","23","33","44")
soi                      = as.numeric(soi)
labels                   = c("Below 1%","1%-2%","2%-3%","3%-4%","4%-5%",
                             "5%-7.5%","7.5%-10%","Above 10%")
breaks                   = c(0,1,2,3,4,5,7.5,10,100)
my_colors                = colorRampPalette(RColorBrewer::brewer.pal(9,"YlGnBu"))(8)
#--------------------------------------------------------------------------------------------------
counties                 = read_sf(dsn=paste(root,"/GIS/Census",sep=""),layer="counties")
states                   = read_sf(dsn=paste(root,"/GIS/Census",sep=""),layer="states")
counties                 = subset(counties,statefips %in% soi)
states                   = subset(states,statefips %in% soi)
#--------------------------------------------------------------------------------------------------
tempcounties             = counties
tempcounties             = merge(tempcounties,rbind(dfbase,dfhipv),by="fips")
#--------------------------------------------------------------------------------------------------
ggplot()+
     geom_sf(data=tempcounties,aes(fill=cut(value,breaks=breaks,include.lowest=TRUE)),linetype=0)+
     theme_bw()+scale_fill_manual(values=my_colors,labels=labels)+
     geom_sf(data=states,colour="black",linetype=1,alpha=0)+coord_sf(crs="+init=epsg:26978")+
     theme(legend.position="bottom")+facet_wrap(vars(scenario),ncol=2)+
     guides(fill=guide_legend(title="Revenue Change"))
ggsave(paste(root,"/Manuscript/revenuemap.pdf",sep=""),width=9,height=6)
#==================================================================================================
# End of File
#==================================================================================================