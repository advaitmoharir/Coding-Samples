#Below is the code for the decomposition of India's 
#trade ratio into three components,namely,
# terms of trade (R), relative import intensity (RMI), and 
#relative expenditure growth (RA).

#Setting the working directory. Please modify this as per your device

setwd("C:/Users/hp/Dropbox/Trade Decomposition Data/Aggregated Decomposition/Goods/Data files and code")

#Uploading necessary packages
library(haven) #to read stata files
library(countrycode)
library(DT)
library(plm)
library(dplyr)
library(stringr)
library(splitstackshape)
library(tis)
library(rlang)
library(collapse)
library(plotly)
library(ggplot2)
library(tidyr)
library(imfr)
library(ggpubr)
library(knitr)

#The first step is to calculate DA and FA. FA is trade weighted expenditure
#growth of India's trade partners. We begin by importing gdp data by expenditure
#for all countries. This is taken from UN-SNA, and has 
#gdp data by expenditure, in nominal, real, usd and domes
#tic currency denominated terms.

#uploading gdp data for all countries
all_nations<-read_dta("gdp.dta")


#We now want to retain only countries which are India's trade partners. For
#merging, we first obtain the ISO3 codes for all countries using R package
#countrycode

#Getting ISO3 codenames
data(codelist)
country_set <- codelist
country_set<- country_set %>% 
  select(country.name.en , iso2c, iso3c) %>% filter( !is.na(iso2c))

#Attaching country codes

##We rename the common columns identically to 
#successfully merge

colnames(all_nations)[1]<-"country"
colnames(country_set)[1]<-"country"


codes<-read.csv("Country Codes.csv")# This file has 
#country codes for all countries.
codes$partner_iso<-toupper(codes$partner_iso)#Converts
#iso codes in file country codes to upper case.

#We now have two files with country names and country
#codes, namely country_set, obtained from an R package,
#and codes, obtained externally. Since, codes has the country
#names exactly as the ones in our gdp dataset, we merge
#country_set and codes to avoid duplication

#Merging country_set and codes
colnames(country_set)[3]<-"partner_iso"#Renamed for merge
codes<- country_set %>% right_join(codes, by="partner_iso")
codes<- select(codes, country.y, partner_iso, iso2c)
colnames(codes)[1]<-"country"

#Now we have the file codes, with country names and 
#ISO codes matching exactly with all_nations. 
#We now merge codes and all_nations to get a dataset
#of gdp of all countries with ISO codes. Since there
#were some issues with the merge with Belgium, we 
#include it separately.

#Final list of countries with ISO2 and ISO3 codes and Belgium code
gdp<-codes %>% right_join(all_nations, by="country")
gdp$partner_iso<-ifelse(gdp$country=="Belgium", "BEL", gdp$partner_iso)
gdp$iso2c<-ifelse(gdp$country=="Belgium", "BE", gdp$iso2c)


#Our selection is still incomplete. We still have to drop
#countries that are NOT India's trade partners. From the
#IMF-DOTS dataset, we get India's merchandise exports to
#each of its partners from 1980-2018

#Downloading DOTS dataset
trade<- imf_data(database_id = "DOT" , indicator = c("TXG_FOB_USD") , 
                 country = "IN" , start = 1980, end = 2018,
                 return_raw = TRUE)
trade_data <- trade$CompactData$DataSet$Series
trade_data <- trade_data %>% 
  filter(`@FREQ` == "A") %>% unnest(Obs) %>% 
  arrange(`@REF_AREA`, `@COUNTERPART_AREA`)
#The code takes some time to extract this data. Please
#wait for 10-15 seconds.

#We now merge trade_data and gdp. 
colnames(trade_data)[4]<-"iso2c"
colnames(trade_data)[7]<-"year"
gdp$year<-as.character(gdp$year)
world<-gdp %>% left_join(trade_data, by=c("year", "iso2c"))
world$Obs<-NULL

#To calculate trade-weights later,we need India's 
#total exports to RoW for each year. 

#Attaching total exports
reporter_exports<-world %>% filter(country=="India") %>% select(nom_exports, year)
world<- reporter_exports %>% right_join(world, by="year")

#We now rename some columns for convenience, and drop
#variables that we do not need.

colnames(world)[1]<-"total_exports"
colnames(world)[10]<-"nom_exports"
colnames(world)[26]<-"exports_by_reporter"
world<-select(world, year, country, partner_iso, iso2c, 
              cons_final, gfcf_final,gfcf_final_nominal,
              cons_final_nominal,nom_exports, 
              nom_imports,real_exports, real_imports, 
              cons_final_nom_dom, gfcf_final_nom_dom,
              nom_exports_dom,nom_imports_dom,gdp_usd,gdp_dom,
              exports_by_reporter, total_exports, gdp_nom_usd)

#We have our panel of India's trade partners ready.
#We set this as panel data for ease of calculations,
#and call the final dataset 'world_panel'

#Creating panel data
world$id <- world %>% group_indices(country)
world_panel<- pdata.frame(world, index= c("id", "year"))

#We now calculate DA_growth, which is weighted sum of 
#consumption and investment growth (govt+prvt)

#DA_growth=s_C*C+s*I*I

#DA_growth
world_panel$C<-as.numeric(ifelse(world_panel$year==1980, NA, G(world_panel$cons_final)/100))
world_panel$I<-ifelse(world_panel$year==1980, NA, G(world_panel$gfcf_final)/100)
world_panel$s_C<-ifelse(world_panel$year==1980, NA,lag(world_panel$cons_final_nominal)/(lag(world_panel$cons_final_nominal)+lag(world_panel$gfcf_final_nominal)))
world_panel$s_I<-ifelse(world_panel$year==1980, NA,1-world_panel$s_C)
world_panel$DA_growth<-(world_panel$C*world_panel$s_C)+(world_panel$I*world_panel$s_I)

#We now calculate DA levels from growth rates, by
#setting 1980=100

#DA
world_panel$DA_level <- world_panel$cons_final + world_panel$gfcf_final
baseDA <- world_panel[world_panel$year==1980, c('country', 'DA_level')]
names(baseDA)[2] <- 'baseDA'
world_panel <- merge(world_panel, baseDA, by='country')
world_panel$DA_index <- 100 * world_panel$DA_level / world_panel$baseDA

#We now calculate FA, or foreign expenditure growth,
#as trade-weighted sum of partner countries'
#DA growth. The weights (w) are the exports from
#country India to that country divided by
#India's total exports to RoW that year. Similar
#to DA, we index 1980=100,for FA levels


#FA
world_panel$exports_by_reporter<-as.numeric(world_panel$exports_by_reporter)
world_panel$w<-ifelse(world_panel$year==1980,NA,lag(world_panel$exports_by_reporter)/lag(world_panel$total_exports))
world_panel$FA_components<-world_panel$w*world_panel$DA_g
fa_growth<-aggregate(FA_components ~ year, data=world_panel,
                     sum)
fa_growth$FA<-(cumprod(fa_growth$FA_components+1))*100
fa_growth$FA<-as.numeric(fa_growth$FA)

#We now extract India's data from the panel, and save it
#as reporter. We name it 'reporter' and not India,
#as this code is replicable for any country's decompo
#sition. All steps remain the same, except that the 
#particular country's name is extracted from the 
#IMF-DOTS package instead of India's.

#Opening reporter's data and attaching DA/FA
reporter_data<-world_panel %>% 
  filter(country=="India") %>% select(year,country,cons_final,
                                      cons_final_nominal, gfcf_final_nominal, gfcf_final,	
                                      nom_exports,nom_exports_dom,nom_imports,nom_imports_dom,real_exports, real_imports, 
                                      DA_index,gdp_usd, gdp_dom, gdp_nom_usd)
reporter<-reporter_data %>% left_join(fa_growth, 
                                      by="year")
reporter$FA<-ifelse(reporter$year==1980, 100, reporter$FA)
reporter<-as.data.frame(reporter)

#We now have all of India's GDP data, trade data,
#as well as FA/DA. We now calculate FMI, DMI, R, RMI
#and RA

#Decomposition
reporter$TR<-(reporter$nom_exports_dom/reporter$nom_imports_dom)
reporter$RA<-(reporter$FA/reporter$DA)
reporter$FMI<-reporter$real_exports/reporter$FA
reporter$DMI<-reporter$real_imports/reporter$DA
reporter$RMI<-reporter$FMI/reporter$DMI
#Terms of trade are calculated using deflator
reporter$R<-reporter$TR*(reporter$real_imports/reporter$real_exports)

#To see contribution of each term in terns of 
#growth rates, we take logs.

#Logs
reporter$log_RA<-log(reporter$RA)
reporter$log_TR<-log(reporter$TR)
reporter$log_R<-log(reporter$R)
reporter$log_RMI<-log(reporter$RMI)
reporter$log_DMI<-log(reporter$DMI)
reporter$log_FMI<-log(reporter$FMI)
reporter$log_DA<-log(reporter$DA)
reporter$log_FA<-log(reporter$FA)

##Our calculation is complete. 

#exporting final output as csv
write.csv(reporter, "India.csv")


#For this sample, I show one figure and one table.

#Figure-1 shows the evolution of the trade ratio and
#the three terms (R, RMI, RA), relative to each other
#by indexing the respective logs of each term in 1980
#to zero.

#Table-1 shows the contribution of each term by various
#time periods




#################FIGURE-1#################


#importing final output as df
india<-read.csv("India.csv")

reindex1<-select(india, year, R, RMI, TR, RA)
reindex1$ToT<-log(reindex1$R/reindex1$R[1])
reindex1$Exp<-log(reindex1$RA/reindex1$RA[1])
reindex1$Tratio<-log(reindex1$TR/reindex1$TR[1])
reindex1$switch<-log(reindex1$RMI/reindex1$RMI[1])

#Figure 1: Decomposition over time(Goods, Indexed)

reindex1<-reindex1[,-c(2:5)]
reindex1%>%pivot_longer(2:5)%>%
  ggplot(aes(x=year,y=value, color=name ))+geom_line()+
  scale_color_discrete(name="", labels=c("log_RA", "log_RMI", "log_R", "log_TR"))+
  theme(plot.caption = element_text(hjust = 0, size=12))+
  labs(x="",y="",caption="Figure-1:Evolution of components of India's merchandise trade ratio(1980-2018)")
##################TABLE-1################

#This table shows the contribution of each of the three
#terms to the change in the trade ratio. For each of the 
# periods (1980-90, 1990-00, 2001-10, 2010-18), the contri
#bution is simply the log differences of R, RMI and RA
# between the starting and end years of the respective
#periods.


#Defining decomp and decomp table func

#The function 'decomp' carries out the decomposition
#by defining contribution of each term as the log-diff
#erences

decomp <- function(data, start, end){
  x <- data.frame(R=0, TR=0, FA=0, DA=0, FMI=0, DMI=0)
  x$TR<- (data[data$year==end, 'log_TR'] - data[data$year==start, 'log_TR'])*100
  x$R <- (data[data$year==end, 'log_R'] - data[data$year==start, 'log_R'])*100
  x$FA <- (data[data$year==end, 'log_FA'] - data[data$year==start, 'log_FA'])*100
  x$DA <- (-1 * (data[data$year==end, 'log_DA'] - data[data$year==start, 'log_DA']))*100
  x$FMI <- (data[data$year==end, 'log_FMI'] - data[data$year==start, 'log_FMI'] )*100
  x$DMI <- (-1 * (data[data$year==end, 'log_DMI'] - data[data$year==start, 'log_DMI'] ))*100
  return(x)
}

#The function decomp.table converts the decomposition data
#to a table
decomp.table <- function(data, starts, ends){
  x <- decomp(reporter, starts[1], ends[1])
  for (i in 2:length(starts)){
    x <- rbind(x, decomp(reporter, starts[i], ends[i]))
  }
  x$period <- paste(starts, ends, sep='-')
  x <- x[,c(7, 1:6)]
  return(x)
}

#Decomposition table

#First, we define start and end points of each period
starts <- c( 1980, 1991, 2002, 2012)
ends <- c(1990, 2001, 2011, 2018)

#We now create the decomposition table
decomposition <- decomp.table(reporter, starts, ends)
decomposition$R<-
  as.numeric(decomposition$R)
decomposition$TR<-as.numeric(decomposition$TR)
#Run line 316 to see table as df
View(decomposition)
#Using the kable command, the table can be formatted as 
#latex code for convenient visualization
decomposition<-decomposition%>%
  mutate_if(is.numeric, format, digits=2,nsmall = 0)
kable(decomposition, "latex")



