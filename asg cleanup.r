#This code extends the Chalfin-McCrary dataset on 
#effect of police on crime elasticity to 2019. The 
#key explanatory variable is police employment, which has
#two measures: UCR and ASG. We begin by extending the
#UCR dataset.

options(digits=16)
#Reading packages
library(tidyverse)
library(dplyr)
library(haven)
library(tidyr)
library(plm)
library(varhandle)
library(collapse)

#Setting working directory.
setwd("C:/Users/hp/Dropbox/JAE Replication Data/My replication/UCR")

#The full-fledged dataset from 1960-2019 is already
#publicly available on the website of Dr. Jacob Kaplan.
# We use this for our replication exercise. Since the raw
#file is too big, a partially cleaned version, consisting
#of only the relevanr variables is used here.

########## UCR DATA###############

ucr_kaplan<-read_dta("UCR_Kaplan.dta")

#The Kaplan dataset provides all the variables for
#all the crimes. To this, we now add the data for the 
#UCR measure of police employment, recorded in the LEOKA
#database. This is also available from 1960-2019 on Dr.
#Kaplan's website

leoka<-read_dta("LEOKA.dta")
leoka<-leoka%>%select(ORI7, STATE, year, total_employees_officers)

#We now merge leoka and ucr_kaplan to get a unified dataset
#of crime and police employment

ucr_all<-ucr_kaplan%>%left_join(leoka, by=c("ORI7", "STATE", "year"))

#We now merge ucr_all (or ORI7) with the base dataset provided by 
#the authors. This will reduce the dataset to the sample used
#in the paper: 242 cities

base<-read_dta("Base.dta")
base_merge<-base%>%select("ORI7")

ucr_final<-unique(base_merge)%>%
  left_join(ucr_all, by="ORI7")%>%
  arrange(ORI7, year)

#We now have our dataset with all relavant UCR variables

###############ASG Data################

#The ASG data is downloaded year-by-year, from 2011-2019
#from the Census website (ASPEP Tables). From the ASG,
#we need the estimates for police employment and city
#population, to use as proxies for the UCR counterparts.


#Setting working directory
setwd("C:/Users/hp/Dropbox/JAE Replication Data/My replication/ASG/ASPEP 2011-19/Police Emp and Pop/Police Employment/CSV")
#The raw ASG files containing information for public
#employment in all sectors has been downloaded year on year
#from 2011. 

#Reading csvs

temp=list.files(pattern="*.csv")
myfiles=lapply(temp, read.csv, header=FALSE,numerals=c("no.loss"))

#All the csvs are now stored as a list in myfiles. Next,
#using a function, we extract only the police employment 
#data, name the columns, and drop irrelevant variables.

#Changing column names

myfiles<-lapply(myfiles, function(x){
  colnames(x)<-c("govid","type_emp","full_time","full_time_flag",
                 "full_time_payroll", "full_time_payroll_flag", "part_time", "part_time_flag",
                 "part_time_payroll", "part_time_payroll_flag", "part_time_hrs", "part_time_hrs_flag",
                 "full_time_equivalent")
  x<-subset(x, type_emp==62, select=c("govid", "full_time"))
  x
  
})

#We now extract the data for each year as seperate
#dataframes, named df1-df9.
for (i in seq(myfiles))
 assign(paste0("df", i), myfiles[[i]])

# Attaching years

df1$year<-2011
df2$year<-2012
df3$year<-2013
df4$year<-2014
df5$year<-2015
df6$year<-2016
df7$year<-2017
df8$year<-2018
df9$year<-2019

#Since all dfs have the same column names, we simply
#do a rbind

asg_extended<-do.call("rbind", list(df1, df2, df3, df4,df5,df6,df7,df8,df9))
asg_extended$govid<-asg_extended$govid/100000#This is
#to address a coding anomaly. 

#Now we have the extended asg dataset (2011-2019).
#We now read in the base asg dataset (1960-2010).
#This was procided by authors.

base_asg<-read_dta("ASG_Original.dta")

#First we merge the base_asg with ucr_final to get
#our unified dataset. 
final<-ucr_final%>%left_join(base_asg, by=c("ORI7", "year"))

#We have the unified dataset. However, to extend the
#dataset, we need a common variable to combine
#'final' and 'asg_extended'. For this, we use 
#the unit-level crosswalk data, which has both
#ORI7 and the census govt id for each data point.

crosswalk<-read_dta("Crosswalk.dta")

#We merge final and crosswalk by ORI7, so that the
#unified datasaet also has the govid(census), which will
#be used to extend the dataset
final<-final%>%left_join(crosswalk, by="ORI7")

################EXTENDING ASG POLICE DATA############
#We begin extending the dataset by ensuring the key column
#has the same name
colnames(asg_extended)[1]<-"GOVID"

#We merge final and asg_extended on govid and year
#so the data from 2011-2018 is included in a 
#seperate column named 'full_time'
final<-final%>%left_join(asg_extended, by=c("GOVID", "year"))


#We now add the city population measure for asg
##################ADDING POPULATION VARIABLE#############

#As with the police employment, year wise city populatio
#n data is available on the Census website, in txt
#files. 

#The exact same procedure as the one for police emp is
#used
setwd("C:/Users/hp/Dropbox/JAE Replication Data/My replication/ASG/ASPEP 2011-19/Police Emp and Pop/Population")
#Importing txt file

temp_pop=list.files(pattern="*.txt")
myfiles2=lapply(temp_pop, read_fwf, fwf_positions(c(1,15,79,80,110,112,126,135), 
                                             c(14,78,79,109,111,114,134,136)))

#Changing column names

myfiles2<-lapply(myfiles2, function(x){
  colnames(x)<-c("govid","name", "region_code", "county_name",
                 "fips_state", "fips_county", "pop_asg","pop_year")
  x<-subset(x, select=c("govid", "pop_asg"))
  x
  
})

#Getting seperate dataframes
for (i in seq(myfiles2))
  assign(paste0("pop", i), myfiles2[[i]])

# Attaching years

pop1$year<-2011
pop2$year<-2012
pop3$year<-2013
pop4$year<-2014
pop5$year<-2015
pop6$year<-2016
pop7$year<-2017
pop8$year<-2018
pop9$year<-2019

pop<-as.data.frame(do.call("rbind", list(pop1,pop2,pop3,pop4,pop5,pop6,pop7,pop8,pop9)))
pop$govid<-as.numeric(pop$govid)
pop$pop_asg<-as.numeric(pop$pop_asg)
colnames(pop)[1]<-"GOVID"
pop$GOVID<-pop$GOVID/100000#2 NAs
pop<-pop%>%drop_na()

#Before performing the merge, we calculate the growth
#rates of asg_population. This is because we only have
#the growth rates in the base files from 1960-2010, and
#this will make the extension easier.

#We first set pop as a panel dataframe

pop_panel<-pdata.frame(pop, index=c("GOVID", "year"))
#pop_panel$g_pop<-NA
pop_panel$g_pop<-diff(log(pop_panel$pop_asg))
pop<-as.data.frame(pop_panel)%>%mutate(GOVID=unfactor(GOVID), year=unfactor(year))

#Attaching population to final
final<-final%>%left_join(pop, by=c("GOVID", "year"))



#We now have all the key variables, and can now
#compute the remaining variables from these
#i.e. the cost weighted sum of crimes, and
#the respective growth rates of the key 
#regressors
############COST-WEIGHTED SUM OF CRIMES#######

#To understand the dollar value of the effect of police 
#on crimes, the authors compute cost-weighted sum of crimes
#for three categories: violent, property and all.
#Murder, rape, robbery and assualt comprise violent crimes
#while burglary, larceny and motor-vehicle theft are
#property crimes.

#The cost weighted sum is simply, the sum number of crimes
#for each category multiplied by its cost, assigned below
#Assigning costs of each crime

cost_murder<-7000000
cost_rape<-142020
cost_robbery<-12624
cost_assault<-38924
cost_burglary<-2104
cost_larceny<-473
cost_mvt<-5786 #these numbers are taken from the paper



#Calculating cost-weighted sum of crimes

final$cw_violent<-cost_murder*final$actual_murder+cost_rape*final$actual_rape_total+cost_robbery*final$actual_robbery_total+cost_assault*final$actual_assault_total
final$cw_property<-cost_burglary*final$actual_burg_total+cost_larceny*final$actual_theft_total+cost_mvt*final$actual_mtr_veh_theft_total
final$cw_all<-final$cw_violent+final$cw_property

#Adding asg growth rates till 2010
#Growth rates of asg_population
base_pop<-base%>%select(ORI7, year, Z,C2,S)
final<-final%>%left_join(base_pop, by=c("ORI7", "year"))

#Now we simply shift the data from 'full_time'
#into the empty 'Z' columns from 2011-2019

final$Z<-ifelse(final$year>2010, Dlog(final$full_time), final$Z)


#To generate unique identifiers for each city we generate
#cityid for each city using ORI7, and the variable
#stateyear to group cities by state and year in order
#to include state-year fixed effects

#state year
final<-final%>%
  group_by(STATE,year)%>%
  mutate(stateyear=cur_group_id())
#cityid
final<-transform(final, cityid=as.numeric(factor(ORI7)))

###########GROWTH RATES###########
#Setting data as panel data

final_panel<-pdata.frame(final, index=c("ORI7", "year"))
relevant_vars<-c("actual_murder",
                 "actual_rape_total",
                 "actual_robbery_total",
                 "actual_assault_total",
                 "actual_burg_total",
                 "actual_theft_total",
                 "actual_mtr_veh_theft_total",
                 "actual_index_property",
                 "actual_index_violent",
                 "cw_violent", "cw_property",
                 "cw_all","total_employees_officers",
                 "population")

#We now calculate the growth rates of all the individual
#crimes, the three cost weighted sums, and the two
#measures of police employment and city population.


final_panel<-final_panel%>%
  mutate_at(relevant_vars, funs(Dlog(.)))%>%
  filter(year!=1960)

#Completing the asg_growth series
final_panel<-final_panel%>%
  mutate(year=unfactor(year), 
         C2=ifelse(year>2010, g_pop,C2))

#We use the 2010 UCR city population as weights, and 
#add them as the variable W

W<-final%>%select(year,ORI7, population)%>%
  filter(year==2010)%>%
  rename(W=population)%>%
  select(-year)

final_panel<-final_panel%>%
  left_join(W, by="ORI7")%>%
  select(ORI7,GOVID,STATE,year,cityid,stateyear,
         all_of(relevant_vars),Z,
         C2, W)
#Dropping all empty observations
final_panel<-final_panel%>%drop_na()

#Converting all -Inf to zero
is.na(final_panel)<-sapply(final_panel, is.infinite)
final_panel[is.na(final_panel)]<-0

#Renaming all variables as per original dataset
final_panel<-final_panel%>%rename(Y1=actual_murder,
                                  Y2=actual_rape_total,
                                  Y3=actual_robbery_total,
                                  Y4=actual_assault_total,
                                  Y5=actual_burg_total,
                                  Y6=actual_theft_total,
                                  Y7=actual_mtr_veh_theft_total,
                                  Y8=actual_index_violent,
                                  Y9=actual_index_property,
                                  Y10=cw_violent, Y11=cw_property,
                                  Y12=cw_all, S=total_employees_officers,
                                  C1=population)
#Outputting final dataset as csv and dta

write.csv(final_panel, "Chalfin_McCrary_Extended.csv")
write_dta(final_panel, "Chalfin_McCrary_Extended.dta")
