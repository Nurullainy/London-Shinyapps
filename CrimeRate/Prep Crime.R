library(sf)
library(tmap)
library(tmaptools)
library(tidyverse)
library(dplyr)

ukshapefile = "ESRI/LSOA_2011_London_gen_MHW.shp"
ldngeo = read_shape(file=ukshapefile, as.sf = TRUE)

LADcode = ldngeo[c(1,5)]
write.csv(LADcode, file = "City Coordinate.csv", row.names = F)

Code = read.csv("City Coordinate.csv")
Code = Code[!duplicated(Code), ]

Crime = read.csv("london_crime.csv")

NewCrime = merge(Crime, Code, by.x = "lsoa_code", by.y = "LSOA11CD", sort = T)
NewCrime = NewCrime[-c(1,4,7)]

# Generate Final and Clean Data------------------------------------------------------------------------
FinalCrime = NewCrime %>%
  group_by(Year, LAD11CD, City, Major) %>%
  summarise(Value = sum(Value))%>%
  ungroup() %>%
  select(Year, LAD11CD, City, Major, Value)

Type = FinalCrime %>%
  group_by(Year, City)%>%
  spread( Major, Value)
Type[is.na(Type)] <- 0

Type$Total = rowSums(Type[4:12])
Type$Top.Crime = colnames(Type[4:12])[apply(Type[4:12],1,which.max)]

FinalTypes = Type %>%
  select(Year, LAD11CD, City, Burglary, 'Criminal Damage', Drugs, Robbery, 'Fraud or Forgery', 'Sexual Offences',
         'Theft and Handling', 'Violence Against the Person', 'Other Notifiable Offences', Total, Top.Crime) %>%
  rename(Vandalism = 'Criminal Damage', Fraud = 'Fraud or Forgery', Sexual = 'Sexual Offences', Theft = 'Theft and Handling',
         Violence = 'Violence Against the Person', Other = 'Other Notifiable Offences')

FinalTypes$BurglaryPct = round(FinalTypes$Burglary/FinalTypes$Total*100, digits = 2)
FinalTypes$VandalismPct = round(FinalTypes$Vandalism/FinalTypes$Total*100, digits = 2)
FinalTypes$DrugsPct = round(FinalTypes$Drugs/FinalTypes$Total*100, digits = 2)
FinalTypes$RobberyPct = round(FinalTypes$Robbery/FinalTypes$Total*100, digits = 2)
FinalTypes$TheftPct = round(FinalTypes$Theft/FinalTypes$Total*100, digits = 2)
FinalTypes$ViolencePct = round(FinalTypes$Violence/FinalTypes$Total*100, digits = 2)
FinalTypes$OtherPct = round(FinalTypes$Other/FinalTypes$Total*100, digits = 2)
FinalTypes[is.na(FinalTypes)] <- 0

write.csv(FinalCrime, file="Final Crime Data.csv", row.names = FALSE)
write.csv(FinalTypes, file="Final Crime Types.csv", row.names = FALSE)

# Data Preparation By Year--------------------------------------------------------------------------------------

Crime2012 = FinalTypes %>%
  filter(Year == 2012) %>%
  group_by(LAD11CD) %>%
  select(LAD11CD:Robbery, Theft:OtherPct)

Crime2013 = FinalTypes %>%
  filter(Year == 2013) %>%
  group_by(LAD11CD) %>%
  select(LAD11CD:Robbery, Theft:OtherPct)


Crime2014 = FinalTypes %>%
  filter(Year == 2014) %>%
  group_by(LAD11CD) %>%
  select(LAD11CD:Robbery, Theft:OtherPct)

Crime2015 = FinalTypes %>%
  filter(Year == 2015) %>%
  group_by(LAD11CD) %>%
  select(LAD11CD:Robbery, Theft:OtherPct)

Crime2016 = FinalTypes %>%
  filter(Year == 2016) %>%
  group_by(LAD11CD) %>%
  select(LAD11CD:Robbery, Theft:OtherPct)

write.csv(Crime2012, file="2012 Crime Data.csv", row.names = FALSE)
write.csv(Crime2013, file="2013 Crime Data.csv", row.names = FALSE)
write.csv(Crime2014, file="2014 Crime Data.csv", row.names = FALSE)
write.csv(Crime2015, file="2015 Crime Data.csv", row.names = FALSE)
write.csv(Crime2016, file="2016 Crime Data.csv", row.names = FALSE)