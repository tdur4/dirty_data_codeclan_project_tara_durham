library(tidyverse)
library(janitor)
library(here)
library(readxl)
library(stringr)
library(reshape2)
library(readr)
library("writexl")
library(plyr)
library(dplyr)
library(data.table)
library(readxl)

here::here()

#boing_boing_candy_2015 <- read_excel(here("/raw_data/boing-boing-candy-2015.xlsx"))
#boing_boing_candy_2016 <- read_excel(here("/raw_data/boing-boing-candy-2016.xlsx"))
#boing_boing_candy_2017 <- read_excel(here("/raw_data/boing-boing-candy-2017.xlsx"))
boing_boing_candy_2015 <- read_excel("raw_data/boing-boing-candy-2015.xlsx")
boing_boing_candy_2016 <- read_excel("raw_data/boing-boing-candy-2016.xlsx")
boing_boing_candy_2017 <- read_excel("raw_data/boing-boing-candy-2017.xlsx")

colnames(boing_boing_candy_2015)
reduced_2015 <- boing_boing_candy_2015[,-97:-113] 
colnames(reduced_2015)
final_2015 <- reduced_2015[,-99:-107]
colnames(final_2015)
colnames(final_2015)[2] ="age"
colnames(final_2015)[3] ="going_out"
final_2015$numbers
options(scipen = 999)
pivot_2015 <- final_2015 %>% 
  pivot_longer(cols= -c(Timestamp,age, going_out), names_to = "candy_type",values_to =  "rating") %>% 
  mutate(candy_type = str_replace(candy_type, "\\[", "")) %>% 
  mutate(candy_type = str_replace(candy_type, "\\]", "")) %>% 
  mutate(year = 2015)
pivot_2015 <- pivot_2015 [, -c(1)]

colnames(boing_boing_candy_2016)
final_2016 <- boing_boing_candy_2016[, -c(107,108,109,110,111,112,113,114,115,116,117,118,119,120,121,122,123,6)]
colnames(final_2016)[4] ="age" 
colnames(final_2016)[2] ="going_out"
colnames(final_2016)[3] ="your_gender"
colnames(final_2016)[5] ="country"
final_2016$numbers
options(scipen = 999)
final_country_2016 <- final_2016 %>% 
  mutate(country = str_replace(country, "United States", "US")) %>%
  mutate(country = str_replace(country, "USA", "US")) %>% 
  mutate(country = str_replace(country, "usa", "US")) %>% 
  mutate(country = str_replace(country, "US of America", "US")) %>% 	
  mutate(country = str_replace(country, "uSA", "US")) %>% 
  mutate(country = str_replace(country, "united states", "US")) %>% 
  mutate(country = str_replace(country, "us", "US")) %>% 
  mutate(country = str_replace(country, "US!", "US")) %>% 
  mutate(country = str_replace(country, "U.S.A.", "US")) %>% 
  mutate(country = str_replace(country, "Murica", "US")) %>% 
  mutate(country = str_replace(country, "Usa", "US")) %>% 
  mutate(country = str_replace(country, "U.S.", "US")) %>% 
  mutate(country = str_replace(country, "Units States", "US")) %>% 
  mutate(country = str_replace(country, "Us", "US")) %>% 
  mutate(country = str_replace(country, "United states", "US")) %>% 
  mutate(country = str_replace(country, "US USA USA", "US")) %>% 
  mutate(country = str_replace(country, "the best one - US", "US")) %>% 
  mutate(country = str_replace(country, "US USA! USA!	", "US")) %>% 
  mutate(country = str_replace(country, "u.s.", "US")) %>% 
  mutate(country = str_replace(country, "there isn't one for old men", "US")) %>% 
  mutate(country = str_replace(country, "u.s.", "US")) %>% 
  mutate(country = str_replace(country, "The Yoo Ess of Aaayyyyyy", "US")) %>% 
  mutate(country = str_replace(country, "US of america", "US")) %>% 
  mutate(country = str_replace(country, "u.s.", "US")) %>% 
  mutate(country = str_replace(country, "u.s.", "US")) %>% 
  mutate(country = str_replace(country, "United states", "US")) %>% 
  mutate(country = str_replace(country, "	US!!!!!", "US")) %>% 
  mutate(country = str_replace(country, "US USA!", "US")) %>% 
  mutate(country = str_replace(country, "United Sates", "US")) %>% 
  mutate(country = str_replace(country, "Sub-Canadian North America...'Merica", "US")) %>% 
  mutate(country = str_replace(country, "Trumpistan", "US")) %>% 
  mutate(country = str_replace(country, "America", "US")) %>% 
  mutate(country = str_replace(country, "US USA! USA!", "US")) %>% 
  mutate(country = str_replace(country, "Japan", "Other")) %>% 
  mutate(country = str_replace(country, "canada", "Canada")) %>% 
  mutate(country = str_replace(country, "france", "Other")) %>% 
  mutate(country = str_replace(country, "A tropical island south of the equator", "Other")) %>% 
  mutate(country = str_replace(country, "england", "UK")) %>% 
  mutate(country = str_replace(country, "uk", "UK")) %>% 
  mutate(country = str_replace(country, "Switzerland", "Other")) %>% 
  mutate(country = str_replace(country, "United Kingdom", "UK")) %>% 
  mutate(country = str_replace(country, "Korea", "Other")) %>% 
  mutate(country = str_replace(country, "51.0", "US")) %>% 
  mutate(country = str_replace(country, "croatia", "Other")) %>% 
  mutate(country = str_replace(country, "Portugal", "Other")) %>% 
  mutate(country = str_replace(country, "England", "Other")) %>% 
  mutate(country = str_replace(country, "US USA!", "US")) %>% 
  mutate(country = str_replace(country, "47.0", "US")) %>% 
  mutate(country = str_replace(country, "Cascadia", "US")) %>% 
  mutate(country = str_replace(country, "españa", "Other")) %>% 
  mutate(country = str_replace(country, "france", "Other")) %>% 
  mutate(country = str_replace(country, "Panama", "Other")) %>% 
  mutate(country = str_replace(country, "United Kindom", "UK")) %>% 
  mutate(country = str_replace(country, "AUStralia", "Other")) %>% 
  mutate(country = str_replace(country, "hungary", "Other")) %>% 
  mutate(country = str_replace(country, "Austria", "Other")) %>% 
  mutate(country = str_replace(country, "New Zealand	", "Other")) %>% 
  mutate(country = str_replace(country, "54.0", "US")) %>% 
  mutate(country = str_replace(country, "Germany", "Other")) %>% 
  mutate(country = str_replace(country, "Mexico", "Other")) %>% 
  mutate(country = str_replace(country, "AUStralia", "Other")) %>% 
  mutate(country = str_replace(country, "44.0", "US")) %>% 
  mutate(country = str_replace(country, "Brasil", "Other")) %>% 
  mutate(country = str_replace(country, "South Korea", "Other")) %>% 
  mutate(country = str_replace(country, "US!!!!!", "Other")) %>% 
  mutate(country = str_replace(country, "Philippines", "Other")) %>% 
  mutate(country = str_replace(country, "EUA", "Other")) %>% 
  mutate(country = str_replace(country, "45.0", "Other")) %>% 
  mutate(country = str_replace(country, "sweden", "Other")) %>% 
  mutate(country = str_replace(country, "The Netherlands", "Other")) %>% 
  mutate(country = str_replace(country, "Finland", "Other")) %>% 
  mutate(country = str_replace(country, "US USA!", "US")) %>% 
  mutate(country = str_replace(country, "belgium", "Other")) %>%
  mutate(country = str_replace(country, "France", "Other")) %>% 
  mutate(country = str_replace(country, "AUStria", "Other")) %>% 
  mutate(country = str_replace(country, "New Zealand", "Other")) %>% 
  mutate(country = str_replace(country, "South Other", "Other"))
pivot_2016 <- final_country_2016 %>% 
  pivot_longer(cols= -c(Timestamp,age, your_gender, country, going_out), names_to = "candy_type",values_to =  "rating") %>% 
  mutate(candy_type = str_replace(candy_type, "\\[", "")) %>% 
  mutate(candy_type = str_replace(candy_type, "\\]", "")) %>% 
  mutate(year = 2016)
pivot_2016 <- pivot_2016 [, -c(1)]

colnames(boing_boing_candy_2017)
final_2017 <- boing_boing_candy_2017[, -c(110:120)]
colnames(final_2017)[4] ="age"
colnames(final_2017)[2] ="going_out"
colnames(final_2017)[3] ="your_gender"
colnames(final_2017)[5] ="country"
final_2017$numbers
options(scipen = 999)
final_country_2017 <- final_2017 %>% 
  mutate(country = str_replace(country, "United States", "US")) %>%
  mutate(country = str_replace(country, "USA", "US")) %>% 
  mutate(country = str_replace(country, "usa", "US")) %>% 
  mutate(country = str_replace(country, "US of America", "US")) %>% 	
  mutate(country = str_replace(country, "uSA", "US")) %>% 
  mutate(country = str_replace(country, "united states", "US")) %>% 
  mutate(country = str_replace(country, "us", "US")) %>% 
  mutate(country = str_replace(country, "US!", "US")) %>% 
  mutate(country = str_replace(country, "U.S.A.", "US")) %>% 
  mutate(country = str_replace(country, "Murica", "US")) %>% 
  mutate(country = str_replace(country, "Usa", "US")) %>% 
  mutate(country = str_replace(country, "U.S.", "US")) %>% 
  mutate(country = str_replace(country, "Units States", "US")) %>% 
  mutate(country = str_replace(country, "Us", "US")) %>% 
  mutate(country = str_replace(country, "United states", "US")) %>% 
  mutate(country = str_replace(country, "US USA USA", "US")) %>% 
  mutate(country = str_replace(country, "the best one - US", "US")) %>% 
  mutate(country = str_replace(country, "US USA! USA!	", "US")) %>% 
  mutate(country = str_replace(country, "u.s.", "US")) %>% 
  mutate(country = str_replace(country, "there isn't one for old men", "US")) %>% 
  mutate(country = str_replace(country, "u.s.", "US")) %>% 
  mutate(country = str_replace(country, "The Yoo Ess of Aaayyyyyy", "US")) %>% 
  mutate(country = str_replace(country, "US of america", "US")) %>% 
  mutate(country = str_replace(country, "u.s.", "US")) %>% 
  mutate(country = str_replace(country, "u.s.", "US")) %>% 
  mutate(country = str_replace(country, "United states", "US")) %>% 
  mutate(country = str_replace(country, "	US!!!!!", "US")) %>% 
  mutate(country = str_replace(country, "US USA!", "US")) %>% 
  mutate(country = str_replace(country, "United Sates", "US")) %>% 
  mutate(country = str_replace(country, "Sub-Canadian North America...'Merica", "US")) %>% 
  mutate(country = str_replace(country, "Trumpistan", "US")) %>% 
  mutate(country = str_replace(country, "America", "US")) %>% 
  mutate(country = str_replace(country, "US USA! USA!", "US")) %>% 
  mutate(country = str_replace(country, "Japan", "Other")) %>% 
  mutate(country = str_replace(country, "canada", "Canada")) %>% 
  mutate(country = str_replace(country, "france", "Other")) %>% 
  mutate(country = str_replace(country, "A tropical island south of the equator", "Other")) %>% 
  mutate(country = str_replace(country, "england", "UK")) %>% 
  mutate(country = str_replace(country, "uk", "UK")) %>% 
  mutate(country = str_replace(country, "Switzerland", "Other")) %>% 
  mutate(country = str_replace(country, "United Kingdom", "UK")) %>% 
  mutate(country = str_replace(country, "Korea", "Other")) %>% 
  mutate(country = str_replace(country, "51.0", "US")) %>% 
  mutate(country = str_replace(country, "croatia", "Other")) %>% 
  mutate(country = str_replace(country, "Portugal", "Other")) %>% 
  mutate(country = str_replace(country, "England", "Other")) %>% 
  mutate(country = str_replace(country, "US USA!", "US")) %>% 
  mutate(country = str_replace(country, "47.0", "US")) %>% 
  mutate(country = str_replace(country, "Cascadia", "US")) %>% 
  mutate(country = str_replace(country, "españa", "Other")) %>% 
  mutate(country = str_replace(country, "france", "Other")) %>% 
  mutate(country = str_replace(country, "Panama", "Other")) %>% 
  mutate(country = str_replace(country, "United Kindom", "UK")) %>% 
  mutate(country = str_replace(country, "AUStralia", "Other")) %>% 
  mutate(country = str_replace(country, "hungary", "Other")) %>% 
  mutate(country = str_replace(country, "Austria", "Other")) %>% 
  mutate(country = str_replace(country, "New Zealand	", "Other")) %>% 
  mutate(country = str_replace(country, "54.0", "US")) %>% 
  mutate(country = str_replace(country, "Germany", "Other")) %>% 
  mutate(country = str_replace(country, "Mexico", "Other")) %>% 
  mutate(country = str_replace(country, "AUStralia", "Other")) %>% 
  mutate(country = str_replace(country, "44.0", "US")) %>% 
  mutate(country = str_replace(country, "Brasil", "Other")) %>% 
  mutate(country = str_replace(country, "South Korea", "Other")) %>% 
  mutate(country = str_replace(country, "US!!!!!", "Other")) %>% 
  mutate(country = str_replace(country, "Philippines", "Other")) %>% 
  mutate(country = str_replace(country, "EUA", "Other")) %>% 
  mutate(country = str_replace(country, "45.0", "Other")) %>% 
  mutate(country = str_replace(country, "sweden", "Other")) %>% 
  mutate(country = str_replace(country, "The Netherlands", "Other")) %>% 
  mutate(country = str_replace(country, "Finland", "Other")) %>% 
  mutate(country = str_replace(country, "US USA!", "US")) %>% 
  mutate(country = str_replace(country, "belgium", "Other")) %>%
  mutate(country = str_replace(country, "France", "Other")) %>% 
  mutate(country = str_replace(country, "AUStria", "Other")) %>% 
  mutate(country = str_replace(country, "New Zealand", "Other")) %>% 
  mutate(country = str_replace(country, "South Other", "Other"))
df2 <- final_country_2017[, c(6:109)]
df1 <- final_country_2017[, c(1:5)]
set_names(substring(colnames(df2), 5))
colnames(df2) <- setNames(substring(colnames(df2), 6), colnames(df2)[7:109])
final_country_2017 <- cbind(df1, df2) 
f2017 <- final_country_2017[, -c(6)]
colnames(final_2017)
pivot_2017 <- f2017 %>% 
  pivot_longer(cols= -c('Internal ID',age, your_gender, country, going_out), names_to = "candy_type",values_to =  "rating") %>% 
  mutate(year = 2017)
pivot_2017 <- pivot_2017 [, -c(1)]

all_data_bind <- rbind.fill(pivot_2015, pivot_2016, pivot_2017)
all_data_bind_age <- all_data_bind %>% 
  mutate(age = round(as.numeric(gsub("[^[:digit:]]:","",age)),0))%>%
  mutate(age = ifelse(age > 100, NA, age)) 



