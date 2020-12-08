# This script runs the analyses for the impact of marriage equality legislation on employment

# Update the directories 'C:/Users/User/Documents/Data/PUMS/' and 'C:/Users/User/Documents/Data/Obergfell/' as appropriate

# Loading libraries

library(stargazer)
library(lfe)
library(dplyr)
library(progress)
library(fastDummies)
library(sandwich)

# Creating a list of column names to drop to save memory

keep <- c('SERIALNO', 'WKW', 'WKHP', 'WAGP', 'NOC', 'TEN', 'AGEP', 'SCHL', 'PUMA', 'ST', 'RAC1P', 'OIP', 'MAR', 'DIS', 'HUGCL', 'RETP', 'SEX', 'ESR', 'QTRBIR', 'PUBCOV', 'HICOV', 'FOD1P', 'INDP', 'NAICSP', 'HISPEED', 'VALP', 'FINCP')

# Creating references for converting WKW

maxhrs <- c(52,49,47,39,26,14) # Max values
minhrs <- c(50,48,40,27,15,0) # Min values
meanhrs <- (maxhrs + minhrs) / 2 # Mean values

# Reading in the data

file_list <- list.files(path = 'C:/Users/User/Documents/Data/PUMS/', '.csv')
data <- data.frame()

for (i in 1:19) {
  
  # Reading in data sets and merging them
  
  yr <- 1999 + i # Current year
  print(yr) # Visualizing our progress
  print('Reading in household data.......') # Visualizing our progress
  htemp <- read.csv(paste('C:/Users/User/Documents/Data/PUMS/', file_list[i], sep = '')) # Read in annual household data files
  htemp <- htemp[,(names(htemp) %in% keep)] # Dropping some extraneous columns
  print('Reading in personal data.......') # Visualizing our progress
  ptemp <- read.csv(paste('C:/Users/User/Documents/Data/PUMS/', file_list[i+20], sep = '')) # Read in annual personal data file
  ptemp <- ptemp[,(names(ptemp) %in% keep)] # Dropping some extraneous columns
  print('Performing merge on SERIALNO.......') # Visualizing our progress
  temp <- merge(htemp, ptemp, by = c('SERIALNO')) # Merge on SERIALNO
  print('Adding variables to the data.frame.......') # Visualizing our progress
  temp$YEAR <- c(rep(yr,dim(temp)[1])) # Add year to annual data.frame
  
  if (yr < 2008) { # Hours worked over the past year / 12 months
    
    temp$H52max <- temp$WKW * temp$WKHP
    temp$H52min <- temp$WKW * temp$WKHP
    temp$H52mean <- temp$WKW * temp$WKHP
    
  } else{
    
    temp$H52max <- maxhrs[temp$WKW] * temp$WKHP
    temp$H52min <- maxhrs[temp$WKW] * temp$WKHP
    temp$H52mean <- maxhrs[temp$WKW] * temp$WKHP
    
  }
  
  temp$WAGERATEmax <- temp$WAGP / temp$H52max # Imputed wage rate
  temp$WAGERATEmin <- temp$WAGP / temp$H52min # Imputed wage rate
  temp$WAGERATEmean <- temp$WAGP / temp$H52mean # Imputed wage rate
  
  # Removing extraneous rows
  
  temp <- temp[which(temp$AGEP > 17),] # Children
  temp <- temp[which(temp$ESR != 4),] # Not in labor force
  temp <- temp[which(temp$ESR != 5),] # Not in labor force
  temp <- temp[which(temp$ESR != 'NA'),] # Not in labor force
  temp <- temp[complete.cases(temp),] # Remove observation with missing data
  
  # Identifying queer people
  
  print('Identifying queer people.......')
  temp <- temp[order(temp$SERIALNO),] # Order by serial number for ease of identifying queer people
  temp <- temp[which(is.na(temp$SEX) == FALSE),] # Dropping observations with no sex data
  queer <- c()
  pbar <- progress_bar$new(total = dim(temp)[1]) # Progress bar

  for (i in 1:dim(temp)[1]) {
    
    pbar$tick() # Progress bar update
    
    if (i == 1) {
      
      if (temp$SERIALNO[i] == temp$SERIALNO[i+1] && temp$SEX[i] == temp$SEX[i+1]) {
        
        queer <- c(queer,1)
        
      } else {
        
        queer <- c(queer,0)
        
      }
      
    } else if (i == dim(temp)[1]) {
      
      if (temp$SERIALNO[i] == temp$SERIALNO[i-1] && temp$SEX[i] == temp$SEX[i-1]) {
        
        queer <- c(queer,1)
        
      } else {
        
        queer <- c(queer,0)
        
      }
      
    } else {
      
      if (temp$SERIALNO[i] != temp$SERIALNO[i+1] && temp$SERIALNO[i] != temp$SERIALNO[i-1]) { # No shared household
        
        queer <- c(queer,0)
        
      } else if (dim(temp[which(temp$SERIALNO == temp$SERIALNO[i]),])[1] > 2) { # More than 2 adults in same household
        
        queer <- c(queer,0)
        
      } else if (temp$SERIALNO[i] == temp$SERIALNO[i+1] && temp$SEX[i] != temp$SEX[i+1]) { # Straights
        
        queer <- c(queer,0)
        
      } else if (temp$SERIALNO[i] == temp$SERIALNO[i-1] && temp$SEX[i] != temp$SEX[i-1]) {
        
        queer <- c(queer,0)
        
      } else {
        
        queer <- c(queer,1)
        
      }
      
    }
    
  }
  
  temp$QUEER <- queer # Appending queer indicator variable to data.frame
  rm(queer) # Save memory
  
  # Appending annual data to the data.frame
  
  print('Appending annual data to the main data.frame.......')
  temp <- temp[,names(temp) != 'SERIALNO']
  data <- bind_rows(data, temp) # bind annual data to panel
  rm(htemp, ptemp, temp) # Save memory
  print(dim(data)) # Visual check that things are running smoothly
  
}

# Adding some additional computed data to the data.frame

# Identifying gay men and lesbian women (this is as nuanced as the data gets..)

queerxsex <- data$QUEER * data$SEX # 0 == straight; 1 == gay; 2 == lesbian
data$QXSEX <- queerxsex # Add to the data.frame
data <- fastDummies::dummy_cols(data, select_columns = 'QXSEX') # QXSEX_0 == straight; QXSEX_1 == gay; QXSEX_2 == lesbian 
names(data)[names(data) == 'QXSEX_1'] <- 'GAY' # Renaming column
names(data)[names(data) == 'QXSEX_2'] <- 'LESBIAN' # Renaming column
rm(queerxsex) # Saving memory

# Creating the labor force participation and employment variables

# Creating a binary participation variable from the variable ESR

data$Participated <- 1 - floor(data$ESR/6) # floor yields 1 if data value is 6 (not in labor force) and 0 otherwise

# Creating a variable for employed

data$Employed <- as.integer(data$ESR != 3) # ESR == 3 indicates unemployed

# Create a new dependent variable indicator whether the person is part time or full time

data$Full40 <- floor(data$WKHP/40) - floor(data$WKHP/80) # This assigns 1 iff WKHP >= 40
data$Full35 <- floor(data$WKHP/35) - floor(data$WKHP/70) # This assigns 1 iff WKHP >= 35

# Creating a single column for state variables (ST.x == ST.y and ST \cap ST.x == \emptyset; switch occurs at 2154653--2154654)

state <- c(data$ST[1:2154653],data$ST.x[2154654:dim(data)[1]]) # Create state data
data <- data[, !(names(data) %in% c('ST.x', 'ST.y', 'ST'))] # Remove ST, ST.x, ST.y from data.frame
data$ST <- state # Add ST to data.frame
rm(state) # Saving memory

# Same for PUMA (PUMA.x == PUMA.y and are what we want)

names(data)[names(data) == 'PUMA.x'] <- 'PUMA' # Renaming PUMA.x to PUMA
data <- data[, !(names(data) %in% c('PUMA.y'))] # Remove PUMA.y from data.frame

# Adding the treatment effect variable to the data.frame

equality <- c(2015,2014,2015,2014,2015,2013,2015,2014,2008,2013,
              2010,2015,2015,2015,2013,2014,2014,2014,2009,2015,
              2015,2015,2012,2013,2004,2015,2013,2015,2015,2014,
              2015,2014,2010,2013,2013,2011,2014,2015,2015,2014,
              2014,2014,2015,2013,2014,2015,2015,2015,2014,2009,
              2014,2015,2012,2014,2014,2014) # Mapping of states to years when same sex marraige was legalized

# Creating additional data that were included in Hansen et al., 2019

data$HOMEOWNER <- as.integer(data$TEN %in% c(1,2))
data$PARTNERINC <- data$FINCP - data$WAGP
data$HOMEVAL <- data$HOMEOWNER * data$VALP

# Creating the treatment effect variable

treat0 <- as.integer(data$YEAR >= equality[data$ST]) # Creating the data
treat1 <- as.integer(data$YEAR > equality[data$ST]) # Creating the data
data$TREAT0 <- treat0 # Appending TREAT0 to the data.frame
data$TREAT1 <- treat1 # Appending TREAT1 to the data.frame
rm(treat0) # Saving memory
rm(treat1) # Saving memory

# Creating additional treatment effect variables for a multi-time period difference in differences model

multitreat0 <- pmax(0,data$YEAR - equality[data$ST]) # Creating the data
multitreat1 <- pmax(0,data$YEAR - equality[data$ST] - 1) # Creating the data
data$MULTITREAT0 <- multitreat0 # Appending to data.frame
data$MULTITREAT1 <- multitreat1 # Appending to data.frame
rm(multitreat0) # Saving memory
rm(multitreat1) # Saving memory

# Saving the finalized data.frame as a csv for future use

write.csv(data,'C:/Users/User/Documents/Data/Obergefell/data.csv', row.names = FALSE)

# Generating summary statistics

stargazer(data[which(data$YEAR >= 2009),], type = 'text') # Viewing the summary statistics
write.csv(stargazer(data[which(data$YEAR >= 2009),]), 'C:/Users/User/Documents/Data/Obergefell/summary_statistics_all.txt', row.names = FALSE) # Writing them to file

stargazer(data, type = 'text') # Viewing the summary statistics
write.csv(stargazer(data), 'C:/Users/User/Documents/Data/Obergefell/summary_statistics_all_all_years.txt', row.names = FALSE) # Writing them to file

# Running the regressions

# Regressions for 2009-2018

# Hours worked per week

data2 <- data[which(data$YEAR >= 2009),] # data.frame for this time period

m <- lm(WKHP ~ QUEER*TREAT1 + factor(SEX) + AGEP + I(AGEP^2) + factor(ST)
        + PARTNERINC + factor(DIS) + factor(RAC1P) + factor(MAR)
        + factor(SCHL) + NOC + factor(NAICSP) + factor(YEAR), data = data2)

stargazer(m, omit = c('NAICSP', 'MAR', 'SCHL', 'ST', 'YEAR'), type = 'text')
cov <- vcovHC(m, type = 'HC0', cluster = data$PUMA)
rse1 <- sqrt(diag(cov))
stargazer(m, omit = c('NAICSP', 'MAR', 'SCHL', 'ST', 'YEAR'), se = list(rse1), type = 'text')
write.csv(stargazer(m, omit = c('NAICSP', 'MAR', 'SCHL', 'ST', 'YEAR'), se = list(rse1)),
          'C:/Users/User/Documents/Data/Obergefell/hours_worked_all.txt', row.names = FALSE)
rm(m) # Save memory

# Annual hours worked

m1 <- lm(H52mean ~ QUEER*TREAT1 + factor(SEX) + AGEP + I(AGEP^2) + factor(ST)
        + PARTNERINC + factor(DIS) + factor(RAC1P) + factor(MAR)
        + factor(SCHL) + NOC + factor(NAICSP) + factor(YEAR), data = data2)

stargazer(m1, omit = c('NAICSP', 'MAR', 'SCHL', 'ST', 'YEAR'), type = 'text')
cov <- vcovHC(m1, type = 'HC0', cluster = data$PUMA)
rse1 <- sqrt(diag(cov))
stargazer(m1, omit = c('NAICSP', 'MAR', 'SCHL', 'ST', 'YEAR'), se = list(rse1), type = 'text')
write.csv(stargazer(m1, omit = c('NAICSP', 'MAR', 'SCHL', 'ST', 'YEAR'), se = list(rse1)),
          'C:/Users/User/Documents/Data/Obergefell/annual_hours_worked_(mean)_all.txt', row.names = FALSE)
rm(m1) # Save memory

# Wage rate

m2 <- lm(WAGERATEmax ~ QUEER*TREAT1 + factor(SEX) + AGEP + I(AGEP^2) + factor(ST)
        + PARTNERINC + factor(DIS) + factor(RAC1P) + factor(MAR)
        + factor(SCHL) + NOC + factor(NAICSP) + factor(YEAR), data = data2)

stargazer(m2, omit = c('NAICSP', 'MAR', 'SCHL', 'ST', 'YEAR'), type = 'text')
cov <- vcovHC(m2, type = 'HC0', cluster = data$PUMA)
rse2 <- sqrt(diag(cov))
stargazer(m2, omit = c('NAICSP', 'MAR', 'SCHL', 'ST', 'YEAR'), se = list(rse2), type = 'text')
write.csv(stargazer(m2, omit = c('NAICSP', 'MAR', 'SCHL', 'ST', 'YEAR'), se = list(rse2)),
          'C:/Users/User/Documents/Data/Obergefell/wage_rate_all.txt', row.names = FALSE)
rm(m2) # Save memory

# Income

m3 <- lm(WAGP ~ QUEER*TREAT1 + factor(SEX) + AGEP + I(AGEP^2) + factor(ST)
        + PARTNERINC + factor(DIS) + factor(RAC1P) + factor(MAR)
        + factor(SCHL) + NOC + factor(NAICSP) + factor(YEAR), data = data2)

stargazer(m3, omit = c('NAICSP', 'MAR', 'SCHL', 'ST', 'YEAR'), type = 'text')
cov <- vcovHC(m3, type = 'HC0', cluster = data$PUMA)
rse3 <- sqrt(diag(cov))
stargazer(m3, omit = c('NAICSP', 'MAR', 'SCHL', 'ST', 'YEAR'), se = list(rse3), type = 'text')
write.csv(stargazer(m3, omit = c('NAICSP', 'MAR', 'SCHL', 'ST', 'YEAR'), se = list(rse3)),
          'C:/Users/User/Documents/Data/Obergefell/income_all.txt', row.names = FALSE)
rm(m3) # Save memory

# Labor force participation

m4 <- glm(Participated ~ QUEER*TREAT1 + factor(SEX) + AGEP + I(AGEP^2) + factor(ST)
        + PARTNERINC + factor(DIS) + factor(RAC1P) + factor(MAR)
        + factor(SCHL) + NOC + factor(NAICSP) + factor(YEAR), data = data2, family = 'binomial')

stargazer(m4, omit = c('NAICSP', 'MAR', 'SCHL', 'ST', 'YEAR'), type = 'text')
cov <- vcovHC(m4, type = 'HC0', cluster = data$PUMA)
rse4 <- sqrt(diag(cov))
stargazer(m4, omit = c('NAICSP', 'MAR', 'SCHL', 'ST', 'YEAR'), se = list(rse4), type = 'text')
write.csv(stargazer(m4, omit = c('NAICSP', 'MAR', 'SCHL', 'ST', 'YEAR'), se = list(rse4)),
          'C:/Users/User/Documents/Data/Obergefell/participation_all.txt', row.names = FALSE)
rm(m4) # Save memory

# Employed

m5 <- glm(Employed ~ QUEER*TREAT1 + factor(SEX) + AGEP + I(AGEP^2) + factor(ST)
          + PARTNERINC + factor(DIS) + factor(RAC1P) + factor(MAR)
          + factor(SCHL) + NOC + factor(NAICSP) + factor(YEAR), data = data2, family = 'binomial')

stargazer(m5, omit = c('NAICSP', 'MAR', 'SCHL', 'ST', 'YEAR'), type = 'text')
cov <- vcovHC(m5, type = 'HC0', cluster = data$PUMA)
rse5 <- sqrt(diag(cov))
stargazer(m5, omit = c('NAICSP', 'MAR', 'SCHL', 'ST', 'YEAR'), se = list(rse5), type = 'text')
write.csv(stargazer(m5, omit = c('NAICSP', 'MAR', 'SCHL', 'ST', 'YEAR'), se = list(rse5)),
          'C:/Users/User/Documents/Data/Obergefell/employed_all.txt', row.names = FALSE)
rm(m5) # Save memory

# Full time employment (40 hours per week threshold)

m6 <- glm(Full40 ~ QUEER*TREAT1 + factor(SEX) + AGEP + I(AGEP^2) + factor(ST)
          + PARTNERINC + factor(DIS) + factor(RAC1P) + factor(MAR)
          + factor(SCHL) + NOC + factor(NAICSP) + factor(YEAR), data = data2, family = 'binomial')

stargazer(m6, omit = c('NAICSP', 'MAR', 'SCHL', 'ST', 'YEAR'), type = 'text')
cov <- vcovHC(m6, type = 'HC0', cluster = data$PUMA)
rse6 <- sqrt(diag(cov))
stargazer(m6, omit = c('NAICSP', 'MAR', 'SCHL', 'ST', 'YEAR'), se = list(rse6), type = 'text')
write.csv(stargazer(m6, omit = c('NAICSP', 'MAR', 'SCHL', 'ST', 'YEAR'), se = list(rse6)),
          'C:/Users/User/Documents/Data/Obergefell/full_time_40_all.txt', row.names = FALSE)
rm(m6) # Save memory

# Full time employment (35 hours per week threshold)

m7 <- glm(Full35 ~ QUEER*TREAT1 + factor(SEX) + AGEP + I(AGEP^2) + factor(ST)
          + PARTNERINC + factor(DIS) + factor(RAC1P) + factor(MAR)
          + factor(SCHL) + NOC + factor(NAICSP) + factor(YEAR), data = data2, family = 'binomial')

stargazer(m7, omit = c('NAICSP', 'MAR', 'SCHL', 'ST', 'YEAR'), type = 'text')
cov <- vcovHC(m7, type = 'HC0', cluster = data$PUMA)
rse7 <- sqrt(diag(cov))
stargazer(m7, omit = c('NAICSP', 'MAR', 'SCHL', 'ST', 'YEAR'), se = list(rse7), type = 'text')
write.csv(stargazer(m7, omit = c('NAICSP', 'MAR', 'SCHL', 'ST', 'YEAR'), se = list(rse7)),
          'C:/Users/User/Documents/Data/Obergefell/full_time_35_all.txt', row.names = FALSE)
rm(m7) # Save memory

# Repeat by gender for 2009-2018 time period

gata <- data2[which(data2$SEX == 1),] # the data.frame for men
lata <- data2[which(data2$SEX == 2),] # the data.frame for women
rm(data2) # Save memory

# Gay and Lesbian level summary statistics

stargazer(gata, type = 'text') # Viewing the summary statistics
write.csv(stargazer(gata), 'C:/Users/User/Documents/Data/Obergefell/summary_statistics_gay.txt', row.names = FALSE) # Writing them to file

stargazer(lata, type = 'text') # Viewing the summary statistics
write.csv(stargazer(lata), 'C:/Users/User/Documents/Data/Obergefell/summary_statistics_lesbian.txt', row.names = FALSE) # Writing them to file

# Hours worked per year

m <- lm(WKHP ~ GAY*TREAT1 + AGEP + I(AGEP^2) + factor(ST)
        + PARTNERINC + factor(DIS) + factor(RAC1P) + factor(MAR)
        + factor(SCHL) + NOC + factor(NAICSP) + factor(YEAR), data = gata)

stargazer(m, omit = c('NAICSP', 'MAR', 'SCHL', 'ST', 'YEAR'), type = 'text')
cov <- vcovHC(m, type = 'HC0', cluster = data$PUMA)
rse1 <- sqrt(diag(cov))
stargazer(m, omit = c('NAICSP', 'MAR', 'SCHL', 'ST', 'YEAR'), se = list(rse1), type = 'text')
write.csv(stargazer(m, omit = c('NAICSP', 'MAR', 'SCHL', 'ST', 'YEAR'), se = list(rse1)),
          'C:/Users/User/Documents/Data/Obergefell/hours_worked_gay.txt', row.names = FALSE)
rm(m) # Save memory

# Annual hours worked

m1 <- lm(H52mean ~ GAY*TREAT1 + factor(SEX) + AGEP + I(AGEP^2) + factor(ST)
         + PARTNERINC + factor(DIS) + factor(RAC1P) + factor(MAR)
         + factor(SCHL) + NOC + factor(NAICSP) + factor(YEAR), data = gata)

stargazer(m1, omit = c('NAICSP', 'MAR', 'SCHL', 'ST', 'YEAR'), type = 'text')
cov <- vcovHC(m1, type = 'HC0', cluster = data$PUMA)
rse1 <- sqrt(diag(cov))
stargazer(m1, omit = c('NAICSP', 'MAR', 'SCHL', 'ST', 'YEAR'), se = list(rse1), type = 'text')
write.csv(stargazer(m1, omit = c('NAICSP', 'MAR', 'SCHL', 'ST', 'YEAR'), se = list(rse1)),
          'C:/Users/User/Documents/Data/Obergefell/annual_hours_worked_(mean)_gay.txt', row.names = FALSE)
rm(m1) # Save memory

# Wage rate

m2 <- lm(WAGERATEmax ~ GAY*TREAT1 + AGEP + I(AGEP^2) + factor(ST)
         + PARTNERINC + factor(DIS) + factor(RAC1P) + factor(MAR)
         + factor(SCHL) + NOC + factor(NAICSP) + factor(YEAR), data = gata)

stargazer(m2, omit = c('NAICSP', 'MAR', 'SCHL', 'ST', 'YEAR'), type = 'text')
cov <- vcovHC(m2, type = 'HC0', cluster = data$PUMA)
rse2 <- sqrt(diag(cov))
stargazer(m2, omit = c('NAICSP', 'MAR', 'SCHL', 'ST', 'YEAR'), se = list(rse2), type = 'text')
write.csv(stargazer(m2, omit = c('NAICSP', 'MAR', 'SCHL', 'ST', 'YEAR'), se = list(rse2)),
          'C:/Users/User/Documents/Data/Obergefell/wage_rate_gay.txt', row.names = FALSE)
rm(m2) # Save memory

# Income

m3 <- lm(WAGP ~ GAY*TREAT1 + AGEP + I(AGEP^2) + factor(ST)
         + PARTNERINC + factor(DIS) + factor(RAC1P) + factor(MAR)
         + factor(SCHL) + NOC + factor(NAICSP) + factor(YEAR), data = gata)

stargazer(m3, omit = c('NAICSP', 'MAR', 'SCHL', 'ST', 'YEAR'), type = 'text')
cov <- vcovHC(m3, type = 'HC0', cluster = data$PUMA)
rse3 <- sqrt(diag(cov))
stargazer(m3, omit = c('NAICSP', 'MAR', 'SCHL', 'ST', 'YEAR'), se = list(rse3), type = 'text')
write.csv(stargazer(m3, omit = c('NAICSP', 'MAR', 'SCHL', 'ST', 'YEAR'), se = list(rse3)),
          'C:/Users/User/Documents/Data/Obergefell/income_gay.txt', row.names = FALSE)
rm(m3) # Save memory

# Labor force participation

m4 <- glm(Participated ~ GAY*TREAT1 + AGEP + I(AGEP^2) + factor(ST)
          + PARTNERINC + factor(DIS) + factor(RAC1P) + factor(MAR)
          + factor(SCHL) + NOC + factor(NAICSP) + factor(YEAR), data = gata, family = 'binomial')

stargazer(m4, omit = c('NAICSP', 'MAR', 'SCHL', 'ST', 'YEAR'), type = 'text')
cov <- vcovHC(m4, type = 'HC0', cluster = data$PUMA)
rse4 <- sqrt(diag(cov))
stargazer(m4, omit = c('NAICSP', 'MAR', 'SCHL', 'ST', 'YEAR'), se = list(rse4), type = 'text')
write.csv(stargazer(m4, omit = c('NAICSP', 'MAR', 'SCHL', 'ST', 'YEAR'), se = list(rse4)),
          'C:/Users/User/Documents/Data/Obergefell/participation_gay.txt', row.names = FALSE)
rm(m4) # Save memory

# Employed

m5 <- glm(Employed ~ GAY*TREAT1 + AGEP + I(AGEP^2) + factor(ST)
          + PARTNERINC + factor(DIS) + factor(RAC1P) + factor(MAR)
          + factor(SCHL) + NOC + factor(NAICSP) + factor(YEAR), data = gata, family = 'binomial')

stargazer(m5, omit = c('NAICSP', 'MAR', 'SCHL', 'ST', 'YEAR'), type = 'text')
cov <- vcovHC(m5, type = 'HC0', cluster = data$PUMA)
rse5 <- sqrt(diag(cov))
stargazer(m5, omit = c('NAICSP', 'MAR', 'SCHL', 'ST', 'YEAR'), se = list(rse5), type = 'text')
write.csv(stargazer(m5, omit = c('NAICSP', 'MAR', 'SCHL', 'ST', 'YEAR'), se = list(rse5)),
          'C:/Users/User/Documents/Data/Obergefell/employed_gay.txt', row.names = FALSE)
rm(m5) # Save memory

# Full time employment (40 hours per week threshold)

m6 <- glm(Full40 ~ GAY*TREAT1 + AGEP + I(AGEP^2) + factor(ST)
          + PARTNERINC + factor(DIS) + factor(RAC1P) + factor(MAR)
          + factor(SCHL) + NOC + factor(NAICSP) + factor(YEAR), data = gata, family = 'binomial')

stargazer(m6, omit = c('NAICSP', 'MAR', 'SCHL', 'ST', 'YEAR'), type = 'text')
cov <- vcovHC(m6, type = 'HC0', cluster = data$PUMA)
rse6 <- sqrt(diag(cov))
stargazer(m6, omit = c('NAICSP', 'MAR', 'SCHL', 'ST', 'YEAR'), se = list(rse6), type = 'text')
write.csv(stargazer(m6, omit = c('NAICSP', 'MAR', 'SCHL', 'ST', 'YEAR'), se = list(rse6)),
          'C:/Users/User/Documents/Data/Obergefell/full_time_40_gay.txt', row.names = FALSE)
rm(m6) # Save memory

# Full time employment (35 hours per week threshold)

m7 <- glm(Full35 ~ GAY*TREAT1 + AGEP + I(AGEP^2) + factor(ST)
          + PARTNERINC + factor(DIS) + factor(RAC1P) + factor(MAR)
          + factor(SCHL) + NOC + factor(NAICSP) + factor(YEAR), data = gata, family = 'binomial')

stargazer(m7, omit = c('NAICSP', 'MAR', 'SCHL', 'ST', 'YEAR'), type = 'text')
cov <- vcovHC(m7, type = 'HC0', cluster = data$PUMA)
rse7 <- sqrt(diag(cov))
stargazer(m7, omit = c('NAICSP', 'MAR', 'SCHL', 'ST', 'YEAR'), se = list(rse7), type = 'text')
write.csv(stargazer(m7, omit = c('NAICSP', 'MAR', 'SCHL', 'ST', 'YEAR'), se = list(rse7)),
          'C:/Users/User/Documents/Data/Obergefell/full_time_35_gay.txt', row.names = FALSE)
rm(m7) # Save memory

# Hours worked per year

m <- lm(WKHP ~ LESBIAN*TREAT1 + AGEP + I(AGEP^2) + factor(ST)
        + PARTNERINC + factor(DIS) + factor(RAC1P) + factor(MAR)
        + factor(SCHL) + NOC + factor(NAICSP) + factor(YEAR), data = lata)

stargazer(m, omit = c('NAICSP', 'MAR', 'SCHL', 'ST', 'YEAR'), type = 'text')
cov <- vcovHC(m, type = 'HC0', cluster = data$PUMA)
rse1 <- sqrt(diag(cov))
stargazer(m, omit = c('NAICSP', 'MAR', 'SCHL', 'ST', 'YEAR'), se = list(rse1), type = 'text')
write.csv(stargazer(m, omit = c('NAICSP', 'MAR', 'SCHL', 'ST', 'YEAR'), se = list(rse1)),
          'C:/Users/User/Documents/Data/Obergefell/hours_worked_lesbian.txt', row.names = FALSE)
rm(m) # Save memory

# Annual hours worked

m1 <- lm(H52mean ~ LESBIAN*TREAT1 + factor(SEX) + AGEP + I(AGEP^2) + factor(ST)
         + PARTNERINC + factor(DIS) + factor(RAC1P) + factor(MAR)
         + factor(SCHL) + NOC + factor(NAICSP) + factor(YEAR), data = lata)

stargazer(m1, omit = c('NAICSP', 'MAR', 'SCHL', 'ST', 'YEAR'), type = 'text')
cov <- vcovHC(m1, type = 'HC0', cluster = data$PUMA)
rse1 <- sqrt(diag(cov))
stargazer(m1, omit = c('NAICSP', 'MAR', 'SCHL', 'ST', 'YEAR'), se = list(rse1), type = 'text')
write.csv(stargazer(m1, omit = c('NAICSP', 'MAR', 'SCHL', 'ST', 'YEAR'), se = list(rse1)),
          'C:/Users/User/Documents/Data/Obergefell/annual_hours_worked_(mean)_lesbian.txt', row.names = FALSE)
rm(m1) # Save memory

# Wage rate

m2 <- lm(WAGERATEmax ~ LESBIAN*TREAT1 + AGEP + I(AGEP^2) + factor(ST)
         + PARTNERINC + factor(DIS) + factor(RAC1P) + factor(MAR)
         + factor(SCHL) + NOC + factor(NAICSP) + factor(YEAR), data = lata)

stargazer(m2, omit = c('NAICSP', 'MAR', 'SCHL', 'ST', 'YEAR'), type = 'text')
cov <- vcovHC(m2, type = 'HC0', cluster = data$PUMA)
rse2 <- sqrt(diag(cov))
stargazer(m2, omit = c('NAICSP', 'MAR', 'SCHL', 'ST', 'YEAR'), se = list(rse2), type = 'text')
write.csv(stargazer(m2, omit = c('NAICSP', 'MAR', 'SCHL', 'ST', 'YEAR'), se = list(rse2)),
          'C:/Users/User/Documents/Data/Obergefell/wage_rate_lesbian.txt', row.names = FALSE)
rm(m2) # Save memory

# Income

m3 <- lm(WAGP ~ LESBIAN*TREAT1 + AGEP + I(AGEP^2) + factor(ST)
         + PARTNERINC + factor(DIS) + factor(RAC1P) + factor(MAR)
         + factor(SCHL) + NOC + factor(NAICSP) + factor(YEAR), data = lata)

stargazer(m3, omit = c('NAICSP', 'MAR', 'SCHL', 'ST', 'YEAR'), type = 'text')
cov <- vcovHC(m3, type = 'HC0', cluster = data$PUMA)
rse3 <- sqrt(diag(cov))
stargazer(m3, omit = c('NAICSP', 'MAR', 'SCHL', 'ST', 'YEAR'), se = list(rse3), type = 'text')
write.csv(stargazer(m3, omit = c('NAICSP', 'MAR', 'SCHL', 'ST', 'YEAR'), se = list(rse3)),
          'C:/Users/User/Documents/Data/Obergefell/income_lesbian.txt', row.names = FALSE)
rm(m3) # Save memory

# Labor force participation

m4 <- glm(Participated ~ LESBIAN*TREAT1 + AGEP + I(AGEP^2) + factor(ST)
          + PARTNERINC + factor(DIS) + factor(RAC1P) + factor(MAR)
          + factor(SCHL) + NOC + factor(NAICSP) + factor(YEAR), data = lata, family = 'binomial')

stargazer(m4, omit = c('NAICSP', 'MAR', 'SCHL', 'ST', 'YEAR'), type = 'text')
cov <- vcovHC(m4, type = 'HC0', cluster = data$PUMA)
rse4 <- sqrt(diag(cov))
stargazer(m4, omit = c('NAICSP', 'MAR', 'SCHL', 'ST', 'YEAR'), se = list(rse4), type = 'text')
write.csv(stargazer(m4, omit = c('NAICSP', 'MAR', 'SCHL', 'ST', 'YEAR'), se = list(rse4)),
          'C:/Users/User/Documents/Data/Obergefell/participation_lesbian.txt', row.names = FALSE)
rm(m4) # Save memory

# Employed

m5 <- glm(Employed ~ LESBIAN*TREAT1 + AGEP + I(AGEP^2) + factor(ST)
          + PARTNERINC + factor(DIS) + factor(RAC1P) + factor(MAR)
          + factor(SCHL) + NOC + factor(NAICSP) + factor(YEAR), data = lata, family = 'binomial')

stargazer(m5, omit = c('NAICSP', 'MAR', 'SCHL', 'ST', 'YEAR'), type = 'text')
cov <- vcovHC(m5, type = 'HC0', cluster = data$PUMA)
rse5 <- sqrt(diag(cov))
stargazer(m5, omit = c('NAICSP', 'MAR', 'SCHL', 'ST', 'YEAR'), se = list(rse5), type = 'text')
write.csv(stargazer(m5, omit = c('NAICSP', 'MAR', 'SCHL', 'ST', 'YEAR'), se = list(rse5)),
          'C:/Users/User/Documents/Data/Obergefell/employed_lesbian.txt', row.names = FALSE)
rm(m5) # Save memory

# Full time employment (40 hours per week threshold)

m6 <- glm(Full40 ~ LESBIAN*TREAT1 + AGEP + I(AGEP^2) + factor(ST)
          + PARTNERINC + factor(DIS) + factor(RAC1P) + factor(MAR)
          + factor(SCHL) + NOC + factor(NAICSP) + factor(YEAR), data = lata, family = 'binomial')

stargazer(m6, omit = c('NAICSP', 'MAR', 'SCHL', 'ST', 'YEAR'), type = 'text')
cov <- vcovHC(m6, type = 'HC0', cluster = data$PUMA)
rse6 <- sqrt(diag(cov))
stargazer(m6, omit = c('NAICSP', 'MAR', 'SCHL', 'ST', 'YEAR'), se = list(rse6), type = 'text')
write.csv(stargazer(m6, omit = c('NAICSP', 'MAR', 'SCHL', 'ST', 'YEAR'), se = list(rse6)),
          'C:/Users/User/Documents/Data/Obergefell/full_time_40_lesbian.txt', row.names = FALSE)
rm(m6) # Save memory

# Full time employment (35 hours per week threshold)

m7 <- glm(Full35 ~ LESBIAN*TREAT1 + AGEP + I(AGEP^2) + factor(ST)
          + PARTNERINC + factor(DIS) + factor(RAC1P) + factor(MAR)
          + factor(SCHL) + NOC + factor(NAICSP) + factor(YEAR), data = lata, family = 'binomial')

stargazer(m7, omit = c('NAICSP', 'MAR', 'SCHL', 'ST', 'YEAR'), type = 'text')
cov <- vcovHC(m7, type = 'HC0', cluster = data$PUMA)
rse7 <- sqrt(diag(cov))
stargazer(m7, omit = c('NAICSP', 'MAR', 'SCHL', 'ST', 'YEAR'), se = list(rse7), type = 'text')
write.csv(stargazer(m7, omit = c('NAICSP', 'MAR', 'SCHL', 'ST', 'YEAR'), se = list(rse7)),
          'C:/Users/User/Documents/Data/Obergefell/full_time_35_lesbian.txt', row.names = FALSE)
rm(m7) # Save memory







# data <- read.csv('C:/Users/User/Documents/Data/Obergefell/data.csv')





# repeat this with multi-period DID models

# repeat everything over full time period with NA controls removed (can rm data2 here) -- these can be robustness checks

# need to also run H52min and H52mean throughout everything

# should i log wage, hours?



# need a common trends plot for straight men v gay men and straight women v lesbian women

# plot annual means for straight men, gay men, straight women, lesbian women in 4 different colors and add trend lines
# add vertical line at effect year
# do this for individual state(s); for all states in a given year for all such years (relatively few plots - include which states in each description)

# need to make gay and lesbian summary statistics for all years (subset from data)







