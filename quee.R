# This script runs the analyses for the impact of marriage equality legislation on employment

# Update the directories 'C:/Users/Michael/Documents/Data/PUMS/' and 'C:/Users/Michael/Documents/Data/Obergfell/' as appropriate

# Loading libraries

library(stargazer)
library(lfe)
library(dplyr)
library(progress)
library(fastDummies)
library(sandwich)
library(lmtest)
library(ggplot2)

# Creating a list of column names to drop to save memory

keep <- c('SERIALNO', 'WKW', 'WKHP', 'WAGP', 'NOC', 'TEN', 'AGEP', 'SCHL', 'PUMA', 'ST', 'RAC1P', 'OIP', 'MAR', 'DIS', 'HUGCL', 'RETP', 'SEX', 'ESR', 'QTRBIR', 'PUBCOV', 'HICOV', 'FOD1P', 'INDP', 'NAICSP', 'HISPEED', 'VALP', 'FINCP')

# Creating references for converting WKW

maxhrs <- c(52,49,47,39,26,14) # Max values
minhrs <- c(50,48,40,27,15,0) # Min values
meanhrs <- (maxhrs + minhrs) / 2 # Mean values

# Reading in the data

file_list <- list.files(path = 'C:/Users/Michael/Documents/Data/PUMS/', '.csv')
data <- data.frame()

for (i in 1:19) {
  
  # Reading in data sets and merging them
  
  yr <- 1999 + i # Current year
  print(yr) # Visualizing our progress
  print('Reading in household data.......') # Visualizing our progress
  htemp <- read.csv(paste('C:/Users/Michael/Documents/Data/PUMS/', file_list[i], sep = '')) # Read in annual household data files
  htemp <- htemp[,(names(htemp) %in% keep)] # Dropping some extraneous columns
  print('Reading in personal data.......') # Visualizing our progress
  ptemp <- read.csv(paste('C:/Users/Michael/Documents/Data/PUMS/', file_list[i+20], sep = '')) # Read in annual personal data file
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
              2014,2015,2012,2014,2014,2014) # Mapping of states to years when same sex marriage was legalized

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

write.csv(data,'C:/Users/Michael/Documents/Data/Obergefell/data.csv', row.names = FALSE)

# Generating summary statistics

stargazer(data[which(data$YEAR >= 2009),], type = 'text') # Viewing the summary statistics
write.csv(stargazer(data[which(data$YEAR >= 2009),]), 'C:/Users/Michael/Documents/Data/Obergefell/summary_statistics_all.txt', row.names = FALSE) # Writing them to file

stargazer(data, type = 'text') # Viewing the summary statistics
write.csv(stargazer(data), 'C:/Users/Michael/Documents/Data/Obergefell/summary_statistics_all_all_years.txt', row.names = FALSE) # Writing them to file

# Running the regressions

# Regressions for 2009-2018

# Hours worked per week

data2 <- data[which(data$YEAR >= 2009),] # data.frame for this time period

m <- lm(WKHP ~ QUEER*TREAT1 + factor(SEX) + AGEP + I(AGEP^2) + factor(ST)
        + PARTNERINC + factor(DIS) + factor(RAC1P) + factor(MAR)
        + factor(SCHL) + NOC + factor(NAICSP) + factor(YEAR), data = data2)

mx <- coeftest(m, vcov = vcovCL, cluster = ~PUMA)

stargazer(m, mx, omit = c('NAICSP', 'MAR', 'SCHL', 'ST', 'YEAR'), type = 'text')

write.csv(stargazer(m, mx, omit = c('NAICSP', 'MAR', 'SCHL', 'ST', 'YEAR')),
          'C:/Users/Michael/Documents/Data/Obergefell/hours_worked_all.txt', row.names = FALSE)

# Annual hours worked

m1 <- lm(H52mean ~ QUEER*TREAT1 + factor(SEX) + AGEP + I(AGEP^2) + factor(ST)
        + PARTNERINC + factor(DIS) + factor(RAC1P) + factor(MAR)
        + factor(SCHL) + NOC + factor(NAICSP) + factor(YEAR), data = data2)

m1x <- coeftest(m1, vcov = vcovCL, cluster = ~PUMA)

stargazer(m1, m1x, omit = c('NAICSP', 'MAR', 'SCHL', 'ST', 'YEAR'), type = 'text')

write.csv(stargazer(m1, m1x, omit = c('NAICSP', 'MAR', 'SCHL', 'ST', 'YEAR')),
          'C:/Users/Michael/Documents/Data/Obergefell/annual_hours_worked_(mean)_all.txt', row.names = FALSE)

# Wage rate

m2 <- lm(WAGERATEmax ~ QUEER*TREAT1 + factor(SEX) + AGEP + I(AGEP^2) + factor(ST)
        + PARTNERINC + factor(DIS) + factor(RAC1P) + factor(MAR)
        + factor(SCHL) + NOC + factor(NAICSP) + factor(YEAR), data = data2)

m2x <- coeftest(m2, vcov = vcovCL, cluster = ~PUMA)

stargazer(m2, m2x, omit = c('NAICSP', 'MAR', 'SCHL', 'ST', 'YEAR'), type = 'text')

write.csv(stargazer(m2, m2x, omit = c('NAICSP', 'MAR', 'SCHL', 'ST', 'YEAR')),
                   'C:/Users/Michael/Documents/Data/Obergefell/wage_rate_all.txt', row.names = FALSE)

# Income

m3 <- lm(WAGP ~ QUEER*TREAT1 + factor(SEX) + AGEP + I(AGEP^2) + factor(ST)
        + PARTNERINC + factor(DIS) + factor(RAC1P) + factor(MAR)
        + factor(SCHL) + NOC + factor(NAICSP) + factor(YEAR), data = data2)

m3x <- coeftest(m3, vcov = vcovCL, cluster = ~PUMA)

stargazer(m3, m3x, omit = c('NAICSP', 'MAR', 'SCHL', 'ST', 'YEAR'), type = 'text')

write.csv(stargazer(m3, m3x, omit = c('NAICSP', 'MAR', 'SCHL', 'ST', 'YEAR')),
          'C:/Users/Michael/Documents/Data/Obergefell/income_all.txt', row.names = FALSE)

# Labor force participation

m4 <- lm(Participated ~ QUEER*TREAT1 + factor(SEX) + AGEP + I(AGEP^2) + factor(ST)
        + PARTNERINC + factor(DIS) + factor(RAC1P) + factor(MAR)
        + factor(SCHL) + NOC + factor(NAICSP) + factor(YEAR), data = data2)

m4x <- coeftest(m4, vcov = vcovCL, cluster = ~PUMA)

stargazer(m4, m4x, omit = c('NAICSP', 'MAR', 'SCHL', 'ST', 'YEAR'), type = 'text')

write.csv(stargazer(m4, m4x, omit = c('NAICSP', 'MAR', 'SCHL', 'ST', 'YEAR')),
          'C:/Users/Michael/Documents/Data/Obergefell/participation_all.txt', row.names = FALSE)

# Employed

m5 <- lm(Employed ~ QUEER*TREAT1 + factor(SEX) + AGEP + I(AGEP^2) + factor(ST)
          + PARTNERINC + factor(DIS) + factor(RAC1P) + factor(MAR)
          + factor(SCHL) + NOC + factor(NAICSP) + factor(YEAR), data = data2)

m5x <- coeftest(m5, vcov = vcovCL, cluster = ~PUMA)

stargazer(m5, m5x, omit = c('NAICSP', 'MAR', 'SCHL', 'ST', 'YEAR'), type = 'text')

write.csv(stargazer(m5, m5x, omit = c('NAICSP', 'MAR', 'SCHL', 'ST', 'YEAR')),
          'C:/Users/Michael/Documents/Data/Obergefell/employed_all.txt', row.names = FALSE)

# Full time employment (40 hours per week threshold)

m6 <- lm(Full40 ~ QUEER*TREAT1 + factor(SEX) + AGEP + I(AGEP^2) + factor(ST)
          + PARTNERINC + factor(DIS) + factor(RAC1P) + factor(MAR)
          + factor(SCHL) + NOC + factor(NAICSP) + factor(YEAR), data = data2)

m6x <- coeftest(m6, vcov = vcovCL, cluster = ~PUMA)

stargazer(m6, m6x, omit = c('NAICSP', 'MAR', 'SCHL', 'ST', 'YEAR'), type = 'text')

write.csv(stargazer(m6, m6x, omit = c('NAICSP', 'MAR', 'SCHL', 'ST', 'YEAR')),
          'C:/Users/Michael/Documents/Data/Obergefell/full_time_40_all.txt', row.names = FALSE)

# Full time employment (35 hours per week threshold)

m7 <- lm(Full35 ~ QUEER*TREAT1 + factor(SEX) + AGEP + I(AGEP^2) + factor(ST)
          + PARTNERINC + factor(DIS) + factor(RAC1P) + factor(MAR)
          + factor(SCHL) + NOC + factor(NAICSP) + factor(YEAR), data = data2)

m7x <- coeftest(m7, vcov = vcovCL, cluster = ~PUMA)

stargazer(m7, m7x, omit = c('NAICSP', 'MAR', 'SCHL', 'ST', 'YEAR'), type = 'text')

write.csv(stargazer(m7, omit = c('NAICSP', 'MAR', 'SCHL', 'ST', 'YEAR')),
          'C:/Users/Michael/Documents/Data/Obergefell/full_time_35_all.txt', row.names = FALSE)

# Repeat by gender for 2009-2018 time period

gata <- data2[which(data2$SEX == 1),] # the data.frame for men
lata <- data2[which(data2$SEX == 2),] # the data.frame for women

# Gay and Lesbian level summary statistics

stargazer(gata, type = 'text') # Viewing the summary statistics
write.csv(stargazer(gata), 'C:/Users/Michael/Documents/Data/Obergefell/summary_statistics_gay.txt', row.names = FALSE) # Writing them to file

stargazer(lata, type = 'text') # Viewing the summary statistics
write.csv(stargazer(lata), 'C:/Users/Michael/Documents/Data/Obergefell/summary_statistics_lesbian.txt', row.names = FALSE) # Writing them to file

# Hours worked per year

m <- lm(WKHP ~ GAY*TREAT1 + AGEP + I(AGEP^2) + factor(ST)
        + PARTNERINC + factor(DIS) + factor(RAC1P) + factor(MAR)
        + factor(SCHL) + NOC + factor(NAICSP) + factor(YEAR), data = gata)

mx <- coeftest(m, vcov = vcovCL, cluster = ~PUMA)

stargazer(m, mx, omit = c('NAICSP', 'MAR', 'SCHL', 'ST', 'YEAR'), type = 'text')

write.csv(stargazer(m, omit = c('NAICSP', 'MAR', 'SCHL', 'ST', 'YEAR')),
          'C:/Users/Michael/Documents/Data/Obergefell/hours_worked_gay.txt', row.names = FALSE)

# Annual hours worked

m1 <- lm(H52mean ~ GAY*TREAT1 + factor(SEX) + AGEP + I(AGEP^2) + factor(ST)
         + PARTNERINC + factor(DIS) + factor(RAC1P) + factor(MAR)
         + factor(SCHL) + NOC + factor(NAICSP) + factor(YEAR), data = gata)

m1x <- coeftest(m1, vcov = vcovCL, cluster = ~PUMA)

stargazer(m1, m1x, omit = c('NAICSP', 'MAR', 'SCHL', 'ST', 'YEAR'), type = 'text')

write.csv(stargazer(m1, omit = c('NAICSP', 'MAR', 'SCHL', 'ST', 'YEAR')),
          'C:/Users/Michael/Documents/Data/Obergefell/annual_hours_worked_(mean)_gay.txt', row.names = FALSE)

# Wage rate

m2 <- lm(WAGERATEmax ~ GAY*TREAT1 + AGEP + I(AGEP^2) + factor(ST)
         + PARTNERINC + factor(DIS) + factor(RAC1P) + factor(MAR)
         + factor(SCHL) + NOC + factor(NAICSP) + factor(YEAR), data = gata)

m2x <- coeftest(m2, vcov = vcovCL, cluster = ~PUMA)

stargazer(m2, m2x, omit = c('NAICSP', 'MAR', 'SCHL', 'ST', 'YEAR'), type = 'text')

write.csv(stargazer(m2, omit = c('NAICSP', 'MAR', 'SCHL', 'ST', 'YEAR')),
          'C:/Users/Michael/Documents/Data/Obergefell/wage_rate_gay.txt', row.names = FALSE)

# Income

m3 <- lm(WAGP ~ GAY*TREAT1 + AGEP + I(AGEP^2) + factor(ST)
         + PARTNERINC + factor(DIS) + factor(RAC1P) + factor(MAR)
         + factor(SCHL) + NOC + factor(NAICSP) + factor(YEAR), data = gata)

m3x <- coeftest(m3, vcov = vcovCL, cluster = ~PUMA)

stargazer(m3, m3x, omit = c('NAICSP', 'MAR', 'SCHL', 'ST', 'YEAR'), type = 'text')

write.csv(stargazer(m3, omit = c('NAICSP', 'MAR', 'SCHL', 'ST', 'YEAR')),
          'C:/Users/Michael/Documents/Data/Obergefell/income_gay.txt', row.names = FALSE)

# Labor force participation

m4 <- lm(Participated ~ GAY*TREAT1 + AGEP + I(AGEP^2) + factor(ST)
          + PARTNERINC + factor(DIS) + factor(RAC1P) + factor(MAR)
          + factor(SCHL) + NOC + factor(NAICSP) + factor(YEAR), data = gata)

m4x <- coeftest(m4, vcov = vcovCL, cluster = ~PUMA)

stargazer(m4, m4x, omit = c('NAICSP', 'MAR', 'SCHL', 'ST', 'YEAR'), type = 'text')

write.csv(stargazer(m4, m4x, omit = c('NAICSP', 'MAR', 'SCHL', 'ST', 'YEAR')),
          'C:/Users/Michael/Documents/Data/Obergefell/participation_gay.txt', row.names = FALSE)

# Employed

m5 <- lm(Employed ~ GAY*TREAT1 + AGEP + I(AGEP^2) + factor(ST)
          + PARTNERINC + factor(DIS) + factor(RAC1P) + factor(MAR)
          + factor(SCHL) + NOC + factor(NAICSP) + factor(YEAR), data = gata)

m5x <- coeftest(m5, vcov = vcovCL, cluster = ~PUMA)

stargazer(m5, m5x, omit = c('NAICSP', 'MAR', 'SCHL', 'ST', 'YEAR'), type = 'text')

write.csv(stargazer(m5, m5x, omit = c('NAICSP', 'MAR', 'SCHL', 'ST', 'YEAR')),
          'C:/Users/Michael/Documents/Data/Obergefell/employed_gay.txt', row.names = FALSE)

# Full time employment (40 hours per week threshold)

m6 <- lm(Full40 ~ GAY*TREAT1 + AGEP + I(AGEP^2) + factor(ST)
          + PARTNERINC + factor(DIS) + factor(RAC1P) + factor(MAR)
          + factor(SCHL) + NOC + factor(NAICSP) + factor(YEAR), data = gata)

m6x <- coeftest(m6, vcov = vcovCL, cluster = ~PUMA)

stargazer(m6, m6x, omit = c('NAICSP', 'MAR', 'SCHL', 'ST', 'YEAR'), type = 'text')

write.csv(stargazer(m6, m6x, omit = c('NAICSP', 'MAR', 'SCHL', 'ST', 'YEAR')),
          'C:/Users/Michael/Documents/Data/Obergefell/full_time_40_gay.txt', row.names = FALSE)

# Full time employment (35 hours per week threshold)

m7 <- lm(Full35 ~ GAY*TREAT1 + AGEP + I(AGEP^2) + factor(ST)
          + PARTNERINC + factor(DIS) + factor(RAC1P) + factor(MAR)
          + factor(SCHL) + NOC + factor(NAICSP) + factor(YEAR), data = gata)

m7x <- coeftest(m7, vcov = vcovCL, cluster = ~PUMA)

stargazer(m7, m7x, omit = c('NAICSP', 'MAR', 'SCHL', 'ST', 'YEAR'), type = 'text')

write.csv(stargazer(m7, m7x, omit = c('NAICSP', 'MAR', 'SCHL', 'ST', 'YEAR'), se = list(rse7)),
          'C:/Users/Michael/Documents/Data/Obergefell/full_time_35_gay.txt', row.names = FALSE)

# Hours worked per year

m <- lm(WKHP ~ LESBIAN*TREAT1 + AGEP + I(AGEP^2) + factor(ST)
        + PARTNERINC + factor(DIS) + factor(RAC1P) + factor(MAR)
        + factor(SCHL) + NOC + factor(NAICSP) + factor(YEAR), data = lata)

mx <- coeftest(m, vcov = vcovCL, cluster = ~PUMA)

stargazer(m, mx, omit = c('NAICSP', 'MAR', 'SCHL', 'ST', 'YEAR'), type = 'text')

write.csv(stargazer(m, mx, omit = c('NAICSP', 'MAR', 'SCHL', 'ST', 'YEAR')),
          'C:/Users/Michael/Documents/Data/Obergefell/hours_worked_lesbian.txt', row.names = FALSE)

# Annual hours worked

m1 <- lm(H52mean ~ LESBIAN*TREAT1 + factor(SEX) + AGEP + I(AGEP^2) + factor(ST)
         + PARTNERINC + factor(DIS) + factor(RAC1P) + factor(MAR)
         + factor(SCHL) + NOC + factor(NAICSP) + factor(YEAR), data = lata)

m1x <- coeftest(m1, vcov = vcovCL, cluster = ~PUMA)

stargazer(m1, m1x, omit = c('NAICSP', 'MAR', 'SCHL', 'ST', 'YEAR'), type = 'text')

write.csv(stargazer(m1, omit = c('NAICSP', 'MAR', 'SCHL', 'ST', 'YEAR')),
          'C:/Users/Michael/Documents/Data/Obergefell/annual_hours_worked_(mean)_lesbian.txt', row.names = FALSE)

# Wage rate

m2 <- lm(WAGERATEmax ~ LESBIAN*TREAT1 + AGEP + I(AGEP^2) + factor(ST)
         + PARTNERINC + factor(DIS) + factor(RAC1P) + factor(MAR)
         + factor(SCHL) + NOC + factor(NAICSP) + factor(YEAR), data = lata)

m2x <- coeftest(m2, vcov = vcovCL, cluster = ~PUMA)

stargazer(m2, m2x, omit = c('NAICSP', 'MAR', 'SCHL', 'ST', 'YEAR'), type = 'text')

write.csv(stargazer(m2, m2x, omit = c('NAICSP', 'MAR', 'SCHL', 'ST', 'YEAR')),
          'C:/Users/Michael/Documents/Data/Obergefell/wage_rate_lesbian.txt', row.names = FALSE)

# Income

m3 <- lm(WAGP ~ LESBIAN*TREAT1 + AGEP + I(AGEP^2) + factor(ST)
         + PARTNERINC + factor(DIS) + factor(RAC1P) + factor(MAR)
         + factor(SCHL) + NOC + factor(NAICSP) + factor(YEAR), data = lata)

m3x <- coeftest(m3, vcov = vcovCL, cluster = ~PUMA)

stargazer(m3, m3x, omit = c('NAICSP', 'MAR', 'SCHL', 'ST', 'YEAR'), type = 'text')

write.csv(stargazer(m3, m3x, omit = c('NAICSP', 'MAR', 'SCHL', 'ST', 'YEAR')),
          'C:/Users/Michael/Documents/Data/Obergefell/income_lesbian.txt', row.names = FALSE)

# Labor force participation

m4 <- lm(Participated ~ LESBIAN*TREAT1 + AGEP + I(AGEP^2) + factor(ST)
          + PARTNERINC + factor(DIS) + factor(RAC1P) + factor(MAR)
          + factor(SCHL) + NOC + factor(NAICSP) + factor(YEAR), data = lata)

m4x <- coeftest(m4, vcov = vcovCL, cluster = ~PUMA)

stargazer(m4, m4x, omit = c('NAICSP', 'MAR', 'SCHL', 'ST', 'YEAR'), type = 'text')

write.csv(stargazer(m4, m4x, omit = c('NAICSP', 'MAR', 'SCHL', 'ST', 'YEAR')),
          'C:/Users/Michael/Documents/Data/Obergefell/participation_lesbian.txt', row.names = FALSE)

# Employed

m5 <- lm(Employed ~ LESBIAN*TREAT1 + AGEP + I(AGEP^2) + factor(ST)
          + PARTNERINC + factor(DIS) + factor(RAC1P) + factor(MAR)
          + factor(SCHL) + NOC + factor(NAICSP) + factor(YEAR), data = lata)

m5x <- coeftest(m5, vcov = vcovCL, cluster = ~PUMA)

stargazer(m5, m5x, omit = c('NAICSP', 'MAR', 'SCHL', 'ST', 'YEAR'), type = 'text')

write.csv(stargazer(m5, m5x, omit = c('NAICSP', 'MAR', 'SCHL', 'ST', 'YEAR')),
          'C:/Users/Michael/Documents/Data/Obergefell/employed_lesbian.txt', row.names = FALSE)

# Full time employment (40 hours per week threshold)

m6 <- lm(Full40 ~ LESBIAN*TREAT1 + AGEP + I(AGEP^2) + factor(ST)
          + PARTNERINC + factor(DIS) + factor(RAC1P) + factor(MAR)
          + factor(SCHL) + NOC + factor(NAICSP) + factor(YEAR), data = lata)

m6x <- coeftest(m6, vcov = vcovCL, cluster = ~PUMA)

stargazer(m6, m6x, omit = c('NAICSP', 'MAR', 'SCHL', 'ST', 'YEAR'), type = 'text')

write.csv(stargazer(m6, m6x, omit = c('NAICSP', 'MAR', 'SCHL', 'ST', 'YEAR')),
          'C:/Users/Michael/Documents/Data/Obergefell/full_time_40_lesbian.txt', row.names = FALSE)

# Full time employment (35 hours per week threshold)

m7 <- lm(Full35 ~ LESBIAN*TREAT1 + AGEP + I(AGEP^2) + factor(ST)
          + PARTNERINC + factor(DIS) + factor(RAC1P) + factor(MAR)
          + factor(SCHL) + NOC + factor(NAICSP) + factor(YEAR), data = lata)

m7x <- coeftest(m7, vcov = vcovCL, cluster = ~PUMA)

stargazer(m7, m7x, omit = c('NAICSP', 'MAR', 'SCHL', 'ST', 'YEAR'), type = 'text')

write.csv(stargazer(m7, m7x, omit = c('NAICSP', 'MAR', 'SCHL', 'ST', 'YEAR')),
          'C:/Users/Michael/Documents/Data/Obergefell/full_time_35_lesbian.txt', row.names = FALSE)

# Matching method

data2 <- data[which(data$YEAR >= 2009),] # data.frame for this time period

data2$ID <- 1:nrow(data2)
df <- data2 %>% filter(QUEER == 1)
controls <- data2 %>% filter(QUEER == 0)
matches <- c()
misses <- c()

for (i in 1:nrow(df)) {
  
  print(paste('Matching for unit', i, 'of', nrow(df), '.......', sep = ' '))
  
  tmp <- controls %>% filter(SEX == df$SEX[i])
  tmp <- tmp %>% filter(ST == df$ST[i])
  tmp <- tmp %>% filter(PUMA == df$PUMA[i])
  tmp <- tmp %>% filter(NAICSP == df$NAICSP[i])
  tmp <- tmp %>% filter(AGEP == df$AGEP[i])
  
  if (nrow(tmp) > 0) {
    
    matches <- c(matches, tmp$ID)
    
  } else {
    
    misses <- c(misses, i)  
    
  }
  
}

ids <- unique(matches)
keeps <- ifelse(1:nrow(df) %in% misses, 0, 1)
controls <- controls %>% filter(ID %in% ids)
df$Keep <- keeps
df <- df %>% filter(Keep == 1)
df <- df[,1:50]
df <- rbind(df, controls)

# Generating summary statistics for the matched data

stargazer(df, type = 'text') # Viewing the summary statistics
write.csv(stargazer(df), 'C:/Users/Michael/Documents/Data/Obergefell/matched_summary_statistics_all.txt', row.names = FALSE) # Writing them to file

# Running matched regressions

# Hours worked per week

m <- lm(WKHP ~ QUEER*TREAT1 + factor(SEX) + AGEP + I(AGEP^2) + factor(ST)
        + PARTNERINC + factor(DIS) + factor(RAC1P) + factor(MAR)
        + factor(SCHL) + NOC + factor(NAICSP) + factor(YEAR), data = df)

mx <- coeftest(m, vcov = vcovCL, cluster = ~PUMA)

stargazer(m, mx, omit = c('NAICSP', 'MAR', 'SCHL', 'ST', 'YEAR'), type = 'text')

write.csv(stargazer(m, mx, omit = c('NAICSP', 'MAR', 'SCHL', 'ST', 'YEAR')),
          'C:/Users/Michael/Documents/Data/Obergefell/matched_hours_worked_all.txt', row.names = FALSE)

# Annual hours worked

m1 <- lm(H52mean ~ QUEER*TREAT1 + factor(SEX) + AGEP + I(AGEP^2) + factor(ST)
         + PARTNERINC + factor(DIS) + factor(RAC1P) + factor(MAR)
         + factor(SCHL) + NOC + factor(NAICSP) + factor(YEAR), data = df)

m1x <- coeftest(m1, vcov = vcovCL, cluster = ~PUMA)

stargazer(m1, m1x, omit = c('NAICSP', 'MAR', 'SCHL', 'ST', 'YEAR'), type = 'text')

write.csv(stargazer(m1, m1x, omit = c('NAICSP', 'MAR', 'SCHL', 'ST', 'YEAR')),
          'C:/Users/Michael/Documents/Data/Obergefell/matched_annual_hours_worked_(mean)_all.txt', row.names = FALSE)

# Wage rate

m2 <- lm(WAGERATEmax ~ QUEER*TREAT1 + factor(SEX) + AGEP + I(AGEP^2) + factor(ST)
         + PARTNERINC + factor(DIS) + factor(RAC1P) + factor(MAR)
         + factor(SCHL) + NOC + factor(NAICSP) + factor(YEAR), data = df)

m2x <- coeftest(m2, vcov = vcovCL, cluster = ~PUMA)

stargazer(m2, m2x, omit = c('NAICSP', 'MAR', 'SCHL', 'ST', 'YEAR'), type = 'text')

write.csv(stargazer(m2, m2x, omit = c('NAICSP', 'MAR', 'SCHL', 'ST', 'YEAR')),
          'C:/Users/Michael/Documents/Data/Obergefell/matched_wage_rate_all.txt', row.names = FALSE)

# Income

m3 <- lm(WAGP ~ QUEER*TREAT1 + factor(SEX) + AGEP + I(AGEP^2) + factor(ST)
         + PARTNERINC + factor(DIS) + factor(RAC1P) + factor(MAR)
         + factor(SCHL) + NOC + factor(NAICSP) + factor(YEAR), data = df)

m3x <- coeftest(m3, vcov = vcovCL, cluster = ~PUMA)

stargazer(m3, m3x, omit = c('NAICSP', 'MAR', 'SCHL', 'ST', 'YEAR'), type = 'text')

write.csv(stargazer(m3, m3x, omit = c('NAICSP', 'MAR', 'SCHL', 'ST', 'YEAR')),
          'C:/Users/Michael/Documents/Data/Obergefell/matched_income_all.txt', row.names = FALSE)

# Labor force participation

m4 <- lm(Participated ~ QUEER*TREAT1 + factor(SEX) + AGEP + I(AGEP^2) + factor(ST)
         + PARTNERINC + factor(DIS) + factor(RAC1P) + factor(MAR)
         + factor(SCHL) + NOC + factor(NAICSP) + factor(YEAR), data = df)

m4x <- coeftest(m4, vcov = vcovCL, cluster = ~PUMA)

stargazer(m4, m4x, omit = c('NAICSP', 'MAR', 'SCHL', 'ST', 'YEAR'), type = 'text')

write.csv(stargazer(m4, m4x, omit = c('NAICSP', 'MAR', 'SCHL', 'ST', 'YEAR')),
          'C:/Users/Michael/Documents/Data/Obergefell/matched_participation_all.txt', row.names = FALSE)

# Employed

m5 <- lm(Employed ~ QUEER*TREAT1 + factor(SEX) + AGEP + I(AGEP^2) + factor(ST)
         + PARTNERINC + factor(DIS) + factor(RAC1P) + factor(MAR)
         + factor(SCHL) + NOC + factor(NAICSP) + factor(YEAR), data = df)

m5x <- coeftest(m5, vcov = vcovCL, cluster = ~PUMA)

stargazer(m5, m5x, omit = c('NAICSP', 'MAR', 'SCHL', 'ST', 'YEAR'), type = 'text')

write.csv(stargazer(m5, m5x, omit = c('NAICSP', 'MAR', 'SCHL', 'ST', 'YEAR')),
          'C:/Users/Michael/Documents/Data/Obergefell/matched_employed_all.txt', row.names = FALSE)

# Full time employment (40 hours per week threshold)

m6 <- lm(Full40 ~ QUEER*TREAT1 + factor(SEX) + AGEP + I(AGEP^2) + factor(ST)
         + PARTNERINC + factor(DIS) + factor(RAC1P) + factor(MAR)
         + factor(SCHL) + NOC + factor(NAICSP) + factor(YEAR), data = df)

m6x <- coeftest(m6, vcov = vcovCL, cluster = ~PUMA)

stargazer(m6, m6x, omit = c('NAICSP', 'MAR', 'SCHL', 'ST', 'YEAR'), type = 'text')

write.csv(stargazer(m6, m6x, omit = c('NAICSP', 'MAR', 'SCHL', 'ST', 'YEAR')),
          'C:/Users/Michael/Documents/Data/Obergefell/matched_full_time_40_all.txt', row.names = FALSE)

# Full time employment (35 hours per week threshold)

m7 <- lm(Full35 ~ QUEER*TREAT1 + factor(SEX) + AGEP + I(AGEP^2) + factor(ST)
         + PARTNERINC + factor(DIS) + factor(RAC1P) + factor(MAR)
         + factor(SCHL) + NOC + factor(NAICSP) + factor(YEAR), data = df)

m7x <- coeftest(m7, vcov = vcovCL, cluster = ~PUMA)

stargazer(m7, m7x, omit = c('NAICSP', 'MAR', 'SCHL', 'ST', 'YEAR'), type = 'text')

write.csv(stargazer(m7, omit = c('NAICSP', 'MAR', 'SCHL', 'ST', 'YEAR')),
          'C:/Users/Michael/Documents/Data/Obergefell/matched_full_time_35_all.txt', row.names = FALSE)

# Repeat by gender for 2009-2018 time period

dfg <- df[which(df$SEX == 1),] # the data.frame for men
dfl <- df[which(df$SEX == 2),] # the data.frame for women

# Gay and Lesbian level summary statistics

stargazer(dfg, type = 'text') # Viewing the summary statistics
write.csv(stargazer(dfg), 'C:/Users/Michael/Documents/Data/Obergefell/matched_summary_statistics_gay.txt', row.names = FALSE) # Writing them to file

stargazer(dfl, type = 'text') # Viewing the summary statistics
write.csv(stargazer(dfl), 'C:/Users/Michael/Documents/Data/Obergefell/matched_summary_statistics_lesbian.txt', row.names = FALSE) # Writing them to file

# Hours worked per year

m <- lm(WKHP ~ GAY*TREAT1 + AGEP + I(AGEP^2) + factor(ST)
        + PARTNERINC + factor(DIS) + factor(RAC1P) + factor(MAR)
        + factor(SCHL) + NOC + factor(NAICSP) + factor(YEAR), data = dfg)

mx <- coeftest(m, vcov = vcovCL, cluster = ~PUMA)

stargazer(m, mx, omit = c('NAICSP', 'MAR', 'SCHL', 'ST', 'YEAR'), type = 'text')

write.csv(stargazer(m, omit = c('NAICSP', 'MAR', 'SCHL', 'ST', 'YEAR')),
          'C:/Users/Michael/Documents/Data/Obergefell/matched_hours_worked_gay.txt', row.names = FALSE)

# Annual hours worked

m1 <- lm(H52mean ~ GAY*TREAT1 + factor(SEX) + AGEP + I(AGEP^2) + factor(ST)
         + PARTNERINC + factor(DIS) + factor(RAC1P) + factor(MAR)
         + factor(SCHL) + NOC + factor(NAICSP) + factor(YEAR), data = dfg)

m1x <- coeftest(m1, vcov = vcovCL, cluster = ~PUMA)

stargazer(m1, m1x, omit = c('NAICSP', 'MAR', 'SCHL', 'ST', 'YEAR'), type = 'text')

write.csv(stargazer(m1, omit = c('NAICSP', 'MAR', 'SCHL', 'ST', 'YEAR')),
          'C:/Users/Michael/Documents/Data/Obergefell/matched_annual_hours_worked_(mean)_gay.txt', row.names = FALSE)

# Wage rate

m2 <- lm(WAGERATEmax ~ GAY*TREAT1 + AGEP + I(AGEP^2) + factor(ST)
         + PARTNERINC + factor(DIS) + factor(RAC1P) + factor(MAR)
         + factor(SCHL) + NOC + factor(NAICSP) + factor(YEAR), data = dfg)

m2x <- coeftest(m2, vcov = vcovCL, cluster = ~PUMA)

stargazer(m2, m2x, omit = c('NAICSP', 'MAR', 'SCHL', 'ST', 'YEAR'), type = 'text')

write.csv(stargazer(m2, omit = c('NAICSP', 'MAR', 'SCHL', 'ST', 'YEAR')),
          'C:/Users/Michael/Documents/Data/Obergefell/matched_wage_rate_gay.txt', row.names = FALSE)

# Income

m3 <- lm(WAGP ~ GAY*TREAT1 + AGEP + I(AGEP^2) + factor(ST)
         + PARTNERINC + factor(DIS) + factor(RAC1P) + factor(MAR)
         + factor(SCHL) + NOC + factor(NAICSP) + factor(YEAR), data = dfg)

m3x <- coeftest(m3, vcov = vcovCL, cluster = ~PUMA)

stargazer(m3, m3x, omit = c('NAICSP', 'MAR', 'SCHL', 'ST', 'YEAR'), type = 'text')

write.csv(stargazer(m3, omit = c('NAICSP', 'MAR', 'SCHL', 'ST', 'YEAR')),
          'C:/Users/Michael/Documents/Data/Obergefell/matched_income_gay.txt', row.names = FALSE)

# Labor force participation

m4 <- lm(Participated ~ GAY*TREAT1 + AGEP + I(AGEP^2) + factor(ST)
         + PARTNERINC + factor(DIS) + factor(RAC1P) + factor(MAR)
         + factor(SCHL) + NOC + factor(NAICSP) + factor(YEAR), data = dfg)

m4x <- coeftest(m4, vcov = vcovCL, cluster = ~PUMA)

stargazer(m4, m4x, omit = c('NAICSP', 'MAR', 'SCHL', 'ST', 'YEAR'), type = 'text')

write.csv(stargazer(m4, m4x, omit = c('NAICSP', 'MAR', 'SCHL', 'ST', 'YEAR')),
          'C:/Users/Michael/Documents/Data/Obergefell/matched_participation_gay.txt', row.names = FALSE)

# Employed

m5 <- lm(Employed ~ GAY*TREAT1 + AGEP + I(AGEP^2) + factor(ST)
         + PARTNERINC + factor(DIS) + factor(RAC1P) + factor(MAR)
         + factor(SCHL) + NOC + factor(NAICSP) + factor(YEAR), data = dfg)

m5x <- coeftest(m5, vcov = vcovCL, cluster = ~PUMA)

stargazer(m5, m5x, omit = c('NAICSP', 'MAR', 'SCHL', 'ST', 'YEAR'), type = 'text')

write.csv(stargazer(m5, m5x, omit = c('NAICSP', 'MAR', 'SCHL', 'ST', 'YEAR')),
          'C:/Users/Michael/Documents/Data/Obergefell/matched_employed_gay.txt', row.names = FALSE)

# Full time employment (40 hours per week threshold)

m6 <- lm(Full40 ~ GAY*TREAT1 + AGEP + I(AGEP^2) + factor(ST)
         + PARTNERINC + factor(DIS) + factor(RAC1P) + factor(MAR)
         + factor(SCHL) + NOC + factor(NAICSP) + factor(YEAR), data = dfg)

m6x <- coeftest(m6, vcov = vcovCL, cluster = ~PUMA)

stargazer(m6, m6x, omit = c('NAICSP', 'MAR', 'SCHL', 'ST', 'YEAR'), type = 'text')

write.csv(stargazer(m6, m6x, omit = c('NAICSP', 'MAR', 'SCHL', 'ST', 'YEAR')),
          'C:/Users/Michael/Documents/Data/Obergefell/matched_full_time_40_gay.txt', row.names = FALSE)

# Full time employment (35 hours per week threshold)

m7 <- lm(Full35 ~ GAY*TREAT1 + AGEP + I(AGEP^2) + factor(ST)
         + PARTNERINC + factor(DIS) + factor(RAC1P) + factor(MAR)
         + factor(SCHL) + NOC + factor(NAICSP) + factor(YEAR), data = dfg)

m7x <- coeftest(m7, vcov = vcovCL, cluster = ~PUMA)

stargazer(m7, m7x, omit = c('NAICSP', 'MAR', 'SCHL', 'ST', 'YEAR'), type = 'text')

write.csv(stargazer(m7, m7x, omit = c('NAICSP', 'MAR', 'SCHL', 'ST', 'YEAR')),
          'C:/Users/Michael/Documents/Data/Obergefell/matched_full_time_35_gay.txt', row.names = FALSE)

# Hours worked per year

m <- lm(WKHP ~ LESBIAN*TREAT1 + AGEP + I(AGEP^2) + factor(ST)
        + PARTNERINC + factor(DIS) + factor(RAC1P) + factor(MAR)
        + factor(SCHL) + NOC + factor(NAICSP) + factor(YEAR), data = dfl)

mx <- coeftest(m, vcov = vcovCL, cluster = ~PUMA)

stargazer(m, mx, omit = c('NAICSP', 'MAR', 'SCHL', 'ST', 'YEAR'), type = 'text')

write.csv(stargazer(m, mx, omit = c('NAICSP', 'MAR', 'SCHL', 'ST', 'YEAR')),
          'C:/Users/Michael/Documents/Data/Obergefell/matched_hours_worked_lesbian.txt', row.names = FALSE)

# Annual hours worked

m1 <- lm(H52mean ~ LESBIAN*TREAT1 + factor(SEX) + AGEP + I(AGEP^2) + factor(ST)
         + PARTNERINC + factor(DIS) + factor(RAC1P) + factor(MAR)
         + factor(SCHL) + NOC + factor(NAICSP) + factor(YEAR), data = dfl)

m1x <- coeftest(m1, vcov = vcovCL, cluster = ~PUMA)

stargazer(m1, m1x, omit = c('NAICSP', 'MAR', 'SCHL', 'ST', 'YEAR'), type = 'text')

write.csv(stargazer(m1, omit = c('NAICSP', 'MAR', 'SCHL', 'ST', 'YEAR')),
          'C:/Users/Michael/Documents/Data/Obergefell/matched_annual_hours_worked_(mean)_lesbian.txt', row.names = FALSE)

# Wage rate

m2 <- lm(WAGERATEmax ~ LESBIAN*TREAT1 + AGEP + I(AGEP^2) + factor(ST)
         + PARTNERINC + factor(DIS) + factor(RAC1P) + factor(MAR)
         + factor(SCHL) + NOC + factor(NAICSP) + factor(YEAR), data = dfl)

m2x <- coeftest(m2, vcov = vcovCL, cluster = ~PUMA)

stargazer(m2, m2x, omit = c('NAICSP', 'MAR', 'SCHL', 'ST', 'YEAR'), type = 'text')

write.csv(stargazer(m2, m2x, omit = c('NAICSP', 'MAR', 'SCHL', 'ST', 'YEAR')),
          'C:/Users/Michael/Documents/Data/Obergefell/matched_wage_rate_lesbian.txt', row.names = FALSE)

# Income

m3 <- lm(WAGP ~ LESBIAN*TREAT1 + AGEP + I(AGEP^2) + factor(ST)
         + PARTNERINC + factor(DIS) + factor(RAC1P) + factor(MAR)
         + factor(SCHL) + NOC + factor(NAICSP) + factor(YEAR), data = dfl)

m3x <- coeftest(m3, vcov = vcovCL, cluster = ~PUMA)

stargazer(m3, m3x, omit = c('NAICSP', 'MAR', 'SCHL', 'ST', 'YEAR'), type = 'text')

write.csv(stargazer(m3, m3x, omit = c('NAICSP', 'MAR', 'SCHL', 'ST', 'YEAR')),
          'C:/Users/Michael/Documents/Data/Obergefell/matched_income_lesbian.txt', row.names = FALSE)

# Labor force participation

m4 <- lm(Participated ~ LESBIAN*TREAT1 + AGEP + I(AGEP^2) + factor(ST)
         + PARTNERINC + factor(DIS) + factor(RAC1P) + factor(MAR)
         + factor(SCHL) + NOC + factor(NAICSP) + factor(YEAR), data = dfl)

m4x <- coeftest(m4, vcov = vcovCL, cluster = ~PUMA)

stargazer(m4, m4x, omit = c('NAICSP', 'MAR', 'SCHL', 'ST', 'YEAR'), type = 'text')

write.csv(stargazer(m4, m4x, omit = c('NAICSP', 'MAR', 'SCHL', 'ST', 'YEAR')),
          'C:/Users/Michael/Documents/Data/Obergefell/matched_participation_lesbian.txt', row.names = FALSE)

# Employed

m5 <- lm(Employed ~ LESBIAN*TREAT1 + AGEP + I(AGEP^2) + factor(ST)
         + PARTNERINC + factor(DIS) + factor(RAC1P) + factor(MAR)
         + factor(SCHL) + NOC + factor(NAICSP) + factor(YEAR), data = dfl)

m5x <- coeftest(m5, vcov = vcovCL, cluster = ~PUMA)

stargazer(m5, m5x, omit = c('NAICSP', 'MAR', 'SCHL', 'ST', 'YEAR'), type = 'text')

write.csv(stargazer(m5, m5x, omit = c('NAICSP', 'MAR', 'SCHL', 'ST', 'YEAR')),
          'C:/Users/Michael/Documents/Data/Obergefell/matched_employed_lesbian.txt', row.names = FALSE)

# Full time employment (40 hours per week threshold)

m6 <- lm(Full40 ~ LESBIAN*TREAT1 + AGEP + I(AGEP^2) + factor(ST)
         + PARTNERINC + factor(DIS) + factor(RAC1P) + factor(MAR)
         + factor(SCHL) + NOC + factor(NAICSP) + factor(YEAR), data = dfl)

m6x <- coeftest(m6, vcov = vcovCL, cluster = ~PUMA)

stargazer(m6, m6x, omit = c('NAICSP', 'MAR', 'SCHL', 'ST', 'YEAR'), type = 'text')

write.csv(stargazer(m6, m6x, omit = c('NAICSP', 'MAR', 'SCHL', 'ST', 'YEAR')),
          'C:/Users/Michael/Documents/Data/Obergefell/matched_full_time_40_lesbian.txt', row.names = FALSE)

# Full time employment (35 hours per week threshold)

m7 <- lm(Full35 ~ LESBIAN*TREAT1 + AGEP + I(AGEP^2) + factor(ST)
         + PARTNERINC + factor(DIS) + factor(RAC1P) + factor(MAR)
         + factor(SCHL) + NOC + factor(NAICSP) + factor(YEAR), data = dfl)

m7x <- coeftest(m7, vcov = vcovCL, cluster = ~PUMA)

stargazer(m7, m7x, omit = c('NAICSP', 'MAR', 'SCHL', 'ST', 'YEAR'), type = 'text')

write.csv(stargazer(m7, m7x, omit = c('NAICSP', 'MAR', 'SCHL', 'ST', 'YEAR')),
          'C:/Users/Michael/Documents/Data/Obergefell/matched_full_time_35_lesbian.txt', row.names = FALSE)

