#-----------------------------------------#
#--- Vorausberechnung der SchülerInnen ---#
#-- in allgemeinbildenen und beruflichen -#
#---- Schulen in NRW auf Schulebene ------#
#-----------------------------------------#

#-------------------#
#### 0. Settings ####
#-------------------#

  #Cleaning Environment
    rm(list =ls())

  #Library
    if(!require(rstudioapi)) install.packages("rstudioapi") # for rstudioapi::getActiveDocumentContext()

    if(!require(dplyr)) install.packages("dplyr") # for %>%
    library("dplyr")
  
  #Set working directroy to source file location
    setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
    
    
#-------------------------------------------------------------------#
#### 1. Web Scraping of legislative process data from ministries ####
#-------------------------------------------------------------------#

  #BMAS <- read.csv2("./datasets/BMAS.csv",
  #                  colClasses = c("character", "character", "Date", "character", "character",
  #                                 "Date", "character", "Date", "character", "Date", 
  #                                 "character", "Date", "character"))
    
  BMAS <- readRDS("./datasets/BMAS.rds")  

  BMF <- readRDS("./datasets/BMF.rds")  
  
  BMG <- readRDS("./datasets/BMG.rds") 
  
  BMI <- readRDS("./datasets/BMI.rds") 

    
    
  #combine datasets from all ministries  
    data <- bind_rows(
              lapply(ls(), 
                     function(x) eval(parse(text = x))
              )
            )
    
    
  #keep only observations from 2019 -> Referentenentwurf
    data.2019 <- data[data$Referentenentwurf.Date>="2019-01-01" & data$Referentenentwurf.Date>="2019-12-31",]
    
  #keep only observations from 2019 -> last.updates
    data.2019 <- data[data$last.updates>="2019-01-01" & data$last.updates>="2019-12-31",]
    
  #keep only data of bills
    data.2019 <- data[data$Vorgangstyp=="Gesetz" & !is.na(data$Vorgangstyp),]
    
  
    
  #Descriptives
    
    #Distribution to the ministries
      table(data$ministry)
      
    #actor per bill
      data.2019 %>%
        group_by(names) %>%
        summarise(n = n()) %>%
        mutate(n = ifelse(n>1, n, 0)) #if there is only one row, there is no actor -> 0
      
    #average actor per bill
      data.2019 %>%
        group_by(names) %>%
        summarise(n = n()) %>%
        mutate(n = ifelse(n>1, n, 0)) %>% #if there is only one row, there is no actor -> 0
        summarise(mean = mean(n))
      
    #average actor per bill by ministry
      data.2019 %>%
        group_by(ministry, names) %>%
        summarise(n = n()) %>%
        mutate(n = ifelse(n>1, n, 0)) %>% #if there is only one row, there is no actor -> 0
        group_by(ministry) %>%
        summarise(mean = mean(n)) 
      
    
    
#-------------------------------------------------------#
#### 2. Comparison of school statistics and our data ####
#-------------------------------------------------------#    
    
  source("2. Comparison of school statistics and our data.r")  
    
