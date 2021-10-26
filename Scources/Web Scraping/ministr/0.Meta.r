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
    #useage of 
    if(!require(rstudioapi)) install.packages("rstudioapi")

    if(!require(dplyr)) install.packages("dplyr") # for %>%
    library("dplyr")
  
    if(!require(plm)) install.packages("plm") # for lag()
    library("plm")

    #"Java Runtime Environment (64 Bit)" needed
    if(!require("xlsx")) install.packages("xlsx")
    library(xlsx)

    if(!require("skimr")) install.packages("skimr") #overview of dataset
    library(skimr)


  #Set working directroy to source file location
    setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
    
    
#-------------------------------------------------------------------#
#### 1. Web Scraping of legislative process data from ministries ####
#-------------------------------------------------------------------#

  source("1.1. Web scraping (BMAS).r")   
  source("1.2. Web scraping (BMF).r") 
  source("1.3. Web scraping (BMG).r") 
  source("1.4. Web scraping (BMI).r") 
  source("1.5. Web scraping (BMJ).r") 
  source("1.6. Web scraping (BMW).r")
  source("1.7. Web scraping (BMFSFJ).r")
  source("1.8. Web scraping (BMBF).r")
  source("1.9. Web scraping (BMEL).r")
  source("1.10. Web scraping (BMVI).r")
    
    
#---------------------------#
#### 2. Data preparation ####
#---------------------------#    
    
  source("2.Data preparation.r")  
    
