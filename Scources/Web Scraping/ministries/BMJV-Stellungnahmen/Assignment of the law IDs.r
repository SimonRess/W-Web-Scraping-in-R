#-------------------------------------------------------------------#
#### Assignment of law IDs from "gesamt.csv" to statment-dataset ####
#-------------------------------------------------------------------#

  #Clear environment
    rm(list = ls())

  #Set Working Directory
    setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) # setting working directory to source file location
  
  data <- read.csv2("BMJV-Stellungnahmen-overview.csv")

  data.IDs <- read.csv2("Y:/Datensicherung INTERARENA/Eingabemaske/gesamt.csv")
    #Available variables
      # V10b2 Name of the bills (draft laws)
      # V10b Bill’s (draft laws) ID number
      # V10b1 ID number "Beschlussempfehlung und Bericht" of the related Bill
  
  #Merge law IDs by full name to the statments
    IDs <- merge(data, data.IDs[,c("V10b2","V10b")], by.x = "law", by.y = "V10b2")
    
    #There are several observation per law -> keep only one raw
      IDs <- IDs[!duplicated(IDs$law),] # 4 IDs
  
    #Assign IDs to the statement-dataset
      data <- merge(data, IDs[,c("law","V10b")], by = "law", all.x = T)
        IDs <- NULL
  
  #Merge law by part of the name
    if(!require("stringr")) install.packages("stringr")
    library(stringr)
        
    data$sub <- NA
    data$sub <- ifelse(substr(data$law,1,10)=="Gesetz zur", str_split_fixed(data$law,"Gesetz zur ", n =Inf)[,2], data$sub) 
    data$sub <- ifelse(is.na(data$sub), data$law, data$sub)
      length(data$sub[!is.na(data$sub)]) # 1875 -> correct
    
    IDs <- lapply(data$sub, function(x) grep(x, data.IDs$V10b2))
    IDs <- lapply(IDs, function(l) l[1])
      length(IDs[!is.na(IDs)]) # 696
      
    data <- cbind(data,IDs=unlist(IDs))
  
    data$V10b <- unlist(lapply(data$IDs, function(x) data.IDs[x,"V10b"]))
    data <- data[,1:6]
      length(data$V10b[!is.na(data$V10b)]) # 696
    
  #Save dataset  
    write.csv2(data, "BMJV-Stellungnahmen-overview.csv", row.names = FALSE)
