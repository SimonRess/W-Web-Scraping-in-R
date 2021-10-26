#---------------------------------------------------------------------#
#### Assignment of group IDs from "master.csv" to statment-dataset ####
#---------------------------------------------------------------------#

  #Clear environment
    rm(list = ls())

  #Set Working Directory
    setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) # setting working directory to source file location
  
    data <- read.csv2("BMJV-Stellungnahmen-overview.csv")

    data.IDs <- read.csv2("Y:/Datensicherung INTERARENA/Eingabemaske/master.csv")

  #Merge group IDs by full name to the actor
    IDs <- merge(data, data.IDs[,c("V1a","V4")], by.x = "actor", by.y = "V1a")
    
    #There are several observation per actor -> keep only one raw
      IDs <- IDs[!duplicated(IDs$actor),] # 97
  
    #Assign IDs to the statement-dataset
      data <- merge(data, IDs[,c("actor","V4")], by = "actor", all.x = T)
        IDs <- NULL
  
  #Merge law by part of the name
    if(!require("stringr")) install.packages("stringr")
    library(stringr)    
    data$sub <-  str_split_fixed(data$actor,"\\(", n =Inf)[,1] # Delete abbreviations in parentheses
    data$sub <-  str_split_fixed(data$sub," e.V.", n =Inf)[,1] # Delete "e.V."
    data$sub <-  str_split_fixed(data$sub," e. V.", n =Inf)[,1] # Delete "e. V."
  
    #Delete abbreviations before or after the name who are detached by "-"
      data$nchar.a <- nchar(str_split_fixed(data$sub," -", n =Inf)[,1])
      data$nchar.b <- nchar(str_split_fixed(data$sub," -", n =Inf)[,2])
      
      data$sub <- ifelse(data$nchar.a > data$nchar.b, str_split_fixed(data$sub," -", n =Inf)[,1], data$sub)
      data$sub <- ifelse(data$nchar.a <= data$nchar.b, str_split_fixed(data$sub," -", n =Inf)[,2], data$sub)
      
    #Trim whitespace from the names
      library(stringr)
      data$sub <- str_trim(data$sub, "left")
      data$sub <- str_trim(data$sub, "right")

    IDs <- unlist(lapply(data$sub[1:1875], function(x) grep(x, data.IDs$V1a)[1]))
      length(IDs[!is.na(IDs)]) # 1081

    
    data <- cbind(data,IDs=unlist(IDs))
  
    data$V4 <- unlist(lapply(data$IDs, function(x) data.IDs[x,"V4"]))
    data <- data[,1:7]
      length(data$V4[!is.na(data$V4)]) # 1080
    
  #Save dataset  
    write.csv2(data, "BMJV-Stellungnahmen-overview.csv", row.names = FALSE)
