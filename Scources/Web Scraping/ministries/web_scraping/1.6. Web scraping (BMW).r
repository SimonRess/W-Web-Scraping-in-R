#-------------------------------------------#
#--- Web Scraping public hearings in BMF ---#
#-------------------------------------------#


  ##Web-Scraping Requirements in "robots.txt"
  
    #Link: https://www.bundesfinanzministerium.de/robots.txt
  
      #-> NICHT VERFÜGBAR (Stand 18.05.2020)
  
  
  ### Terms of Use 
  
    #Link: https://www.bundesfinanzministerium.de/Web/DE/Meta/Impressum/impressum.html
      #Die Webseiten des BMF sind urheberrechtlich geschützt
      #Auf den Webseiten des BMF zur Verfügung gestellte Texte, Textteile, Grafiken, Tabellen oder Bildmaterialien 
      #dürfen ohne vorherige Zustimmung des BMF nicht vervielfältigt, nicht verbreitet und nicht ausgestellt  werden.
  
    #Link: https://www.bundesfinanzministerium.de/Web/DE/Meta/Benutzerhinweise/benutzerhinweise.html;jsessionid=F8EE5747C067CD877EFE6420E620D45F.delivery2-replication


#---------------------------------#
#### 0.1 install/load packages ####
#---------------------------------#

  if(!require("RSelenium")) install.packages("RSelenium")
    library(RSelenium) # R implementation of <Selenium Webdriver API> (Selenium is a project focused on automating web browsers) 

  if(!require("rvest")) install.packages("rvest")
    library(rvest) #for html_attr & read_html
  
  if(!require("dplyr")) install.packages("dplyr")
    library(dplyr) # for %>%

  if(!require("stringr")) install.packages("stringr")
    library(stringr) # for str_detect()

#----------------------------#
#### 0.2 General Settings ####
#----------------------------#

#Page 1: https://www.bundesfinanzministerium.de/SiteGlobals/Forms/Listen/DE/Gesetze/gesetze_und_verordnungen_Formular.html?resourceId=158cd251-d0ec-484c-8bd0-26110afe2fa6&input_=3179ab92-c13c-420d-89f8-6c92a8f9148e&pageLocale=de&templateQueryString=&dateOfIssueAfter=01.01.2019&dateOfIssueBefore=31.12.2019&baseDocType=Law&folder=%2Fbmf%2FContent%2FDE%2FGesetzestexte%2FGesetze_Gesetzesvorhaben%2FAbteilungen%2F*&showFacetCategoryYear=true&showFacetCategoryThemen=true&submit=Senden#Suchformular
#Page 2: https://www.bundesfinanzministerium.de/SiteGlobals/Forms/Listen/DE/Gesetze/gesetze_und_verordnungen_Formular.html;jsessionid=F79DB0B23CDD887185C305BBB778D52C.delivery2-replication?input_=3179ab92-c13c-420d-89f8-6c92a8f9148e&gtp=%252626993af1-36c3-4cc6-a4c3-598eb36eff24_list%253D2&submit=Senden&resourceId=158cd251-d0ec-484c-8bd0-26110afe2fa6&dateOfIssueBefore=31.12.2019&showFacetCategoryYear=true&dateOfIssueAfter=01.01.2019&folder=%2Fbmf%2FContent%2FDE%2FGesetzestexte%2FGesetze_Gesetzesvorhaben%2FAbteilungen%2F*&baseDocType=Law&showFacetCategoryThemen=true&pageLocale=de

  #Close browser
    if(exists("rD")) rD$server$stop() # Clean all ports
  
  #Clean working space
    rm(list=ls())
  
  #Set Working Directory
    setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) # setting working directory to source file location
  
  
  #showConnections(all=TRUE)
  #closeAllConnections()
  #on.exit(close(url))
  
  #start browser
    rD <- rsDriver(port = 4568L, browser = "firefox") # "chrome" / "firefox" is possible
    remDr <- rD[["client"]]
    
    #timeout error after 10 seconds, when page is not loaded
      remDr$setTimeout(type = "page load", milliseconds = 10000) 
    
  
  #Save start time  
    start  <- Sys.time()
    
    
#-----------------------------#  
#### 0.3 Define Functions ##### 
#-----------------------------# 
    
  #extract macro information (Name/Link/Date) about legislative proposals
    extractLinksNamesDates <- function() {
      
      #Save page   
        page <- remDr$getPageSource() #Save page to R dataset 
        page <- page %>% unlist() %>% read_html() # save web page as searchable element
          
      
      #extract number of pages -> No total number of pages displayed
        #nr.pages <- html_nodes(page, xpath = "/html/body/div[1]/div/div/div[3]/div/div/div[2]/div/div/div[2]/div[2]/ul[1]/li[3]/ul/li[4]/a/text()") %>%
        #  html_text() %>%
        #  as.numeric()
        
      #create empty an dataset that can be filled
        data <- data.frame(matrix(ncol = 3, nrow = 0))
        colnames(data) <- c("links", "names", "last.updates")

      #extract infos of all pages
        while(!is.na(html_node(page, xpath = "/html/body/main/div[5]/ul/li[a[contains(@aria-label, 'Next')]]/a"))) {
          
          #list of links
            links <- html_nodes(page, css = "#main > div.container.container-search-results > div > ul > li > div > div > a") %>%
              html_attr(.,"href") %>%
              paste0("https://www.bmwi.de/",.)

          #list of names
            names <- html_nodes(page, css = "#main > div.container.container-search-results > div > ul > li > div > div > p.card-title > strong") %>%
              html_text() %>%
              trimws(which = c("both"))
                #gsub("[^[:alnum:][:blank:]?&.,()/\\-]", "",.) #Removing special characters from text except of "?", "&", ".", ",", "(", ")", "/" and "-"  //https://rpubs.com/Mentors_Ubiqum/Clean_Text
          
          #list of last-update-time
            last.updates <- html_nodes(page, css = "#main > div.container.container-search-results > div > ul > li > div > div > p.card-topline") %>%
              lapply(., function(x) 
                html_node(x, css = "span.date") %>%
                html_text(.) %>%
                {ifelse(identical(., character(0)), NA, .)} 
              ) %>% 
              unlist() %>%
              as.Date(., format='%d.%m.%Y') #transform into international date format

            
          #append dataset
            data <- data.frame(links = append(as.character(data$links), links),
                               names = append(as.character(data$names), names),
                               last.updates = append(data$last.updates, last.updates),
                               stringsAsFactors = FALSE)
          
          if(!is.na(html_node(page, xpath = "/html/body/main/div[5]/ul/li[a[contains(@aria-label, 'Next')]]/a"))) {
            #Click next-page-button up to the penultimate page 
              next.page <- remDr$findElement(using = 'xpath', 
                                          "/html/body/main/div[5]/ul/li[a[contains(@aria-label, 'Next')]]/a") # Information about the "Suchen" button
                                           
              next.page$clickElement() # Click the "Suchen" Button
          }    

          #clear working space
            rm(page, links, names, last.updates)
            
            
          #Wait until page is loaded 
            Sys.sleep(rnorm(1, 2, 0.4))
          #Save page to R dataset     
            page <- remDr$getPageSource() #Save page to R dataset 
            page <- page %>% unlist() %>% read_html() # save web page as searchable element
            
          
        }
        
        #assign data tp global environment
          assign("data", data, envir = .GlobalEnv)
        
    }    
    
    
    
    
  
    
  
#----------------------------------#  
#### 1. Enter search criteria  ##### 
#----------------------------------#  

  remDr$navigate("https://www.bmwi.de/Navigation/DE/Service/Gesetze-und-Verordnungen/gesetze-und-verordnungen.html") # completed legislative processes
    #remDr$navigate("https://www.bmwi.de/Navigation/DE/Service/Stellungnahmen/Aktuelle-Gesetzgebungsverfahren/aktuelle-gesetzgebungsverfahren.html") #current legislative processes
    
    
  #Wait until page is loaded 
    Sys.sleep(2)
    
  #No settings needed
        
#---------------------------------------------#  
#### 2. Extract meta data & build dataset ##### 
#---------------------------------------------#    
  #Extract Links to, names of, and last updates of lawas & legislative proposals
        
    extractLinksNamesDates()
      
  #Determine acticity type  
    data$Vorgangstyp <- NA
      data$Vorgangstyp <- ifelse(str_detect(data$names,"erordnung") == TRUE, "Verordnung", data$Vorgangstyp)
      data$Vorgangstyp <- ifelse(str_detect(data$names, "esetz") == TRUE, "Gesetz", data$Vorgangstyp) #delete G, because of capital leter
    
  #some legislative processes are multiple times in the database  -> keep each process only once
    data <- data %>% 
      group_by(names, last.updates) %>% 
      summarise(links = links[1], #sometimes there are several links -> keep first links, its newer and updated
                Vorgangstyp = Vorgangstyp[1] 
                ) %>%
      ungroup()
    
#-------------------------------------------------------#  
#### 3. Add rows for each actor & actor-name column ##### 
#-------------------------------------------------------#         
        
      
  #create empty an dataset that can be filled
    data.actors <- data.frame(matrix(ncol = 13, nrow = 0))
    colnames(data.actors) <- c("links", "names", "last.updates", "Vorgangstyp", 
                               "Referentenentwurf.Link", "Referentenentwurf.Date", "Regierungsentwurf.Link", "Regierungsentwurf.Date", "Gesetz.Link", "Gesetz.Date",
                               "actors", "statement.date", "statement.links")
    
  #Add information for all legislative processes
    for(i in 1:nrow(data)) {
    
    #Open page of legislative process  
      remDr$navigate(data[[i, "links"]])
          
    #Wait until page is loaded 
      Sys.sleep(rnorm(1, 2, 0.5))  
    #Save page to R dataset     
      page <- remDr$getPageSource() #Save page to R dataset 
      page <- page %>% unlist() %>% read_html() # save web page as searchable element
      

    #Save Link to "Referentenentwurf" / Link to "Regierungsentwurf" / Link to "Verkündetes Gesetz" / names of actor (who submitted a statement) / links to the statments
      
      #Save informatin within legislative process
        
      #1"Gesetz"
        #Link      
          #Link called "Bundesgesetsblatt"
            if(length(html_nodes(page, xpath = "/html/body/main/div[3]/div/div/div/p/a[span[contains(text(), 'Bundesgesetzblatt')]]") %>% html_attr(.,"href"))>0) {
              Gesetz.Link <- html_nodes(page, xpath = "/html/body/main/div[3]/div/div/div/p/a[span[contains(text(), 'Bundesgesetzblatt')]]") %>% 
                html_attr(.,"href")
            }
          
          #Link called "Zum PDF-Download auf den Seiten der juris GmbH."
            if(length(html_nodes(page, xpath = "/html/body/main/div[2]/div/div/ul/li/p[1]/a[span[contains(text(), 'Zum PDF-Download auf den Seiten der juris')]]") %>% html_attr(.,"href"))>0) {
              Gesetz.Link <- html_nodes(page, xpath = "/html/body/main/div[2]/div/div/ul/li/p[1]/a[span[contains(text(), 'Zum PDF-Download auf den Seiten der juris')]]") %>% 
                html_attr(.,"href")
            }
      
            if(!exists("Gesetz.Link")) Gesetz.Link <- as.character(NA)
        
        #Date
          Gesetz.Date <- html_nodes(page, xpath = "/html/body/main/div[2]/div/div/ul/li[2]/p[1]") %>%  #HERE IS THE PROBLEM WITH Gesetz.Date became numeric (without as.character())
            html_text() %>%
            str_extract(., "(?<=Ausfertigungsdatum)(.{11})") %>%
            gsub("\u00A0", "", ., fixed =TRUE) %>%
            as.Date(., format='%d.%m.%Y')
            
            if(length(Gesetz.Date)==0) Gesetz.Date <- as.Date(NA)
      
            
      #go to page with further informations (if available): statements, Referentenentwurf, ...
        if(!is.na(html_node(page, xpath = "/html/body/main/div[4]/div/div/ul/li/div/div/a[contains(@title, 'Artikel')]"))) {
          
          link.to.further.information <- html_node(page, xpath = "/html/body/main/div[4]/div/div/ul/li/div/div/a[contains(@title, 'Artikel')]") %>%
            html_attr("href") %>%
            paste0("https://www.bmwi.de/",.)
          
          #Open page of legislative process  
            remDr$navigate(link.to.further.information)
          
          #Wait until page is loaded 
            Sys.sleep(rnorm(1, 2, 0.5))  
          #Save page to R dataset     
            page <- remDr$getPageSource() #Save page to R dataset 
            page <- page %>% unlist() %>% read_html() # save web page as searchable element
          
            
          #1."Referentenentwurf" keep only information of the last
            #Link                                              /html/body/main/div[3]/div
              Referentenentwurf.Link <- html_nodes(page, xpath = "/html/body/main/div[3]/div[h2[contains(text(), 'hrende Informationen')]]") %>% # page not utf8 coded -> no search for "Weiterführende Informationen" because of "ü" possible
                html_node(., xpath = "div/ul/li/div/div[p[contains(text(), 'Referentenentwurf des Bundesministerium')]]/a") %>% # keep only lastest version
                html_attr("href") 
          
              Referentenentwurf.Link <- ifelse(any(length(Referentenentwurf.Link)>0 | !is.na(Referentenentwurf.Link) | !identical(Referentenentwurf.Link, character(0))),
                                             paste0("https://www.bmwi.de",Referentenentwurf.Link), as.character(NA))
              
                if(any(Referentenentwurf.Link == "https://www.bmwi.deNA" | is.na(Referentenentwurf.Link))) Referentenentwurf.Link <- as.character(NA) #lets be save
            
            #Date
              Referentenentwurf.Date <- html_nodes(page, xpath = "/html/body/main/div[3]/div[h2[contains(text(), 'hrende Informationen')]]") %>% # page not utf8 coded -> no search for "Weiterführende Informationen" because of "ü" possible
                html_node(., xpath = "div/ul/li/div/div[p[contains(text(), 'Referentenentwurf des Bundesministerium')]]/p/span[contains(@class, 'date')]") %>% # keep only lastest version
                html_text() %>%
                as.Date(., format='%d.%m.%Y') #transform into international date format
              
                if(length(Referentenentwurf.Date)==0) Referentenentwurf.Date <- as.Date(NA)
              
              
          #2."Regierungsentwurf" keep only information of the last
            #Link
              Regierungsentwurf.Link <- html_nodes(page, xpath = "/html/body/main/div[3]/div[h2[contains(text(), 'hrende Informationen')]]") %>% # page not utf8 coded -> no search for "Weiterführende Informationen" because of "ü" possible
                html_nodes(., xpath = "div/ul/li/div/div[p[contains(text(), 'Gesetzentwurf der Bundesregierung')]]/a") %>%
                html_attr("href") 
              
              Regierungsentwurf.Link <- ifelse(any(length(Regierungsentwurf.Link)>0 | !is.na(Regierungsentwurf.Link) | !identical(Referentenentwurf.Link, character(0))),
                                               paste0("https://www.bmwi.de",Regierungsentwurf.Link), as.character(NA))

            #Date
              Regierungsentwurf.Date <- html_nodes(page, xpath = "/html/body/main/div[3]/div[h2[contains(text(), 'hrende Informationen')]]") %>% # page not utf8 coded -> no search for "Weiterführende Informationen" because of "ü" possible
                html_nodes(., xpath = "div/ul/li/div/div[p[contains(text(), 'Gesetzentwurf der Bundesregierung')]]/p/span[contains(@class, 'date')]") %>%
                html_text() %>%
                as.Date(., format='%d.%m.%Y') #transform into international date format
              
              if(length(Regierungsentwurf.Date)==0) Regierungsentwurf.Date <- as.Date(NA)
 
           
          #3. Statements 
              
            if(length(html_nodes(page, xpath = "/html/body/main/div/div[h2[contains(text(), 'Stellungnahmen')]]/div/ul/li/div/div/a[not(contains(@title, 'Publikation'))]"))>0) {
              
              link.to.further.information <- html_node(page, xpath = "/html/body/main/div/div[h2[contains(text(), 'Stellungnahmen')]]/div/ul/li/div/div/a") %>%
                html_attr("href") %>%
                paste0("https://www.bmwi.de/",.)
              
              #Open page of legislative process  
                remDr$navigate(link.to.further.information)
              
              #Wait until page is loaded 
                Sys.sleep(rnorm(1, 2, 0.5))  
              #Save page to R dataset     
                page <- remDr$getPageSource() #Save page to R dataset 
                page <- page %>% unlist() %>% read_html() # save web page as searchable element    
                
              #Click through all pages with statements
                statement.date <- as.Date(character(0))
                actors <- as.character()
                statement.links <- as.character()
                
                more.pages <- T
                while(more.pages == T) {
                  more.pages <- ifelse(length(html_nodes(page, xpath = "/html/body/main/div[ul[contains(@class, 'pagination')]]"))>0,
                                       ifelse(length(html_nodes(page, xpath = "/html/body/main/div/ul/li/a[contains(@aria-label, 'Next')]"))>0, T, F),
                                       F)
                                                  
                   actors <- append(actors,
                                    html_nodes(page, xpath = "/html/body/main/div/div/ul/li") %>%
                                       lapply(., function(x) x %>%
                                                html_nodes(., xpath = "div/div/p[2]/strong") %>%
                                                html_text()
                                         ) %>% 
                                       unlist()
                   )
                     
                   
                   statement.date <- append(statement.date,
                                            html_nodes(page, xpath = "/html/body/main/div/div/ul/li") %>%
                                             lapply(., function(x) x %>%
                                                      html_nodes(., xpath = "div/div/p[1]/span[1]") %>%
                                                      html_text()
                                             ) %>% 
                                             unlist() %>%
                                             as.Date(., format='%d.%m.%Y')
                   )
                   
                   
                   statement.links <- append(statement.links,
                                             html_nodes(page, xpath = "/html/body/main/div/div/ul/li") %>%
                                               lapply(., function(x) x %>%
                                                        html_nodes(., xpath = "div/div/a") %>%
                                                        html_attr("href")
                                               ) %>% 
                                               unlist() %>%
                                               {ifelse(!is.na(.), paste0("https://www.bmwi.de",.), as.character(NA))}
                   )
                   
                 if(length(html_nodes(page, xpath = "/html/body/main/div[ul[contains(@class, 'pagination')]]"))>0) {
                   if(length(html_nodes(page, xpath = "/html/body/main/div/ul/li/a[contains(@aria-label, 'Next')]"))>0) {
                     #Open next page with statements
                     next.page <- remDr$findElement(using = 'xpath',
                                                    "/html/body/main/div/ul/li/a[contains(@aria-label, 'Next')]")
                     next.page$clickElement() # Click the "Next page" Button
                     #Wait until page is loaded 
                     Sys.sleep(rnorm(1, 1, 0.1))      
                     #Save page to R dataset     
                     page <- remDr$getPageSource() #Save page to R dataset 
                     page <- page %>% unlist() %>% read_html() # save web page as searchable element
                   } 
                 }
                   
                }
               
            } else{
                actors <- as.character(NA) 
                statement.date <- as.Date(NA)
                statement.links <- as.character(NA) 
              }
              
        } else{
            Referentenentwurf.Link <- as.character(NA) 
            Referentenentwurf.Date <- as.Date(NA)
            Regierungsentwurf.Link <- as.character(NA) 
            Regierungsentwurf.Date <- as.Date(NA)
            #if this condition is FALSE the inner loop is not executed and no values are assigned to actors, statement.date/links
              actors <- as.character(NA) 
              statement.date <- as.Date(NA)
              statement.links <- as.character(NA)
          }


          
      #append dataset
        n.actors <- length(actors)
        
        data.actors <- data.frame(links = append(as.character(data.actors$links), rep(data$links[i], n.actors)),
                           names = append(as.character(data.actors$names), rep(data$names[i], n.actors)),
                           last.updates = append(data.actors$last.updates, rep(data$last.updates[i], n.actors)),
                           Vorgangstyp = append(data.actors$Vorgangstyp, rep(data$Vorgangstyp[i], n.actors)),
                             Referentenentwurf.Link = append(data.actors$Referentenentwurf.Link, rep(Referentenentwurf.Link, n.actors)),
                             Referentenentwurf.Date = append(data.actors$Referentenentwurf.Date, rep(Referentenentwurf.Date, n.actors)),
                             Regierungsentwurf.Link = append(data.actors$Regierungsentwurf.Link, rep(Regierungsentwurf.Link, n.actors)),
                             Regierungsentwurf.Date = append(data.actors$Regierungsentwurf.Date, rep(Regierungsentwurf.Date, n.actors)),
                             Gesetz.Link = append(data.actors$Gesetz.Link, rep(Gesetz.Link, n.actors)),
                             Gesetz.Date = append(data.actors$Gesetz.Date, rep(Gesetz.Date, n.actors)),
                               actors = append(data.actors$actors, actors),
                               statement.date = append(data.actors$statement.date, statement.date),
                               statement.links = append(data.actors$statement.links, statement.links),
                           stringsAsFactors = FALSE)

      #clear working space
        rm(Referentenentwurf.Link, Referentenentwurf.Date, Regierungsentwurf.Link, Regierungsentwurf.Date,
           Gesetz.Link, Gesetz.Date, actors, statement.date, statement.links)

    #assign data tp global environment
      assign("data.full", data.actors, envir = .GlobalEnv)
    
        
    } # End of the extraction of informations of each legislative process 

    #clear working space
      rm(i)
      
    str(data.full)  
      
    #Add "ministry" variable
      data.full$ministry <- "BMW" 
      
    #Save dataset
      write.csv2(data.full, "./datasets/BMW.csv", row.names = F)
      saveRDS(data.full, "./datasets/BMW.rds")
      

      
      #Number of legislative processes 2019
        unique(data.full$names[data.full$last.updates>"2019-01-01" & data.full$last.updates<"2019-12-31"])
        
        #Number of laws 2019
          unique(data.full$names[data.full$last.updates>"2019-01-01" & data.full$last.updates<"2019-12-31" & data.full$Vorgangstyp=="Gesetz"])
        
      #Number of actors 2019
        length(data.full$actors[data.full$last.updates>"2019-01-01" & data.full$last.updates<"2019-12-31"])
        
        data<- data.full[data.full$last.updates>"2019-01-01" & data.full$last.updates<"2019-12-31",]
        
#-------------------------------------------------------------------------------------------------#  
#### 4. Download draft bill, statements & letter of invitation for each legislative proposals ##### 
#-------------------------------------------------------------------------------------------------#
      
        
        
                 

  #Save page (with open drop-down-menu) to R dataset 
    #library(httr)
    #library(xml2)
    #library(XML)
    
    page <- remDr$getPageSource(header = TRUE)[[1]] %>% read_html()
      xml_type(page)=="element" # if "element" then it worked
    
    
    #Scraping informations  
      name <- html_text(html_node(html_nodes(page, css = "div.stellungnahmen"), css = "a"))
      
      link.law <- paste("https://www.bmjv.de/",html_attr(html_node(html_nodes(page, css = "div.stellungnahmen"), css = "a"), "href"), sep="")
      
      link.statement <- paste("https://www.bmjv.de/",html_attr(html_node(html_nodes(page, css = "div.stellungnahmen"), css = ".downloadLink"), "href"), sep="")
      
      #Sometimes actor is not called -> So this algorithm is needed
        number <- length(html_text(html_nodes(page, css = "div.teaser > div:nth-child(1) > span:nth-child(1)")))
        actor <- character()
        date <- character()
        library(stringr) # for: str_split_fixed
        
        for(l in 1:number) {
          var <- html_text(html_nodes(page, css = paste("div.teaser:nth-child(",l,") > div:nth-child(1) > span")))
          
          if(length(var)==5) {
            actor[l] <- trimws(str_split_fixed(var[3], "\n", n =Inf)[,2])
            date[l]  <- var[5]
          }
          
          if(length(var)==3) {
            actor[l] <- NA
            date[l]  <- var[3]
          }   
          
        }
    
    #Save all infos in a table      
      table <- data.frame(law = name, link.law = link.law, actor = actor, date = date, link.statement = link.statement)
      
    
  #Download all documents
    for(i in 1:length(table$link.statement)) {
      
      actor <- substr(table$actor[i],1, 40)
      name <- substr(table$law[i],1, 50)
      download.file(as.character(table$link.statement[i]), paste("./statements/",i,"-",gsub('/|\\?|"|:','',actor),"--",gsub('/|\\?|"|:','',name),"....pdf", sep=""), mode="wb")  
      n <- i
      
      #Slowing down download 
      Sys.sleep(1) 
    }
    
 
  #Checking if a next page is available    
    next.page <- !is.na(html_attr(html_nodes(page,".forward"),"href")) # Variable becomes 'TRUE' when next page is available
    
    i <- 1
    while(isTRUE(next.page)) {
      i <- i + 1 #needed to construct the css of the table
  
    
    #Click on arrow on the right (NEXT page)   
      webElem <- remDr$findElement(using = 'css',
                                   value = ".forward") 
      webElem$clickElement()
    
    #Wait until page is loaded  
      Sys.sleep(1)      
    
    #Save page to R dataset 
      page <- remDr$getPageSource(header = TRUE)[[1]] %>% read_html()
      xml_type(page) # if "element" then it worked
      

    #Save table                               
      name <- html_text(html_node(html_nodes(page, css = "div.stellungnahmen"), css = "a"))
      
      link.law <- paste("https://www.bmjv.de/",html_attr(html_node(html_nodes(page, css = "div.stellungnahmen"), css = "a"), "href"), sep="")
      
      link.statement <- paste("https://www.bmjv.de/",html_attr(html_node(html_nodes(page, css = "div.stellungnahmen"), css = ".downloadLink"), "href"), sep="")
      link.statement <- unlist(lapply(link.statement,function(x) ifelse(x=="https://www.bmjv.de/NA",NA,x)))

      #Sometimes actor is not called -> So this algorithm is needed
        number <- length(html_text(html_nodes(page, css = "div.teaser > div:nth-child(1) > span:nth-child(1)")))
        actor <- character()
        date <- character()
        library(stringr) # for: str_split_fixed
        
        for(l in 1:number) {
          var <- html_text(html_nodes(page, css = paste("div.teaser:nth-child(",l,") > div:nth-child(1) > span")))
          
          if(length(var)==5) {
            actor[l] <- trimws(str_split_fixed(var[3], "\n", n =Inf)[,2])
            date[l]  <- var[5]
          }
          
          if(length(var)==3) {
            actor[l] <- NA
            date[l]  <- var[3]
          }   
          
        }
        
        
    #Save all infos in a table      
      a <- data.frame(law = name, link.law = link.law, actor = actor, date = date, link.statement = link.statement) 
                                                                          
    #Combine tables
      table <- rbind(table,a)
      
      
    #Download all documents
      for(a in (n+1):length(table$link.statement)) {
       
        actor <- substr(table$actor[a],1, 40)
        name <- substr(table$law[a],1, 50)
        if(!is.na(table$link.statement[a])) download.file(as.character(table$link.statement[a]), paste("./statements/",a,"-",gsub('/|\\?|"|:','',actor),"--",gsub('/|\\?|"|:','',name),"....pdf", sep=""), mode="wb")  
        if(is.na(table$link.statement[a])) cat(" ",file=paste("./statements/",a,"-KEIN LINK-",gsub('/|\\?|"|:','',actor),"--",gsub('/|\\?|"|:','',name),"....txt", sep=""),sep="\n")
        n <- a
        
        #Slowing down download 
        Sys.sleep(1) 
      }
      
    #Checking if a next page is available    
      next.page <- !is.na(html_attr(html_nodes(page,".forward"),"href")) # Variable becomes 'TRUE' when next page is available
    
  }
  
  print("Ende")
  
  write.csv2(table, "BMJV-Stellungnahmen-overview.csv", row.names = FALSE)
  
  #Wait until dataset is saved
    Sys.sleep(1)  
    
    
  #Close browser
    rD$server$stop() # Clean all ports
    
    
    
  #Save start time  
    end  <- Sys.time()    
    
  #Save time dataset 
    time <- data.frame(start = start, end = end, duration = end-start)
    write.csv2(time, "Duration_of_Web-Scraping.csv", row.names = FALSE)
    
  #Wait till the dataset is saved
    Sys.sleep(5)  
    
  #Shutdown PC  
    system('shutdown -s')    
  