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
    library(dplyr) # for %>%  & contains()

  if(!require("stringr")) install.packages("stringr")
    library(stringr) # for str_detect()

  if(!require("magrittr")) install.packages("magrittr")
    library(magrittr) # for extract()  [within pipes]

  if(!require("purrr")) install.packages("purrr")
    library(purrr) # for map()

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
      #Get number of pages    
        page <- remDr$getPageSource() #Save page to R dataset 
        page <- page %>% unlist() %>% read_html() # save web page as searchable element
        #Wait until page is loaded 
          Sys.sleep(rnorm(1,2,0.5))   
      
      #list of names
        names <- html_text(html_nodes(page, css = "#skiplink2maincontent > div.col-xs-12.col-sm-10.col-sm-offset-1.col-md-8.col-md-offset-2 > div > div > div > div.panel-heading > h2")) %>%
          trimws(which = c("both")) %>%
          gsub("[^[:alnum:][:blank:]?&.,()/\\-]", "",.) #Removing special characters from text except of "?", "&", ".", ",", "(", ")", "/" and "-"  //https://rpubs.com/Mentors_Ubiqum/Clean_Text
            
      #list of links // !!!! HERE STELLUNGNAHMEN !!!!
        links <- html_nodes(page, css = '#skiplink2maincontent > div.col-xs-12.col-sm-10.col-sm-offset-1.col-md-8.col-md-offset-2 > div') %>%
          map(~{
            .x %>%
              xml_find_all('./div/div/div[contains(@class,"panel-body")]/p') %>%
              as.character() %>%
              str_extract_all('(?<=Referentenentwurf)(.*)(?=<..>)')  %>% #
              str_extract_all('(?<=- <a href=\")(.*?)(?=\"(.*?)Stellungnahmen)')%>%
              #str_extract_all('(?<=<a href=\")(.*?)(?=\")') %>%
              unlist() %>%
              first()
          }) %>%
          unlist() %>%
          ifelse(startsWith(., "/"), paste0("https://www.bundesgesundheitsministerium.de",.), .)
        
            
      #list of last-update-time
        last.updates <- as.Date(NA, format='%d.%m.%Y') #transform "NA" into international date format
            
        
      #Referentenentwurf.Link
        Referentenentwurf.Link <- html_nodes(page, css = '#skiplink2maincontent > div.col-xs-12.col-sm-10.col-sm-offset-1.col-md-8.col-md-offset-2 > div') %>%
          map(~{
            .x %>%
              xml_find_all('./div/div/div[contains(@class,"panel-body")]/p') %>%
              as.character() %>%
              str_extract_all('(?<=Referentenentwurf)(.*?)(?=<(..)>)')  %>% #
              str_extract_all('(?<=<a href=\")(.*?)(?=\"(.*?)Download)')%>%
              #str_extract_all('(?<=<a href=\")(.*?)(?=\")') %>%
              unlist() %>%
              first()
          }) %>%
        unlist() %>%
          ifelse(startsWith(., "/"), paste0("https://www.bundesgesundheitsministerium.de",.), .)

       
        
      #Referentenentwurf.Date
        Referentenentwurf.Date <- lapply(seq(length(names)), function(x){
          date <- html_nodes(page, css = paste0("#skiplink2maincontent > div.col-xs-12.col-sm-10.col-sm-offset-1.col-md-8.col-md-offset-2 > div:nth-child(",x*2,") > div > div > div.panel-body > p")) %>%
            extract(
              str_detect(html_text(html_nodes(page, css = paste0("#skiplink2maincontent > div.col-xs-12.col-sm-10.col-sm-offset-1.col-md-8.col-md-offset-2 > div:nth-child(",x*2,") > div > div > div.panel-body > p"))),
                         "Referentenentwurf")
            ) %>%
            html_nodes(xpath = "text()") %>%
            grep("Referentenentwurf",.,value=TRUE) %>%
            gsub("Referentenentwurf:", "",.) %>%
            trimws(which = c("both")) %>%
            strsplit(" ") %>%  
            sapply(head, 1) #keep frist element = date
          
          ifelse(length(date)==0, NA, date)  # set link to "NA" if there is no Link to Referentenentwurf
        }) %>% 
          unlist() %>%
          as.Date(NA, format='%d.%m.%Y')
        
        
      #Regierungsentwurf.Link -> here "Kabinett"
        Regierungsentwurf.Link <- html_nodes(page, css = '#skiplink2maincontent > div.col-xs-12.col-sm-10.col-sm-offset-1.col-md-8.col-md-offset-2 > div') %>%
          map(~{
            .x %>%
              xml_find_all('./div/div/div[contains(@class,"panel-body")]/p') %>%
              as.character() %>%
              str_extract_all('(?<=Kabinett)(.*?)(?=<(..)>)')  %>% #
              str_extract_all('(?<=<a href=\")(.*?)(?=\"(.*?)Download)')%>%
              #str_extract_all('(?<=<a href=\")(.*?)(?=\")') %>%
              unlist() %>%
              first()
          }) %>%
          unlist() %>%
          ifelse(startsWith(., "/"), paste0("https://www.bundesgesundheitsministerium.de",.), .)
        
        
      #Regierungsentwurf.Date
        Regierungsentwurf.Date <- lapply(seq(length(names)), function(x){
          date <- html_nodes(page, css = paste0("#skiplink2maincontent > div.col-xs-12.col-sm-10.col-sm-offset-1.col-md-8.col-md-offset-2 > div:nth-child(",x*2,") > div > div > div.panel-body > p")) %>%
            extract(
              str_detect(html_text(html_nodes(page, css = paste0("#skiplink2maincontent > div.col-xs-12.col-sm-10.col-sm-offset-1.col-md-8.col-md-offset-2 > div:nth-child(",x*2,") > div > div > div.panel-body > p"))),
                         "Kabinett")
            ) %>%
            html_nodes(xpath = "text()") %>%
            grep("Kabinett",.,value=TRUE) %>%
            gsub("Kabinett:", "",.) %>%
            trimws(which = c("both")) %>%
            strsplit(" ") %>%  
            sapply(head, 1) #keep frist element = date
          
          ifelse(length(date)==0, NA, date)  # set link to "NA" if there is no Link to Referentenentwurf
        }) %>% 
          unlist() %>%
          as.Date(NA, format='%d.%m.%Y')
        
        
      #Gesetz.Link -> here "Inkrafttreten"
        Gesetz.Link <- html_nodes(page, css = '#skiplink2maincontent > div.col-xs-12.col-sm-10.col-sm-offset-1.col-md-8.col-md-offset-2 > div') %>%
          map(~{
            .x %>%
              xml_find_all('./div/div/div[contains(@class,"panel-body")]/p') %>%
              as.character() %>%
              str_extract_all('(?<=Inkrafttreten)(.*?)(?=<(..)>)')  %>% #
              str_extract_all('(?<=<a href=\")(.*?)(?=\"(.*?)Wortlaut)')%>%
              #str_extract_all('(?<=<a href=\")(.*?)(?=\")') %>%
              unlist() %>%
              first()
          }) %>%
          unlist() %>%
          ifelse(startsWith(., "/"), paste0("https://www.bundesgesundheitsministerium.de",.), .)
        
        
      #Gesetz.Date
        Gesetz.Date <- lapply(seq(length(names)), function(x){
          date <- html_nodes(page, css = paste0("#skiplink2maincontent > div.col-xs-12.col-sm-10.col-sm-offset-1.col-md-8.col-md-offset-2 > div:nth-child(",x*2,") > div > div > div.panel-body > p")) %>%
            extract(
              str_detect(html_text(html_nodes(page, css = paste0("#skiplink2maincontent > div.col-xs-12.col-sm-10.col-sm-offset-1.col-md-8.col-md-offset-2 > div:nth-child(",x*2,") > div > div > div.panel-body > p"))),
                         "Inkrafttreten")
            ) %>%
            html_nodes(xpath = "text()") %>%
            grep("Inkrafttreten",.,value=TRUE) %>%
            gsub("Inkrafttreten:", "",.) %>%
            trimws(which = c("both")) %>%
            strsplit(" ") %>%  
            sapply(head, 1) #keep frist element = date
          
          ifelse(length(date)==0, NA, date)  # set link to "NA" if there is no Link to Referentenentwurf
        }) %>% 
          unlist() %>%
          as.Date(NA, format='%d.%m.%Y')
        
            
    #append dataset
      data <- data.frame(links = links,
                         names = names,
                         last.updates = last.updates,
                         Referentenentwurf.Link = Referentenentwurf.Link,
                         Referentenentwurf.Date = Referentenentwurf.Date,
                         Regierungsentwurf.Link = Regierungsentwurf.Link,
                         Regierungsentwurf.Date = Regierungsentwurf.Date,
                         Gesetz.Link = Gesetz.Link,
                         Gesetz.Date = Gesetz.Date,
                         stringsAsFactors = FALSE)
          
      #clear working space
        rm(page, links, names, last.updates, Referentenentwurf.Link, Referentenentwurf.Date, Regierungsentwurf.Link, Regierungsentwurf.Date, Gesetz.Link, Gesetz.Date)
          
    #assign data tp global environment
      assign("data", data, envir = .GlobalEnv)  
        }


  
#----------------------------------#  
#### 1. Enter search criteria  ##### 
#----------------------------------#  
  #-> NOT NEEDED
    # (It's just one page with all legislative processes)
    
  remDr$navigate("https://www.bundesgesundheitsministerium.de/service/gesetze-und-verordnungen.html")

  #Wait until page is loaded 
    Sys.sleep(1)
    
        
#---------------------------------------------#  
#### 2. Extract meta data & build dataset ##### 
#---------------------------------------------#    
  #Extract Links to, names of, and last updates of lawas & legislative proposals
        
    extractLinksNamesDates()
      
  #Dertermine acticity type  
    data$Vorgangstyp <- NA
      data$Vorgangstyp <- ifelse(str_detect(data$names, "esetz") == TRUE, "Gesetz", data$Vorgangstyp) #delete G, because of capital leter
      data$Vorgangstyp <- ifelse((str_detect(data$names,"erordnung") == TRUE) | (str_detect(data$names,"VO") == TRUE), "Verordnung", data$Vorgangstyp)
    
  
#-------------------------------------------------------#  
#### 3. Add rows for each actor & actor-name column ##### 
#-------------------------------------------------------#         
       
 
  #create empty an dataset that can be filled
    data.actors <- data.frame(matrix(ncol = 13, nrow = 0))
    colnames(data.actors) <- c("links", "names", "last.updates", "Vorgangstyp", 
                               "Referentenentwurf.Link", "Referentenentwurf.Date", "Regierungsentwurf.Link", "Regierungsentwurf.Date", "Gesetz.Link", "Gesetz.Date",
                               "actors", "statment.data", "statement.links")
  #Add information for all legislative processes
    for(i in 1:nrow(data)) {

    #if "Stellungnahmen" (-> Link) available   
      if(!is.na(data[i, "links"])) {
        
      #Open page of legislative process  
        remDr$navigate(data[i, "links"])
      
      #Save page to R dataset     
        page <- remDr$getPageSource() #Save page to R dataset 
        page <- page %>% unlist() %>% read_html() # save web page as searchable element
      
      #Wait until page is loaded 
        Sys.sleep(rnorm(1, 1, 0.3))
      
      #Save names of actor (who submitted a statement) / links to the statments
      
        #actor
          #actors <- html_nodes(page, xpath = "/html/body/div[3]/div/article/div[2]/div/ul/li") %>%
          actors <- html_nodes(page, xpath = "/html/body/div[3]/div/article/div[2]/div") %>%
            map(~{
              .x %>%
                html_nodes("a") %>%
                html_text() #%>%
                #str_split("- PDF") #%>%
                #unlist() }) %>%
                #first() %>%
                #trimws(which ="both") 
              }) %>%
            unlist()
                
        #Date of statement (not available)
          statement.date <- rep(as.Date(NA, format='%d.%m.%Y'),length(actors))
      
        #Link to statements
          statement.links <- html_nodes(page, xpath = "/html/body/div[3]/div/article/div[2]/div") %>%
            map(~{
              .x %>%
                html_nodes("a") %>%
                html_attr("href") 
            }) %>%
            unlist() %>%
            paste0("https://www.bundesgesundheitsministerium.de",.)
        
      #append dataset
        n.actors <- length(actors)
      
        
      }else{
        actors <- as.character(NA)
        statement.date <- as.Date(NA, format='%d.%m.%Y')
        statement.links <- as.character(NA)
        n.actors <- 1
      }
      
    
        data.actors <- data.frame(links = append(as.character(data.actors$links), as.character(rep(data$links[i], n.actors))),
                                  names = append(as.character(data.actors$names), as.character(rep(data$names[i], n.actors))),
                                  last.updates = append(data.actors$last.updates, as.Date(as.character(rep(data$last.updates[i], n.actors)))),
                                  Vorgangstyp = append(data.actors$Vorgangstyp, rep(data$Vorgangstyp[i], n.actors)),
                                  Referentenentwurf.Link = append(data.actors$Referentenentwurf.Link, rep(data$Referentenentwurf.Link[i], n.actors)),
                                  Referentenentwurf.Date = append(data.actors$Referentenentwurf.Date, rep(data$Referentenentwurf.Date[i], n.actors)),
                                  Regierungsentwurf.Link = append(data.actors$Regierungsentwurf.Link, rep(data$Regierungsentwurf.Link[i], n.actors)),
                                  Regierungsentwurf.Date = append(data.actors$Regierungsentwurf.Date, rep(data$Regierungsentwurf.Date[i], n.actors)),
                                  Gesetz.Link = append(data.actors$Gesetz.Link, rep(data$Gesetz.Link[i], n.actors)),
                                  Gesetz.Date = append(data.actors$Gesetz.Date, rep(data$Gesetz.Date[i], n.actors)),
                                  actors = append(data.actors$actors, actors),
                                  statement.date = append(data.actors$statement.date, statement.date),
                                  statement.links = append(data.actors$statement.links, statement.links),
                                  stringsAsFactors = FALSE)
        
      #clear working space
      rm(actors, statement.date, statement.links, n.actors)
      
      #assign data tp global environment
      assign("data.full", data.actors, envir = .GlobalEnv)
      
      
    } # End of the extraction of informations of each legislative process 
    
    #Save end time 
      end <- Sys.time()
      
      time <- end - start
    
    #clear working space
    rm(i, data, data.actors, page, rD, remDr, start, end)
    
    
    
    
    #Add "ministry" variable
      data.full$ministry <- "BMG"
    
    
    #Save dataset
      write.csv2(data.full, "./datasets/BMG.csv", row.names = F)
      saveRDS(data.full, "./datasets/BMG.rds")
      
      
      
      
      #Number of legislative processes 2019
        unique(data.full$names[data.full$Referentenentwurf.Date>"2019-01-01" & data.full$Referentenentwurf.Date<"2019-12-31"])
      
      #Number of laws 2019
        unique(data.full$names[data.full$Referentenentwurf.Date>"2019-01-01" & data.full$Referentenentwurf.Date<"2019-12-31" & data.full$Vorgangstyp=="Gesetz"])
      
      #Number of actors 2019
        length(data.full$actors[data.full$Referentenentwurf.Date>"2019-01-01" & data.full$Referentenentwurf.Date<"2019-12-31"])
      
        data<- data.full[data.full$Referentenentwurf.Date>"2019-01-01" & data.full$Referentenentwurf.Date<"2019-12-31",]      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
       
  remDr$navigate(data[1, "links"])
        
  #Save page to R dataset     
    page <- remDr$getPageSource() #Save page to R dataset 
    page <- page %>% unlist() %>% read_html() # save web page as searchable element
    #Wait until page is loaded 
      Sys.sleep(0.5)
   
  #Save Link to "Referentenentwurf" / Link to "Regierungsentwurf" / Link to "Verkündetes Gesetz" / names of actor (who submitted a statement) / links to the statments
    
    #Save informatin within legislative process
      #1.  Link to "Referentenentwurf"
        link <- html_nodes(page, css = ".akkordeon> div:nth-child(2)")[html_text(html_nodes(html_nodes(page, css = "#content > div > div.article-content-wrapper > div > div"),css = "h3")) %>% trimws(which = c("both")) == "Referentenentwurf"] %>%
                 html_nodes("a") %>%
                 html_attr("href")
  
        Link.Referentenentwurf <- ifelse(length(link)>0, paste0("https://www.bundesfinanzministerium.de/",link), "") #If no information is available, save ""
          rm(link)
          
      #2.  Link to "Regierungsentwurf"
        link <- html_nodes(page, css = ".akkordeon> div:nth-child(2)")[html_text(html_nodes(html_nodes(page, css = "#content > div > div.article-content-wrapper > div > div"),css = "h3")) %>% trimws(which = c("both")) == "Regierungsentwurf"] %>%
          html_nodes("a") %>%
          html_attr("href")
        
        Link.Regierungsentwurf <- ifelse(length(link)>0, link, "") #If no information is available, save ""
          rm(link)
          
      #3.  Link to "Verkündetes Gesetz"
        link <- html_nodes(page, css = ".akkordeon> div:nth-child(2)")[html_text(html_nodes(html_nodes(page, css = "#content > div > div.article-content-wrapper > div > div"),css = "h3")) %>% trimws(which = c("both")) == "Verkündetes Gesetz"] %>%
          html_nodes("a") %>%
          html_attr("href")
        
        Link.Verkündetes.Gesetz <- ifelse(length(link)>0, paste0("https://www.bundesfinanzministerium.de/",link), "") #If no information is available, save ""
          rm(link)
        
    #Several informations within legislative process    
      #4.  Link to "Verkündetes Gesetz"
        link <- html_nodes(page, css = ".akkordeon> div:nth-child(2)")[html_text(html_nodes(html_nodes(page, css = "#content > div > div.article-content-wrapper > div > div"),css = "h3")) %>% trimws(which = c("both")) == "Verkündetes Gesetz"] %>%
          html_nodes("a") %>%
          html_attr("href")
        
        Link.Verkündetes.Gesetz <- ifelse(length(link)>0, paste0("https://www.bundesfinanzministerium.de/",link), "") #If no information is available, save ""
        rm(link)
        
        
        
        
        
        
        
      
  #go to expandable list "Referentenentwurf" <- must be visible to be clickable
        go.to <- remDr$findElement(using = 'css selector', 
                                   "#content > div > div.article-content-wrapper > div > div > div:nth-child(3) > div.akkordeon-button > a")
        go.to$getElementLocationInView() 
        
        go.to$mouseMoveToLocation(x = 661.5, y = 1045.283)
        
        
  #Click on button -> open list
    open <- remDr$findElement(using = 'xpath', 
                                    "/html/body/div[1]/div/div/div[3]/div/div/div[1]/div/div/div/div[2]/div/div/div[1]/div[2]/ul/li/a") 
      search$clickElement() # Click Button
      
      links <- paste0("https://www.bundesfinanzministerium.de/", )
                      
                      html_attr(html_nodes(page, css = ".akkordeon> div:nth-child(2)> ul > li > a"), "href") 
                      
                      
                      
                      html_attr(html_nodes(page, css = "#searchResult > li > div > div > div > h3 > a"),"href")
                      
                      
                      
      
     
  #Save page to R dataset     
      page <- remDr$getPageSource() #Save page to R dataset 
      page <- page %>% unlist() %>% read_html() # save web page as searchable element
      #Wait until page is loaded 
      Sys.sleep(0.5)
      
      
  #save link to speaker-draft
    link.speakerdraft <- paste0("https://www.bundesfinanzministerium.de/", 
                    html_attr(html_nodes(page, css = "#collapse8b380768-bb01-41ab-a742-1f79e16ed7af-0-0 > ul > li > a"),"href")
    ) %>% 
      data.frame() %>%
      rename("link" = ".") # delete ":nth-child(1)" from css-selector of first link
      
        
        
        
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
  