#Start
  #Install and load the package/library
    if(!require("rvest")) install.packages("rvest")
      library(rvest)
    if(!require("dplyr")) install.packages("dplyr")
      library(dplyr)    
  
  #Basic example
    #Start by reading a HTML page 
      html <- read_html("https://resssis4.io.noc.ruhr-uni-bochum.de/homepage/")
    #Extract information
      html %>%
        html_elements(css="p:nth-of-type(1)") %>%
        #html_elements(xpath="//p[1]") %>%
        html_text()
    
      
  #Start by reading a HTML page
    starwars <- read_html("https://rvest.tidyverse.org/articles/starwars.html")
  #Get an overview of the structure
    films <- starwars %>% 
      html_elements(css="body > div.container.template-article > div > div.col-md-9.contents > section > h2") %>%
      #html_elements(xpath="/html/body/div/div/div/section/h2") %>%
      html_text(., trim = TRUE)
    films    
   

  #Extract the movies' names /extract one element per film
    title <- starwars %>% 
      html_elements(css="h2") %>% 
      #html_elements(xpath="//h2") %>%
      html_text(., trim = TRUE)
    title
  
  #Extract episodes
    episode <- starwars %>% 
      html_elements(css="h2") %>% 
      #html_elements(xpath="//h2") %>%
      html_attr("data-id") %>% 
      as.numeric()
    episode
    
  #Extract date
    date <- starwars %>% 
      html_elements(css="section > p:nth-child(2)") %>%
      #html_elements(xpath="//section/p[1]") %>%
      html_text(., trim = TRUE)
    date
    
    data.frame(Titel=title, Episode=episode, Date=date)
    
    
  #Start by reading a HTML page
    html <- read_html("https://en.wikipedia.org/w/index.php?title=The_Lego_Movie&oldid=998422565")
    
  #extract table
    html %>% 
      html_elements(css=".tracklist") %>% 
      #html_elements(xpath="//*[@class='tracklist']") %>% 
      html_table()
    
  #another web page
    html <- read_html("https://www.tagesschau.de/")
    html %>% 
      html_elements(css="#content > div > div:nth-child(1) > div > a > div:nth-child(2) > div > div > h3 > span.teaser__headline") %>% 
      #html_elements(xpath = "/html/body/main/div/div/div/div[1]/div/a/div[2]/div/div/h3/span[2]") %>%
      html_text(., trim = T)
    
    
    
  #Scrape from URL to be created (e.g. for search result in a database)
    #Example:
      #Base url: https://dip.bundestag.de
      #Search for: Gesetz zur Stärkung der Vor-Ort-Apotheken
      #url of results (previously unknows): https://dip.bundestag.de/suche?term=Gesetz%20zur%20Stärkung%20der%20Vor-Ort-Apotheken&rows=25

    if(!require("stringr")) install.packages("stringr")
      library(stringr)
    
      #create search term
        searchterm <- "Gesetz zur Stärkung der Vor-Ort-Apotheken" %>% 
          str_replace_all(., c("ä" = "%C3%A4", "Ä" = "%C3%84", "ö" = "%C3%B6", "Ö" = "%C3%96", "ü" = "%C3%BC", "Ü" = "%C3%9C")) %>%
          strsplit(., " ") %>% 
          unlist() %>%
          paste0(., collapse="%20") 

      #combine base-url + "suche?term=" + search-term
        base <- "https://dip.bundestag.de"
        url <- paste0(base,"/suche?term=", searchterm, "&rows=25")
    
   gesetz <- read_html(url) %>%
     html_elements(xpath="/html/body/div/div/main") %>% #"body/div/main/div/div/ul/li/section/a"
     html_text()
   
    #Problem:
      #The result is an empty character vector because the HTML document only contains JS snippets 
      #and not the content that we see in the browser. 
      #These JS snippets are filled with content in the browser by means of communication with the server. 
      #The source code in the HTML document therefore only represents the framework in which the content 
      #is subsequently inserted. 
      #This means that the element related to the CSS selector that we can see in the browser is empty 
      #in the document from which R is trying to extract data.
   
    #Solution:
      # -> R Selenium
      #It retrieves the HTML file from the specified URL and store it into a local HTML file, 
      #so that R can read contents from that file instead of reading the contents directly from the URL.