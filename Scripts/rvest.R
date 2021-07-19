#Start
  #Install and load the package/library
    if(!require("rvest")) install.packages("rvest")
    library(rvest)
  
  #Start by reading a HTML page
    starwars <- read_html("https://rvest.tidyverse.org/articles/starwars.html")
  
  #Get an overview of the structure
    films <- starwars %>% 
      html_nodes("body > div.container.template-article > div > div.col-md-9.contents > section > h2") %>%
      html_text(., trim = TRUE)
    films    
        
  
  #Extract the movies' names /extract one element per film
    title <- starwars %>% 
      html_nodes("h2") %>% 
      html_text(., trim = TRUE)
    title
  
  #Extract episodes
    episode <- starwars %>% 
      html_nodes("h2") %>% 
      html_attr("data-id") %>% 
      as.numeric()
    episode
    
  #Extract date
    date <- starwars %>% 
      html_nodes("section > p:nth-child(2)") %>%
      html_text(., trim = TRUE)
    date
    
    data.frame(Titel=title, Episode=episode, Date=date)
    
    
  #Start by reading a HTML page
    html <- read_html("https://en.wikipedia.org/w/index.php?
                    title=The_Lego_Movie&oldid=998422565")
    
  #extract table
    html %>% 
      html_node(".tracklist") %>% 
      html_table()
    
    html <- read_html("https://www.spiegel.de/politik/deutschland/")
    html %>% 
      html_nodes("#Inhalt > section > section.bg-white.shadow.rounded > section > div > article > div > div > header > h2 span[class*=align-middle]") %>% #h2
      html_text(., trim = T)
    
    
    
    