#Start
  #Install and load the package/library
  if(!require("RSelenium")) install.packages("RSelenium")
  library(RSelenium)
  
  # start the server and browser(you can use other browsers here)
  rD <- rsDriver(port = 4570L,browser=c("firefox")) # rD <- rsDriver(port = 4568L, browser = "firefox")
  driver <- rD[["client"]]
  
  # navigate to an URL
    #driver$navigate("http://books.toscrape.com/")
  
  #close the driver
    #driver$close()
  
  #close the server
    #rD[["server"]]$stop()


#Click element
  # navigate to an URL
  driver$navigate("http://books.toscrape.com/")
  
  page <- driver$getPageSource() #Save page to R dataset 
  page <- page %>% unlist() %>% read_html() # save web page as searchable element
  
  links <- page %>% 
    html_nodes("#default > div > div > div > aside > div.side_categories > ul > li > ul > li > a") %>%
    html_attrs() %>%
    paste0("http://books.toscrape.com/", .)
  
  
  data <- data.frame(matrix(NA, 0,3))
  colnames(data) <- c("Kategorie", "Name", "Preis")
  
  # click the first link 
  for(i in 1:50) {
    driver$navigate(links[i])
    
    #save current page
      page <- driver$getPageSource() #Save page to R dataset 
      page <- page %>% unlist() %>% read_html() # save web page as searchable element
      
    #save prices
      price <- page %>%
        html_nodes("p.price_color") %>%
        html_text() %>%
        str_remove(.,"£") %>%
        as.numeric()
      
    #save title
      name <- page %>%
        html_nodes("h3 > a") %>%
        html_text()
      
    #save category
      category <- page %>%
        html_nodes("h1") %>%
        html_text()
    
    #append dataframe
      data <- data.frame(Kategorie = append(as.character(data$Kategorie), rep(category,length(name))),
                         Name = append(as.character(data$Name), name),
                         Preis = append(data$Preis, price),
                         stringsAsFactors = FALSE) 
  }
  
  
#Navigate
  # navigate back and forward
  driver$goBack()
  driver$goForward()


#Scroll down
  driver$navigate("http://quotes.toscrape.com/scroll")
  
  # find the webpage body
  element <- driver$findElement("css", "body")
  
  #scroll down once ----
  element$sendKeysToElement(list(key = "page_down"))
  
  
  
#Scroll multiple times
  element <- driver$findElement("css", "body")
  
  # Scroll down 10 times
    for(i in 1:2){
      element$sendKeysToElement(list("key"="page_down"))
      # please make sure to sleep a couple of seconds to since it takes time to load contents
      Sys.sleep(2) 
    }
  
  
#Click ones
  #locate element using CSS(find the first match)
    driver$navigate("https://scrapethissite.com/pages/ajax-javascript/#2011")
    element <- driver$findElement(using = "css",".year-link")
    element$clickElement()
    
#Click several times
    driver$navigate("https://scrapethissite.com/pages/ajax-javascript/#2011")
    elements <- driver$findElements(using = "css",".year-link")
    for(element in elements){
      element$clickElement()
      Sys.sleep(2)
    }
    
    
#Simulate text input
  driver$navigate("https://www.google.com/")
  
  #select input box
    element <- driver$findElement(using = "css",'input[name="q"]')
  #send text to input box. don't forget to use `list()` when sending text
    element$sendKeysToElement(list("Web Scraping"))
  
  #select search button
    element <- driver$findElement(using = "css",'body > div.L3eUgb > div.o3j99.ikrT4e.om7nvf > form > div:nth-child(2) > div.A8SBwf > div.FPdoLc.tfB0Bf > center > input.gNO89b')
    element$clickElement()
    
  
#Clear input box
  driver$navigate("https://www.google.com/")
    
  #selcet input box
    element <- driver$findElement(using = "css",'input[name="q"]')
    element$sendKeysToElement(list("Web Scraping"))
    
  #clear input box
    element$clearElement()
    
    
#Save current page
    driver$navigate("https://www.google.com/")
    page <- driver$getPageSource() #Save page to R dataset 
    page <- page %>% unlist() %>% read_html() # save web page as searchable element
    
    
#Pause between steps
    Sys.sleep(1)
    Sys.sleep(rnorm(1, 4.5, 0.1))
