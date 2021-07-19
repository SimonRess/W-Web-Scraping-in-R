#Package: ralger
#Link: https://github.com/feddelegrand7/ralger

  if(!require("ralger")) install.packages("ralger")
  library(ralger)
  
  #scrap()
    #scrap(link = <link>, node = <node>)
  
      #Example I:
        my_link <- "http://www.shanghairanking.com/ARWU2020.html"
        
        my_node <- "#UniversityRanking a" # The ID HTML attribute, SelectorGadget extension is recommanded
        
        best_uni <- scrap(link = my_link, node = my_node, askRobot = TRUE)
        
        head(best_uni)
        
      #Example II (scraping several urls):
        base_link <- "https://global.rstudio.com/student/catalog/list?category_ids=1796-speakers&page="
        
        links <- paste0(base_link, 1:3) # the speakers are listed from page 1 to 3
        
        node <- ".pr-1" #usage of "." because it's a class
        
        speakers <- scrap(links, node)
        head(speakers, 10)
      
        
  #table_scrap() - extract an HTML Table
      data <- table_scrap(link ="https://www.boxofficemojo.com/chart/top_lifetime_gross/?area=XWW")
      
      head(data)
      
      
  #tidy_scrap() - extract information in a tabular manner while these information are not provided in an HTML-table format
      
      my_link <- "https://www.imdb.com/search/title/?groups=top_250&sort=user_rating"
      
      my_nodes <- c(
        ".lister-item-header a", # The title 
        ".text-muted.unbold", # The year of release #one identifying contiguous string
        ".ratings-imdb-rating strong" # The rating) #one identifying contiguous string
      )
      
      names <- c("title", "year", "rating") # respect the nodes order
      
      tidy_scrap(link = my_link, nodes = my_nodes, colnames = names)
      
      
  #titles_scrap() - scrape titles which correspond to the h1, h2 & h3 HTML tags
      
      titles_scrap(link = "https://www.nytimes.com/")
      titles_scrap(link = "https://www.nytimes.com/", contain = "TrUMp", case_sensitive = FALSE)
      
      
  #paragraphs_scrap() - extract paragraphs. This function relies on the p HTML tag
      paragraphs_scrap(link = "https://ropensci.org/")
      paragraphs_scrap(link = "https://ropensci.org/", collapse = TRUE) # collapse the paragraphs into one bag of words
      
      
  #weblink_scrap() - scrape the web links available within a web page
      weblink_scrap(link = "https://www.worldbank.org/en/access-to-information/reports/", 
                    contain = "PDF", 
                    case_sensitive = FALSE)
      
      
  #images_preview() - scrape the URLs of the images available within a web page
      images_preview(link = "https://rstudio.com/")
      
  #images_scrap() - download the images available within a web page
      images_scrap(link = "https://rstudio.com/", 
                   imgpath = here::here("my_images"), 
                   extn = "png") # without the .
      