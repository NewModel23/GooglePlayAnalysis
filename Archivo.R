
  setwd("C:/TEMPORAL")
  
  
  getwd()
  
  list.files(getwd())

# Check if we have the libraries installed
  packages <- c("xlsx","readr","tidyverse", "sqldf","stringr")
  
  if (length(setdiff(packages, rownames(installed.packages()))) > 0)
  {install.packages(setdiff(packages, rownames(installed.packages())))}
  
  library(xlsx)
  library(readr)
  library(tidyverse)
  library(sqldf)
  library(stringr)
  

  
# Read data
  # Note that there is a value on Reviews that not match with the column type, thats why i use col_types = cols(.default = "c")
  # Because it will return a NA instead of "3.0M"
  
  data1 <- read_csv("googleplaystore.csv",  col_types = cols(.default = "c"))

  
# See the contents of Data Frame
  View(data1)

  
# Data Cleaning
  
  # Replace "3.0M" for 3000000
  data1$Reviews <- gsub("3.0M","3000000",data1$Reviews)
  data1$Reviews <- as.numeric(as.character(data1$Reviews))
  
  
  # Replace "1.9" for "No category"
  data1$Category <- gsub("1.9", "No category", data1$Category)
  

  
# See the top of categories with most reviews
  
  # Create a Data frame to plotting Category vs Reviews
    CatvsReviews <- sqldf("Select
                          Category, 
                          COUNT(reviews) Reviews
                          from data1 
                          group by Category
                          order by reviews desc")
    View(CatvsReviews)
  
  # See it in plot
      CatvsReviews %>%
        arrange(desc(Reviews)) %>%
        head(10) %>%
        ggplot(aes(x=Category, y=Reviews)) +
        geom_bar(stat="identity") +
        labs(x="Categories", y="Reviews") +
        theme(axis.text.x = element_text(angle=0))
        
    
  
  

