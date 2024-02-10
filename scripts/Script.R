# 
# Big Data y Machine Learning para Economia Aplicada
# Maria Camila Arias, Martin Velasquez, Mario Velasquez, Daniela Vlasak
# Problem Set 1
# 

# Initial Setup -----------------------------------------------------------

if(!require(pacman)) install.packages("pacman") ; require(pacman)
library(tidyverse)


p_load(rio, # import/export data
       tidyverse, # tidy-data
       skimr, # summary data
       gridExtra, ## visualizing missing data
       corrplot, ## Correlation Plots 
       stargazer, ## tables/output to TEX. 
       MASS,
       rvest,
       httr)   

# Initial Data Manipulation -----------------------------------------------

my_url="https://ignaciomsarmiento.github.io/GEIH2018_sample/"
browseURL(my_url)
my_html = read_html(my_url)

## I get the links of each chunk of data
links<- my_html %>%
  html_elements("ul") %>%
  html_elements("a") %>% 
  html_attr("href") %>%
  keep( ~ grepl("page",.))
## I paste the first part of the address to the links
links<- paste0(my_url, links)


## I want to open each page and extract their table
## I create a function to fetch HTML content from a url I give as input
fetch_html <- function(url) {
  read_html(url)
}

## Now I loop through each link and fetch the HTML content
html_content_list <- lapply(links,fetch_html)

##Since the tables cmes from another HTML page, I
## make a GET request to the URL of the secodnary web page
response <- GET("https://ignaciomsarmiento.github.io/GEIH2018_sample/pages/geih_page_1.html", verbose())

# Extract the HTTP status code from the response headers
status_code <- httr::status_code(response)
print(status_code)

# Check if the request was successful
if (status_code == 200) {
  # Parse the HTML content of the response
  parsed_html <- read_html(content(response, "text"))
  
  # Use CSS selectors or XPath expressions to locate the table element
  # For example, if the table has an id "my_table", you can use:
   #table_element <- html_node(parsed_html, "#tableHTML_rownames")
  
  # Extract the table data
   table_data <- html_table(parsed_html)
  # or use html_nodes followed by html_text to extract specific elements
  
} else {
  cat("Error: Request failed with status code", status_code, "\n")
}

# I convert the list to a data frame
bd <- bind_rows(table_data)




# Variables and Descriptive Statistics ------------------------------------
## Filtro sólo a los empleados mayores de 18 años
bd <- bd %>% 
  filter(bd, age>=18, dsi==0)





# Age-wage Profile --------------------------------------------------------






# The Gender Earnings GAP -------------------------------------------------


