# 
# Big Data y Machine Learning para Economia Aplicada
# Maria Camila Arias, Martin Velasquez, Mario Velasquez, Daniela Vlasak
# Problem Set 1
# 

# Initial Setup -----------------------------------------------------------

if(!require(pacman)) install.packages("pacman") ; require(pacman)
library(tidyverse)

p_load(tidyverse, # contiene las librerías ggplot, dplyr...
       rvest) # web-scraping
vignette("rvest")


p_load(rio, # import/export data
       tidyverse, # tidy-data
       skimr, # summary data
       gridExtra, ## visualizing missing data
       corrplot, ## Correlation Plots 
       stargazer, ## tables/output to TEX. 
       MASS)   

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




# Variables and Descriptive Statistics ------------------------------------
## Filtro sólo a los empleados mayores de 18 años
bd <- bd %>% 
  filter(bd, age>=18, dsi==0)





# Age-wage Profile --------------------------------------------------------






# The Gender Earnings GAP -------------------------------------------------


