# 
# Big Data y Machine Learning para Economia Aplicada
# Maria Camila Arias, Martin Velasquez, Mario Velasquez, Daniela Vlasak
# Problem Set 1
# 

# Initial Setup -----------------------------------------------------------

rm(list = ls())
if(!require(pacman)) install.packages("pacman") ; require(pacman)


p_load(rio, # import/export data
       tidyverse, # tidy-data
       skimr, # summary data
       gridExtra, ## visualizing missing data
       corrplot, ## Correlation Plots 
       stargazer, ## tables/output to TEX. 
       MASS,
       rvest,
       httr,
       dplyr,
       ggplot2,
       visdat)   

# Initial Data Manipulation -----------------------------------------------


##Since the tables come from another HTML page, I
## call them from the URL of the secodnary web page

data_list <- list()
for (i in 1:10){
  print(i)
  url <- paste0("https://ignaciomsarmiento.github.io/GEIH2018_sample/pages/geih_page_",i,".html")
  webpage <- read_html(url)
  table <- html_table(webpage)[[1]]
  data_list[[i]] <- table
}

geih <- bind_rows(data_list)

# Variables and Descriptive Statistics ------------------------------------
bd <- as_tibble(geih)
## Filtro sólo a los empleados mayores de 18 años
bd <- bd %>% 
 dplyr::filter(age >= 18 & dsi == 0) ##Here I reduce my sample from 32177 to 22640

ggplot(bd, aes(x = ocu, y = dsi)) +
  geom_point()



## I keep only the variables of my interest:
  ## y_ingLab_m_ha: the variable I want to predict
  ## age: my predictor for the point 2 and one of my sample criteria
  ## sex: my predictor for the point 3
  ## dsi: it's one of my sample criteria
  ## ocu, estrato1, oficio, formal, informal, ingtot, ingtotes, y_ingLab_m, maxEducLevel: other variables
    ## I consider useful for the prediction in point 4
bd <- bd %>% dplyr::select(y_ingLab_m_ha, age, sex, dsi, ocu, 
                           estrato1, oficio, formal, informal, 
                           ingtot, ingtotes, y_ingLab_m, maxEducLevel)

## Here I detect the missing values of the data
bd_miss <- skim(bd) %>%
  dplyr::select(skim_variable, n_missing)

nobs=nrow(bd)

bd_miss <- bd_miss %>%   mutate(p_missing= n_missing/nobs)

bd_miss <- bd_miss %>%   arrange(-n_missing)
bd_miss ## I see 56% of my dependent variable is missing, the same for the monthly

vis_miss(bd)

solo_desocu <- bd %>% filter(ocu==0)
vis_miss(solo_desocu) ## Si no están ocupados no tienen ingresos, ni formal/informal
## Entonces podemos igualar esos ingresos a 0:
## y_ingLab_m_ha, oficio, formal, informal, y_ingLab_m = 0






# Age-wage Profile --------------------------------------------------------






# The Gender Earnings GAP -------------------------------------------------


