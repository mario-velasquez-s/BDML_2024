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
       MASS)   



# Initial Data Manipulation -----------------------------------------------

bd <- import("https://ignaciomsarmiento.github.io/GEIH2018_sample/")




# Variables and Descriptive Statistics ------------------------------------
## Filtro sólo a los empleados mayores de 18 años
bd <- bd %>% 
  filter(bd, age>=18, dsi==0)





# Age-wage Profile --------------------------------------------------------






# The Gender Earnings GAP -------------------------------------------------


