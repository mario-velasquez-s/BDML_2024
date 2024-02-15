# 
# Big Data y Machine Learning para Economia Aplicada
# Maria Camila Arias, Martin Velasquez, Mario Velasquez, Daniela Vlasak
# Problem Set 1
# 

# Initial Setup -----------------------------------------------------------

rm(list = ls())
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

bd <- bind_rows(data_list)

# Variables and Descriptive Statistics ------------------------------------
## Filtro sólo a los empleados mayores de 18 años
bd <- bd %>% 
  filter(bd, age>=18, dsi==0)





# Age-wage Profile --------------------------------------------------------






# The Gender Earnings GAP -------------------------------------------------

#In the regression, female = 1 so I edit db so that female = 1 and male = 0
bd$sex <- 1 - bd$sex

#Otras posibles variables dependientes: y_ingLab_m  y_ingLab_m_ha
#Estimating the unconditional wage gap
gap_lm_monthly <- lm(log(y_salary_m)~ sex, data = bd)
gap_lm_hourly <- lm(log(y_salary_m_hu)~ sex, data = bd)
stargazer(gap_lm_monthly, gap_lm_hourly, type = "text")


#Controles importantes: cuentaPropia,  dsi, formal, hoursWorkUsual, inac, maxEducLevel, oficio
summary(bd$oficio)
bd$maxEducLevel.f <- factor(bd$maxEducLevel)
bd$oficio.f <- factor(bd$oficio)

#Will finish later today###
gap_lm_controsl <- lm()
