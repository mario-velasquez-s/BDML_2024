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
set.seed(999)

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


#Esto me sirve, por ahora, para las regresiones del punto 4.
bd <- bd %>%
  filter(!is.na(y_salary_m), !is.na(age),!is.na(cuentaPropia), !is.na(formal), !is.na(hoursWorkUsual), 
         !is.na(inac), !is.na(maxEducLevel), !is.na(oficio))



# Age-wage Profile --------------------------------------------------------






# The Gender Earnings GAP -------------------------------------------------

#In the regression, female = 1 so I edit db so that female = 1 and male = 0
bd$sex <- 1 - bd$sex

#Otras posibles variables dependientes: y_ingLab_m  y_ingLab_m_ha
#Estimating the unconditional wage gap
gap_lm_monthly <- lm(log(y_salary_m)~ sex, data = bd)
gap_lm_hourly <- lm(log(y_salary_m_hu)~ sex, data = bd)
stargazer(gap_lm_monthly, gap_lm_hourly, type = "text")


#Controles importantes: age, cuentaPropia, formal, hoursWorkUsual, inac, maxEducLevel, oficio
bd$maxEducLevel.f <- factor(bd$maxEducLevel)
bd$oficio.f <- factor(bd$oficio)

#X_1 will be the variable female
#Expected results
results <- lm(log(y_salary_m) ~ sex + age + cuentaPropia + formal + hoursWorkUsual + inac + maxEducLevel.f + oficio.f, data = bd)
stargazer(results, type = "text")


#Regress all the variables in X1 on X2 and take the resuduals
#El siguiente se debería poder correr cuando se limpien los datos
#bd <- bd %>% mutate(femaleResidControls = lm(sex ~ cuentaPropia + dsi + formal + hoursWorkUsual + inac + maxEducLevel.f + oficio.f, data = bd)$residuals)

#Este sirve para datos no limpios
bd <- bd %>%
  mutate(femaleResidControls = lm(sex ~ age + cuentaPropia + formal + hoursWorkUsual + 
                                    inac + maxEducLevel.f + oficio.f, data = .)$residuals)


#Este también sirve para datos no limpios
bd <- bd %>%
  mutate(log_salaryResidControls = lm(log(y_salary_m) ~ age + cuentaPropia + formal + 
                                        hoursWorkUsual + inac + maxEducLevel.f + oficio.f, 
                                      data = .)$residuals)


#Regresión de residuales
reg_res <- lm(log_salaryResidControls ~ femaleResidControls, data = bd)
stargazer(results, reg_res,type="text",digits=7)

#Verificar que la suma de residuales de igual
sum(resid(results)^2)
sum(resid(reg_res)^2)

### Using FWL with boothstrap (Not finished)

B <- 1000
eta_mod1 <- rep(0,B)

for(i in 1:B){
  
  bd_sample<- sample_frac(bd,size=0.5,replace=TRUE) #takes a sample with replacement of the same size of the original sample (1 or 100%)
  
  bd <- bd %>%
    mutate(reg_1_bootstrap = lm(sex ~ cuentaPropia + dsi + formal + hoursWorkUsual + 
                                      inac + maxEducLevel.f + oficio.f, data = bd)$residuals)
  
  bd <- bd %>%
    mutate(reg_2_bootstrap = lm(log(y_salary_m) ~ cuentaPropia + dsi + formal + 
                                          hoursWorkUsual + inac + maxEducLevel.f + oficio.f, 
                                        data = bd)$residuals)
  
  reg_3_bootstrap <- lm(reg_2_bootstrap ~ reg_1_bootstrap, data = bd)
  
  coefs<-reg_3_bootstrap$coefficients[2] # gets the coefficient of interest that coincides with the elasticity of demand
  
  eta_mod1[i]<-coefs #saves it in the above vector
}

length(eta_mod1)
plot(hist(eta_mod1))
mean(eta_mod1)
sqrt(var(eta_mod1))
