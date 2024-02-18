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
       visdat,
       caret)   # For predictive model assessment

# 1: Initial Data Manipulation -----------------------------------------------
set.seed(999)

##Since the tables come from another HTML page, I
## call them from the URL of the secondary web page

data_list <- list()
for (i in 1:10){
  print(i)
  url <- paste0("https://ignaciomsarmiento.github.io/GEIH2018_sample/pages/geih_page_",i,".html")
  webpage <- read_html(url)
  table <- html_table(webpage)[[1]]
  data_list[[i]] <- table
}

geih <- bind_rows(data_list)

# 2: Variables and Descriptive Statistics ------------------------------------
bd <- as_tibble(geih)
## Filtro sólo a los empleados mayores de 18 años
bd <- bd %>% 
 dplyr::filter(age >= 18 & ocu == 1) ##Here I reduce my sample from 32177 to 22640

## I keep only the variables of my interest:
  ## y_salary_m_hu/y_ingLab_m_ha: the variable I want to predict
  ## age: my predictor for the point 2 and one of my sample criteria
  ## sex: my predictor for the point 3
  ## dsi: it's one of my sample criteria
  ## ocu, estrato1, oficio, formal, informal, ingtot, ingtotes, y_ingLab_m, maxEducLevel: other variables
    ## I consider useful for the prediction in point 4
bd <- bd %>% dplyr::select(y_salary_m_hu, age, 
                           sex, dsi, ocu, estrato1, oficio, 
                           formal, informal, ingtot, ingtotes, 
                           y_ingLab_m, maxEducLevel, cuentaPropia, 
                           hoursWorkUsual, inac, y_ingLab_m)


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
bd <- bd %>% mutate(y_salary_m_hu = ifelse(ocu==0, 0, y_salary_m_hu))
bd <- bd %>% mutate(y_ingLab_m_ha = ifelse(ocu==0, 0, y_ingLab_m_ha))
vis_miss(bd) ## Now missings are reduced to 29%

## 29% It's still a high proportion.
## I will impute the rest using the hourly wage by estrato1

# Distribution of hourly wage
ggplot(bd, aes(y_salary_m_hu)) +
  geom_histogram(color = "#000000", fill = "#0099F8") +
  geom_vline(xintercept = median(bd$y_salary_m_hu, na.rm = TRUE), linetype = "dashed", color = "red") +
  geom_vline(xintercept = mean(bd$y_salary_m_hu, na.rm = TRUE), linetype = "dashed", color = "blue") +  
  ggtitle("Salario horario") +
  theme_classic() +
  theme(plot.title = element_text(size = 18))

## Since the graph shows me a long right tail, I will impute with the 
## median to avoid an overestimation of hourly wage imputations.
bd <- bd %>% mutate(y_salary_m_hu = ifelse(is.na(y_salary_m_hu)== TRUE, median(bd$y_salary_m_hu, na.rm = TRUE), 
                                           y_salary_m_hu))
vis_miss(bd) ## No missings


#Esto me sirve, por ahora, para las regresiones del punto 4.
  # Aca cambié y_salary_m a y_salary_m_hu porque se dropeo antes y no corria

bd <- bd %>%
  filter(!is.na(y_salary_m_hu), !is.na(age),!is.na(cuentaPropia), !is.na(formal), !is.na(hoursWorkUsual), 
         !is.na(inac), !is.na(maxEducLevel), !is.na(oficio))


# 3: Age-wage Profile --------------------------------------------------------

# Creo que no es y_ingLab_m sino y_salary_m_hu (o y_ingLab_m_ha), deberiamos ponernos de acuerdo
modelo_punto_3 <- lm(log(y_ingLab_m) ~ age + I(age^2), data = bd)

stargazer(modelo_punto_3, title="Regresión de salario contra edad", type="latex", out="regresion_punto_3.tex")
stargazer(modelo_punto_3, title="Regresión de salario contra edad - texto", type="text")

# Funcion del "peak-age"
bootstrap_peak_age <- function(bd, indices) {
  sampled_data <- bd[indices, ]
  modelo_boot <- lm(log(y_ingLab_m) ~ age + I(age^2), data = sampled_data)
  
  # Calculate peak age using the formula: -b1 / (2 * b2)
  peak_age <- -coef(modelo_boot)[2] / (2 * coef(modelo_boot)[3])
  return(peak_age)
}

bootstrap_peak_age_results <- boot(bd, bootstrap_peak_age, R = 1000)


peak_age_conf_intervals <- boot.ci(bootstrap_peak_age_results, type = "bca")

hist(bootstrap_peak_age_results$t, main = "Distribucion de 'Peak Age'", xlab = "Peak Age", ylab = "Frecuencia", col = "lightblue", border = "black")

abline(v = peak_age_conf_intervals$bca[4], col = "red", lty = 2)
abline(v = peak_age_conf_intervals$bca[5], col = "red", lty = 2)



# 4: The Gender Earnings GAP -------------------------------------------------

#In the regression, female = 1 so I edit bd so that female = 1 and male = 0

bd$sex <- 1 - bd$sex

# a) Estimating the unconditional wage gap

gap_lm_hourly <- lm(log(y_salary_m_hu)~ sex, data = bd)
stargazer(gap_lm_hourly, type = "text")

# b) Part i. Conditional age gap incorporating controls like age, cuentaPropia, formal, hoursWorkUsual, inac, maxEducLevel, oficio


bd$maxEducLevel.f <- factor(bd$maxEducLevel) #Converts variable to factor
bd$oficio.f <- factor(bd$oficio) #Converts variable to factor

# When applying FWL, the first vector of explanatory variables (#X_1) will only contain the variable female


# First, perform the regression without the FWL method to know which results should we expect from the FWL process since outcomes are the same
results <- lm(log(y_salary_m_hu) ~ sex + poly(age,2, raw = TRUE) + cuentaPropia + formal + hoursWorkUsual + inac + maxEducLevel.f + oficio.f, data = bd)
stargazer(results, type = "text")


#Step 1 from the FWL process. Regress all the X_2 variables against X_1 (sex)

bd <- bd %>%
  mutate(femaleResidControls = lm(sex ~ poly(age,2, raw = TRUE) + cuentaPropia + formal + hoursWorkUsual + 
                                    inac + maxEducLevel.f + oficio.f, data = .)$residuals)


#Setp 2 from the FWL process. Regress the outcome variable against the X_2 variables.

bd <- bd %>%
  mutate(log_salaryResidControls = lm(log(y_salary_m_hu) ~ poly(age,2, raw = TRUE) + cuentaPropia + formal + 
                                        hoursWorkUsual + inac + maxEducLevel.f + oficio.f, 
                                      data = .)$residuals)

#Step 3 from the FWL process. Regress the residuals from step 2 as the result variable against the residuals from step 1.

reg_res <- lm(log_salaryResidControls ~ femaleResidControls, data = bd)
stargazer(results, reg_res,type="text",digits=7)

#Verificar que la suma de residuales de igual
sum(resid(results)^2)
sum(resid(reg_res)^2)

# b) Part ii. Using FWL with bootstrap

B <- 1000
eta_mod1 <- rep(0,B)
bd_copy <- bd

for(i in 1:B){
  
  bd_sample<- sample_frac(bd_copy,size=1,replace=TRUE) #takes a sample with replacement of the same size of the original sample (1 or 100%)
  
  bd <- bd %>%
    mutate(reg_1_bootstrap = lm(sex ~ poly(age,2, raw = TRUE) + cuentaPropia + formal + hoursWorkUsual + 
                                  inac + maxEducLevel.f + oficio.f, data = .)$residuals)
  
  bd <- bd %>%
    mutate(reg_2_bootstrap = lm(log(y_salary_m_hu) ~ poly(age,2, raw = TRUE) + cuentaPropia + formal + 
                                  hoursWorkUsual + inac + maxEducLevel.f + oficio.f, 
                                data = .)$residuals)
  
  reg_3_bootstrap <- lm(reg_2_bootstrap ~ reg_1_bootstrap, data = bd_sample)
  
  coefs<-reg_3_bootstrap$coefficients[2] # gets the coefficient of interest that coincides with the elasticity of demand
  
  eta_mod1[i]<-coefs #saves it in the above vector
}

length(eta_mod1)
plot(hist(eta_mod1))
mean(eta_mod1)
sqrt(var(eta_mod1))


# c) Ploting the age-wage profile 



# 5: Predicting earnings------------------------------------

  # a. Splitting sample 70% training 30% testing 

  set.seed(10101)  # Set set for replicability purposes 


inTrain <- createDataPartition(
  y = bd$y_salary_m_hu,  ## the outcome data are needed
  p = .70, ## The percentage of data in the
  list = FALSE
)

training <- bd[ inTrain,]
testing  <- bd[-inTrain,]

# Model 1 - modelo punto 3
# Training
form_1<- log(y_salary_m_hu) ~ age + I(age^2) 


modelo_cv1 <- lm(form_1,
               data = training)
# Prediction
predictions <- predict(modelo_cv1, testing)


score_cv1<- RMSE(predictions, testing$y_salary_m_hu )
score_cv1 # 10584.75


# Model 2 - unconditional wage gap
# Training
form_2<- log(y_salary_m_hu)~ sex

modelo_cv2 <- lm(form_2,
               data = training)
# Prediction
predictions <- predict(modelo_cv2, testing)


score_cv2<- RMSE(predictions, testing$y_salary_m_hu )
score_cv2 # 10584.76

  # b. Comparing models based on RMSE

# para por ahora
scores_cv<- data.frame( Model= c(1, 2),
                     RMSE_vsa= c(score_cv1, score_cv2)
)


scores_cv<- data.frame( Model= c(1, 2, 3, 4, 5, 6, 7),
                     RMSE_vsa= c(score_cv1, score_cv2, score_cv3, score_cv4, 
                                 score_cv5, score_cv6, score_cv7))

head(scores_cv)

  # c. Comments on results 

  # d. LOOCV for the two models with the lowest RMSE

ctrl <- trainControl(
  method = "LOOCV") 

modelo_loocv1 <- train(FALTA DEFINIR,
                  data = db,
                  method = 'lm', 
                  trControl= ctrl)

score_loocv1<-RMSE(modelo1c$pred$pred, db$totalHoursWorked)



modelo_loocv2 <- train(FALTA DEFINIR,
                       data = db,
                       method = 'lm', 
                       trControl= ctrl)

score_loocv2<-RMSE(modelo1c$pred$pred, db$totalHoursWorked)






