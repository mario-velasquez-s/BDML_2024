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
       caret,
       xtable)   # For predictive model assessment

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

# 2: Data Cleaning -----------------------------------------------------------
bd <- as_tibble(geih)
## Filtro sólo a los empleados mayores de 18 años
bd <- bd %>% 
 dplyr::filter(age >= 18 & ocu == 1) ##Here I reduce my sample from 32177 to 16542 obs

## I keep only the variables of my interest:
  ## y_salary_m_hu: the variable I want to predict
  ## age: my predictor for exercise 2 and one of my sample criteria
  ## sex: my predictor for exercise 3
  ## ocu: it's one of my sample criteria
  ## estrato1, oficio, formal, informal, maxEducLevel, cuentaPropia, 
  ## hoursWorkUsual, inac: other variables
    ## I consider useful for the prediction in point 4
bd <- bd %>% dplyr::select(y_salary_m_hu, age, 
                           sex, estrato1, oficio, 
                           formal, 
                           maxEducLevel, cuentaPropia, 
                           hoursWorkUsual, inac, p6426)




without_imputation <- bd
df_without_imputation <- na.omit(bd)
## Here I detect the missing values of the data
bd_miss <- skim(bd) %>%
  dplyr::select(skim_variable, n_missing)

nobs=nrow(bd)

bd_miss <- bd_miss %>%   mutate(p_missing= n_missing/nobs)

bd_miss <- bd_miss %>%   arrange(-n_missing)
bd_miss ## I see 40% of my dependent variable is missing, and there is one missing for the maxEducLevel
vis_miss(bd)

## First, we will drop the only observation with missing maxEducLevel
bd <- bd %>% filter(!is.na(maxEducLevel))

## Now, we will impute the hourly wage

# Distribution of hourly wage
impu_1 <- ggplot(bd, aes(y_salary_m_hu)) +
  geom_histogram(color = "#000000", fill = "#0099F8") +
  geom_vline(xintercept = median(bd$y_salary_m_hu, na.rm = TRUE), linetype = "dashed", color = "red") +
  geom_vline(xintercept = mean(bd$y_salary_m_hu, na.rm = TRUE), linetype = "dashed", color = "blue") +
  theme_classic() +
  theme(plot.title = element_text(size = 18))
#ggsave("C:/Users/Maria.Arias/OneDrive - Universidad de los andes/MSc Economics/Big Data & Machine Learning/Problem set 1/BDML_2024/views/imputation/distri_wage.pdf", impu_1)

#Distribution of hourly wage with the mean by sex
ggplot(bd) +
  geom_histogram(mapping=aes(x=y_salary_m_hu, group=as.factor(sex),fill=as.factor(sex))) +
  geom_vline(xintercept = mean((bd%>%filter(sex==0))$y_salary_m_hu, na.rm = TRUE), linetype = "dashed", color = "#B55049") +
  geom_vline(xintercept = mean((bd%>%filter(sex==1))$y_salary_m_hu, na.rm = TRUE), linetype = "dashed", color = "#358291") +
  theme_classic() +
  theme(plot.title = element_text(size = 18)) +
  scale_fill_manual(values = c("0"="#F36A60" , "1"="#46B4CA"), labels = c("0"="Mujer", "1"="Hombre"), name="Sexo")

## The graph shows me a long right tail and, 
## the two variables we will use to predict are sex and age.
## According to the literature in Colombia, the social strata and
## the hourly wage are strongly correlated.

impu_2 <- ggplot(data=bd, mapping = aes(as.factor(estrato1), y_salary_m_hu)) +
  geom_boxplot() +
  xlab("Estrato") + 
  ylab("Salario horario") +
  theme_bw()
#ggsave("C:/Users/Maria.Arias/OneDrive - Universidad de los andes/MSc Economics/Big Data & Machine Learning/Problem set 1/BDML_2024/views/imputation/wage_estrato1.pdf", impu_2)

## In addition, the social strata (estrato1) are independent to sex and age.
estrato_orto <- lm(estrato1 ~ age + as.factor(sex), bd)
summary(estrato_orto) ## Coeficientes muy cercanos a 0.
impu_3<-stargazer(estrato_orto, type="latex", title="Relación estrato vs edad y sexo")
#write(impu_3, file="C:/Users/Maria.Arias/OneDrive - Universidad de los andes/MSc Economics/Big Data & Machine Learning/Problem set 1/BDML_2024/views/imputation/estrato1_age_sex.tex")

## Therefore, we will impute with the mean of hourly wage conditioned to the estrato1
## in order to avoid affecting the prediction model.
bd <- bd %>% 
  group_by(estrato1) %>%
  mutate(y_salary_m_hu = if_else(is.na(y_salary_m_hu),mean(y_salary_m_hu, na.rm=TRUE),y_salary_m_hu)) %>%
  ungroup()

## We check again the missing values
bd_miss <- skim(bd) %>%   dplyr::select(skim_variable, n_missing)
nobs=nrow(bd)
bd_miss <- bd_miss %>%   mutate(p_missing= n_missing/nobs)
bd_miss <- bd_miss %>%   arrange(-n_missing)
bd_miss ## No missings.


# 2a: Variables and Descriptive Statistics ------------------------------------
names(bd)

##Summary of all variables
des_vars <- c("age", "sex", "hoursWorkUsual", "formal", "cuentaPropia")
stargazer::stargazer(as.data.frame(bd[,des_vars]), type="latex", title="Descriptivas de las variables explicatorias",
                     out="C:/Users/Maria.Arias/OneDrive - Universidad de los andes/MSc Economics/Big Data & Machine Learning/Problem set 1/BDML_2024/views/descriptive/descriptivas_numericas.tex")

## Graph to describe "maxEducLevel", "estrato1"
bd$maxEducLevel <- factor(bd$maxEducLevel, levels = c(1,2,3,4,5,6,7,9), 
                          labels = c("Ninguno", "Pre-escolar", "Primaria incompleta", "Primaria completa",
                                     "Secundaria incompleta", "Secundaria completa","Terciaria","N/A"))

des_2 <- ggplot(bd, aes(x=maxEducLevel)) + 
  geom_bar(fill="#0099F8") +
  labs(x="Máximo nivel de educación alcanzado",
         y= "Cantidad") + 
  theme_bw() ## Esta distribución parecería atípica, pero como nuestra muestra sólo contiene 
            ## personas ocupadas, puede que tenga sentido. Completar con estadísticas laborales en documento.
ggsave("C:/Users/Maria.Arias/OneDrive - Universidad de los andes/MSc Economics/Big Data & Machine Learning/Problem set 1/BDML_2024/views/descriptive/descriptiva_maxEduc.pdf", des_2)

des_3 <- ggplot(bd, aes(x=as.factor(estrato1))) + 
  geom_bar(fill="#0099F8") +
  labs(x="Estrato de energía",
       y= "Cantidad") + 
  theme_bw()
ggsave("C:/Users/Maria.Arias/OneDrive - Universidad de los andes/MSc Economics/Big Data & Machine Learning/Problem set 1/BDML_2024/views/descriptive/descriptiva_estrato.pdf", des_3)


## Graph of why to transform wage to ln(wage)
bd$ln_wage <- log(bd$y_salary_m_hu)
des_4 <- ggplot(bd, aes(x=ln_wage)) +
  geom_histogram(fill="#0099F8") +
  labs(x="ln(salario horario)", y="Frecuencia") +
  theme_bw()
ggsave("C:/Users/Maria.Arias/OneDrive - Universidad de los andes/MSc Economics/Big Data & Machine Learning/Problem set 1/BDML_2024/views/descriptive/descriptiva_ln_salario.pdf", des_4)

## Graph of age vs wage
bd$sex <- factor(bd$sex, levels=c(0,1), labels = c("Mujer", "Hombre"))
des_5 <- ggplot(bd) + 
  geom_point(mapping = aes(x=age, y=ln_wage, color=as.factor(sex))) + 
  geom_smooth(mapping = aes(x=age, y=ln_wage)) +
  labs(x="Edad", y="ln(salario horario)") + 
  theme_bw()
ggsave("C:/Users/Maria.Arias/OneDrive - Universidad de los andes/MSc Economics/Big Data & Machine Learning/Problem set 1/BDML_2024/views/descriptive/descriptiva_salario_predictores.pdf", des_5)




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

#In the regression, women = 1 so I edit bd so that women = 1 and man = 0

bd$sex <- 1 - bd$sex
df_without_imputation$sex <- 1 - df_without_imputation$sex

# a) Estimating the unconditional wage gap

gap_lm_hourly <- lm(log(y_salary_m_hu)~ sex, data = bd)
gap_lm_hourly_ni <- lm(log(y_salary_m_hu)~ sex, data = df_without_imputation)
stargazer(gap_lm_hourly, type = "text")
stargazer(gap_lm_hourly, gap_lm_hourly_ni, type = "latex")

# b) Part i. Conditional age gap incorporating controls like age, cuentaPropia, formal, hoursWorkUsual, inac, maxEducLevel, oficio

bd$maxEducLevel.f <- factor(bd$maxEducLevel) #Converts variable to factor
bd$oficio.f <- factor(bd$oficio) #Converts variable to factor

#T-test to look at the relevance of the controls
control_variables <- setdiff(names(bd), c("sex", "maxEducLevel.f", "oficio.f"))
t_test_results <- data.frame(variable = character(), p_value = numeric(), stringsAsFactors = FALSE)
for (var in control_variables) {
  t_test_result <- t.test(bd[bd$sex == 1, var], bd[bd$sex == 0, var])
  t_test_results <- rbind(t_test_results, data.frame(variable = var, p_value = t_test_result$p.value))
}

t_test_results_raw <- data.frame(variable = character(), p_value = numeric(), stringsAsFactors = FALSE)
for (var in control_variables) {
  t_test_result_raw <- t.test(df_without_imputation[df_without_imputation$sex == 1, var], df_without_imputation[df_without_imputation$sex == 0, var])
  t_test_results_raw <- rbind(t_test_results_raw, data.frame(variable = var, p_value = t_test_result_raw$p.value))
}

print(t_test_results)
print(t_test_results_raw)

latex_table <- xtable(t_test_results)
latex_table_raw <- xtable(t_test_results_raw)
# Print the LaTeX code
print(latex_table, include.rownames = FALSE)
print(latex_table_raw, include.rownames = FALSE)

# When applying FWL, the first vector of explanatory variables (#X_1) will only contain the variable female


# First, perform the regression without the FWL method to know which results should we expect from the FWL process since outcomes are the same
results <- lm(log(y_salary_m_hu) ~ sex + poly(age,2, raw = TRUE) + cuentaPropia + formal + hoursWorkUsual + maxEducLevel.f + oficio.f, data = bd)
stargazer(results, type = "text")

bd<- bd %>% mutate(leverage = hatvalues(results))
bd<- bd %>% mutate(residuals= results$residuals)

ggplot(bd , aes(y = leverage , x = residuals  )) +
  geom_point() + # add points
  theme_bw() + #black and white theme
  labs(x = "Residuales",  
       y = "Leverage",
       title = "") # labels
p <- mean(bd$leverage)
cutt <- 3*p
bd_no_leverage <-  bd %>% 
  dplyr:: filter(leverage<= cutt)

results_no_leverage <- lm(log(y_salary_m_hu) ~ sex + poly(age,2, raw = TRUE) + cuentaPropia + formal + hoursWorkUsual + maxEducLevel.f + oficio.f, data = bd_no_leverage)


#Step 1 from the FWL process. Regress all the X_2 variables against X_1 (sex)

bd <- bd %>%
  mutate(femaleResidControls = lm(sex ~ poly(age,2, raw = TRUE) + cuentaPropia + formal + hoursWorkUsual + maxEducLevel.f + oficio.f, data = .)$residuals)


#Setp 2 from the FWL process. Regress the outcome variable against the X_2 variables.

bd <- bd %>%
  mutate(log_salaryResidControls = lm(log(y_salary_m_hu) ~ poly(age,2, raw = TRUE) + cuentaPropia + formal + 
                                        hoursWorkUsual + maxEducLevel.f + oficio.f, 
                                      data = .)$residuals)

#Step 3 from the FWL process. Regress the residuals from step 2 as the result variable against the residuals from step 1.

reg_res <- lm(log_salaryResidControls ~ femaleResidControls, data = bd)
stargazer(results, reg_res,type="text",digits=7)
summary_table <- stargazer(results, results_no_leverage, reg_res,type="text",digits=4)

#Verificar que la suma de residuales de igual
sum(resid(results)^2)
sum(resid(reg_res)^2)

# b) Part ii. Using FWL with bootstrap

B <- 1000
eta_mod1 <- rep(0,1000)

for(i in 1:B){
  
  bd_sample<- sample_frac(bd,size=1,replace=TRUE) #takes a sample with replacement of the same size of the original sample (1 or 100%)
  
  bd <- bd %>%
    mutate(reg_1_bootstrap = lm(sex ~ poly(age,2, raw = TRUE) + cuentaPropia + formal + hoursWorkUsual + maxEducLevel.f + oficio.f, data = bd_sample)$residuals)
  
  bd <- bd %>%
    mutate(reg_2_bootstrap = lm(log(y_salary_m_hu) ~ poly(age,2, raw = TRUE) + cuentaPropia + formal + 
                                  hoursWorkUsual + maxEducLevel.f + oficio.f, 
                                data = bd_sample)$residuals)
  
  reg_3_bootstrap <- lm(reg_2_bootstrap ~ reg_1_bootstrap, data = bd_sample)
  
  coefs<-reg_3_bootstrap$coefficients[2] # gets the coefficient of interest that coincides with the elasticity of demand
  
  eta_mod1[i]<-coefs #saves it in the above vector
}

length_eta <- length(eta_mod1)
mean_eta <- mean(eta_mod1)
std_dev_eta <- sd(eta_mod1)

# Create data frame
summary_df <- data.frame(Length = length_eta,
                         Mean = mean_eta,
                         Std_Dev = std_dev_eta)

# Print the data frame
print(summary_df)

hist(eta_mod1, 
     main = "Distribucion de la estimación",
     xlab = "Valores",
     ylab = "Frecuencia",
     col = "skyblue",
     border = "black")


distribution_summary <- xtable(summary_df, digits = 7)
# Print the LaTeX code
print(distribution_summary, include.rownames = FALSE)

# c) Ploting the age-wage profile

# Genera un rango de edades desde la mínima a la máxima encontrada en la variable 'age' de 'bd', con incrementos de 1.
age_range <- seq(min(bd$age), max(bd$age), by = 1)

# Crea un marco de datos 'conditions' con condiciones para diferentes variables.
conditions <- data.frame(
  sex = rep(c(0, 1), each = length(age_range)),  # Valores para sexo (0 y 1), cada uno repetido para cada edad
  age = rep(age_range, 2),                        # Cada edad repetida dos veces, una para cada sexo
  cuentaPropia = median(bd$cuentaPropia),         # Valor medio de cuentaPropia
  formal = median(bd$formal),                    # Valor medio de formal
  hoursWorkUsual = median(bd$hoursWorkUsual),     # Valor medio de hoursWorkUsual
  maxEducLevel.f = 6,                            # Valor fijo para maxEducLevel.f
  oficio.f = 45                                  # Valor fijo para oficio.f
)

# Define niveles para maxEducLevel.f y oficio.f
education_levels <- c(1:7, 9)  # Niveles de educación
oficio_levels <- 1:99          # Niveles para oficio

# Convierte maxEducLevel.f y oficio.f a factores con niveles especificados
conditions$maxEducLevel.f <- factor(conditions$maxEducLevel.f, levels = education_levels)
conditions$oficio.f <- factor(conditions$oficio.f, levels = oficio_levels)

# Realiza predicciones separadas para hombres y mujeres

# Perform predictions with confidence intervals
pred_male <- predict(results, newdata = conditions[conditions$sex == 0,], interval = "confidence")
pred_female <- predict(results, newdata = conditions[conditions$sex == 1,], interval = "confidence")

# Extract upper and lower confidence limits
lower_male <- pred_male[, "lwr"]
upper_male <- pred_male[, "upr"]
lower_female <- pred_female[, "lwr"]
upper_female <- pred_female[, "upr"]

# Plot the scatter plot with confidence intervals
plot(bd$age, log(bd$y_salary_m_hu), col = 'blue', xlab = 'Edad', ylab = 'Log Salario', pch = NA, ylim = c(8,9))
lines(age_range, pred_male[, "fit"], col = 'red', lwd = 2)  # Regression line for males
lines(age_range, pred_female[, "fit"], col = 'green', lwd = 2)  # Regression line for females
lines(age_range, upper_male, col = 'red', lty = 2)  # Upper confidence interval for males
lines(age_range, lower_male, col = 'red', lty = 2)  # Lower confidence interval for males
lines(age_range, upper_female, col = 'green', lty = 2)  # Upper confidence interval for females
lines(age_range, lower_female, col = 'green', lty = 2)  # Lower confidence interval for females

# Add legend (ignoring the label for 'Salarios Reales')
legend('topright', legend = c(NA, 'Línea de Regresión para Hombres', 'Línea de Regresión para Mujeres', 'Intervalo de Confianza para Hombres', 'Intervalo de Confianza para Mujeres'), 
       col = c('blue', 'red', 'green', 'red', 'green'), lty = c(NA, 1, 1, 2, 2), lwd = c(NA, 2, 2, 1, 1))


#Ahora asumiendo, por ejemplo, que se trata de ejecutivos con alto logro educativo

conditions <- data.frame(
  sex = rep(c(0, 1), each = length(age_range)),  # Valores para sexo (0 y 1), cada uno repetido para cada edad
  age = rep(age_range, 2),                        # Cada edad repetida dos veces, una para cada sexo
  cuentaPropia = median(bd$cuentaPropia),         # Valor medio de cuentaPropia
  formal = median(bd$formal),                    # Valor medio de formal
  hoursWorkUsual = median(bd$hoursWorkUsual),     # Valor medio de hoursWorkUsual
  maxEducLevel.f = 7,                            # Educación terciaria
  oficio.f = 21                                  # Ejecutivos o gerentes
)

# Define niveles para maxEducLevel.f y oficio.f
education_levels <- c(1:7, 9)  # Niveles de educación
oficio_levels <- 1:99          # Niveles para oficio

# Convierte maxEducLevel.f y oficio.f a factores con niveles especificados
conditions$maxEducLevel.f <- factor(conditions$maxEducLevel.f, levels = education_levels)
conditions$oficio.f <- factor(conditions$oficio.f, levels = oficio_levels)


# Perform predictions with confidence intervals
pred_male <- predict(results, newdata = conditions[conditions$sex == 0,], interval = "confidence")
pred_female <- predict(results, newdata = conditions[conditions$sex == 1,], interval = "confidence")

# Extract upper and lower confidence limits
lower_male <- pred_male[, "lwr"]
upper_male <- pred_male[, "upr"]
lower_female <- pred_female[, "lwr"]
upper_female <- pred_female[, "upr"]

# Plot the scatter plot with confidence intervals
plot(bd$age, log(bd$y_salary_m_hu), col = 'blue', xlab = 'Edad', ylab = 'Log Salario', pch = NA, ylim = c(8.8,10))
lines(age_range, pred_male[, "fit"], col = 'red', lwd = 2)  # Regression line for males
lines(age_range, pred_female[, "fit"], col = 'green', lwd = 2)  # Regression line for females
lines(age_range, upper_male, col = 'red', lty = 2)  # Upper confidence interval for males
lines(age_range, lower_male, col = 'red', lty = 2)  # Lower confidence interval for males
lines(age_range, upper_female, col = 'green', lty = 2)  # Upper confidence interval for females
lines(age_range, lower_female, col = 'green', lty = 2)  # Lower confidence interval for females

# Add legend (ignoring the label for 'Salarios Reales')
legend('topright', legend = c(NA, 'Línea de Regresión para Hombres', 'Línea de Regresión para Mujeres', 'Intervalo de Confianza para Hombres', 'Intervalo de Confianza para Mujeres'), 
       col = c('blue', 'red', 'green', 'red', 'green'), lty = c(NA, 1, 1, 2, 2), lwd = c(NA, 2, 2, 1, 1))

#Ahora asumiendo, por ejemplo, que se trata de docentes con alto logro educativo

conditions <- data.frame(
  sex = rep(c(0, 1), each = length(age_range)),  # Valores para sexo (0 y 1), cada uno repetido para cada edad
  age = rep(age_range, 2),                        # Cada edad repetida dos veces, una para cada sexo
  cuentaPropia = median(bd$cuentaPropia),         # Valor medio de cuentaPropia
  formal = median(bd$formal),                    # Valor medio de formal
  hoursWorkUsual = median(bd$hoursWorkUsual),     # Valor medio de hoursWorkUsual
  maxEducLevel.f = 7,                            # Educación terciaria
  oficio.f = 13                                  # Docentes
)

# Define niveles para maxEducLevel.f y oficio.f
education_levels <- c(1:7, 9)  # Niveles de educación
oficio_levels <- 1:99          # Niveles para oficio

# Convierte maxEducLevel.f y oficio.f a factores con niveles especificados
conditions$maxEducLevel.f <- factor(conditions$maxEducLevel.f, levels = education_levels)
conditions$oficio.f <- factor(conditions$oficio.f, levels = oficio_levels)


# Perform predictions with confidence intervals
pred_male <- predict(results, newdata = conditions[conditions$sex == 0,], interval = "confidence")
pred_female <- predict(results, newdata = conditions[conditions$sex == 1,], interval = "confidence")

# Extract upper and lower confidence limits
lower_male <- pred_male[, "lwr"]
upper_male <- pred_male[, "upr"]
lower_female <- pred_female[, "lwr"]
upper_female <- pred_female[, "upr"]

# Plot the scatter plot with confidence intervals
plot(bd$age, log(bd$y_salary_m_hu), col = 'blue', xlab = 'Edad', ylab = 'Log Salario', pch = NA, ylim = c(8.6,9.8))
lines(age_range, pred_male[, "fit"], col = 'red', lwd = 2)  # Regression line for males
lines(age_range, pred_female[, "fit"], col = 'green', lwd = 2)  # Regression line for females
lines(age_range, upper_male, col = 'red', lty = 2)  # Upper confidence interval for males
lines(age_range, lower_male, col = 'red', lty = 2)  # Lower confidence interval for males
lines(age_range, upper_female, col = 'green', lty = 2)  # Upper confidence interval for females
lines(age_range, lower_female, col = 'green', lty = 2)  # Lower confidence interval for females

# Add legend (ignoring the label for 'Salarios Reales')
legend('topright', legend = c(NA, 'Línea de Regresión para Hombres', 'Línea de Regresión para Mujeres', 'Intervalo de Confianza para Hombres', 'Intervalo de Confianza para Mujeres'), 
       col = c('blue', 'red', 'green', 'red', 'green'), lty = c(NA, 1, 1, 2, 2), lwd = c(NA, 2, 2, 1, 1))

#Ahora asumiendo, por ejemplo, que se trata de agricultores sin logro educativo

conditions <- data.frame(
  sex = rep(c(0, 1), each = length(age_range)),  # Valores para sexo (0 y 1), cada uno repetido para cada edad
  age = rep(age_range, 2),                        # Cada edad repetida dos veces, una para cada sexo
  cuentaPropia = median(bd$cuentaPropia),         # Valor medio de cuentaPropia
  formal = median(bd$formal),                    # Valor medio de formal
  hoursWorkUsual = median(bd$hoursWorkUsual),     # Valor medio de hoursWorkUsual
  maxEducLevel.f = 1,                            # Sin logro educativo
  oficio.f = 61                                  # Agricultores
)
median(bd$cuentaPropia)
median(bd$formal)


# Define niveles para maxEducLevel.f y oficio.f
education_levels <- c(1:7, 9)  # Niveles de educación
oficio_levels <- 1:99          # Niveles para oficio

# Convierte maxEducLevel.f y oficio.f a factores con niveles especificados
conditions$maxEducLevel.f <- factor(conditions$maxEducLevel.f, levels = education_levels)
conditions$oficio.f <- factor(conditions$oficio.f, levels = oficio_levels)


# Perform predictions with confidence intervals
pred_male <- predict(results, newdata = conditions[conditions$sex == 0,], interval = "confidence")
pred_female <- predict(results, newdata = conditions[conditions$sex == 1,], interval = "confidence")

# Extract upper and lower confidence limits
lower_male <- pred_male[, "lwr"]
upper_male <- pred_male[, "upr"]
lower_female <- pred_female[, "lwr"]
upper_female <- pred_female[, "upr"]

# Plot the scatter plot with confidence intervals
plot(bd$age, log(bd$y_salary_m_hu), col = 'blue', xlab = 'Edad', ylab = 'Log Salario', pch = NA, ylim = c(8,9.8))
lines(age_range, pred_male[, "fit"], col = 'red', lwd = 2)  # Regression line for males
lines(age_range, pred_female[, "fit"], col = 'green', lwd = 2)  # Regression line for females
lines(age_range, upper_male, col = 'red', lty = 2)  # Upper confidence interval for males
lines(age_range, lower_male, col = 'red', lty = 2)  # Lower confidence interval for males
lines(age_range, upper_female, col = 'green', lty = 2)  # Upper confidence interval for females
lines(age_range, lower_female, col = 'green', lty = 2)  # Lower confidence interval for females

# Add legend (ignoring the label for 'Salarios Reales')
legend('topright', legend = c(NA, 'Línea de Regresión para Hombres', 'Línea de Regresión para Mujeres', 'Intervalo de Confianza para Hombres', 'Intervalo de Confianza para Mujeres'), 
       col = c('blue', 'red', 'green', 'red', 'green'), lty = c(NA, 1, 1, 2, 2), lwd = c(NA, 2, 2, 1, 1))



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

training <- training %>% filter(oficio!=96) 
testing <- testing %>% filter(oficio!=96) 



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

# Model 3 - En este modelo se tiene en cuenta la edad, el sexo, el oficio, el 
# máximo nivel de educación si eltrabajo es formal o no, si el trabajo es 
# independiente o no 



form_3<- log(y_salary_m_hu)~ sex  + oficio.f +age + formal + maxEducLevel.f + cuentaPropia

modelo_cv3 <- lm(form_3,
                 data = training)
# Prediction
predictions <- predict(modelo_cv3, testing)


score_cv3<- RMSE(predictions, testing$y_salary_m_hu )


# Model 4 - para est emodelo se explora la relación cuadrática que puede haber 
# entre la edad y los ingresos y el tiempo de duración en el trabajo actual y no 
# tomamos en cuenta si es formal y si trabaja por cuenta propia

form_4<- log(y_salary_m_hu)~  sex + oficio.f + age + (age^2) + maxEducLevel.f  + p6426 

modelo_cv4 <- lm(form_4,
                 data = training)

# Prediction
predictions <- predict(modelo_cv4, testing)


score_cv4<- RMSE(predictions, testing$y_salary_m_hu )


# Model 5 - Se explora un polinomo de grado 3 para la edad, para ver si hay 
# alguna diferencia interesante con respecto al polinomio de grado 2

form_5<- log(y_salary_m_hu)~  sex + oficio.f + age + (age^2) + (age^3) + (age^4) + maxEducLevel.f 

modelo_cv5 <- lm(form_5,
                 data = training)

# Prediction
predictions <- predict(modelo_cv5, testing)


score_cv5<- RMSE(predictions, testing$y_salary_m_hu )

# Model 6 - Se explora la posible interacción entre el tipo de oficio y el sexo, 
# este modelo está pensado en las existentes brechas que hay en ciertos sectores
# de remuneración más alta donde dominan los hombres

form_6<- log(y_salary_m_hu)~  sex + oficio + sex*oficio.f + age + (age^2) + sex*formal + maxEducLevel.f + p6426  + cuentaPropia

modelo_cv6 <- lm(form_6,
                 data = training)

# Prediction
predictions <- predict(modelo_cv6, testing)


score_cv6<- RMSE(predictions, testing$y_salary_m_hu )

# Model 7 - Este modelo explora si existe alguna relación entre la edad entre 
# hombres y mujeres y sus ingresos

form_7<- log(y_salary_m_hu)~  sex + oficio.f + sex*age + age + (age^2) + formal + maxEducLevel.f + p6426  + cuentaPropia

modelo_cv7 <- lm(form_7,
                 data = training)

# Prediction
predictions <- predict(modelo_cv7, testing)


score_cv7<- RMSE(predictions, testing$y_salary_m_hu )




  # b. Comparing models based on RMSE

# para por ahora
scores_cv<- data.frame( Model= c(1, 2),
                     RMSE_vsa= c(score_cv1, score_cv2)
)


scores_cv<- data.frame( Model= c(1, 2, 3, 4, 5, 6, 7),
                     RMSE_vsa= c(score_cv1, score_cv2, score_cv3, score_cv4, 
                                 score_cv5, score_cv6, score_cv7))

head(scores_cv)

stargazer(modelo_cv1, modelo_cv2, modelo_cv3, modelo_cv4, modelo_cv5, type="text", out="models.txt")


stargazer(modelo_cv1, modelo_cv2, modelo_cv3, modelo_cv4, modelo_cv5, 
          modelo_cv6, modelo_cv7, type="text", out="models.txt")


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






