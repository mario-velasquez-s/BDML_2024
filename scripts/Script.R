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
       xtable,  # For predictive model assessment
       fixest)  # Fixed effects 

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
df_without_imputation$mujer <- 1 - df_without_imputation$sex
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
#ggsave("./views/imputation/distri_wage.pdf", impu_1)

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
#ggsave("./views/imputation/wage_estrato1.pdf", impu_2)

## In addition, the social strata (estrato1) are independent to sex and age.
estrato_orto <- lm(estrato1 ~ age + as.factor(sex), bd)
summary(estrato_orto) ## Coeficientes muy cercanos a 0.
impu_3<-stargazer(estrato_orto, type="latex", title="Relación estrato vs edad y sexo")
#write(impu_3, file="./estrato1_age_sex.tex")

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

# Variable creation
bd$ln_wage <- log(bd$y_salary_m_hu)
bd$maxEducLevel.f <- factor(bd$maxEducLevel) #Converts variable to factor
bd$oficio.f <- factor(bd$oficio) #Converts variable to factor
bd$mujer <- 1 - bd$sex #Crear variable con mujer igual a 1"

# 2a: Variables and Descriptive Statistics ------------------------------------
names(bd)

##Summary of all variables
des_vars <- c("age", "mujer", "hoursWorkUsual", "formal", "cuentaPropia")
stargazer::stargazer(as.data.frame(bd[,des_vars]), type="latex", title="Descriptivas de las variables explicatorias",
                     out="./views/descriptive/descriptivas_numericas.tex")

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
ggsave("./views/descriptive/descriptiva_maxEduc.pdf", des_2)

des_3 <- ggplot(bd, aes(x=as.factor(estrato1))) + 
  geom_bar(fill="#0099F8") +
  labs(x="Estrato de energía",
       y= "Cantidad") + 
  theme_bw()
ggsave("./views/descriptive/descriptiva_estrato.pdf", des_3)


## Graph of why to transform wage to ln(wage)

des_4 <- ggplot(bd, aes(x=ln_wage)) +
  geom_histogram(fill="#0099F8") +
  labs(x="ln(salario horario)", y="Frecuencia") +
  theme_bw()
ggsave("./views/descriptiva_ln_salario.pdf", des_4)

## Graph of age vs wage
bd$sex <- factor(bd$sex, levels=c(0,1), labels = c("Mujer", "Hombre"))
des_5 <- ggplot(bd) + 
  geom_point(mapping = aes(x=age, y=ln_wage, color=as.factor(sex))) + 
  geom_smooth(mapping = aes(x=age, y=ln_wage)) +
  labs(x="Edad", y="ln(salario horario)") + 
  theme_bw()
ggsave("./views/descriptive/descriptiva_salario_predictores.pdf", des_5)




# 3: Age-wage Profile --------------------------------------------------------

# Creo que no es y_ingLab_m sino y_salary_m_hu (o y_ingLab_m_ha), deberiamos ponernos de acuerdo
modelo_punto_3 <- lm(log(y_salary_m_hu) ~ age + I(age^2), data = bd)

stargazer(modelo_punto_3, title="Regresión de salario contra edad", type="latex", out="./document/regresion_punto_3.tex")
stargazer(modelo_punto_3, title="Regresión de salario contra edad - texto", type="text")

# Funcion del "peak-age"
bootstrap_peak_age <- function(bd, indices) {
  sampled_data <- bd[indices, ]
  modelo_boot <- lm(log(y_salary_m_hu) ~ age + I(age^2), data = sampled_data)
  
  # Calculate peak age using the formula: -b1 / (2 * b2)
  peak_age <- -coef(modelo_boot)[2] / (2 * coef(modelo_boot)[3])
  return(peak_age)
}

bootstrap_peak_age_results <- boot(bd, bootstrap_peak_age, R = 5000)


hist(bootstrap_peak_age_results$t, main = "Distribucion de 'Peak Age'", xlab = "Peak Age", ylab = "Frecuencia", col = "lightblue", border = "black", breaks=25)

ci_lower <- quantile(bootstrap_peak_age_results$t, 0.025)
ci_upper <- quantile(bootstrap_peak_age_results$t, 0.975)

abline(v = ci_lower, col = "red", lty = 2)
abline(v = ci_upper, col = "red", lty = 2)




# 4: The Gender Earnings GAP -------------------------------------------------

#In the regression, women = 1 so I edit bd so that women = 1 and man = 0

# a) Estimating the unconditional wage gap


gap_lm_hourly <- lm(ln_wage ~ mujer, data = bd)
gap_lm_hourly_ni <- lm(log(y_salary_m_hu) ~ mujer, data = df_without_imputation)
stargazer(gap_lm_hourly, gap_lm_hourly_ni)

# b) Part i. Conditional age gap incorporating controls like age, cuentaPropia, formal, hoursWorkUsual, inac, maxEducLevel, oficio

#bd$maxEducLevel.f <- factor(bd$maxEducLevel) #Converts variable to factor
#bd$oficio.f <- factor(bd$oficio) #Converts variable to factor

#T-test to look at the relevance of the controls
control_variables <- setdiff(names(bd), c("sex", "mujer", "maxEducLevel.f", "oficio.f"))
t_test_results <- data.frame(variable = character(), p_value = numeric(), stringsAsFactors = FALSE)
for (var in control_variables) {
  t_test_result <- t.test(bd[bd$sex == 1, var], bd[bd$sex == 0, var])
  t_test_results <- rbind(t_test_results, data.frame(variable = var, p_value = t_test_result$p.value))
}

print(t_test_results)

latex_table <- xtable(t_test_results)
# Print the LaTeX code
print(latex_table, include.rownames = FALSE)

# When applying FWL, the first vector of explanatory variables (#X_1) will only contain the variable female


# First, perform the regression without the FWL method to know which results should we expect from the FWL process since outcomes are the same
results <- lm(log(y_salary_m_hu) ~ sex + poly(age,2, raw = TRUE) + cuentaPropia + formal + hoursWorkUsual + maxEducLevel.f + oficio.f, data = bd)

bd<- bd %>% mutate(leverage = hatvalues(results))
bd<- bd %>% mutate(residuals= results$residuals)

leverage <- ggplot(bd , aes(y = leverage , x = residuals  )) +
  geom_point() + # add points
  theme_bw() + #black and white theme
  labs(x = "Residuales",  
       y = "Leverage",
       title = "") # labels
dir.create("./views/gender_gap", recursive = TRUE)
ggsave("./views/gender_gap/leverage.pdf", plot = leverage)


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
summary_table <- stargazer(results, results_no_leverage, reg_res,type="latex",digits=4)

#Verificar que la suma de residuales de igual
sum(resid(results)^2)
sum(resid(reg_res)^2)

# b) Part ii. Using FWL with bootstrap


B <- 1000
eta_mod1 <- rep(0,1000)

#Run the two previous lines before the loop

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

histogram <- ggplot(data = data.frame(x = eta_mod1), aes(x = eta_mod1)) +
  geom_histogram(fill="#0099F8") +
  labs(x="Valores", y="Frecuencia") +
  theme_bw()
# Save the plot to a specific path
ggsave("./views/gender_gap/histogram.pdf", plot = histogram)


# c) Ploting the age-wage profile

# Genera un rango de edades desde la mínima a la máxima encontrada en la variable 'age' de 'bd', con incrementos de 1.
age_range <- seq(min(bd$age), max(bd$age), by = 1)

# Crea un marco de datos 'conditions' con condiciones para diferentes variables.
conditions <- data.frame(
  sex = rep(c(0, 1), each = length(age_range)),  # Valores para sexo (0 y 1), cada uno repetido para cada edad
  age = rep(age_range, 2),                        # Cada edad repetida dos veces, una para cada sexo
  cuentaPropia = median(bd$cuentaPropia),         # Valor medio de cuentaPropia. Es igual a no cuenta propia
  formal = median(bd$formal),                    # Valor medio de formal. Es igual a formal
  hoursWorkUsual = median(bd$hoursWorkUsual),     # Valor medio de hoursWorkUsual. Igual a 48
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

# Open a PDF device to save the plot
pdf("./views/gender_gap/ambulantes.pdf")

# Extract upper and lower confidence limits
lower_male <- pred_male[, "lwr"]
upper_male <- pred_male[, "upr"]
lower_female <- pred_female[, "lwr"]
upper_female <- pred_female[, "upr"]

# Plot the scatter plot with confidence intervals
plot(bd$age, log(bd$y_salary_m_hu), col = 'blue', xlab = 'Edad', ylab = 'Log Salario', pch = NA, ylim = c(8.1,9.1))
lines(age_range, pred_male[, "fit"], col = 'red', lwd = 2)  # Regression line for males
lines(age_range, pred_female[, "fit"], col = 'green', lwd = 2)  # Regression line for females
lines(age_range, upper_male, col = 'red', lty = 2)  # Upper confidence interval for males
lines(age_range, lower_male, col = 'red', lty = 2)  # Lower confidence interval for males
lines(age_range, upper_female, col = 'green', lty = 2)  # Upper confidence interval for females
lines(age_range, lower_female, col = 'green', lty = 2)  # Lower confidence interval for females

# Add legend (ignoring the label for 'Salarios Reales')
legend('topright', legend = c(NA, 'Línea de Regresión para Hombres', 'Línea de Regresión para Mujeres', 'Intervalo de Confianza para Hombres', 'Intervalo de Confianza para Mujeres'), 
       col = c('blue', 'red', 'green', 'red', 'green'), lty = c(NA, 1, 1, 2, 2), lwd = c(NA, 2, 2, 1, 1))

dev.off()

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

# Open a PDF device to save the plot
pdf("C:/Users/marti/OneDrive/Documentos/GIT_Repositories/BDML_2024/views/gender_gap/ejecutivos.pdf")

# Extract upper and lower confidence limits
lower_male <- pred_male[, "lwr"]
upper_male <- pred_male[, "upr"]
lower_female <- pred_female[, "lwr"]
upper_female <- pred_female[, "upr"]

# Plot the scatter plot with confidence intervals
plot(bd$age, log(bd$y_salary_m_hu), col = 'blue', xlab = 'Edad', ylab = 'Log Salario', pch = NA, ylim = c(9.2,10.3))
lines(age_range, pred_male[, "fit"], col = 'red', lwd = 2)  # Regression line for males
lines(age_range, pred_female[, "fit"], col = 'green', lwd = 2)  # Regression line for females
lines(age_range, upper_male, col = 'red', lty = 2)  # Upper confidence interval for males
lines(age_range, lower_male, col = 'red', lty = 2)  # Lower confidence interval for males
lines(age_range, upper_female, col = 'green', lty = 2)  # Upper confidence interval for females
lines(age_range, lower_female, col = 'green', lty = 2)  # Lower confidence interval for females

# Add legend (ignoring the label for 'Salarios Reales')
legend('topright', legend = c(NA, 'Línea de Regresión para Hombres', 'Línea de Regresión para Mujeres', 'Intervalo de Confianza para Hombres', 'Intervalo de Confianza para Mujeres'), 
       col = c('blue', 'red', 'green', 'red', 'green'), lty = c(NA, 1, 1, 2, 2), lwd = c(NA, 2, 2, 1, 1))

dev.off()

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

# Open a PDF device to save the plot
pdf("./views/gender_gap/docentes.pdf")

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

dev.off()

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

# Open a PDF device to save the plot
pdf("./views/gender_gap/agricultores.pdf")

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

dev.off()

# 5: Predicting earnings------------------------------------

bd$maxEducLevel.f <- factor(bd$maxEducLevel)
bd$oficio.f <- factor(bd$oficio)

  # a. Splitting sample 70% training 30% testing 

  set.seed(10101)  # Set set for replicability purposes 


inTrain <- createDataPartition(
  y = bd$ln_wage,  ## the outcome data are needed
  p = .70, ## The percentage of data in the
  list = FALSE
)

training <- bd[ inTrain,]
testing  <- bd[-inTrain,]

training <- training %>% filter(oficio!=96) 
testing <- testing %>% filter(oficio!=96) 

training <- training %>% filter(oficio!=78) 
testing <- testing %>% filter(oficio!=78) 



# Model 1 - modelo punto 3
# Training
form_1<- ln_wage ~ age + I(age^2) 


modelo_cv1 <- lm(form_1,
               data = training)
# Prediction
predictions <- predict(modelo_cv1, testing)


score_cv1<- RMSE(predictions, testing$ln_wage )
score_cv1 # 10584.75


# Model 2 - unconditional gender wage gap
# Training
form_2<- ln_wage ~ sex
modelo_cv2 <- lm(form_2,
               data = training)
# Prediction
predictions <- predict(modelo_cv2, testing)


score_cv2<- RMSE(predictions, testing$ln_wage )
score_cv2 # 10584.76

# Model 3 - From question 4, age, gender, occupation, highest level of education, 
# whether the job is formal or not, and whether the job is independent or not are
# taken into account.


form_3<- ln_wage ~ sex + sex*age + sex*I(age^2) + hoursWorkUsual + oficio.f + maxEducLevel.f + formal+ cuentaPropia

modelo_cv3 <- lm(form_3,
                 data = training)
# Prediction
predictions <- predict(modelo_cv3, testing)


score_cv3<- RMSE(predictions, testing$ln_wage )


# Model 4 - For this model, the quadratic relationship between age and income, 
# as well as the duration of current job, is explored, without considering 
# whether it is formal employment or self-employment.

form_4<- ln_wage ~  sex + sex*age + sex*I(age^2) + hoursWorkUsual + oficio.f + maxEducLevel.f + formal+ cuentaPropia + p6426 

modelo_cv4 <- lm(form_4, 
                 data = training)

# Prediction
predictions <- predict(modelo_cv4, testing)


score_cv4<- RMSE(predictions, testing$ln_wage )


# Model 5 - A third-degree polynomial for age is explored to see if there are 
# any interesting differences compared to the second-degree polynomial.

form_5<- ln_wage ~  sex + sex*age + sex*I(age^2) + sex*I(age^3) + hoursWorkUsual + oficio.f + maxEducLevel.f + formal+ cuentaPropia + p6426 


modelo_cv5 <- lm(form_5,
                 data = training)

# Prediction
predictions <- predict(modelo_cv5, testing)


score_cv5<- RMSE(predictions, testing$ln_wage )

# Model 6 - The possible interaction between occupation type and gender is 
# explored in this model, which is designed to address existing gaps in certain 
# sectors with higher compensation where men dominate.

form_6<- ln_wage ~  sex + sex*age + sex*I(age^2) + sex*I(age^3) + sex*hoursWorkUsual + oficio.f + maxEducLevel.f + formal+ cuentaPropia + p6426

modelo_cv6 <- lm(form_6,
                 data = training)

# Prediction
predictions <- predict(modelo_cv6, testing)


score_cv6<- RMSE(predictions, testing$ln_wage )

# Model 7 - 

form_7<- ln_wage ~  sex + sex*age + sex*I(age^2) + sex*I(age^3) + sex*hoursWorkUsual + oficio.f + sex*maxEducLevel.f + sex*formal+ sex*cuentaPropia + sex*p6426


modelo_cv7 <- lm(form_7,
                 data = training)

# Prediction
predictions <- predict(modelo_cv7, testing)


score_cv7<- RMSE(predictions, testing$ln_wage )

# Model 8 - 

form_8<- ln_wage ~  sex + sex*age + sex*I(age^2) + sex*I(age^3) + sex*hoursWorkUsual + sex*oficio.f + sex*maxEducLevel.f + sex*formal+ sex*cuentaPropia + sex*p6426


modelo_cv8 <- lm(form_8,
                 data = training)

# Prediction
predictions <- predict(modelo_cv8, testing)


score_cv8<- RMSE(predictions, testing$ln_wage )





  # b. Comparing models based on RMSE


scores_cv<- data.frame( Model= c(1, 2, 3, 4, 5, 6, 7, 8),
                     RMSE_vsa= c(score_cv1, score_cv2, score_cv3, score_cv4, 
                                 score_cv5, score_cv6, score_cv7, score_cv8))

scores_cv
# Models 6 and 7 have de lowest RMSE



  # c. Comments on results 

  # d. LOOCV for the two models with the lowest RMSE

ctrl <- trainControl(
  method = "LOOCV",
  verboseIter = TRUE,
  allowParallel = TRUE) 


modelo_loocv1 <- train(form_6,
                  data = bd,
                  method = 'lm', 
                  trControl= ctrl)

head(modelo_loocv1$pred)

score_loocv1<-RMSE(modelo_loocv1$pred$pred, bd$ln_wage)
# 0.4923283

modelo_loocv2 <- train(form_7,
                       data = bd,
                       method = 'lm', 
                       trControl= ctrl,)
head(modelo_loocv2$pred)

score_loocv2<-RMSE(modelo_loocv2$pred$pred, bd$ln_wage)
# 0.4907426

