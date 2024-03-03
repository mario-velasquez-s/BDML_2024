# BDML 2024

# Problem Set 1 - Análisis de los principales determinantes de los ingresos laborales en Bogotá

## Sobre el Proyecto:

- Se realiza un análisis descriptivo de los datos obtenidos a través del scraping de un sitio web que contiene información demográfica y laboral de individuos en Bogotá.
- Se enfoca en personas empleadas mayores de 18 años y aborda el desafío de manejar observaciones con datos faltantes o ingresos nulos.
- Estima el perfil de ingresos por edad, siguiendo la evidencia de la economía laboral que sugiere un patrón predecible en el cual los salarios tienden a ser bajos en trabajadores jóvenes, aumentan con la edad y alcanzan su máximo alrededor de los 50 años.
- Utiliza un modelo de regresión logarítmica que incluye términos cuadráticos de edad para capturar posibles no linealidades en la relación entre la edad y los ingresos.
- Investiga la brecha salarial de género, tanto de forma incondicional como condicional, controlando por características de los trabajadores y el empleo.
- Emplea técnicas de regresión lineal y evalúa la robustez de los resultados utilizando bootstrap para estimar los errores estándar y construir intervalos de confianza.
- Evalúa la capacidad predictiva de los modelos desarrollados, dividiendo los datos en conjuntos de entrenamiento y prueba y comparando los errores de predicción de diferentes especificaciones del modelo.
- Discute las fortalezas y limitaciones de cada modelo, así como las implicaciones de las observaciones atípicas en la precisión predictiva.
- Realiza una validación cruzada para evaluar la estabilidad de los resultados y investiga la relación entre los errores de predicción y la influencia estadística de las observaciones.
- El proyecto busca proporcionar herramientas analíticas para mejorar la detección del fraude fiscal y la equidad salarial en el sector público, utilizando datos empíricos y técnicas estadísticas avanzadas.

## Paquetes Necesarios y Prerrequisitos:

- Los paquetes requeridos para replicar el código son:
  - rio
  - tidyverse
  - skimr
  - gridExtra
  - corrplot
  - stargazer
  - MASS
  - rvest
  - httr
  - dplyr
  - ggplot2
  - visdat
  - caret
  - xtable
  - fixest

## Uso:

- El repositorio de GitHub está dividido en 4 carpetas:
  - Un documento que consolida la información de forma resumida, las principales conclusiones y los hallazgos.
  - Un Script en R que se utilizó para completar el código.
  - Una carpeta llamada stores que, en este caso, no fue utilizada.
  - La carpeta views contiene las tablas y gráficas que fueron utilizadas durante el documento y que son resultado de ser exportadas directamente desde el script en R.
  - Estas se encuentran en formato pdf y divididas según la sección del documento final en el que fueron presentadas.

## Frameworks, Librerías y Programas:

- Los scripts del proyecto fueron llevados a cabo en R.
- El documento final que consolida las conclusiones y el análisis fue realizado en LaTex.

## Autores:

- María Camila Arias
- Martín Velásquez
- Mario Velásquez
- Daniela Vlasak

