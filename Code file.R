#---------------------
#Cargar librerias
#_____________________

library(tidyverse)

#----------------------
#Cargar bases de datos
#______________________

#Url de archivos 
url_2022_ingresos <- "https://www.inegi.org.mx/contenidos/programas/enigh/nc/2022/microdatos/enigh2022_ns_ingresos_csv.zip"
url_2020_ingresos <- "https://www.inegi.org.mx/contenidos/programas/enigh/nc/2020/microdatos/enigh2020_ns_ingresos_csv.zip"
url_2018_ingresos <- "https://www.inegi.org.mx/contenidos/programas/enigh/nc/2018/microdatos/enigh2018_ns_ingresos_csv.zip"
url_2016_ingresos <- "https://www.inegi.org.mx/contenidos/programas/enigh/nc/2016/microdatos/enigh2016_ns_ingresos_csv.zip"

enigh_2022_ingresos <- load_inegi(url_2022_ingresos, 
                                  "ingresos.csv")
enigh_2020_ingresos <- load_inegi(url_2020_ingresos, 
                                  "ingresos.csv")
enigh_2018_ingresos <- load_inegi(url_2018_ingresos, 
                                  "ingresos.csv")
enigh_2016_ingresos <- load_inegi(url_2016_ingresos, 
                                  "ingresos.csv")

#----------------------
#Cargar bases de datos
#______________________

enigh_2022_ingresos %>% select(ing_tri) %>% summary()
enigh_2022_ingresos %>% select(ing_tri) %>% summary()
enigh_2022_ingresos %>% select(ing_tri) %>% summary()
enigh_2022_ingresos %>% select(ing_tri) %>% summary()

#----------------------
#Analisis de datos
#______________________

# Creación del data frame con quantiles
percentiles_data <- data.frame(
  percentile = seq(0, 1, length = 101),
  value_2022 = quantile(enigh_2022_ingresos$ing_tri, prob = seq(0, 1, length = 101), na.rm = TRUE),
  value_2020 = quantile(enigh_2020_ingresos$ing_tri, prob = seq(0, 1, length = 101), na.rm = TRUE),
  value_2018 = quantile(enigh_2018_ingresos$ing_tri, prob = seq(0, 1, length = 101), na.rm = TRUE),
  value_2016 = quantile(enigh_2016_ingresos$ing_tri, prob = seq(0, 1, length = 101), na.rm = TRUE)
)

#Ingresos por año
{
percentiles_data %>% filter(percentile>0.01 & percentile<0.99) %>% 
  ggplot() + 
  geom_line(aes(x = percentile, y = value_2022), color = "blue") +
  geom_line(aes(x = percentile, y = value_2020), color = "green") +
  geom_line(aes(x = percentile, y = value_2018), color = "orange")+
  geom_line(aes(x = percentile, y = value_2016), color = "red")+
  theme_classic()
}
  
#Diferencia de ingresos 
{
  
percentiles_data  %>%
  filter(percentile >= 0.01 & percentile <= 0.99) %>% 
  mutate(
    diferencia_2022_2020 = value_2022 - value_2020,
    diferencia_2020_2018 = value_2020 - value_2018,
    diferencia_2018_2016 = value_2018 - value_2016,
    diferencia_2022_2016 = value_2022 - value_2016
  ) %>% 
  pivot_longer(
    cols = starts_with("diferencia_"),
    names_to = "linea",
    values_to = "diferencia"
  ) %>% 
ggplot(aes(x = percentile, y = diferencia, color = linea)) +
  geom_line(size = 1) +
  labs(
    title = "Diferencias entre Percentiles de Ingresos en Diferentes Años",
    x = "Percentil",
    y = "Diferencia",
    color = "Línea"
  ) +
  theme_classic()

}

#Tasa de crecimiento de ingresos 
{
percentiles_data  %>%
  filter(percentile >= 0.01 & percentile <= 0.99) %>% 
  mutate(
    diferencia_2022_2020 = ((value_2022/value_2020)-1)*100,
    diferencia_2020_2018 = ((value_2020 / value_2018)-1)*100,
    diferencia_2018_2016 = ((value_2018 / value_2016)-1)*100,
    diferencia_2022_2016 = ((value_2022 / value_2016)-1)*100
  ) %>% 
  pivot_longer(
    cols = starts_with("diferencia_"),
    names_to = "linea",
    values_to = "diferencia"
  ) %>% 
  ggplot(aes(x = percentile, y = diferencia, color = linea)) +
  geom_line(size = 1) +
  labs(
    title = "Diferencias entre Percentiles de Ingresos en Diferentes Años",
    x = "Percentil",
    y = "Diferencia",
    color = "Línea"
  ) +
  theme_classic()
}





