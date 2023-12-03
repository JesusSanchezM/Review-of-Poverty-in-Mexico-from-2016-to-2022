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

# Filtro para las bases de datos específicas
enigh_2022_ingresos_filtered <- enigh_2022_ingresos

enigh_2020_ingresos_filtered <- enigh_2020_ingresos %>% filter(clave == "P104")
enigh_2018_ingresos_filtered <- enigh_2018_ingresos %>% filter(clave == "P104")
enigh_2016_ingresos_filtered <- enigh_2016_ingresos %>% filter(clave == "P104")

# Creación del data frame con quantiles
quantiles_data <- data.frame(
  quantile = seq(0, 1, length = 11),
  value_2022 = quantile(enigh_2022_ingresos$ing_tri, prob = seq(0, 1, length = 11), na.rm = TRUE),
  value_2020 = quantile(enigh_2020_ingresos$ing_tri, prob = seq(0, 1, length = 11), na.rm = TRUE),
  value_2018 = quantile(enigh_2018_ingresos$ing_tri, prob = seq(0, 1, length = 11), na.rm = TRUE),
  value_2016 = quantile(enigh_2016_ingresos$ing_tri, prob = seq(0, 1, length = 11), na.rm = TRUE)
)

a <- quantiles_data %>%
  mutate(
    p_2022_2020 = (value_2022-value_2020),
    p_2020_2018 = (value_2020-value_2018)
    )

a %>% filter(quantile<0.99)%>% 
  ggplot() + 
  geom_line(aes(x = quantile, y = value_2022), color = "blue") +
  geom_line(aes(x = quantile, y = value_2020), color = "green") +
  geom_line(aes(x = quantile, y = value_2018), color = "orange")+
  geom_line(aes(x = quantile, y = value_2016), color = "red")+
  theme_classic()

a %>%  ggplot() + 
  geom_line(aes(x=quantile, y=p_2022_2020)) + theme_classic()



