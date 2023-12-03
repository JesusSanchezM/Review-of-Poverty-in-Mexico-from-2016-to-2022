
concatenacion <- function (base_de_datos, codigo) {

Datos_por_hogares <- base_de_datos %>%
  group_by(`ï..folioviv`, foliohog) %>%
  summarize(total_ing_tri = sum(ing_tri, na.rm = TRUE))

Datos_gasto_codigo <- base_de_datos %>%  filter(clave==codigo) %>%
  group_by(`ï..folioviv`, foliohog) %>%
  summarize(total_ing_tri = sum(ing_tri, na.rm = TRUE))

result <- left_join(Datos_por_hogares, Datos_gasto_codigo, 
                    by = c("ï..folioviv", "foliohog"), 
                    suffix = c("_total", paste0("_", codigo)))

}

prueba <- concatenacion(enigh_2022_ingresos, "P104")


library(dplyr)

# Assuming b and c are your data frames
result <- left_join(b, c, by = c("ï..folioviv", "foliohog"), suffix = c("_total", "_P104"))

filtered_result <- result %>% na.omit(result$total_ing_tri_P104)

# Summing the columns after removing NAs
sum(filtered_result$total_ing_tri_P104)/sum(filtered_result$total_ing_tri_total) 
sum(filtered_result$total_ing_tri_P104)/sum(result$total_ing_tri_total)

library(ggplot2)

resultado <- prueba %>%
  na.omit(total_ing_tri_P104) %>%
  summarize(prop = sum(total_ing_tri_P104) / sum(total_ing_tri_total))

# Mostrar el resultado
print(resultado)

# Crear un gráfico de barras para la proporción
ggplot(resultado) + geom_histogram(aes(x=prop)) + theme_classic()
