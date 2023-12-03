

b <- enigh_2022_ingresos %>%
  group_by(`ï..folioviv`, foliohog) %>%
  summarize(total_ing_tri = sum(ing_tri, na.rm = TRUE))

c <- enigh_2022_ingresos %>%  filter(clave=="P104") %>%
  group_by(`ï..folioviv`, foliohog) %>%
  summarize(total_ing_tri = sum(ing_tri, na.rm = TRUE))

library(dplyr)

# Assuming b and c are your data frames
result <- left_join(b, c, by = c("ï..folioviv", "foliohog"), suffix = c("_total", "_P104"))

filtered_result <- result %>% na.omit(result$total_ing_tri_P104)

# Summing the columns after removing NAs
sum(filtered_result$total_ing_tri_P104)/sum(filtered_result$total_ing_tri_total) 




