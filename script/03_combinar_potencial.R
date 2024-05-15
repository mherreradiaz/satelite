source('script/00_paquetes.R')

data <- read_rds('data/processed/data.rds')

write_rds(data,'data/processed/data_satelital.rds')

# data_x <- data |> 
#   filter(temporada == '2022-2023',
#          sitio == 'la_esperanza',
#          tratamiento == 'T1') |> 
#   mutate(fecha = as.Date(fecha),
#          dias = as.numeric(fecha - min(fecha))) |> 
#   select(dias,ndmi)

data |> 
  filter(temporada == '2022-2023') |> 
  mutate(fecha = as.POSIXct(fecha)) |> 
  ggplot(aes(fecha,ndwi)) +
  geom_point() +
  # geom_line() +
  facet_grid(tratamiento~sitio,scale='free')

data_x |> 
  ggplot(aes(fecha,ndwi)) +
  geom_point()

gam_model <- gam(ndmi ~ s(dias), data = data_x, method = "REML")

plot(data_x$dias, data_x$ndmi, main = "Whittaker-Eilers Smoothing", col = "blue", pch = 20)
lines(data_x$dias, predict(gam_model), col = "red")
legend("topleft", legend = c("Original", "Suavizado"), col = c("blue", "red"), pch = 20,)
