library(tidyverse)
library(rvest)
library(httr)
library(readxl)
library(stringr)
library(purrr) # to split strings
library(hrbrthemes)
library(viridis) # pallette of colors
library(viridisLite) # pallette of colors
library(highcharter)

#________Wrangling_________
df <- read_excel('celulares.xlsx')

# transform to numeric the price variable
df[,2] <- lapply(df[,2], as.numeric)

# get out accesories
df_filtered <- df |> filter(precio > 30)

df_filtered <- df_filtered |> 
  mutate(marca1 = str_extract(caracteristicas, regex("Apple|iPhone"))) |> 
  mutate(marca2 = str_extract(caracteristicas, regex("Samsung|Galaxy"))) |>
  mutate(marca3 = str_extract(caracteristicas, regex("Huawei"))) |>
  mutate(marca4 = str_extract(caracteristicas, regex("Xiaomi|Redmi|Poco"))) |>
  mutate(marca5 = str_extract(caracteristicas, regex("Infinix"))) |>
  mutate(marca6 = str_extract(caracteristicas, regex("Nokia"))) |>
  mutate(marca7 = str_extract(caracteristicas, regex("Google"))) |>
  mutate(marca8 = ifelse(is.na(marca1) & is.na(marca2) &
                           is.na(marca3) & is.na(marca4) &
                           is.na(marca5) & is.na(marca6) &
                           is.na(marca7), 'Otro', ''))

df_filtered <- replace_na(df_filtered, 
                        list(marca1="", marca2="", marca3="", marca4="",
                             marca5="", marca6="", marca7=""))
df_filtered$marca <- paste(df_filtered$marca1, 
                         df_filtered$marca2, 
                         df_filtered$marca3,
                         df_filtered$marca4,
                         df_filtered$marca5,
                         df_filtered$marca6,
                         df_filtered$marca7,
                         df_filtered$marca8
                         ) 

df_filtered <- df_filtered |> 
  mutate(marca = str_replace_all(marca, "iPhone","Apple")) |>
  mutate(marca = str_replace_all(marca, "Galaxy", "Samsung")) |>
  mutate(marca = str_replace_all(marca, "Redmi", "Xiaomi")) |>
  mutate(marca = str_replace_all(marca, "Poco", "Xiaomi")) |>
  mutate(marca = str_replace_all(marca, "Apple Samsung Huawei", "Huawei"))


#___________Visualization____________
plot_density <- df_filtered |> ggplot(aes(log10(precio))) +
  geom_density(fill="#99627A") +
  labs(x = "Precio Celular (normalizado)")

plot_density

plot_point <- df_filtered |> ggplot(aes(c(1:nrow(df_filtered)), precio, color= precio)) +
  scale_color_gradient(low="#EBB02D",high="#159895")+
  geom_point() +
  labs(x = "No. Celulares Ecuador")
plot_point

# Median
median(df_filtered$precio)
# Mean
mean(df_filtered$precio)

# Create the function to calculate the MODE
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}
moda <- getmode(df_filtered$precio)
moda

# number of terminals by fabricant
table(df_filtered$marca)
#proportio
prop.table(table(df_filtered$marca))

# Interctive plot
hchart(df_filtered, 
       "scatter", 
       hcaes(caracteristicas, precio, group= marca)) |>
       hc_add_theme(hc_theme_darkunica())



