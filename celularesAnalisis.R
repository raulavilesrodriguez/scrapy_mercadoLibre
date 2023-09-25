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

df_prueba <- df_filtered |> 
  mutate(marca = )

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

# Interctive plot
hchart(df_filtered, 
       "scatter", 
       hcaes(caracteristicas, precio)) |>
       hc_add_theme(hc_theme_darkunica())



