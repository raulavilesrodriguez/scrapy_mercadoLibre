library(tidyverse)
library(rvest)
library(httr)
library(readxl)
library(stringr)
library(purrr) # to split strings
library(hrbrthemes)
library(viridis) # pallette of colors
library(viridisLite) # pallette of colors

#______SCRAPY WEB_______
# Algoritm to download all pages
set.seed(1)
page_id <- 51

while (TRUE) {
  result <- try({
    page_id <- page_id + 50
    url_web <- paste0('https://celulares.mercadolibre.com.ec/celulares-smartphones/celulares_Desde_',page_id,'_NoIndex_True')
    dest_page <- paste0('data24sep23/page-',page_id,'.html')
    download.file(url_web, destfile = dest_page, quiet=TRUE)
    #2001 is the last web page. 50*38 + 51
    if (page_id == 2001) stop('error')  
    url_web
  }, silent = TRUE)
  if (inherits(result, 'try-error')){
    message('breaking. It is the end')
    break
  } else message(sprintf('The page is %s', result))
}

# list of files in a Directory
pages <- (list.files(path = 'data24sep23/', pattern = '.html', all.files = TRUE, full.names = TRUE, recursive=FALSE))
# order for Date of creation
file_info <- file.info(pages)
file_info <- file_info[with(file_info, order(as.POSIXct(mtime))), ]
pages_ordered <- rownames(file_info)

pages_ordered <- list(pages_ordered)

# ....SCRAPY OF EACH PAGE....
# Name of smartphone and characteristics
carac_celulares <- list()
total_precios <- list()

list_caract <- sapply(pages_ordered[[1]], function(page){
  print(page)
  data <-read_html(page, encoding="UTF-8")
  caracteristicas <- data |> html_elements(xpath = '//h2[@class="ui-search-item__title shops__item-title"]') |> html_text2()
  print(caracteristicas)
  carac_celulares <- c(
    carac_celulares,
    caracteristicas
  )
})

list_precios <- sapply(pages_ordered[[1]], function(page){
  print(page)
  data <-read_html(page, encoding="UTF-8")
  precios <- data |> html_elements(xpath = '//span[@class="andes-money-amount__fraction"]') |> html_text2()
  print(precios)
  total_precios <- c(
    total_precios,
    precios
  )
})

# converting the list containg the scraped data into tibble
df_celulares <- tibble(
  unlist(list_caract),
  unlist(list_precios)
)
names(df_celulares) <- c('caracteristicas', 'precio')


#____________Wrangling____________
#Export tibble
writexl::write_xlsx(df_celulares, 'celulares.xlsx')




