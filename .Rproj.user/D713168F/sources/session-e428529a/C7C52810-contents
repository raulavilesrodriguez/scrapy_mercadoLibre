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




