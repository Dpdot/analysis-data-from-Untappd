
library(httr)
library(rjson)
library(stringr)
library(tidyverse)
library(lubridate)
library(openxlsx)
library(arules)
library(arulesViz)
library(data.table)
library(rvest)
library(shiny)
library(ggplot2)
library(colorspace)
library(grDevices)
library(ggExtra)

untappdAPI <- function(adres) {
  adres_url <- paste(adres,"&client_id=BEE5D99036C1A685A993F03F2112D91372339C48&client_secret=923B3926CB8482D5AF8CCC7661929E129DA4FAC0", sep = "")
  
  wez <- GET(adres_url)
  
  fromJSON(rawToChar(wez$content))
}

jakoData <- function(untappddata) {
  
  data_rozdziel <- str_split(untappddata, " ")[[1]]
  
  data_format <- paste(data_rozdziel[4],data_rozdziel[3],data_rozdziel[2],data_rozdziel[5])
  
  ymd_hms(data_format)
  
}

`%notin%` <- Negate(`%in%`)

