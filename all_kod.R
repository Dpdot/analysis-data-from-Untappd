
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

serie <- vector()

grupy <- vector()


tablica_piw_full <- data.frame()

for(j in 1:80) {
  
  start <- "https://api.untappd.com/v4"
  warunki = "?start_date=2020-10-01&end_date=2021-09-30"
  
  
  user_1 <- profile_UT[j]
  
  adres_url <- paste(start,"/user/beers/",user_1,warunki, sep = "")
  
  daty <- vector()
  
  style <- vector()
  
  dodaj <- 0
  
  lista_tablic <- list()
  
  while(adres_url != "") {
    
    dane_user_1 <- untappdAPI(adres_url)
    
    if(length(dane_user_1$response$beers$items) > 0) {
      
      for(i in 1:length(dane_user_1$response$beers$items)){
        
        daty[i + dodaj] <- jakoData(dane_user_1$response$beers$items[[i]]$recent_created_at)
        
        style[i + dodaj] <- dane_user_1$response$beers$items[[i]]$beer$beer_style
      }
      
      adres_url <- dane_user_1$response$pagination$next_url
      dodaj = dodaj + 25
    } else {adres_url <- ""}
  }
  
  
  tablica_piw <- data.frame(daty, style)
  
  
  tablica_piw$daty <- as.POSIXct(daty, origin = "1970-01-01", tz = "GMT")
  
  tablica_piw$user <- user_1
  
  tablica_piw_full <- rbind(tablica_piw_full,tablica_piw)
}

beer_style <- read_html("https://www.brewersassociation.org/edu/brewers-association-beer-style-guidelines/")

style_i_grupy <- beer_style %>%
  html_nodes("div#beer-styles-toc li") %>%
  html_text()

tabela_styli <- data.frame()

i <- 1
j <- 1

while(i <= length(style_i_grupy)) {
  
  if(style_i_grupy[i] == "") {
    kategoria <- style_i_grupy[i+1]
    i = i+2
  } else {
    tabela_styli[j,1] <- kategoria
    tabela_styli[j,2] <- style_i_grupy[i]
    i = i+1
    j = j+1
  }
}

names(tabela_styli) <- c("Kategoria", "Styl")

style_tablica <- data.frame(tablica_piw_full$style)


style_grupowanie <- style_tablica %>% group_by(tablica_piw_full.style) %>% count(tablica_piw_full.style)

write.csv(style_grupowanie, file = "style_grupowanie.csv")


mapowanie_styli <- read.xlsx("mapowanie_input.xlsx", sheet = "input")


tabela_piw_map <- merge(tablica_piw_full,mapowanie_styli, by.x='style', by.y='Label.-.orginal', all.x = T) %>% 
  select(`Label.-.mapping`, daty, user ) %>% 
  rename(style = `Label.-.mapping`) %>%
  arrange(user, desc(daty))

style_count_a <- tabela_piw_map %>% group_by(style) %>% count(style) %>% arrange(desc(n))

style_count <- style_count_a %>% mutate(udzial = round(n/sum(style_count_a$n)*100,2))

style_count_kategoria <- merge(style_count,tabela_styli, by.x="style", by.y="Styl", all.x = T)


style_count_grupy <- style_count_kategoria %>% mutate(grupa = if_else(is.na(Kategoria),style,if_else(udzial < 2, Kategoria, style)))

grupy_count <- style_count_grupy %>% group_by(grupa) %>% summarise(sum(n))

style_count_grupy <- style_count_grupy %>% mutate(grupa = replace(grupa, grupa %in% 
                                                                    c("Other Origin Lager Styles","North American Origin Lager Styles"), "Other Lager Styles")) 

mapa_grupy <- style_count_grupy %>% select(style,grupa) %>% filter(style != "-")

tabela_piw_map_final <- merge(tabela_piw_map,mapa_grupy) %>% 
  select(grupa, daty, user ) %>% 
  arrange(user, desc(daty))

licz_grupy_dane <- unique(tabela_piw_map_final$grupa)

serie <- c()
grupy <- c()

for(i in 1:length(profile_UT)) {
  
  user_act <- profile_UT[i]
  
  skrocona_tabela <- tabela_piw_map_final %>% filter(user == user_act)
  
  
  
  numer <- 1
  skrocona_tabela$seria[1] <- paste(user_act,"_",numer, sep = "")
  
  for(j in 1:(length(skrocona_tabela$seria)-1)) {
    
    
    if (as.duration(skrocona_tabela$daty[j] - skrocona_tabela$daty[j+1]) < 21600) {
      skrocona_tabela$seria[j+1] <- paste(user_act,"_",numer, sep = "")
    } else {
      numer = numer + 1
      skrocona_tabela$seria[j+1] <- paste(user_act,"_",numer, sep = "")
    }
    
  }
  
  
  
  serie <- c(serie, skrocona_tabela$seria)
  
  grupy <- c(grupy, skrocona_tabela$grupa)
  
  print(paste("Iteracja:",i," user:",user_act," dlugosc tabeli:",length(skrocona_tabela$grupa)," dlugosc wektora:",length(grupy)))
  
}

serie_1 <- serie

grupy_1 <- grupy

dane_transakcyjne <- data.frame(serie_1, grupy_1)

dane_transakcyjne_dis <- distinct(dane_transakcyjne)

dane_transakcyjne_dis$serie_1 <- factor(dane_transakcyjne_dis$serie_1)

lista_danych = split(dane_transakcyjne_dis$grupy_1,
                     dane_transakcyjne_dis$serie_1)

basket_market <- as(lista_danych, "transactions")

inspect(head(basket_market))

summary(basket_market)

image(basket_market[1:50])

itemFrequencyPlot(basket_market,
                  topN = 5,
                  main ='Stosunek wystąpień grup styli do liczby transakcji',
                  col = rainbow(5),
                  cex.main = 1,
                  cex.names = 0.7,
                  type = 'relative',
                  horiz = TRUE)

czestosc_grup_1 <- apriori(basket_market ,
                           parameter = list(supp = 0.01, maxlen = 1, target="frequent itemsets"))

czestosc_grup_2 <- apriori(basket_market ,
                           parameter = list(supp = 0.04, minlen = 2, target="frequent itemsets"))

inspect(sort(czestosc_grup_1, by="support"))

inspect(sort(czestosc_grup_2, by="support"))

basket_rules <- apriori(basket_market ,
                        parameter = list(sup = 0.01, conf = 0.5, target="rules", minlen = 2))

basket_rules <- sort(basket_rules, by ="lift")

inspect(basket_rules) 

rules_dt <- DATAFRAME(basket_rules)

ggplot(rules_dt, aes(support,confidence)) +
  geom_point(size = 3, aes(colour = lift)) +
  ggplot2::scale_color_gradient(low = "pink", high = "dark red") +
  theme_minimal()

rules_html <- plot(basket_rules, method ="graph", engine ="htmlwidget")
rules_html

ggplot(rules_dt, aes(rhs,lhs)) +
  geom_point(aes(colour = support, size = confidence)) +
  ggplot2::scale_color_gradient(low = "light blue", high = "dark blue") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 30, vjust = 1, hjust=1, size = 8),axis.text.y = element_text(size = 8), axis.title.x=element_blank(), axis.title.y=element_blank())


