library(httr)
library(rjson)

untappdAPI <- function(adres) {
  adres_url <- paste(adres,"&client_id=BEE5D99036C1A685A993F03F2112D91372339C48&client_secret=923B3926CB8482D5AF8CCC7661929E129DA4FAC0", sep = "")
  
  wez <- GET(adres_url)
  
  fromJSON(rawToChar(wez$content))
}

start <- "https://api.untappd.com/v4"
warunki = "?start_date=2021-05-01&end_date=2021-05-31"

user_1 <- "gregorycold"

adres <- paste(start,"/user/beers/",user_1,warunki, sep = "")

dane_user_1 <- untappdAPI(adres)

ilosc_piw <- dane_user_1$response$beers$count
adres_url <- dane_user_1$response$pagination$next_url

while(adres_url != "") {
  dane_user <-untappdAPI(adres_url)
  ilosc_piw = ilosc_piw + dane_user$response$beers$count
  adres_url <- dane_user$response$pagination$next_url
}

print(ilosc_piw)







beer_1 <- dane_user_1$response$beers$items[[1]]$beer$bid

adres_beer <- paste(start,"/beer/info/",beer_1,identyfikator, sep = "")

wez_beer <- GET(adres_beer)

user_1_beer_1 <- fromJSON(rawToChar(wez_beer$content))
