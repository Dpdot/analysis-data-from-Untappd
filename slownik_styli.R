
style_tablica <- data.frame(tablica_piw_full$style)


style_grupowanie <- style_tablica %>% group_by(tablica_piw_full.style) %>% count(tablica_piw_full.style)

write.csv(style_grupowanie, file = "style_grupowanie.csv")

##Ładowanie pliku z mapa

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
