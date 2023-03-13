profile_UT <- read.xlsx("Badanie.xlsx", sheet = 2)

#profile_UT[9,] <- "_maros_"
#profile_UT[27,] <- "wroclovefood"
#profile_UT[70,] <- profile_UT[81,]
#profile_UT <- profile_UT[-81,]

profile_UT %>% group_by(NAZWA) %>% count() %>% arrange(desc(n))



