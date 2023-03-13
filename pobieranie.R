library(tidyverse)
library(data.table)


Dane <- read.table(file = 'en.openfoodfacts.org.products.tsv', sep = '\t', header = T, fill = T)


Dane <- read.csv("drug_info.tsv", sep = "\t")


Dane <- data.table(Dane)

Dane[categories_en = "meat"]

mieso <- grep("meat|Meat", Dane$categories_en)


dane_mieso <- Dane[mieso,]



vegetarian <- grep("vegan|Vegan|vegetarian|Vegetarian", Dane$labels_en)


dane_vegetarian <- Dane[vegetarian,]



