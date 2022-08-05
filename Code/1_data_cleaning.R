#Setup----------------------------------
  library(tidyverse)
  library(lubridate)


#Data Cleaning - based on original CovFed data -------------
  D_tops<-readRDS("Data/corona_tops.rds")
  
  
  D_tops$type_simple <- fct_recode(as.factor(D_tops$type),
                             "Aktuelle Stunden" = "1",
                             "Regierungserklärungen/ Unterrichtungen" = "2",
                             "Regierungserklärungen/ Unterrichtungen" = "3",
                             "Gesetze/ Rechtsverordnungen" = "4",
                             "Gesetze/ Rechtsverordnungen" = "5",
                             "Parlamentarische Frageverfahren" = "6",
                             "Anträge" = "7",
                             "Andere" = "8")
  