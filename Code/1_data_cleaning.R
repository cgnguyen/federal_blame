#Setup----------------------------------
  library(tidyverse)
  library(lubridate)
  library(questionr)

  D<-readRDS("Data/corona_codes.rds")

#Data Cleaning - based on original CovFed data -------------
  ##Clean Party IDs-------------------------------
  D<-
    D%>%
    mutate(party=fct_recode(party,
                            "UNION" = "CDU",
                            "UNION" = "CSU",
                            "UNION" = "CDU/CSU",
                            "GRUENE"="GRÜNE"))%>%
    mutate(party=fct_relevel(party,"AFD","FW","LINKE","FDP","GRUENE","SPD","UNION"))
  

  ## assign main labels to code --------
  #Do separately for Bund and Land due to different length of codes 
  D_land<-
    D%>%
    filter(parl!="BT")%>%
    mutate(code_1=str_sub(code, 1,1))%>%
    mutate(code_1=as.factor(fct_recode(code_1,
                               "Passing the Buck" = "1",
                               "Blame Shifting" = "2",
                               "Self-Praise" = "3",
                               "Cooperation" = "4",
                               "Federalism" = "5")))%>%
    filter(code_1=="Passing the Buck"|code_1=="Blame Shifting")
  
  #Here 1=10 = Federalism Code
  D_bund<-
    D%>%
    filter(parl=="BT")%>%
    mutate(code_1=str_sub(code, 1,1))%>%
    mutate(code_1=as.factor(fct_recode(code_1,
                                       "Passing the Buck" = "6",
                                       "Blame Shifting" = "7",
                                       "Self-Praise" = "8",
                                       "Cooperation" = "9",
                                       "Federalism" = "1")))
 
  ##Code Specific types of Blame ------------------------------------------
  ###Direction of Blame ---------------------------------------------
  
  D_land$direction <- as.factor(fct_recode(D_land$code,
                            "diffuse" = "101",
                            "vertical" = "102",
                            "vertical" = "102a",
                            "vertical" = "103",
                            "vertical" = "104",
                            "horizontal" = "105",
                            "horizontal" = "106",
                            "horizontal" = "107",
                            "horizontal" = "108",
                            "horizontal" = "109",
                            "horizontal" = "110",
                            "diffuse" = "111",
                            "diffuse" = "201",
                            "vertical" = "202",
                            "vertical" = "202a",
                            "vertical" = "203",
                            "vertical" = "204",
                            "horizontal" = "205",
                            "horizontal" = "206",
                            "horizontal" = "207",
                            "horizontal" = "208",
                            "horizontal" = "209",
                            "horizontal" = "210",
                            "diffuse" = "211"))
  
  D_land$specificity<- as.factor(fct_recode(D_land$code,
                          "diffuse" = "101",
                          "specific" = "102",
                          "diffuse" = "102a",
                          "specific" = "103",
                          "specific" = "104",
                          "diffuse" = "105",
                          "diffuse" = "106",
                          "specific" = "107",
                          "specific" = "108",
                          "specific" = "109",
                          "specific" = "110",
                          "diffuse" = "111",
                          "diffuse" = "201",
                          "specific" = "202",
                          "diffuse" = "202a",
                          "specific" = "203",
                          "specific" = "204",
                          "diffuse" = "205",
                          "diffuse" = "206",
                          "specific" = "207",
                          "specific" = "208",
                          "specific" = "209",
                          "specific" = "210",
                          "diffuse" = "211"))
  
  
  
  
  

  
  
  
  
  #Direction of blame
  D$direction <- fct_recode(D$code_2,
                            "Bund und Länder" = "101 Bund Länder (allgemein)",
                            "Bund und Länder" = "101 Bund und Länder (allgemein)",
                            "Vertikal" = "102 Bund / Bundesregierung",
                            "Vertikal" = "103 Bundesakteure (Parteifarbe identisch)",
                            "Vertikal" = "104 Bundesakteure (Parteifarbe abweichend)",
                            "Horizontal" = "105 Länder (allgemein)",
                            "Horizontal" = "106 andere Länder (allgemein)",
                            "Horizontal" = "107 andere Länder (Parteifarbe MP identisch)",
                            "Horizontal" = "108 andere Länder (Parteifarbe MP abweichend)",
                            "Horizontal" = "109 Akteure in anderen Ländern (Parteifarbe identisch)",
                            "Horizontal" = "110 Akteure in anderen Ländern (Parteifarbe abweichend)",
                            "MPK" = "111 Bund-Länder-Konferenz / MPK / FMK",
                            "Bund und Länder" = "201 Bund und Länder (allgemein)",
                            "Bund" = "202 Bund / Bundesregierung",
                            "Bund" = "203 Bundesakteure (Parteifarbe identisch)",
                            "Bund" = "204 Bundesakteure (Parteifarbe abweichend)",
                            "Andere Länder" = "205 Länder (allgemein)",
                            "Andere Länder" = "206 andere Länder (allgemein)",
                            "Andere Länder" = "207 andere Länder (Parteifarbe MP identisch)",
                            "Andere Länder" = "208 andere Länder (Parteifarbe MP abweichend)",
                            "Andere Länder" = "209 Akteure in anderen Ländern (Parteifarbe identisch)",
                            "Andere Länder" = "210 Akteure in anderen Ländern (Parteifarbe abweichend)",
                            "MPK" = "211 Bund-Länder-Konferenz / MPK / FMK",
                            "Bund und Länder" = "401 Bund und Länder (allgemein)",
                            "Vertikal" = "402 Bund / Bundesregierung",
                            "Vertikal" = "403 Bundesakteure (Parteifarbe identisch)",
                            "Vertikal" = "404 Bundesakteure (Parteifarbe abweichend)",
                            "Horizontal" = "405 Länder (allgemein)",
                            "Horizontal" = "406 andere Länder (allgemein)",
                            "Horizontal" = "407 andere Länder (Parteifarbe MP identisch)",
                            "Horizontal" = "408 andere Länder (Parteifarbe MP abweichend)",
                            "Horizontal" = "409 Akteure in anderen Ländern (Parteifarbe identisch)",
                            "Horizontal" = "410 Akteure in anderen Ländern (Parteifarbe abweichend)",
                            "MPK" = "411 Bund-Länder-Konferenz / MPK / FMK"
  )
  
  
  
  
  ## Recoding D$code_2 into D$code_2_rec
  D$parteifarbe <- fct_recode(D$code_2,
                              "Keine klare \nParteizuordnung\nmöglich" = "201 Bund und Länder (allgemein)",
                              "Keine klare \nParteizuordnung\nmöglich" = "202 Bund / Bundesregierung",
                              "Parteifarbe identisch" = "203 Bundesakteure (Parteifarbe identisch)",
                              "Parteifarbe abweichend" = "204 Bundesakteure (Parteifarbe abweichend)",
                              "Keine klare \nParteizuordnung\nmöglich" = "205 Länder (allgemein)",
                              "Keine klare \nParteizuordnung\nmöglich" = "206 andere Länder (allgemein)",
                              "Parteifarbe identisch" = "207 andere Länder (Parteifarbe MP identisch)",
                              "Parteifarbe abweichend" = "208 andere Länder (Parteifarbe MP abweichend)",
                              "Parteifarbe identisch" = "209 Akteure in anderen Ländern (Parteifarbe identisch)",
                              "Parteifarbe abweichend" = "210 Akteure in anderen Ländern (Parteifarbe abweichend)",
                              "Keine klare \nParteizuordnung\nmöglich" = "211 Bund-Länder-Konferenz / MPK / FMK",
                              
                              "Keine klare \nParteizuordnung\nmöglich" = "101 Bund und Länder (allgemein)",
                              "Keine klare \nParteizuordnung\nmöglich" = "102 Bund / Bundesregierung",
                              "Parteifarbe identisch" = "103 Bundesakteure (Parteifarbe identisch)",
                              "Parteifarbe abweichend" = "104 Bundesakteure (Parteifarbe abweichend)",
                              "Keine klare \nParteizuordnung\nmöglich" = "105 Länder (allgemein)",
                              "Keine klare \nParteizuordnung\nmöglich" = "106 andere Länder (allgemein)",
                              "Parteifarbe identisch" = "107 andere Länder (Parteifarbe MP identisch)",
                              "Parteifarbe abweichend" = "108 andere Länder (Parteifarbe MP abweichend)",
                              "Parteifarbe identisch" = "109 Akteure in anderen Ländern (Parteifarbe identisch)",
                              "Parteifarbe abweichend" = "110 Akteure in anderen Ländern (Parteifarbe abweichend)",
                              "Keine klare \nParteizuordnung\nmöglich" = "111 Bund-Länder-Konferenz / MPK / FMK",
                              
                              "Keine klare \nParteizuordnung\nmöglich" = "401 Bund und Länder (allgemein)",
                              "Keine klare \nParteizuordnung\nmöglich" = "402 Bund / Bundesregierung",
                              "Parteifarbe identisch" = "403 Bundesakteure (Parteifarbe identisch)",
                              "Parteifarbe abweichend" = "404 Bundesakteure (Parteifarbe abweichend)",
                              "Keine klare \nParteizuordnung\nmöglich" = "405 Länder (allgemein)",
                              "Keine klare \nParteizuordnung\nmöglich" = "406 andere Länder (allgemein)",
                              "Parteifarbe identisch" = "407 andere Länder (Parteifarbe MP identisch)",
                              "Parteifarbe abweichend" = "408 andere Länder (Parteifarbe MP abweichend)",
                              "Parteifarbe identisch" = "409 Akteure in anderen Ländern (Parteifarbe identisch)",
                              "Parteifarbe abweichend" = "410 Akteure in anderen Ländern (Parteifarbe abweichend)",
                              "Keine klare \nParteizuordnung\nmöglich" = "411 Bund-Länder-Konferenz / MPK / FMK")
  
  
  
  ##Replace "R" für Regierung with actual parties for now----------------
  
  # Add Vorgangscode as names Factor ----------------------------------------
  ## Recoding DATA$VORGANG into DATA$VORGANG_rec
  DATA$VORGANG <- fct_recode(DATA$VORGANG,
                             "Aktuelle Stunde" = "1",
                             "Regierungserklärung" = "2",
                             "Regierungserklärung" = "3",
                             "Fiskalische Beratung" = "4",
                             "Beratung über Gesetzgebung" = "5",
                             "Fragestunden" = "6",
                             "Antrag" = "7",
                             "Andere" = "8"
  )
  
  
  
  
  