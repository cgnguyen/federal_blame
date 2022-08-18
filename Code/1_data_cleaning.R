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
  

  D<-
    D%>%
      mutate(gov_p=as.factor(`r-o`))
  
  
  ## assign main labels to code --------
  #Do separately for Bund and Land due to different length of codes 
  D_land<-
    D%>%
    filter(parl!="BT")%>%
    mutate(code_1=str_sub(code, 1,1))%>%
    mutate(code_1=as.factor(fct_recode(code_1,
                               "Responsibility" = "1",
                               "Blame" = "2",
                               "Self-Praise" = "3",
                               "Cooperation" = "4",
                               "Federalism" = "5")))
  
  #Here 1=10 = Federalism Code
  D_bund<-
    D%>%
    filter(parl=="BT")%>%
    mutate(code_1=str_sub(code, 1,1))%>%
    mutate(code_1=as.factor(fct_recode(code_1,
                                       "Responsibility" = "6",
                                       "Blame" = "7",
                                       "Self-Praise" = "8",
                                       "Cooperation" = "9",
                                       "Federalism" = "1")))
  
  
  D_bund$gov_p
  
  
  D<-rbind(D_bund,D_land)
 
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

  
  
  
  