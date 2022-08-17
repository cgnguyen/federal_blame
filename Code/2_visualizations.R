#Setup --------------------
  #first run 1_data_cleaning.R



#


#Breakdown of Specific and Diffuse Codes by direction -----------------------


  D_land %>%
    filter(code_1 %in% c("Blame Shifting","Passing the Buck"))%>%
    group_by(direction,specificity)%>%
    summarize(n=n())%>%
    ggplot+
      aes(x=direction, y=n,fill=specificity)+
      geom_col(position=position_dodge())+
      theme_minimal()+
      xlab("Level of Assignment")+ylab("Number of Coded Statements")+
      coord_flip()


D_land %>%
  group_by(party,direction,specificity)%>%
  summarize(n=n())%>%
  ggplot+
  aes(x=direction, y=n,fill=specificity)+
  geom_col(position=position_dodge())+
  theme_minimal()+
  xlab("Level of Assignment")+ylab("Number of Coded Statements")+
  coord_flip()+
  facet_wrap(.~party)
