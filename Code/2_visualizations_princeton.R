#Setup --------------------
  #first run 1_data_cleaning.R
  library(viridis)
  library(car)

#Hypothesis 1---------------------------
  #Political parties are more likely to leverage federal blame-shifting strategies if they are in government
  h1_tab<-
    D%>%
      filter(code_1 %in% c("Passing the Buck","Blame Shifting","Self-Praise"))%>%
      group_by(party,gov_p,code_1)%>%
      summarize(N=n())%>%
      mutate(ratio = round(N / sum(N), 10),
             ratio_label= round(N / sum(N), 2))%>%
      mutate(code_1=relevel(code_1, ref="Self-Praise"))
  
  h1_tab%>%
    filter(party!="FW")%>%
    mutate(code_1=as.factor(code_1))%>%
    mutate(label_y = cumsum(ratio) -  ratio)%>%
    ggplot(aes(x=gov_p ,y=ratio,fill=code_1))+
      geom_col(position=position_stack())+
      facet_grid(.~party)+
      theme_minimal()+
      scale_fill_manual(values=viridis(5), name="Coded Strategies")+
      xlab("")+ylab("Percentage of Coded Segments")+
      theme(legend.position="bottom")+
      scale_x_discrete(labels=c("O" = "Opp.", "R" = "Gov."))
  
    h1_tab%>%
      filter(party!="FW")%>%
      mutate(code_1=as.factor(code_1))%>%
      mutate(label_y = cumsum(ratio) -  ratio)%>%
      ggplot(aes(x=party ,y=ratio,fill=gov_p))+
      geom_col(position=position_dodge2(preserve="single", width=0.2), width=0.9)+
      facet_grid(.~code_1)+
      theme_minimal()+
      scale_fill_manual(values=viridis(5), name="Government or Opposition", )+
      xlab("")+ylab("Percentage of Coded Segments")+
      theme(legend.position="bottom")


#Hypothesis 3---------------------
    #Sub-national political parties are more likely to blame their party peers at the federal level or in other constituent units if they run an election campaign.

    h3_tab<-
      D_land%>%
        filter(code_1 %in% c("Passing the Buck","Blame Shifting","Self-Praise"))%>%
        group_by(election,code_1)%>%
        summarize(N=n())%>%
        mutate(ratio = round(N / sum(N), 10),
               ratio_label= round(N / sum(N), 2))
    
    
    h3_tab%>%
      mutate(election=car::recode(election, "1='Election';2='No Election'"))%>%
      ggplot(aes(x=code_1 ,y=ratio,fill=election))+
      geom_col(position=position_dodge2(preserve="single", width=0.2), width=0.9)+
      theme_minimal()+
      scale_fill_manual(values=viridis(5), name="", )+
      xlab("")+ylab("Percentage of Coded Segments")+
      theme(legend.position="bottom")
    
    
#Hypothesis 4---------------------------------------
 #Sub-national political parties prefer to shift the blame vertically to the federal level than horizontally to other constituent units. 
    
    D_land %>%
      filter(code_1 %in% c("Blame Shifting","Passing the Buck"))%>%
      group_by(direction)%>%
      summarize(n=n())%>%
      ggplot+
      aes(x=direction, y=n, fill=direction)+
      geom_col(position=position_dodge())+
      theme_minimal()+
      xlab("Level of Blame Assignment")+ylab("Number of Coded Statements")+
      scale_fill_manual(values=viridis(5))+
      theme(legend.position = "none")
    
    
#Hypothesis 5-------------------------------
  #H5: Party representatives which are part of the coalition governments at federal and sub-national levels prefer the “softer” strategy of self-praise over blame-shifting.
    
    
    
    
    