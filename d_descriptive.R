


#DESCRIPTIVE STATISTICS

#Load tables
    visits_day = as.data.table(read.csv(file='./data/visits_day.csv'))

#Get total number of visits  
    sum(visits_day$visits)

#Get total number of IDyear
    length(unique(dd$IDyear))
    length(unique(dd$IDyear[dd$sex==1]))
    length(unique(dd$IDyear[dd$sex==2]))

#Get proportion of IDyear that made at least 1 visit
    length(unique(dd$IDyear[dd$visit==1])) / length(unique(dd$IDyear)) 

#Get percentage days with at least one visits
    mean(dd$visit)    

#Get number of days on which individual made at least 1 visit (only including those that amde at least one visit)
    summary(dd[visit==1, .N, by=IDyear]$N)

#Get number of territories visited by each IDyear (only including those that amde at least one visit)      
    table(bb[visited_terr==1,.(length(unique(box))), by=IDyear]$V1)


#Histograms
  #Number of visits to a particular box per day    
    nrow(visits_day[visits==1])/nrow(visits_day)
    nrow(visits_day[visits>10])/nrow(visits_day)
    p1 = ggplot() +
      geom_histogram(data=visits_day, aes(x=visits, fill=factor(sex)), binwidth=1, position = position_dodge(width=.75), colour="black", alpha=.9) +
      scale_fill_manual(name = NULL, labels = c('male (n=603)', 'female (n=592)'), values=c('#619CFF','#F8766D')) +
      ylab("Count") + xlab("Number of visits to a box per day (if ≥ 1)") +
      scale_x_continuous(limits = c(0,20), breaks = c(0:20), labels = c("0","","","","","5","","","","","10","","","","","15","","","","","20")) +
      labs(tag = "a)") +
      theme_classic() +
      theme(axis.title=element_text(size=12),
            axis.text=element_text(size=12),
            legend.position = c(0.8, 0.9),
            plot.tag = element_text(size=16),
            legend.text = element_text(size=10))
    p1
    
  #Number of days with at least one visit (including all individuals)  
    foo = dd[,.(days=sum(visit)), by=c('ID','year_','sex')]
    summary(foo$days)
    p2 = ggplot() +
      geom_histogram(data=foo, aes(x=days, fill=sex), binwidth=5, position = position_dodge(width=3.75), colour="black", alpha=.9) +
      scale_fill_manual(name = 'Sex', labels = c('male', 'female'),values=c('#619CFF','#F8766D')) +
      xlab("Number of days with ≥ 1 visit") + ylab("Count") + 
      scale_x_continuous(limits = c(-3,63), breaks = seq(0,60,5), labels = c("0","","","","20","","","","40","","","","60")) +
      labs(tag = "b)") +
      theme_classic() +
      theme(legend.position = 'none',
            axis.text=element_text(size=12),
            axis.title=element_text(size=12),
            plot.tag = element_text(size=16))
    p2
    
    
  #Number of territories visited (including all individuals)  
    foo1 = ddd[visited_terr==1,.(boxes = length(unique(box))), by=c('IDyear', 'sex')]
    foo2 = unique(ddd[,.(IDyear, sex)])
    foo = merge(foo2, foo1, by=c('IDyear', 'sex'), all.x=TRUE)
    foo[, boxes := ifelse(is.na(boxes),0,boxes)]
    summary(foo$boxes)
    nrow(foo[boxes!=0])
    nrow(foo[boxes==1]);nrow(foo[boxes==1])/nrow(foo[boxes!=0])
    nrow(foo[boxes==2]);nrow(foo[boxes==2])/nrow(foo[boxes!=0])
    nrow(foo[boxes==3]);nrow(foo[boxes==3])/nrow(foo[boxes!=0])
    nrow(foo[boxes==4]);nrow(foo[boxes==4])/nrow(foo[boxes!=0])
    nrow(foo[boxes>4]);nrow(foo[boxes>4])/nrow(foo[boxes!=0])
    p3 = ggplot() +
      geom_histogram(data=foo, aes(x=boxes, fill=sex), binwidth=1, position = position_dodge(width=.75), colour="black", alpha=.9) +
      scale_fill_manual(name = 'Sex', labels = c('male', 'female'),values=c('#619CFF','#F8766D')) +
      xlab("Number of unique territories visited") + ylab("Count") + 
      scale_x_continuous(limits = c(-.5,15.6), breaks = c(0:15), labels = c("0","","","","","5","","","","","10","","","","","15")) +
      scale_y_continuous(limits = c(0,300)) +
      labs(tag = "c)") +
      theme_classic() +
      theme(legend.position = 'none',
            axis.text=element_text(size=12),
            axis.title=element_text(size=12),
            plot.tag = element_text(size=16))
    p3
    
    
    fig1 = grid.arrange(p1, p2, p3, ncol=1)
    ggsave(fig1, file='plot.png', width=4.5, height=10)
    
    
