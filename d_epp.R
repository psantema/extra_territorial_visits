
#IS EXTRA-PAIR PATERNITY PREDICTED BY WHETHE OR NOT AN INDIVIDUAL MADE EXTRA-TERRITORIAL NESTBOX-VISITS


#Load tables
    visits_day = as.data.table(read.csv(file='./data/visits_day.csv'))
    ep = as.data.table(read.csv(file='./data/ep.csv'))
    bri = as.data.table(read.csv(file='./data/bri.csv'))
    sex = as.data.table(read.csv(file='./data/sex.csv'))
    

#Prepare data
  #Get for each individual whether it made at least one visit and how many visits
    vv = copy(visits_day)
    vv = vv[,.(visit_days = length(unique(yday))), by=c('year_','ID')]
    
  #Get paternity data
    pp = unique(ep[,.(year_, ID, ep)])
    
  #Get breeding data and merge with visit data                  
    ee = copy(bri)
  #Add visit behaviour to data    
    ee = merge(ee, vv, by=c('ID','year_'), all.x=TRUE)
    ee[, visit_days := ifelse(is.na(visit_days), 0, visit_days)]
    ee[, visit := ifelse(visit_days==0, 0, 1)]
    ee = merge(ee, sex, by=c('ID'))
  #Add paternity data
    ee = merge(ee, pp, by=c('ID','year_'), all.x=TRUE)
    ee[, ep := ifelse(is.na(ep),0,ep)]
    
    
#Model data        
    m1 = glmer(ep ~ visit + age + (1|ID) + (1|year_), family=binomial, data=ee[sex==1])
    summary(m1)
    m2 = glmer(ep ~ visit + age + (1|ID) + (1|year_), family=binomial, data=ee[sex==2])
    summary(m2)
    
    m1b = glmer(ep ~ visit_days + age + (1|ID) + (1|year_), family=binomial, data=ee[sex==1])
    summary(m1b)
    m2b = glmer(ep ~ visit_days + age + (1|ID) + (1|year_), family=binomial, data=ee[sex==2])
    summary(m2b)
    
    
    
#LIKELIHOOD OF EXTRA-PAIR PATERNITY WITH PARTICULAR BOX   
  #Model
    bb1 = bb[sex==1 & eppyn==1 & no<=2]
    length(unique(bb1$IDyear))
    m1 = glmer(ep ~ visited_terr + no + (1|year_) + (1|ID), family=binomial, data=bb1)
    summary(m1)
    m1b = glmer(ep ~ visit_days_terr + no + (1|year_) + (1|ID), family=binomial, data=bb1)
    summary(m1b)
    
    bb2 = bb[sex==2 & eppyn==1 & no<=2]
    length(unique(bb1$IDyear))
    m2 = glmer(ep ~ visited_terr + no + (1|year_) + (1|ID), family=binomial, data=bb2)
    summary(m2)
    m2b = glmer(ep ~ visit_days_terr + no + (1|year_) + (1|ID), family=binomial, data=bb2)
    summary(m2b)
    
    
  #Plot 
    d1 = allEffects(m1, xlevels=2)
    d1 = d1$"visited_terr"  %>% data.frame  %>% data.table
    d1[, sex := 1]
    d2 = allEffects(m2, xlevels=2)
    d2 = d2$"visited_terr"  %>% data.frame  %>% data.table
    d2[, sex := 2]
    e=rbind(d1,d2)
    e[, visited_terr := as.factor(visited_terr)]
    e[, sex := as.factor(sex)]
    p3 = ggplot() +
      geom_point(data=e, aes(x=visited_terr, y=fit, group=sex, colour=sex), position = position_dodge(width=.5 )) +
      geom_errorbar(data=e, aes(x=visited_terr, ymin=upper, ymax=lower, group=sex, colour=sex), width=.5, position = position_dodge(width=.5 )) +
      ylab("Likelihood of EPP at box") + xlab("") +
      scale_colour_manual(name = NULL, labels = c('male', 'female'), values=c('#619CFF', '#F8766D')) +
      scale_x_discrete(labels=c('No visit', 'Visit')) +
      ylim(0,.25) +
      labs(tag = "d)") +
      theme_classic() +
      theme(axis.title=element_text(size=14),
            axis.text=element_text(size=12),
            legend.position="none",
            plot.tag = element_text(size=16),
            legend.text = element_text(size=12))
    p3
    
    p = grid.arrange(p1, p2, p_tod, p3, ncol=2) 
    ggsave(p, file='plot.png', width=10, height=9)
    
    