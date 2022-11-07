
#Load tables
    bri = as.data.table(read.csv(file='./data/bri.csv'))
    sex = as.data.table(read.csv(file='./data/sex.csv'))
    terrbox = as.data.table(read.csv(file='./data/terrbox.csv'))
    brx = as.data.table(read.csv(file='./data/brx.csv'))
    neighbour = as.data.table(read.csv(file='./data/neighbour.csv'))
    visits_day = as.data.table(read.csv(file='./data/visits_day.csv'))
    terrbox = as.data.table(read.csv(file='./data/terrbox.csv'))
    ep = as.data.table(read.csv(file='./data/ep.csv'))

#Create new table
  #produce all combinations of boxes and IDyear  
    bb = as.data.table(expand.grid(box=c(1:277), IDyear=unique(bri$IDyear)))
  #add breeding data of breeders
    bb = merge(bb, bri[,.(ID, year_, IDyear, box, age)], c('IDyear'), all.x=TRUE, suffixes=c('','_br'))
  #add sex  
    bb = merge(bb, sex, by=c('ID'))
    bb[, sex := as.factor(sex)]  
  #add for each box which territory it belongs to
    bb = merge(bb, terrbox, by=c('year_','box'))  
  #remove boxes within an individual's own territory
    bb = bb[box_br!=territory]  
  #Only consider boxes that are in the breeding table 
    bb = merge(bb, brx, c('year_','box')) 
    bb[, territory:=NULL]
  #add distance and neighbourhood order
    bb = merge(bb, neighbour, by.x=c('box','box_br','year_'), by.y=c('box1','box2','year_'))
  #only include boxes within 3 territories  
    bb = bb[no<=3] 
  #get visits data 
    v_box = copy(visits_day)
    v_box = v_box[territory==box,.(visited_box=1, visit_days_box=.N, visits_box=sum(visits)), by=c('year_','ID', 'territory')]
    v_terr = copy(visits_day)
    v_terr = v_terr[,.(visited_terr=1, visit_days_terr=.N, visits_terr=sum(visits)), by=c('year_','ID', 'territory')] # use instead of above when assessing visits to territory
  #add visits information to data
    bb = merge(bb, v_box, by.x=c('year_','ID', 'box'), by.y=c('year_','ID', 'territory'), all.x=TRUE)
    bb = merge(bb, v_terr, by.x=c('year_','ID', 'box'), by.y=c('year_','ID', 'territory'), all.x=TRUE)
    bb[, visited_box := ifelse(is.na(visited_box),0,visited_box)]
    bb[, visit_days_box := ifelse(is.na(visit_days_box),0,visit_days_box)]
    bb[, visits_box := ifelse(is.na(visits_box),0,visits_box)]
    bb[, visited_terr := ifelse(is.na(visited_terr),0,visited_terr)]
    bb[, visit_days_terr := ifelse(is.na(visit_days_terr),0,visit_days_terr)]
    bb[, visits_terr := ifelse(is.na(visits_terr),0,visits_terr)]
  #add epp with individual of visited territory
    bb = merge(bb, ep[,.(year_, ID, box_partner, ep)], by.x=c('year_', 'ID', 'box'), by.y=c('year_', 'ID', 'box_partner'), all.x=TRUE) 
    bb[, ep := ifelse(is.na(ep),0,ep)]
    bb[, eppyn := max(ep), by=c('ID','year_')]  
    
#Descriptive statistics
  #Likelihood of visiting 1st order neighbour for male and female  
    mean(bb$visited_terr[bb$no==1 & bb$sex==1])*100
    mean(bb$visited_terr[bb$no==1 & bb$sex==2])*100
  #Likelihood of visiting 3rd order neighbour for male and female      
    mean(bb$visited_terr[bb$no==3 & bb$sex==1])*100
    mean(bb$visited_terr[bb$no==3 & bb$sex==2])*100
    
    
#Model visits 
    bb1 = bb[sex==1]
    m1 = glmer(visited_terr ~ poly(no,2) + poly(distance,2) + (1|year_) + (1|ID), family=binomial, data=bb1)
    summary(m1)
    
    bb2 = bb[sex==2]
    m2 = glmer(visited_terr ~ poly(no,2) + poly(distance,2) + (1|year_) + (1|ID), family=binomial, data=bb2)
    summary(m2)
    
    cor.test(bb$no, bb$distance)
    
    
#Plot neighbourhood order 
  #Get predictions male    
    bb1 = bb[sex==1]
    bb1[, no := as.factor(no)]
    m1 = glmer(visited_terr ~ no + (1|year_) + (1|ID), family=binomial, data=bb1)
    d1 = allEffects(m1)
    d1 = d1$"no"  %>% data.frame  %>% data.table
    d1[, sex := 1]
  #Get predictions female  
    bb2 = bb[sex==2]
    bb2[, no := as.factor(no)]
    m2 = glmer(visited_terr ~ no + (1|year_) + (1|ID) , family=binomial, data=bb2)
    d2 = allEffects(m2)
    d2 = d2$"no"  %>% data.frame  %>% data.table
    d2[, sex := 2]
  #Combine male and female  
    e=rbind(d1,d2)
    e[, sex := as.factor(sex)]
  #Plot data  
    p1 = ggplot() +
      geom_point(data=e, aes(x=no, y=fit, group=sex, colour=sex), position = position_dodge(width=.5 )) +
      geom_errorbar(data=e, aes(x=no, ymin=upper, ymax=lower, group=sex, colour=sex), width=.5, position = position_dodge(width=.5 )) +
      ylab("Likelihood of performing ≥1 visit") + xlab("Neighbourhood order") +
      scale_colour_manual(values=c('#619CFF', '#F8766D')) +
      labs(tag = "a)") +
      ylim(0,.35) +
      theme_classic() +
      theme(axis.title=element_text(size=14),
            axis.text=element_text(size=12),
            legend.position="none",
            plot.tag = element_text(size=16))
    p1
    
#Plot distance    
  #Get predictions male  
    bb1 = bb[sex==1]
    m1 = glmer(visited_terr ~ poly(distance,2) + (1|year_) + (1|ID) , family=binomial, data=bb1)
    d1 = allEffects(m1, xlevels=200)
    d1 = d1$"poly(distance,2)"  %>% data.frame  %>% data.table
    d1[, sex := 1]
  #Get predictions female      
    bb2 = bb[sex==2]
    m2 = glmer(visited_terr ~ poly(distance,2) + (1|year_) + (1|ID) , family=binomial, data=bb2)
    d2 = allEffects(m2, xlevels=200)
    d2 = d2$"poly(distance,2)"  %>% data.frame  %>% data.table
    d2[, sex := 2]
  #Combine male and female  
    e=rbind(d1,d2)
    e[, sex := as.factor(sex)]
    e = e[distance>20]
    p2 = ggplot() +
      geom_line(data=e, aes(x=distance, y=fit, group=sex), size=0.3) +
      geom_ribbon(data=e, aes(x=distance, ymin=lower, ymax=upper, group=sex, fill=sex), alpha=0.7) +
      ylab("") + xlab("Distance (m)") +
      scale_fill_manual(name = NULL, labels = c('male', 'female'), values=c('#619CFF', '#F8766D')) +
      labs(tag = "b)") +
      ylim(0,.50) +
      xlim(0,150) +
      theme_classic() +
      theme(axis.title=element_text(size=14),
            axis.text=element_text(size=12),
            legend.position=c(.85,.9),
            plot.tag = element_text(size=16),
            legend.text = element_text(size=11))
    p2
    
    
    fig3 = grid.arrange(p1, p2, ncol=2)
    ggsave(fig3, file='fig3.tiff', width=9, height=5)
    
    