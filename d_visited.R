
#PREPARE TABLE WITH WHETHER A BREEDING PAIR RECEIVED AT LEAST ONE VISIT FOR EACH DAY OF THE SEASON


#Load tables
    visits_day = as.data.table(read.csv(file='./data/visits_day.csv'))
    brx = as.data.table(read.csv(file='./data/brx.csv'))
    bri = as.data.table(read.csv(file='./data/bri.csv'))


#Get table visits
  #get male visits
    mm = copy(visits_day)
    mm[, boxyear := paste(territory, year_, sep='_')] #change territory to box when only considering visits to the breeding box
    mm = mm[sex==1, .(visits_m=sum(visits)), by=c('boxyear', 'yday')]
    mm = unique(mm)
    mm[, visit_m := 1]
  #get male visits
    ff = copy(visits_day)
    ff[, boxyear := paste(territory, year_, sep='_')] #change territory to box when only considering visits to the breeding box
    ff = ff[sex==2, .(visits_f=sum(visits)), by=c('boxyear', 'yday')]
    ff = unique(ff)
    ff[, visit_f := 1]

    
#Get lay date, hatch date and fledge date of all boxes
    br = unique(bri[,.(box, year_, lin, firstEgg, clutch, hatchDate, fledgeDate)])
    br[, boxyear := paste(box, year_, sep='_')]

    
#Create table with all combinations of box and yday
    bb = as.data.table(expand.grid(yday=c(60:151), boxyear=unique(br$boxyear)))
  #Add breeding data  
    bb = merge(bb, br, by=c('boxyear'), all.x=TRUE)
  #Add visit data
    bb = merge(bb, mm, by=c('boxyear','yday'), all.x=TRUE)
    bb = merge(bb, ff, by=c('boxyear','yday'), all.x=TRUE)
    bb[, visit_m := ifelse(is.na(visit_m),0,1)]
    bb[, visits_m := ifelse(is.na(visits_m),0,visits_m)]
    bb[, visit_f := ifelse(is.na(visit_f),0,1)]
    bb[, visits_f := ifelse(is.na(visits_f),0,visits_f)]
  #Create relative timing variables  
    bb[, hatchDate := ifelse(is.na(hatchDate), firstEgg+clutch+14, hatchDate)]
    bb[, fledgeDate := ifelse(is.na(fledgeDate), 150, fledgeDate )]
  #Only include days after start of nest building and before fledging 
    bb = bb[yday>=lin & yday<=fledgeDate]
    bb[, yday_egg := yday-firstEgg]
    bb[, yday_hatch := yday-hatchDate]
    
    
#Model data    
    m1 = glmer(visit_m ~ poly(yday_egg,2) + (1|boxyear), family=binomial, data=bb)
    summary(m1)
    
    m2 = glmer(visit_m ~ poly(yday_hatch,2) + (1|boxyear), family=binomial, data=bb)
    summary(m2)
    
    m3 = glmer(visit_f ~ poly(yday_egg,2) + (1|boxyear), family=binomial, data=bb)
    summary(m3)
    
    m4 = glmer(visit_f ~ poly(yday_hatch,2) + (1|boxyear), family=binomial, data=bb)
    summary(m4)
    
    
#Plot daily likelihood of RECEIVING at least 1 visit (relative to laying start)
  #Get prediction for males  
    bb1 = bb[yday_egg>=-10 & yday_egg<=10 ]
    bb1[, yday_egg := as.factor(yday_egg)]
    m1 = glm(visit_m ~ yday_egg , family = binomial, data=bb1)
    d1 = allEffects(m1, xlevels=21)
    d1 = d1$"yday_egg"  %>% data.frame  %>% data.table
    d1[, sex := 1]
  #Get prediction for females    
    bb2 = bb[yday_egg>=-10 & yday_egg<=10 ]
    bb2[, yday_egg := as.factor(yday_egg)]
    m2 = glm(visit_f ~ yday_egg , family = binomial, data=bb2)
    d2 = allEffects(m2, xlevels=21)
    d2 = d2$"yday_egg"  %>% data.frame  %>% data.table
    d2[, sex := 2]
  #Combine male and female data    
    e=rbind(d1,d2)
    e[, sex := as.factor(sex)]
    e[, yday_egg := as.numeric(paste(yday_egg))]
  #Plot data  
    p1 = ggplot() +
      geom_errorbar(data=e, aes(yday_egg, ymin=upper, ymax=lower, group=sex, colour=sex), position = position_dodge(width=.5 )) +
      geom_point(data=e, aes(x=yday_egg, y=fit, group=sex, colour=sex), position = position_dodge(width=.5 )) +
      ylab("Proportion receiving ≥1 visit") + xlab("Day relative to first egg") +
      scale_colour_manual(values=c('#619CFF', '#F8766D')) +
      geom_vline(xintercept=0, lty=2, size=.5) +
      ylim(0,.4) +
      #labs(tag = "b)") +
      theme_classic() +
      theme(axis.title=element_text(size=14),
            axis.text=element_text(size=12),
            legend.position = "none",
            plot.tag = element_text(size=16))
   p1
    
   
#Plot daily likelihood of at least 1 visit (relative to hatching)    
  #Get prediction for males    
     bb1 = bb[yday_hatch>=-14 & yday_hatch<=20 ]
     bb1[, yday_hatch := as.factor(yday_hatch)]
     m1 = glm(visit_m ~ yday_hatch , family = binomial, data=bb1)
     d1 = allEffects(m1, xlevels=35)
     d1 = d1$"yday_hatch"  %>% data.frame  %>% data.table
     d1[, sex := 1]
  #Get prediction for males    
     bb2 = bb[yday_hatch>=-14 & yday_hatch<=20 ]
     bb2[, yday_hatch := as.factor(yday_hatch)]
     m2 = glm(visit_f ~ yday_hatch , family = binomial, data=bb2)
     d2 = allEffects(m2, xlevels=35)
     d2 = d2$"yday_hatch"  %>% data.frame  %>% data.table
     d2[, sex := 2]
  #Combine male and female data  
     e=rbind(d1,d2)
     e[, sex := as.factor(sex)]
     e[, yday_hatch := as.numeric(paste(yday_hatch))]
  #Plot data      
     p2 = ggplot() +
       geom_errorbar(data=e, aes(yday_hatch, ymin=upper, ymax=lower, group=sex, colour=sex), position = position_dodge(width=.5 )) +
       geom_point(data=e, aes(x=yday_hatch, y=fit, group=sex, colour=sex), position = position_dodge(width=.5 )) +
       ylab("") + xlab("Day relative to hatching") +
       scale_colour_manual(name = NULL, labels = c('male', 'female'), values=c('#619CFF', '#F8766D')) +
       geom_vline(xintercept=0, lty=3, size=.5) +
       ylim(0,.4) +
       #labs(tag = "") +
       theme_classic() +
       theme(axis.title=element_text(size=14),
             axis.text=element_text(size=12),
             legend.position = c(.9,.9),
             plot.tag = element_text(size=16),
             legend.text = element_text(size=12))
     p2
     
     
  #Combine plots  and save
     fig2b = grid.arrange(p1, p2, ncol=2, widths=c(2,3))
     ggsave(fig2b, file='plot.png', width=10, height=6)
     
     
     
     