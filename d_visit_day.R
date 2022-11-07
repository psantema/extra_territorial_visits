

#PREPARE TABLE WITH WHETHER AN INDIVIDUAL MADE AT LEAST ONE VISIT (AND HOW MANY) FOR EACH DAY OF THE SEASON

#Load tables
    visits_day = as.data.table(read.csv(file='./data/visits_day.csv'))
    brx = as.data.table(read.csv(file='./data/brx.csv'))
    bri = as.data.table(read.csv(file='./data/bri.csv'))
    
    
#Get table visits
    foo = copy(visits_day)
    foo = foo[,.(IDyear, yday, visits)]
    foo = unique(foo)
    foo[, visit := 1]
    
#Create table with all combinations of ID and yday
    dd = as.data.table(expand.grid(yday=c(60:151), IDyear=unique(bri$IDyear)))
  #Add breeding data  
    dd = merge(dd, bri[,.(IDyear, ID, age, lin, firstEgg, clutch, hatchDate, fledgeDate, year_)], c('IDyear'), all.x=TRUE)
  #Add visit data
    dd = merge(dd, foo, by=c('IDyear','yday'), all.x=TRUE)
    dd[, visit := ifelse(is.na(visit),0,1)]
    dd[, visits := ifelse(is.na(visits),0,visits)]
  #Create relative timing variables  
    dd[, hatchDate := ifelse(is.na(hatchDate), firstEgg+clutch+14, hatchDate)]
    dd[, fledgeDate := ifelse(is.na(fledgeDate), 150, fledgeDate )]
  #Only include days after start of nest building and before fledging 
    dd = dd[yday>=lin & yday<=fledgeDate]
    dd[, yday_egg := yday-firstEgg]
    dd[, yday_hatch := yday-hatchDate]
  #Add sex of individual  
    sex = data.table(read.csv(file='./data/sex.csv'))
    dd = merge(dd, sex, by=c('ID'))
  #Exclude erroneous datapoint  
    dd[, sex := as.factor(sex)]
    dd[, age := as.factor(age)]
    
    
#Model data    
    m1 = glmer(visit ~ sex*age + sex*yday_egg + poly(yday_egg,2) + (1|IDyear), family=binomial, data=dd)
    summary(m1)
    
    m2 = glmer(visit ~ sex*age + sex*yday_hatch + poly(yday_hatch,2) + (1|IDyear), family=binomial, data=dd)
    summary(m2)
    
    
#Plot daily likelihood of at least 1 visit (relative to laying start)
  #Get prediction for males  
    dd1 = dd[sex==1 & yday_egg>=-10 & yday_egg<=10 ]
    dd1[, yday_egg := as.factor(yday_egg)]
    m1 = glm(visit ~ yday_egg , family = binomial, data=dd1)
    d1 = allEffects(m1, xlevels=21)
    d1 = d1$"yday_egg"  %>% data.frame  %>% data.table
    d1[, sex := 1]
  #Get prediction for females    
    dd2 = dd[sex==2 & yday_egg>=-10 & yday_egg<=10 ]
    dd2[, yday_egg := as.factor(yday_egg)]
    m2 = glm(visit ~ yday_egg , family = binomial, data=dd2)
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
      ylab("Proportion performing ≥1 visit") + xlab("Day relative to first egg") +
      scale_colour_manual(values=c('#619CFF', '#F8766D')) +
      geom_vline(xintercept=0, lty=2, size=.5) +
      ylim(0,.5) +
      #labs(tag = "a)") +
      theme_classic() +
      theme(axis.title=element_text(size=14),
            axis.text=element_text(size=12),
            legend.position = "none",
            plot.tag = element_text(size=16))
    p1
    
#Plot daily likelihood of at least 1 visit (relative to hatching)    
  #Get prediction for males    
    dd1 = dd[sex==1 & yday_hatch>=-14 & yday_hatch<=20 ]
    dd1[, yday_hatch := as.factor(yday_hatch)]
    m1 = glm(visit ~ yday_hatch , family = binomial, data=dd1)
    d1 = allEffects(m1, xlevels=35)
    d1 = d1$"yday_hatch"  %>% data.frame  %>% data.table
    d1[, sex := 1]
  #Get prediction for males    
    dd2 = dd[sex==2 & yday_hatch>=-14 & yday_hatch<=20 ]
    dd2[, yday_hatch := as.factor(yday_hatch)]
    m2 = glm(visit ~ yday_hatch , family = binomial, data=dd2)
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
      ylim(0,.5) +
      #labs(tag = "") +
      theme_classic() +
      theme(axis.title=element_text(size=14),
            axis.text=element_text(size=12),
            legend.position = c(.9,.9),
            plot.tag = element_text(size=16),
            legend.text = element_text(size=12))
    p2
    
  #Combine plots  and save
    fig2a = grid.arrange(p1, p2, ncol=2, widths=c(2,3))
    ggsave(fig2a, file='fig2.tiff', width=10, height=5.5)
    
    
    
