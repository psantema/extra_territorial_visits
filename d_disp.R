
#IS DISPERSAL PREDICTED BY WHETHE RO NOT INDIVIDUAL MADE EXTRA-TERRITORIAL NESTBOX-VISITS


#Load tables
    visits_day = as.data.table(read.csv(file='./data/visits_day.csv'))
    bri = as.data.table(read.csv(file='./data/bri.csv'))
    distance = as.data.table(read.csv(file='./data/distance.csv'))
    terrbox = as.data.table(read.csv(file='./data/terrbox.csv'))
    sex = as.data.table(read.csv(file='./data/sex.csv'))
    
    
#Prepare data
  #Get for each individual whether it made at least one visit and on how many visits it made at leats one visits
    vv = copy(visits_day)
    vv = vv[,.(visit_days = length(unique(yday))), by=c('year_','ID')]

  #Get breeding data
    br = bri[,.(ID, year_, box)]
    br[, year2 := year_+1]

  #Combine breeding data year1 and year2   
    ff = merge(br, br[,.(year_, box, ID)], by.x=c('year2','ID'), by.y=c('year_','ID'), suffixes=c('','2'))
  #get distance between two boxes
    distance = data.table(read.csv(file='./data/distance.csv')) 
    ff = merge(ff, distance, by.x=c('box','box2'), by.y=c('box1','box2'), all.x=TRUE)
    ff[, distance := ifelse(is.na(distance),0,distance)]
  #Add whether box2 was outside territory borders of box1
    ff = merge(ff, terrbox, by.x=c('year_','box2'), by.y=c('year_','box')) 
    ff[, disp := ifelse(box2!=territory,1,0)]
    ff[, disp := ifelse(distance<70,0,disp)]
  #add visit behaviour to data  
    ff = merge(ff, vv, by=c('year_','ID'), all.x=TRUE)
    ff[, visit_days := ifelse(is.na(visit_days), 0, visit_days)]
    ff[, visit := ifelse(visit_days==0, 0, 1)]
    ff = merge(ff, sex, by=c('ID')) 


#Model data    
  #Model whether visits (yes/no) predicts dispersal
    m1 = glmer(disp ~ visit + (1|ID) + (1|year_),  data=ff[sex==1], family = binomial)
    summary(m1)
    m2 = glmer(disp ~ visit + (1|year_),  data=ff[sex==2], family = binomial)
    summary(m2)
    
  #Model whether number of visits (yes/no) predicts dispersal  
    m1b = glmer(disp ~ visit_days + (1|ID) + (1|year_),  data=ff[sex==1], family = binomial)
    summary(m1b)
    m2b = glmer(disp ~ visit_days + (1|ID) + (1|year_),  data=ff[sex==2], family = binomial)
    summary(m2b)
    
    
 
#LIKELIHOOD OF DISPERSAL TO A PARTICULAR BOX  
  #copy data set with all combinations of breeders and occupied nest boxes within 3 neighbourhood orders 
    disp = copy(bb)
    disp = disp[,.(ID, year_, IDyear, box_br, box, no, visited_box, visited_terr, visit_days_terr, sex)]
    disp[, year2 := year_+1]
  #get dispersal data
    mm = copy(ff)
    mm = mm[disp==1,.(ID, year_, box2)]
    mm = merge(mm, terrbox, by.x=c('year_','box2'), by.y=c('year_','box'))
  #restrict data to individual that dispersed 
    disp = merge(disp, mm, by.x=c('year_','ID'), by.y=c('year_','ID'))
  #add whether individual dispersed to particular territory
    disp[, dispersed := ifelse(box==territory,1,0)]
  #add whether individual dispersed to any of the listed (previously occupied) boxes
    disp[, disp := max(dispersed), by=c('ID','year_')]
  #restrict data to bird that dispersed to a box that was previously occupied
    disp = disp[disp==1]
    disp[, disp := NULL]
    
  #Model 
    disp1 = disp[sex==1]
    length(unique(disp1$IDyear))
    m1 = glm(dispersed ~ visited_terr +  no, family=binomial, data=disp1)
    summary(m1)
    m1b = glm(dispersed ~ visit_days_terr +  no, family=binomial, data=disp1)
    summary(m1b)
    
    disp2 = disp[sex==2]
    length(unique(disp2$IDyear))
    m2 = glm(dispersed ~ visited_terr + no, family=binomial, data=disp2)
    summary(m2)
    m2b = glm(dispersed ~ visit_days_terr +  no, family=binomial, data=disp2)
    summary(m2b)
    
    
    
