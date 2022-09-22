
#Load packages
    require(SNB2)
    require(sdb)
    require(data.table)
    require(tidyr)
    require(magrittr)
    require(ggplot2)
    require(gridExtra)
    require(ggbeeswarm)
    require(gganimate)
    require(lme4)
    require(lmerTest)
    require(effects)
    require(tibble)
    require(dplyr)
    require(grid)
    require(chron)
    require(lubridate)
    
#connect to database
    con= dbcon(user = 'psantema', pwd = 'psantema2405', host = 'scidb.mpio.orn.mpg.de')


#GET SEX TABLE
    sex = as.data.table(dbq(con, 'SELECT ID, sex FROM BTatWESTERHOLZ.SEX'))
    write.csv(sex, file='./data/sex.csv', row.names=FALSE)
    

#Get TRANSPONDERS
    transponders_adults = as.data.table(dbq(con, 'SELECT ID, transponder, DATE(capture_date_time) as cap_date FROM BTatWESTERHOLZ.ADULTS 
                           	                                    WHERE transponder is not NULL'))
    transponders_chicks = as.data.table(dbq(con, 'SELECT ID, transponder, DATE(date_time) as cap_date FROM BTatWESTERHOLZ.CHICKS 
                           	                                    WHERE transponder is not NULL'))
    transponders = rbind(transponders_adults, transponders_chicks)
    transponders = transponders[, .(ID, cap_date=min(cap_date)), by=transponder] #Remove duplicates (multiple entries in database) 
    transponders[, transponder := toupper(transponder)]
    transponders = unique(transponders)
    rm(transponders_adults, transponders_chicks)
  #Add sex  
    transponders = merge(transponders, sex, by = 'ID', all.x = TRUE, allow.cartesian = TRUE)
  #Get capture date table
    cap_date = transponders[,.(ID, cap_date)]
    cap_date = cap_date[, .(cap_date=min(cap_date)), by=ID]
  #save table  
    transponders = transponders[,.(ID, transponder, sex)]
    write.csv(transponders, file='./data/transponders.csv', row.names=FALSE)


#GET DISTANCE BETWEEN BOXES
    distance = dbq(con, 'select * FROM BTatWESTERHOLZ.BOX_geoCoordinates order by box')
    distance = distance[, .(x, y) ] %>% dist
    distance = reshape2::melt(as.matrix(distance),  na.rm = TRUE ) %>% data.table %>% .[value > 0]
    setnames(distance, c('box1', 'box2', 'distance'))
    write.csv(distance, file='./data/distance.csv', row.names=FALSE)

#Get box coordinates
    coor = dbq(con, 'select * FROM BTatWESTERHOLZ.BOX_geoCoordinates order by box')
    write.csv(coor, file='./data/coor.csv', row.names=FALSE)
    
#Get start of nest building
    lin2015 = dbq(con, 'SELECT box, year(date_time) year_, dayofyear(date_time) yday from FIELD_2015_BTatWESTERHOLZ.NESTS WHERE nest_stage="LIN" or nest_stage="C" or nest_stage="BC" or nest_stage="B"')
    lin2016 = dbq(con, 'SELECT box, year(date_time) year_, dayofyear(date_time) yday from FIELD_2016_BTatWESTERHOLZ.NESTS WHERE nest_stage="LIN" or nest_stage="C" or nest_stage="BC" or nest_stage="B"')
    lin2017 = dbq(con, 'SELECT box, year(date_time) year_, dayofyear(date_time) yday from FIELD_2017_BTatWESTERHOLZ.NESTS WHERE nest_stage="LIN" or nest_stage="C" or nest_stage="BC" or nest_stage="B"')
    lin2018 = dbq(con, 'SELECT box, year(date_time) year_, dayofyear(date_time) yday from FIELD_2018_BTatWESTERHOLZ.NESTS WHERE nest_stage="LIN" or nest_stage="C" or nest_stage="BC" or nest_stage="B"')
    lin2019 = dbq(con, 'SELECT box, year(date_time) year_, dayofyear(date_time) yday from FIELD_2019_BTatWESTERHOLZ.NESTS WHERE nest_stage="LIN" or nest_stage="C" or nest_stage="BC" or nest_stage="B"')
    lin2020 = dbq(con, 'SELECT box, year(date_time) year_, dayofyear(date_time) yday from FIELD_2020_BTatWESTERHOLZ.NESTS WHERE nest_stage="LIN" or nest_stage="C" or nest_stage="BC" or nest_stage="B"')
    lin = rbind(lin2015, lin2016, lin2017, lin2018, lin2019, lin2020)
    lin = lin[, .(lin = min(yday)), by=c('year_', 'box')]
    write.csv(lin, file='./data/lin.csv', row.names=FALSE)


#GET AGE (code from Mihai)    
    age = dbq(con, 'SELECT ID, min(dt) firstCap , 
                  IF ( month(min(dt)) > 7, year(min(dt))+1, year(min(dt)) ) season, year(min(dt)) `year`  , min(age) ageFirstCap from 
                  (SELECT DISTINCT ID, age, capture_date_time dt from BTatWESTERHOLZ.ADULTS
                  UNION
                  SELECT DISTINCT ID, 0 age, date_time dt from BTatWESTERHOLZ.CHICKS 
                  where ID is not null and ID not like "X%" ) x
                  where dt > "0000-00-00"  
                  group by ID', enhance = TRUE)
    age = age[, .(ID, season, ageFirstCap)]
    age = age[, .(season = season : 2020), by = .(ID, ageFirstCap)]
    age[, k := 1:.N, ID]
  #capture as chicks (potential recruits)
    age[ageFirstCap == 0 & k > 2, age := 2]
    age[ageFirstCap == 0 & k ==2, age := 1]
    age[ageFirstCap == 0 & k ==1, age := 0]
  #captures as juvenile
    age[ ageFirstCap == 1 & k == 1 , age := 1]
    age[ ageFirstCap == 1 & k > 1 , age := 2]
  #captures as adults
    age[ ageFirstCap == 2  , age := 2]  
  #Save file  
    write.csv(age, file='./data/age.csv', row.names=FALSE)
    

#GET BREEDING BOXES AND INDIVIDUALS
    brbox_m = dbq(con, 'SELECT year_, box, IDmale ID FROM BTatWESTERHOLZ.BREEDING WHERE year_ >= 2015')
    brbox_f = dbq(con, 'SELECT year_, box, IDfemale ID FROM BTatWESTERHOLZ.BREEDING WHERE year_ >= 2015')
    brbox = rbind(brbox_m, brbox_f)
    rm(brbox_f, brbox_m)
    brbox = unique(brbox)
    brbox[, breeder := 1]
    brbox[, terr_owner := 1]


#GET ALL BREEDING BOXES    
    brx = unique(brbox[,.(year_,box)])
    rm(brbox)
    write.csv(brx, file='./data/brx.csv', row.names=FALSE)

#GET ALL SYCCESSFULLY BREEDING INDIVIDUALS 
    bri_m = dbq(con, 'SELECT year_, IDmale ID, box, clutch, dayofyear(firstEgg) firstEgg, dayofyear(hatchDate) hatchDate, hatched, fledged, dayofyear(fledgeDate) fledgeDate FROM BTatWESTERHOLZ.BREEDING WHERE year_ >= 2015 and fledged>0')
    bri_f = dbq(con, 'SELECT year_, IDfemale ID, box, clutch, dayofyear(firstEgg) firstEgg, dayofyear(hatchDate) hatchDate, hatched, fledged, dayofyear(fledgeDate) fledgeDate FROM BTatWESTERHOLZ.BREEDING WHERE year_ >= 2015 and fledged>0')
    bri = rbind(bri_m, bri_f)
  #exclude birds associated with two breeding attempts  
    bri[, n := .N, by=c('year_','ID')]
    bri = bri[n==1]
    bri[, n := NULL]
  #add when birds started having an active nest  
    bri = merge(bri, lin, by=c('box', 'year_'), all.x=TRUE)
    bri[, lin := ifelse(is.na(lin), firstEgg, lin)]  
  #Only include birds that were capture before relevant period  
    bri = merge(bri, cap_date, by=c('ID'), all.x=TRUE)
    bri = bri[year(cap_date)<year_ | (year(cap_date)==year_ & yday(cap_date)<lin)]
    bri[, cap_date := NULL]
  #add age  
    age = data.table(read.csv(file='./data/age.csv'))
    bri = merge(bri, age[,.(ID, season, age)], by.x=c("ID","year_"), by.y=c("ID","season"))
    bri[, IDyear := paste(ID,year_)]
    bri[, age := ifelse(age==0,1,age)]
    write.csv(bri, file='./data/bri.csv', row.names=FALSE)
    
#GET EXTRA_PAIR SIRES
    ep = dbq(con, 'SELECT year_, mother IDfemale, father IDmale FROM BTatWESTERHOLZ.PATERNITY WHERE year_ >= 2015 and epy=1')
    ep = merge(ep, bri[,.(ID, year_, box)], by.x=c('year_','IDfemale'), by.y=c('year_','ID'))
    ep = merge(ep, bri[,.(ID, year_, box)], by.x=c('year_','IDmale'), by.y=c('year_','ID'), suffixes=c('_f','_m'))
    ep = ep[box_m!=box_f]
    ep = unique(ep)
    ep1 = ep[,.(year_, ID=IDmale, box=box_m, box_partner=box_f)]    
    ep2 = ep[,.(year_, ID=IDfemale, box=box_f, box_partner=box_m)]    
    ep = rbind(ep1, ep2)
    ep[, ep:=1]   
    write.csv(ep, file='./data/ep.csv', row.names=FALSE)
    
#Get sunrise time
    sun = as.data.table(getSunlightTimes(date = seq(as.Date("2015-01-01"), as.Date("2021-12-31"),'days'), lat = 48.143021, lon = 10.890508, keep = c("sunrise", "sunset"), tz = "CET"))
    sun[, sunrise := format(as.POSIXct(sunrise), format = "%H:%M:%S")]
    sun[, sunrise := chron(times=sunrise)]
    sun[, sunrise := as.numeric(sunrise)*24]
    sun[, sunset := format(as.POSIXct(sunset), format = "%H:%M:%S")]
    sun[, sunset := chron(times=sunset)]
    sun[, sunset := as.numeric(sunset)*24]
    sun[, year_ := year(date)]
    sun[, yday := yday(date)]
    sun = sun[,.(year_, yday, sunrise, sunset)]


#GET ALL TRANSPONDER READINGS
    readings2015 = as.data.table(dbq(con, 'SELECT site box, transponder, datetime_, year(datetime_) year_, dayofyear(datetime_) yday
                                                              FROM BTatWESTERHOLZ.transponders
                                                              WHERE site_type=1 and datetime_>="2015-01-01" and datetime_<="2015-07-31"'))
    readings2016 = as.data.table(dbq(con, 'SELECT site box, transponder, datetime_, year(datetime_) year_, dayofyear(datetime_) yday
                                                              FROM BTatWESTERHOLZ.transponders
                                                              WHERE site_type=1 and datetime_>="2016-01-01" and datetime_<="2016-07-31"'))
    readings2017 = as.data.table(dbq(con, 'SELECT site box, transponder, datetime_, year(datetime_) year_, dayofyear(datetime_) yday
                                                              FROM BTatWESTERHOLZ.transponders
                                                              WHERE site_type=1 and datetime_>="2017-01-01" and datetime_<="2017-07-31"'))
    readings2018 = as.data.table(dbq(con, 'SELECT site box, transponder, datetime_, year(datetime_) year_, dayofyear(datetime_) yday
                                                              FROM BTatWESTERHOLZ.transponders
                                                              WHERE site_type=1 and datetime_>="2018-01-01" and datetime_<="2018-07-31"'))
    readings2019 = as.data.table(dbq(con, 'SELECT site box, transponder, datetime_, year(datetime_) year_, dayofyear(datetime_) yday
                                                              FROM BTatWESTERHOLZ.transponders
                                                              WHERE site_type=1 and datetime_>="2019-01-01" and datetime_<="2019-07-31"'))
    readings2020 = as.data.table(dbq(con, 'SELECT site box, transponder, datetime_, year(datetime_) year_, dayofyear(datetime_) yday
                                                              FROM BTatWESTERHOLZ.transponders
                                                              WHERE site_type=1 and datetime_>="2020-01-01" and datetime_<="2020-07-31"'))
    readings = rbind(readings2015, readings2016, readings2017, readings2018, readings2019, readings2020)
  #Add IDs  
    transponders = data.table(read.csv(file='./data/transponders.csv'))
    readings = merge(readings, transponders, by = 'transponder', all.x = TRUE)
    readings = readings[!is.na(ID)] #remove readings not associated with a bird ID (<.5% of total)
  #add in which territory each box is located      
    terrbox = data.table(read.csv(file='./data/terrbox.csv'))
    readings = merge(readings, terrbox, by=c('box', 'year_'))
  #only retain individual who did not breed in the visited box 
    brbox = data.table(read.csv(file='./data/brbox.csv')) 
    readings = merge(readings, brbox[,.(year_, box, ID, breeder)], by.x=c('territory', 'ID', 'year_'), by.y=c('box', 'ID', 'year_'), all.x = TRUE)
    readings = readings[is.na(breeder)]   
    readings[, breeder := NULL]
  #faulty data: looks like SD card of box 12 in 2015 was swapped with other nest      
    readings = readings[box!='12' | year_!='2015'] 
  #save file  
    readings = readings[, .(year_, box, ID, sex, yday, datetime_, territory)]
    write.csv(readings, file='./data_big/readings.csv', row.names=FALSE)
    

#GET EXTRA-BOX VISITS
    visits = as.data.table(read.csv(file='./data_big/readings.csv'))
  #Get active dates of visiting bird and visited box and exclude visits made when bird or visited box did not have active next
    visits = merge(visits, bri[,.(ID, year_, lin, age, fledgeDate, IDyear)], by=c('ID', 'year_'))
    visits = visits[yday>=lin & yday<=fledgeDate]
  #extract time of day, adjust for year differences, and remove extreme values
    visits[, chron := format(as.POSIXct(datetime_), format = "%H:%M:%S")]
    visits[, datetime_ := NULL]
    visits[, chron := chron(times=chron)]
    visits[, chron := as.numeric(chron)*24]
    visits[, chron := ifelse(year_==2015, chron-1,chron)]
    visits[, chron := ifelse(year_==2016, chron-1,chron)]
    visits[, chron := ifelse(year_==2017, chron-1,chron)]
    visits[, chron := ifelse(year_==2019 & yday<90, chron+1,chron)]
    visits[, chron := ifelse(year_==2020, chron+2,chron)]
  #only keep one visit per 10 minutes  
    visits[, chron2 := floor(chron*6)/6]
    visits = visits[,.SD[sample(.N, min(1,.N))],by=c('year_','ID','yday','box','chron2')]   
    visits[, chron2 := NULL]
  #Remove visits with unrealistic times  
  #visits = visits[chron>4 & chron<22]
  #make copy of all visits
    visits_all = copy(visits)
    visits_all = merge(visits_all, sun, by=c('year_','yday'))
    write.csv(visits_all, file='./data/visits_all.csv', row.names=FALSE)
  #only keep one record per day per box per individual 
    visits[, visits := .N, by=c('year_','ID','yday','box')]
    visits_day = visits[,.SD[sample(.N, min(1,.N))], by=c('year_','ID','yday','box','territory')]   
  #Set variables
    visits_day[, sex := as.factor(sex)]
    visits_day[, age := as.factor(age)]
  #save file
    write.csv(visits_day, file='./data/visits_day.csv', row.names=FALSE)
    

#descriptive statistics
  #total number of visits  
    sum(visits_day$visits)
  #number of visits to breeding boxes  
    sum(visits_day$visits[visits_day$box==visits_day$territory])
    

#proportion of visits before sunrise and after sunset
    100/nrow(visits_all)*nrow(visits_all[chron>sunrise & chron<sunset])
    100/nrow(visits_all)*nrow(visits_all[chron<sunrise])
    100/nrow(visits_all)*nrow(visits_all[chron>sunset])
