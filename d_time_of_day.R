

##VISIT TIME OF DAY

#Get dataset with all visits
    g = as.data.table(read.csv(file='./data/visits_all.csv'))
    
#Descriptive statistics
  #Proportion of visits after sunrise and before sunset  
    100/nrow(g)*nrow(g[chron>sunrise & chron<sunset])
  #Proportion of visits before sunrise 
    100/nrow(g)*nrow(g[chron<sunrise])
  #Proportion of visits after sunset
    100/nrow(g)*nrow(g[chron>sunset])
    
#Plot data
    p_time = ggplot() +
      geom_histogram(data=g[sex==1], aes(x=chron, y=..count..), colour="black", fill="#619CFF", binwidth=1/2, boundary = 0, alpha=.8) +
      geom_histogram(data=g[sex==2], aes(x=chron, y=..count..), colour="black", fill="white", binwidth=1/2, boundary = 0) +
      geom_histogram(data=g[sex==2], aes(x=chron, y=..count..), colour="black", fill="#F8766D", binwidth=1/2, boundary = 0, alpha=.8) +
      scale_x_continuous(limits = c(0,24), breaks = c(0:24), labels = c("00:00","","","","04:00","","","","08:00","","","","12:00","","","","16:00","","","","20:00","","","","24:00")) +
      xlab("Time of day") + ylab("Number of visits") + 
      labs(tag = "c)") +
      theme_classic() +
      theme(axis.text=element_text(size=12),
            plot.tag = element_text(size=16),
            axis.title=element_text(size=14))
    p_time
    
    
