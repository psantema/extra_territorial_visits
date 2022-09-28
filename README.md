
# Blue tit extra-territorial nextbox visits


### Description of scripts
d_data
  Produces all main data files and saves them in 'data' folder
  NOTE: Retrieves data from local database and cannot be run without credentials. However, the resulting files are available in 'data' folder.
  
d_visits_day
  Produces summary table (dd) with visit information for each individual for each day
  Produces figure 2
  
d_visits_box
  Produces summary table (bb) with visit information for each individual for each box within the first 3 neighbourhood orders
  Produces figure 3a and 3b
  
d_time_of_day
  Produces figure 3c
  
d_descriptive
  Produces descriptive statistcs
  Produces figure 1

d_epp
  Analyses relation between extra-territorial visit behaviour and extra-pair paternity
  Produced figure 3d
  
d_disp
  Analyses relation between extra-territorial visit behaviour and dispersal


### Description of data files

age.csv (contains ages of all individuals in each season)
  ID  - ID of individual
  k   - age in years
  age - age as category (1=yearling, 2=adult)
  
  
bri.csv (contains all breeding individuals)
  ID          - unique identifier for each individual
  year_       -
  box         - unique identifier for each nestbox
  clutch      - number of eggs
  firstEgg    - day of first egg (1 = Jan 1)
  hatchDate   - day of first hatching (1 = Jan 1)
  hatched     - number of hatchlings
  fledged     - number of fledglings
  fledgeDate  - day of last fledging (1 = Jan 1)
  lin         - day of start nest building  (1 = Jan 1)
  age         - age as category (1=yearling, 2=adult)
  IDyear      - unique identifier for each individual for each year


brx.csv (contains all occupied breeding boxes)
  year_ -
  box   - unique identifier for each nestbox


coor.csv (contains coordinates for all nest boxes)
  box - unique identifier for each nestbox
  x   - x-coordinate
  y   - y-coordinate


distance.csv (contains distances between all combinations of nestboxes)
  box1      -
  box2      -
  distance  - distance in meters


ep.csv (contains all cases of extra-pair paternity)
  year_       - 
  ID          - unique identifier for each individual
  box         - box of focal individual
  box_partner - box of extra-pair pertner of focal individual


lin.csv (contains start date of nestbuilding for each nest)
  year_ - 
  box   - unique identifier for each nestbox
  lin   - day of start nest building  (1 = Jan 1)
  
  
distance.csv (contains distances and neighbourhood orders between all combinations of occupied nestboxes)  
  year_	    -
  box1	    -
  box2	    -
  distance  - distance in meters
  no        - neighbourhood order


sex.csv (contains sex of each individual)
  ID  - unique identifier for each individual
  sex - sex of indvidual (1=male, 2=female)


terrbox.csv (contains which territory each box belonged to in each year)
  year_     -
  box       - unique identifier for each nestbox
  territory - unique identifier for each territory


transponders.csv (contains transponder number of each individual)
  ID          - unique identifier for each individual
  transponder - unique identifier for each transponder
  sex         - sex of indvidual (1=male, 2=female)


visits_all.csv (contains all extra-territorial nestbox visits)
  year_       -
  yday        - day of year (1 = Jan 1)
  ID          - unique identifier for each individual
  box         - unique identifier for each nestbox
  sex         - sex of indvidual (1=male, 2=female)
  territory   - unique identifier for each territory
  lin         - day of start nest building  (1 = Jan 1)
  age         - age as category (1=yearling, 2=adult)
  fledgeDate  - day of last fledging (1 = Jan 1)
  IDyear      - unique identifier for each individual for each year
  chron       - time of day of visit
  sunrise     - time of sunrise
  sunset      - time of sunset


visits_day.csv (containsdaily summary of extra-territorial nestbox visits for each individual)
  year_       - 
  ID          - unique identifier for each individual
  yday        - day of year (1 = Jan 1)
  box         - unique identifier for each nestbox
  territory   - unique identifier for each territory
  sex         - sex of indvidual (1=male, 2=female)
  lin         - day of start nest building  (1 = Jan 1)
  age         - age as category (1=yearling, 2=adult)
  fledgeDate  - day of last fledging (1 = Jan 1)
  IDyear      - unique identifier for each individual for each year
  chron       - time of day of randomly selected visit
  visits      - number of eextra territorial visits made








