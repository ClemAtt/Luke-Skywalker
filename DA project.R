getwd()
setwd("C:/Users/Public/Documents/ESDA/Energy Data Analytics/Project/Project/EDA project")

install.packages("purrrlyr")

library(tidyverse)
library(lubridate)
library(survey)
library(dplyr)
library(purrrlyr)
library(magrittr)

### Read data from csv

sa1 <- as.data.frame(read.csv("survey_answers.csv",stringsAsFactors = FALSE))
sq <- read.csv("survey_questions.csv",stringsAsFactors = FALSE)

sa$Q236 <- sa1$Q236

### variable compression

do <- sapply(sa[,215:222], str_count, pattern = "Female") %>% data.frame %>% by_row(sum)
sa$n_fem <- as.numeric(do$.out)

do <- sapply(sa[,215:222], str_count, pattern = "Male") %>% data.frame %>% by_row(sum)
sa$n_mal <- as.numeric(do$.out)

sa$f_fract <- sa$n_fem/(sa$n_mal+sa$n_fem)

do <- sapply(sa[,223:230], str_count, pattern = "0-04") %>% data.frame %>% by_row(sum)
sa$a_04 <- as.numeric(do$.out)
do <- sapply(sa[,223:230], str_count, pattern = "05-11") %>% data.frame %>% by_row(sum)
sa$a_0511 <- as.numeric(do$.out)
do <- sapply(sa[,223:230], str_count, pattern = "12-15") %>% data.frame %>% by_row(sum)
sa$a_1215 <- as.numeric(do$.out)
do <- sapply(sa[,223:230], str_count, pattern = "16-17") %>% data.frame %>% by_row(sum)
sa$a_1617 <- as.numeric(do$.out)
do <- sapply(sa[,223:230], str_count, pattern = "18-24") %>% data.frame %>% by_row(sum)
sa$a_1824 <- as.numeric(do$.out)
do <- sapply(sa[,223:230], str_count, pattern = "25-34") %>% data.frame %>% by_row(sum)
sa$a_2534 <- as.numeric(do$.out)
do <- sapply(sa[,223:230], str_count, pattern = "35-44") %>% data.frame %>% by_row(sum)
sa$a_3544 <- as.numeric(do$.out)
do <- sapply(sa[,223:230], str_count, pattern = "45-54") %>% data.frame %>% by_row(sum)
sa$a_4554 <- as.numeric(do$.out)
do <- sapply(sa[,223:230], str_count, pattern = "55-64") %>% data.frame %>% by_row(sum)
sa$a_5564 <- as.numeric(do$.out)
do <- sapply(sa[,223:230], str_count, pattern = "65-74") %>% data.frame %>% by_row(sum)
sa$a_6574 <- as.numeric(do$.out)
do <- sapply(sa[,223:230], str_count, pattern = "75 or older") %>% data.frame %>% by_row(sum)
sa$a_75ov <- as.numeric(do$.out)

dim(sa)
write.csv(sa[,200:374],file = "test sa.csv")

sa$n_child <- rowSums(sa[,349:352],na.rm = TRUE)
sa$n_adult <- rowSums(sa[,349:359],na.rm = TRUE) - rowSums(sa[,349:352],na.rm = TRUE)
View(sa[1:20,])

sa$Q234 <- str_replace(sa$Q234,"Never","0")
sa$Q234 <- str_replace(sa$Q234,"Occasionally","1")
sa$Q234 <- str_replace(sa$Q234,"About half the time","2")
sa$Q234 <- str_replace(sa$Q234,"Most/all weekdays","3")

do <- sapply(sa[,247], str_count, pattern = "Gas") %>% data.frame %>% by_row(sum)
sa$heat_gas <- as.numeric(do$.out)
do <- sapply(sa[,247], str_count, pattern = "Electric") %>% data.frame %>% by_row(sum)
sa$heat_elec <- as.numeric(do$.out)
do <- sapply(sa[,247], str_count, pattern = "No centr") %>% data.frame %>% by_row(sum)
sa$heat_nocentralheat <- as.numeric(do$.out)
do <- sapply(sa[,247], str_count, pattern = "Solid") %>% data.frame %>% by_row(sum)
sa$heat_solid <- as.numeric(do$.out)
do <- sapply(sa[,247], str_count, pattern = "Other") %>% data.frame %>% by_row(sum)
sa$heat_other <- as.numeric(do$.out)
do <- sapply(sa[,247], str_count, pattern = "Don't") %>% data.frame %>% by_row(sum)
sa$heat_dontknow <- as.numeric(do$.out)
do <- sapply(sa[,247], str_count, pattern = "Oil") %>% data.frame %>% by_row(sum)
sa$heat_oil <- as.numeric(do$.out)

do <- sapply(sa[,249], str_count, pattern = "Gas boiler|Hot water storage tank with gas boiler") %>% data.frame %>% by_row(sum)
sa$hwater_gasboiler <- as.numeric(do$.out)
do <- sapply(sa[,249], str_count, pattern = "electric immersion heater|ELECTRIC") %>% data.frame %>% by_row(sum)
sa$hwater_hwater_elecimm <- as.numeric(do$.out)
do <- sapply(sa[,249], str_count, pattern = "COMMUN|CENTRAL|commun") %>% data.frame %>% by_row(sum)
sa$hwater_communal <- as.numeric(do$.out)
View(sa[1:20,])

do <- sa[,c(250:255,262:267)] %>% data.frame %>% by_row(sum)
sa$lights_trad_halogen <- as.numeric(do$.out)
do <- sa[,c(256:261,268:279)] %>% data.frame %>% by_row(sum)
sa$lights_le_led_cfl <- as.numeric(do$.out)

do <- sa[,280:293] %>% data.frame %>% by_row(sum)
sa$fridge_freezer <- as.numeric(do$.out)

names(sa)[294:315] <- c("n_Electric_hob","n_Gas_hob","n_Electric_oven","n_Microwave","n_Washing_machine_no_dryer","n_Tumble_dryer","n_Washer-dryer","n_Dishwasher","n_Electric_shower","n_Over-sink_electric_water_heater","n_Portable_electric_heater","n_Television","n_Desktop_PC","n_Laptop","n_Printer","n_Router","n_Dvdplayer","n_Cable_TV_box","n_Satellite_TV_box","n_Freeview_TV_box","n_Games_console","n_Standby_savers")

sa$Q328 <- str_replace(sa$Q328,"At least once every day","5")
sa$Q328 <- str_replace(sa$Q328,"Several times every day","6")
sa$Q328 <- str_replace(sa$Q328,"Every 2-3 days","4")
sa$Q328 <- str_replace(sa$Q328,"Every 4-5 days","3")
sa$Q328 <- str_replace(sa$Q328,"About once a week","2")
sa$Q328 <- str_replace(sa$Q328,"Less often than once a week","1")
sa$Q328 <- str_replace(sa$Q328,"Don't know|Never","0")

sa %>% group_by(as.factor(sa$Q328)) %>% summarise()

sa$Q329 <- str_replace(sa$Q329,"Don't know|Never","0")
sa$Q329 <- str_replace(sa$Q329,"Less often than once a week","1")
sa$Q329 <- str_replace(sa$Q329,"About once a week","2")
sa$Q329 <- str_replace(sa$Q329,"Every 4-5 days","3")
sa$Q329 <- str_replace(sa$Q329,"Every 2-3 days","4")
sa$Q329 <- str_replace(sa$Q329,"At least once every day","5")
sa$Q329 <- str_replace(sa$Q329,"Several times every day","6")

sa %>% group_by(as.factor(sa$Q329)) %>% summarise()

sa$Q330 <- str_replace(sa$Q330,"Several times every day","6")
sa$Q330 <- str_replace(sa$Q330,"At least once every day","5")
sa$Q330 <- str_replace(sa$Q330,"Every 2-3 days","4")
sa$Q330 <- str_replace(sa$Q330,"Every 4-5 days","3")
sa$Q330 <- str_replace(sa$Q330,"About once a week","2")
sa$Q330 <- str_replace(sa$Q330,"Less often than once a week","1")
sa$Q330 <- str_replace(sa$Q330,"Don't know|Never","0")

sa %>% group_by(as.factor(sa$Q330)) %>% summarise()

sa$Q331 <- str_replace(sa$Q331,"Don't know|Never","0")
sa$Q331 <- str_replace(sa$Q331,"Less often than once a week","1")
sa$Q331 <- str_replace(sa$Q331,"About once a week","2")
sa$Q331 <- str_replace(sa$Q331,"Every 4-5 days","3")
sa$Q331 <- str_replace(sa$Q331,"Every 2-3 days","4")
sa$Q331 <- str_replace(sa$Q331,"At least once every day","5")
sa$Q331 <- str_replace(sa$Q331,"Several times every day","6")

sa %>% group_by(as.factor(sa$prop_type_num)) %>% summarise()

sa$household_size <- as.numeric(sa$Q213)

sa$n_rooms <- as.numeric(sa$Q238)
sa$n_bedrooms <- as.numeric(sa$Q239)

sa$prop_type <- paste(sa$Q235,"_", sa$Q236)

sa$prop_type_num <- str_replace(sa$prop_type_num,"Detached","1")
sa$prop_type_num <- str_replace(sa$prop_type_num,"Semi-detached","2")
sa$prop_type_num <- str_replace(sa$prop_type_num,"Terraced - end","3")
sa$prop_type_num <- str_replace(sa$prop_type_num,"Terraced - middle","4")
sa$prop_type_num <- str_replace(sa$prop_type_num,"In a commercial building","5")
sa$prop_type_num <- str_replace(sa$prop_type_num,"purpose built block or tenement","6")
sa$prop_type_num <- str_replace(sa$prop_type_num,"converted house/building","7")

sa$prop_type_num <- str_remove_all(sa$prop_type_num,"\\)")

sa[,236] <- as.numeric(sa[,236])

dim(sa)
write.csv(sa,file = "sa.csv")


