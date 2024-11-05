# Jihoon Lim
# 01 - Analysis 1
# MCI Data
# September 17, 2024

# Load packages
library(sf); library(dplyr); library("stringr"); library("geosphere"); library(nlme)
library(lmtest); library("AICcmodavg"); library(car); library(ggplot2); library(lubridate)
library(sandwich); library(MASS)
# Source functions
source("C:/Users/limji/Desktop/Research Associate/CITS/00 - Functions.R")

#### Part A: Crime Statistics Data ####
# Read in Data
a_mci <- read.csv("C:/Users/limji/Desktop/Research Associate/CITS/MCI.csv", 
                  header=TRUE, sep=",") # N = 396,735

# # | 1. Remove rows with NSA in specific columns ####
# a01 <- subset(mci, mci$DIVISION != "NSA") # N = 392,743
# a01a <- subset(a01, a01$HOOD_140 != "NSA") # N = 390,448
# a01b <- subset(a01a, a01a$NEIGHBOURHOOD_140 != "NSA") # N = 390,448
# # | 2. Remove rows with missing dates ####
# a02 <- a01b %>% 
#   filter(!is.na(OCC_YEAR) & !is.na(OCC_MONTH) & !is.na(OCC_DAY)) # N = 390,332
# # | 3. Remove events before 1/1/2014 ####
# a03 <- a02 %>% 
#   filter(OCC_YEAR >= 2014 & OCC_DOY >= 1) # N = 388,932
# # | 4. Remove duplicate event ID ####
# a04 <- a03 %>%
#   distinct(EVENT_UNIQUE_ID, .keep_all = TRUE) # N = 339,058
# # | 5. Remove leading and trailing spaces ####
# a05 <- data.frame(lapply(a04, function(x) {
#   if (is.character(x)) trimws(x) else x
# }), stringsAsFactors = FALSE) # N = 339,058
# # | 6. Remove where REPORT_DATE<OCC_DATE ####
# a06 <- a05 %>% 
#   filter(REPORT_DATE >= OCC_DATE) # N = 339,058
# a06$HOOD_140 <- as.numeric(a06$HOOD_140)
# # Check the number of unique
# length(unique(a06$HOOD_140))
# length(unique(a06$NEIGHBOURHOOD_140))
# length(unique(a06$MCI_CATEGORY))
# 
# # | 7. Check if there is a mismatch in neighbourhood names ####
# a07 <- a06 %>% 
#   mutate(Hood_ID2 = regmatches(a06$NEIGHBOURHOOD_140, regexpr("[0-9]+",a06$NEIGHBOURHOOD_140)))
# class(a07$HOOD_140)
# class(a07$Hood_ID2)
# a07 %>% 
#   filter(HOOD_140 != Hood_ID2) %>% 
#   count() # N = 0
# sum(grepl("^\\s*$", a07$HOOD_140)) # N = 0

a_mci_ready <- a_dataorg(a_mci)
a_mci_ready_AS <- subset(a_mci_ready, a_mci_ready$MCI_CATEGORY == "Assault")
a_mci_ready_AT <- subset(a_mci_ready, a_mci_ready$MCI_CATEGORY == "Auto Theft")
a_mci_ready_BE <- subset(a_mci_ready, a_mci_ready$MCI_CATEGORY == "Break and Enter")
a_mci_ready_RB <- subset(a_mci_ready, a_mci_ready$MCI_CATEGORY == "Robbery")
a_mci_ready_TH <- subset(a_mci_ready, a_mci_ready$MCI_CATEGORY == "Theft Over")

#### Part B: Crime Event Coordinates ####
# | 1. Crime by Toronto neighbourhoods ####
b01 <- a_mci_ready[,c(3, 25, 29:31)]
b01$event_date <- lubridate::ymd(paste0(a_mci_ready$OCC_YEAR, a_mci_ready$OCC_MONTH, a_mci_ready$OCC_DAY)) # Add occurrence date
b01 <- st_as_sf(b01, coords = c("LONG_WGS84", "LAT_WGS84"), crs = 4326) # Converts to SF file
b01 <- st_transform(b01, crs = 26917)

# | 2. Map of Toronto neighbourhoods ####
to_140 <- read_sf("C:/Users/limji/Desktop/Research Associate/CITS/geo_project/Neighbourhoods/Neighbourhoods.shp")
b02 <- st_transform(to_140, crs = 26917)

# | 3. SCS locations in Toronto ####
scs <- read.csv("C:/Users/limji/Desktop/Research Associate/CITS/SCS_coordinates.csv", 
                header=T, sep=",")
scs_sf <- st_as_sf(scs, coords = c("Longitude", "Latitude"), crs = 4326) 
to_scs <- st_transform(scs_sf, crs = 26917)
ggplot() +
  geom_sf(data = b01, aes(color = MCI_CATEGORY), shape = 21) + # Crime
  geom_sf(data = b02, color = "black", lwd = 0.7, alpha = 0.1) + # Neighbourhood
  geom_sf(data = to_scs, color = "blue", shape = 16) + # SCS
  labs(title = "Major Crimes in Toronto") +
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.text.x=element_blank(),
        axis.title.x=element_blank(),
        axis.ticks.x=element_blank(), 
        axis.text.y=element_blank(),
        axis.title.y=element_blank(),
        axis.ticks.y=element_blank(),
        panel.border = element_rect(fill = NA),
        plot.caption = element_text(hjust = 0))

# | 4. Descriptive map ####
b_assault <- b_map("Assault", "Assault in Toronto"); b_assault
b_auto <- b_map("Auto Theft", "Auto Theft in Toronto"); b_auto
b_bne <- b_map("Break and Enter", "Break and Enter in Toronto"); b_bne
b_robbery <- b_map("Robbery", "Robbery in Toronto"); b_robbery
b_t5000 <- b_map("Theft Over", "Theft Over $5000 in Toronto"); b_t5000

#### Part C: Data Set for Each SCS ####
# Moss Park
c_moss_park_assault <- c_scs1(input_data = b01, site = "Moss Park", mci = "Assault", open_date = "2017-08-01")
c_moss_park_auto <- c_scs1(input_data = b01, site = "Moss Park", mci = "Auto Theft", open_date = "2017-08-01")
c_moss_park_bne <- c_scs1(input_data = b01, site = "Moss Park", mci = "Break and Enter", open_date = "2017-08-01")
c_moss_park_robbery <- c_scs1(input_data = b01, site = "Moss Park", mci = "Robbery", open_date = "2017-08-01")
c_moss_park_t5000 <- c_scs1(input_data = b01, site = "Moss Park", mci = "Theft Over", open_date = "2017-08-01")

# The Works
c_the_works_assault <- c_scs1(input_data = b01, site = "The Works", mci = "Assault", open_date = "2017-08-01")
c_the_works_auto <- c_scs1(input_data = b01, site = "The Works", mci = "Auto Theft", open_date = "2017-08-01")
c_the_works_bne <- c_scs1(input_data = b01, site = "The Works", mci = "Break and Enter", open_date = "2017-08-01")
c_the_works_robbery <- c_scs1(input_data = b01, site = "The Works", mci = "Robbery", open_date = "2017-08-01")
c_the_works_t5000 <- c_scs1(input_data = b01, site = "The Works", mci = "Theft Over", open_date = "2017-08-01")

# South Riverdale CHC
c_south_riv_assault <- c_scs1(input_data = b01, site = "South Riverdale CHC", mci = "Assault", open_date = "2017-11-01")
c_south_riv_auto <- c_scs1(input_data = b01, site = "South Riverdale CHC", mci = "Auto Theft", open_date = "2017-11-01")
c_south_riv_bne <- c_scs1(input_data = b01, site = "South Riverdale CHC", mci = "Break and Enter", open_date = "2017-11-01")
c_south_riv_robbery <- c_scs1(input_data = b01, site = "South Riverdale CHC", mci = "Robbery", open_date = "2017-11-01")
c_south_riv_t5000 <- c_scs1(input_data = b01, site = "South Riverdale CHC", mci = "Theft Over", open_date = "2017-11-01")

# Fred Victor
c_fred_vict_assault <- c_scs1(input_data = b01, site = "Fred Victor", mci = "Assault", open_date = "2018-02-01")
c_fred_vict_auto <- c_scs1(input_data = b01, site = "Fred Victor", mci = "Auto Theft", open_date = "2018-02-01")
c_fred_vict_bne <- c_scs1(input_data = b01, site = "Fred Victor", mci = "Break and Enter", open_date = "2018-02-01")
c_fred_vict_robbery <- c_scs1(input_data = b01, site = "Fred Victor", mci = "Robbery", open_date = "2018-02-01")
c_fred_vict_t5000 <- c_scs1(input_data = b01, site = "Fred Victor", mci = "Theft Over", open_date = "2018-02-01")
## Remove duplicate crime incidents
c_fred_vict_assault <- anti_join(c_fred_vict_assault, c_moss_park_assault, by = "OBJECTID")
c_fred_vict_auto <- anti_join(c_fred_vict_auto, c_moss_park_auto, by = "OBJECTID")
c_fred_vict_bne <- anti_join(c_fred_vict_bne, c_moss_park_bne, by = "OBJECTID")
c_fred_vict_robbery <- anti_join(c_fred_vict_robbery, c_moss_park_robbery, by = "OBJECTID")
c_fred_vict_t5000 <- anti_join(c_fred_vict_t5000, c_moss_park_t5000, by = "OBJECTID")

# Parkdale CHC
c_park_chc_assault <- c_scs1(input_data = b01, site = "Parkdale CHC", mci = "Assault", open_date = "2018-03-01")
c_park_chc_auto <- c_scs1(input_data = b01, site = "Parkdale CHC", mci = "Auto Theft", open_date = "2018-03-01")
c_park_chc_bne <- c_scs1(input_data = b01, site = "Parkdale CHC", mci = "Break and Enter", open_date = "2018-03-01")
c_park_chc_robbery <- c_scs1(input_data = b01, site = "Parkdale CHC", mci = "Robbery", open_date = "2018-03-01")
c_park_chc_t5000 <- c_scs1(input_data = b01, site = "Parkdale CHC", mci = "Theft Over", open_date = "2018-03-01")

# Parkdale SCS
c_park_scs_assault <- c_scs1(input_data = b01, site = "Parkdale SCS", mci = "Assault", open_date = "2018-03-01")
c_park_scs_auto <- c_scs1(input_data = b01, site = "Parkdale SCS", mci = "Auto Theft", open_date = "2018-03-01")
c_park_scs_bne <- c_scs1(input_data = b01, site = "Parkdale SCS", mci = "Break and Enter", open_date = "2018-03-01")
c_park_scs_robbery <- c_scs1(input_data = b01, site = "Parkdale SCS", mci = "Robbery", open_date = "2018-03-01")
c_park_scs_t5000 <- c_scs1(input_data = b01, site = "Parkdale SCS", mci = "Theft Over", open_date = "2018-03-01")

# Regent Park CHC
c_regent_chc_assault <- c_scs1(input_data = b01, site = "Regent Park CHC", mci = "Assault", open_date = "2018-04-01")
c_regent_chc_auto <- c_scs1(input_data = b01, site = "Regent Park CHC", mci = "Auto Theft", open_date = "2018-04-01")
c_regent_chc_bne <- c_scs1(input_data = b01, site = "Regent Park CHC", mci = "Break and Enter", open_date = "2018-04-01")
c_regent_chc_robbery <- c_scs1(input_data = b01, site = "Regent Park CHC", mci = "Robbery", open_date = "2018-04-01")
c_regent_chc_t5000 <- c_scs1(input_data = b01, site = "Regent Park CHC", mci = "Theft Over", open_date = "2018-04-01")
## Remove duplicate crime incidents
c_regent_chc_assault <- anti_join(c_regent_chc_assault, c_fred_vict_assault, by = "OBJECTID")
c_regent_chc_auto <- anti_join(c_regent_chc_auto, c_fred_vict_auto, by = "OBJECTID")
c_regent_chc_bne <- anti_join(c_regent_chc_bne, c_fred_vict_bne, by = "OBJECTID")
c_regent_chc_robbery <- anti_join(c_regent_chc_robbery, c_fred_vict_robbery, by = "OBJECTID")
c_regent_chc_t5000 <- anti_join(c_regent_chc_t5000, c_fred_vict_t5000, by = "OBJECTID")

# St. Stephens / KMOPS
c_stephens_assault <- c_scs1(input_data = b01, site = "St. Stephens / KMOPS", mci = "Assault", open_date = "2018-04-01")
c_stephens_auto <- c_scs1(input_data = b01, site = "St. Stephens / KMOPS", mci = "Auto Theft", open_date = "2018-04-01")
c_stephens_bne <- c_scs1(input_data = b01, site = "St. Stephens / KMOPS", mci = "Break and Enter", open_date = "2018-04-01")
c_stephens_robbery <- c_scs1(input_data = b01, site = "St. Stephens / KMOPS", mci = "Robbery", open_date = "2018-04-01")
c_stephens_t5000 <- c_scs1(input_data = b01, site = "St. Stephens / KMOPS", mci = "Theft Over", open_date = "2018-04-01")

# Street Health
c_st_health_assault <- c_scs1(input_data = b01, site = "Street Health", mci = "Assault", open_date = "2018-06-01")
c_st_health_auto <- c_scs1(input_data = b01, site = "Street Health", mci = "Auto Theft", open_date = "2018-06-01")
c_st_health_bne <- c_scs1(input_data = b01, site = "Street Health", mci = "Break and Enter", open_date = "2018-06-01")
c_st_health_robbery <- c_scs1(input_data = b01, site = "Street Health", mci = "Robbery", open_date = "2018-06-01")
c_st_health_t5000 <- c_scs1(input_data = b01, site = "Street Health", mci = "Theft Over", open_date = "2018-06-01")

# | Aggregate Counts ####
# Moss Park
c_mp_assault <- c_crime_count(crime_data = c_moss_park_assault, open_date = "2017-08-01")
c_mp_auto <- c_crime_count(crime_data = c_moss_park_auto, open_date = "2017-08-01")
c_mp_bne <- c_crime_count(crime_data = c_moss_park_bne, open_date = "2017-08-01")
c_mp_robbery <- c_crime_count(crime_data = c_moss_park_robbery, open_date = "2017-08-01")
c_mp_t5000 <- c_crime_count(crime_data = c_moss_park_t5000, open_date = "2017-08-01")

# The Works
c_tw_assault <- c_crime_count(crime_data = c_the_works_assault, open_date = "2017-08-01")
c_tw_auto <- c_crime_count(crime_data = c_the_works_auto, open_date = "2017-08-01")
c_tw_bne <- c_crime_count(crime_data = c_the_works_bne, open_date = "2017-08-01")
c_tw_robbery <- c_crime_count(crime_data = c_the_works_robbery, open_date = "2017-08-01")
c_tw_t5000 <- c_crime_count(crime_data = c_the_works_t5000, open_date = "2017-08-01")

# South Riverdale CHC
c_sr_assault <- c_crime_count(crime_data = c_south_riv_assault, open_date = "2017-11-01")
c_sr_auto <- c_crime_count(crime_data = c_south_riv_auto, open_date = "2017-11-01")
c_sr_bne <- c_crime_count(crime_data = c_south_riv_bne, open_date = "2017-11-01")
c_sr_robbery <- c_crime_count(crime_data = c_south_riv_robbery, open_date = "2017-11-01")
c_sr_t5000 <- c_crime_count(crime_data = c_south_riv_t5000, open_date = "2017-11-01")

# Fred Victor
c_fv_assault <- c_crime_count(crime_data = c_fred_vict_assault, open_date = "2018-02-01")
c_fv_auto <- c_crime_count(crime_data = c_fred_vict_auto, open_date = "2018-02-01")
c_fv_bne <- c_crime_count(crime_data = c_fred_vict_bne, open_date = "2018-02-01")
c_fv_robbery <- c_crime_count(crime_data = c_fred_vict_robbery, open_date = "2018-02-01")
c_fv_t5000 <- c_crime_count(crime_data = c_fred_vict_t5000, open_date = "2018-02-01")

# Parkdale CHC
c_pc_assault <- c_crime_count(crime_data = c_park_chc_assault, open_date = "2018-03-01")
c_pc_auto <- c_crime_count(crime_data = c_park_chc_auto, open_date = "2018-03-01")
c_pc_bne <- c_crime_count(crime_data = c_park_chc_bne, open_date = "2018-03-01")
c_pc_robbery <- c_crime_count(crime_data = c_park_chc_robbery, open_date = "2018-03-01")
c_pc_t5000 <- c_crime_count(crime_data = c_park_chc_t5000, open_date = "2018-03-01")

# Parkdale SCS
c_ps_assault <- c_crime_count(crime_data = c_park_scs_assault, open_date = "2018-03-01")
c_ps_auto <- c_crime_count(crime_data = c_park_scs_auto, open_date = "2018-03-01")
c_ps_bne <- c_crime_count(crime_data = c_park_scs_bne, open_date = "2018-03-01")
c_ps_robbery <- c_crime_count(crime_data = c_park_scs_robbery, open_date = "2018-03-01")
c_ps_t5000 <- c_crime_count(crime_data = c_park_scs_t5000, open_date = "2018-03-01")

# Regent Park CHC
c_rp_assault <- c_crime_count(crime_data = c_regent_chc_assault, open_date = "2018-04-01")
c_rp_auto <- c_crime_count(crime_data = c_regent_chc_auto, open_date = "2018-04-01")
c_rp_bne <- c_crime_count(crime_data = c_regent_chc_bne, open_date = "2018-04-01")
c_rp_robbery <- c_crime_count(crime_data = c_regent_chc_robbery, open_date = "2018-04-01")
c_rp_t5000 <- c_crime_count(crime_data = c_regent_chc_t5000, open_date = "2018-04-01")

# St. Stephens / KMOPS
c_ss_assault <- c_crime_count(crime_data = c_stephens_assault, open_date = "2018-04-01")
c_ss_auto <- c_crime_count(crime_data = c_stephens_auto, open_date = "2018-04-01")
c_ss_bne <- c_crime_count(crime_data = c_stephens_bne, open_date = "2018-04-01")
c_ss_robbery <- c_crime_count(crime_data = c_stephens_robbery, open_date = "2018-04-01")
c_ss_t5000 <- c_crime_count(crime_data = c_stephens_t5000, open_date = "2018-04-01")

# Street Health
c_sh_assault <- c_crime_count(crime_data = c_st_health_assault, open_date = "2018-06-01")
c_sh_auto <- c_crime_count(crime_data = c_st_health_auto, open_date = "2018-06-01")
c_sh_bne <- c_crime_count(crime_data = c_st_health_bne, open_date = "2018-06-01")
c_sh_robbery <- c_crime_count(crime_data = c_st_health_robbery, open_date = "2018-06-01")
c_sh_t5000 <- c_crime_count(crime_data = c_st_health_t5000, open_date = "2018-06-01")

#### Part D: Aggregate Analysis ####
# Note: Running the following codes will lead to warning messages, but these can be ignored.
d_assault <- d_aggregate(c_mp_assault, c_tw_assault, c_sr_assault,
                         c_fv_assault, c_pc_assault, c_ps_assault,
                         c_rp_assault, c_ss_assault, c_sh_assault)
d_auto <- d_aggregate(c_mp_auto, c_tw_auto, c_sr_auto,
                         c_fv_auto, c_pc_auto, c_ps_auto,
                         c_rp_auto, c_ss_auto, c_sh_auto)
d_bne <- d_aggregate(c_mp_bne, c_tw_bne, c_sr_bne,
                         c_fv_bne, c_pc_bne, c_ps_bne,
                         c_rp_bne, c_ss_bne, c_sh_bne)
d_robbery <- d_aggregate(c_mp_robbery, c_tw_robbery, c_sr_robbery,
                         c_fv_robbery, c_pc_robbery, c_ps_robbery,
                         c_rp_robbery, c_ss_robbery, c_sh_robbery)
d_t5000 <- d_aggregate(c_mp_t5000, c_tw_t5000, c_sr_t5000,
                         c_fv_t5000, c_pc_t5000, c_ps_t5000,
                         c_rp_t5000, c_ss_t5000, c_sh_t5000)

#### Part E: ITS Analysis ####
# | 1. Regression and Plots ####
# Assault
e_assault_100m <- e_its("crime_100m", d_assault, c(0, 25))
e_assault_200m <- e_its("crime_200m", d_assault, c(0, 110))
e_assault_500m <- e_its("crime_500m", d_assault, c(100, 400))
e_assault_d200m <- e_its("crime_d200m", d_assault, c(0, 110))
e_assault_d500m <- e_its("crime_d500m", d_assault, c(100, 400))
# Auto Theft
e_auto_100m <- e_its("crime_100m", d_auto, c(0, 15))
e_auto_200m <- e_its("crime_200m", d_auto, c(0, 20))
e_auto_500m <- e_its("crime_500m", d_auto, c(0, 45))
e_auto_d200m <- e_its("crime_d200m", d_auto, c(0, 20))
e_auto_d500m <- e_its("crime_d500m", d_auto, c(0, 45))
# Break and Enter
e_bne_100m <- e_its("crime_100m", d_bne, c(0, 35))
e_bne_200m <- e_its("crime_200m", d_bne, c(0, 25))
e_bne_500m <- e_its("crime_500m", d_bne, c(0, 125))
e_bne_d200m <- e_its("crime_d200m", d_bne, c(0, 25))
e_bne_d500m <- e_its("crime_d500m", d_bne, c(0, 125))
# Robbery
e_robbery_100m <- e_its("crime_100m", d_robbery, c(0, 30))
e_robbery_200m <- e_its("crime_200m", d_robbery, c(0, 45))
e_robbery_500m <- e_its("crime_500m", d_robbery, c(0, 100))
e_robbery_d200m <- e_its("crime_d200m", d_robbery, c(0, 45))
e_robbery_d500m <- e_its("crime_d500m", d_robbery, c(0, 100))
# Theft over $5000
e_t5000_100m <- e_its("crime_100m", d_t5000, c(0, 15))
e_t5000_200m <- e_its("crime_200m", d_t5000, c(0, 25))
e_t5000_500m <- e_its("crime_500m", d_t5000, c(0, 30))
e_t5000_d200m <- e_its("crime_d200m", d_t5000, c(0, 25))
e_t5000_d500m <- e_its("crime_d500m", d_t5000, c(0, 30))

# | 2. Predictive changes ####
# Assault
e_assault_100m_5y <- e_pred_changes("crime_100m", d_assault); e_assault_100m_5y
e_assault_200m_5y <- e_pred_changes("crime_200m", d_assault); e_assault_200m_5y
e_assault_500m_5y <- e_pred_changes("crime_500m", d_assault); e_assault_500m_5y
e_assault_d200m_5y <- e_pred_changes("crime_d200m", d_assault); e_assault_d200m_5y
e_assault_d500m_5y <- e_pred_changes("crime_d500m", d_assault); e_assault_d500m_5y
# Auto theft
e_auto_100m_5y <- e_pred_changes("crime_100m", d_auto); e_auto_100m_5y
e_auto_200m_5y <- e_pred_changes("crime_200m", d_auto); e_auto_200m_5y
e_auto_500m_5y <- e_pred_changes("crime_500m", d_auto); e_auto_500m_5y
e_auto_d200m_5y <- e_pred_changes("crime_d200m", d_auto); e_auto_d200m_5y
e_auto_d500m_5y <- e_pred_changes("crime_d500m", d_auto); e_auto_d500m_5y
# Break and enter
e_bne_100m_5y <- e_pred_changes("crime_100m", d_bne); e_bne_100m_5y
e_bne_200m_5y <- e_pred_changes("crime_200m", d_bne); e_bne_200m_5y
e_bne_500m_5y <- e_pred_changes("crime_500m", d_bne); e_bne_500m_5y
e_bne_d200m_5y <- e_pred_changes("crime_d200m", d_bne); e_bne_d200m_5y
e_bne_d500m_5y <- e_pred_changes("crime_d500m", d_bne); e_bne_d500m_5y
# Robbery
e_robbery_100m_5y <- e_pred_changes("crime_100m", d_robbery); e_robbery_100m_5y
e_robbery_200m_5y <- e_pred_changes("crime_200m", d_robbery); e_robbery_200m_5y
e_robbery_500m_5y <- e_pred_changes("crime_500m", d_robbery); e_robbery_500m_5y
e_robbery_d200m_5y <- e_pred_changes("crime_d200m", d_robbery); e_robbery_d200m_5y
e_robbery_d500m_5y <- e_pred_changes("crime_d500m", d_robbery); e_robbery_d500m_5y
# Theft over $5000
e_t5000_100m_5y <- e_pred_changes("crime_100m", d_t5000); e_t5000_100m_5y
e_t5000_200m_5y <- e_pred_changes("crime_200m", d_t5000); e_t5000_200m_5y
e_t5000_500m_5y <- e_pred_changes("crime_500m", d_t5000); e_t5000_500m_5y
e_t5000_d200m_5y <- e_pred_changes("crime_d200m", d_t5000); e_t5000_d200m_5y
e_t5000_d500m_5y <- e_pred_changes("crime_d500m", d_t5000); e_t5000_d500m_5y