# Jihoon Lim
# 01 - Analysis 2
# Other Data
# September 24, 2024

library(sf); library(dplyr); library("stringr"); library("geosphere"); library(nlme)
library(lmtest); library("AICcmodavg"); library(car); library(ggplot2); library(lubridate)
library(sandwich); library(MASS)

#### Part A: Toronto Data ####
# Read in Data
a_bike <- read.csv("C:/Users/limji/Desktop/Research Associate/CITS/bike_thefts.csv", 
                   header=TRUE, sep=",")
a_motor <- read.csv("C:/Users/limji/Desktop/Research Associate/CITS/motor_vehicle_thefts.csv", 
                    header=TRUE, sep=",")
a_mhaa <- read.csv("C:/Users/limji/Desktop/Research Associate/CITS/mha_apprenhensions.csv", 
                    header=TRUE, sep=",")
scs <- read.csv("C:/Users/limji/Desktop/Research Associate/CITS/SCS_coordinates.csv", 
                header=T, sep=",")
scs_sf <- st_as_sf(scs, coords = c("Longitude", "Latitude"), crs = 4326) 

# Organize data
a_bike_ready <- a_dataorg(a_bike)
a_motor_ready <- a_dataorg(a_motor)
a_mhaa_ready <- a_dataorg(a_mhaa)

#### Part B: Crime Event Coordinates ####
# | 1. Crime by Toronto neighbourhoods ####
## Bike thefts
b_bike <- a_bike_ready[,c(1, 30:33)]
b_bike$event_date <- lubridate::ymd(paste0(a_bike_ready$OCC_YEAR, 
                                           a_bike_ready$OCC_MONTH, 
                                           a_bike_ready$OCC_DAY)) # Add occurrence date
b_bike <- st_as_sf(b_bike, coords = c("LONG_WGS84", "LAT_WGS84"), crs = 4326) # Converts to SF file
b_bike <- st_transform(b_bike, crs = 26917)
## Motor vehicle thefts
b_motor <- a_motor_ready[,c(1, 26:29)]
b_motor$event_date <- lubridate::ymd(paste0(a_motor_ready$OCC_YEAR, 
                                            a_motor_ready$OCC_MONTH, 
                                            a_motor_ready$OCC_DAY)) # Add occurrence date
b_motor <- st_as_sf(b_motor, coords = c("LONG_WGS84", "LAT_WGS84"), crs = 4326) # Converts to SF file
b_motor <- st_transform(b_motor, crs = 26917)
b_motor <- b_motor %>% arrange(HOOD_140, event_date)
## Mental health apprehensions
b_mhaa <- a_mhaa_ready[,c(24:25)]
b_mhaa$event_date <- lubridate::ymd(paste0(a_mhaa_ready$OCC_YEAR, 
                                           a_mhaa_ready$OCC_MONTH, 
                                           a_mhaa_ready$OCC_DAY))

#### Part C: Data Set for Each SCS ####
# Moss Park
c_moss_park_bike <- c_scs2(input_data = b_bike, site = "Moss Park", open_date = "2017-08-01")
c_moss_park_motor <- c_scs2(input_data = b_motor, site = "Moss Park", open_date = "2017-08-01")

# The Works
c_the_works_bike <- c_scs2(input_data = b_bike, site = "The Works", open_date = "2017-08-01")
c_the_works_motor <- c_scs2(input_data = b_motor, site = "The Works", open_date = "2017-08-01")

# South Riverdale CHC
c_south_riv_bike <- c_scs2(input_data = b_bike, site = "South Riverdale CHC", open_date = "2017-11-01")
c_south_riv_motor <- c_scs2(input_data = b_motor, site = "South Riverdale CHC", open_date = "2017-11-01")

# Fred Victor
c_fred_vict_bike <- c_scs2(input_data = b_bike, site = "Fred Victor", open_date = "2018-02-01")
c_fred_vict_motor <- c_scs2(input_data = b_motor, site = "Fred Victor", open_date = "2018-02-01")
## Remove duplicate crime incidents
c_fred_vict_bike <- anti_join(c_fred_vict_bike, c_moss_park_bike, by = "OBJECTID")
c_fred_vict_motor <- anti_join(c_fred_vict_motor, c_moss_park_motor, by = "OBJECTID")

# Parkdale CHC
c_park_chc_bike <- c_scs2(input_data = b_bike, site = "Parkdale CHC", open_date = "2018-03-01")
c_park_chc_motor <- c_scs2(input_data = b_motor, site = "Parkdale CHC", open_date = "2018-03-01")

# Parkdale SCS
c_park_scs_bike <- c_scs2(input_data = b_bike, site = "Parkdale SCS", open_date = "2018-03-01")
c_park_scs_motor <- c_scs2(input_data = b_motor, site = "Parkdale SCS", open_date = "2018-03-01")

# Regent Park CHC
c_regent_chc_bike <- c_scs2(input_data = b_bike, site = "Regent Park CHC", open_date = "2018-04-01")
c_regent_chc_motor <- c_scs2(input_data = b_motor, site = "Regent Park CHC", open_date = "2018-04-01")
## Remove duplicate crime incidents
c_regent_chc_bike <- anti_join(c_regent_chc_bike, c_fred_vict_bike, by = "OBJECTID")
c_regent_chc_motor <- anti_join(c_regent_chc_motor, c_fred_vict_motor, by = "OBJECTID")

# St. Stephens / KMOPS
c_stephens_bike <- c_scs2(input_data = b_bike, site = "St. Stephens / KMOPS", open_date = "2018-04-01")
c_stephens_motor <- c_scs2(input_data = b_motor, site = "St. Stephens / KMOPS", open_date = "2018-04-01")

# Street Health
c_st_health_bike <- c_scs2(input_data = b_bike, site = "Street Health", open_date = "2018-06-01")
c_st_health_motor <- c_scs2(input_data = b_motor, site = "Street Health", open_date = "2018-06-01")

# | Aggregate Counts ####
# Moss Park
c_mp_bike <- c_crime_count(crime_data = c_moss_park_bike, open_date = "2017-08-01")
c_mp_motor <- c_crime_count(crime_data = c_moss_park_motor, open_date = "2017-08-01")

# The Works
c_tw_bike <- c_crime_count(crime_data = c_the_works_bike, open_date = "2017-08-01")
c_tw_motor <- c_crime_count(crime_data = c_the_works_motor, open_date = "2017-08-01")

# South Riverdale CHC
c_sr_bike <- c_crime_count(crime_data = c_south_riv_bike, open_date = "2017-11-01")
c_sr_motor <- c_crime_count(crime_data = c_south_riv_motor, open_date = "2017-11-01")

# Fred Victor
c_fv_bike <- c_crime_count(crime_data = c_fred_vict_bike, open_date = "2018-02-01")
c_fv_motor <- c_crime_count(crime_data = c_fred_vict_motor, open_date = "2018-02-01")

# Parkdale CHC
c_pc_bike <- c_crime_count(crime_data = c_park_chc_bike, open_date = "2018-03-01")
c_pc_motor <- c_crime_count(crime_data = c_park_chc_motor, open_date = "2018-03-01")

# Parkdale SCS
c_ps_bike <- c_crime_count(crime_data = c_park_scs_bike, open_date = "2018-03-01")
c_ps_motor <- c_crime_count(crime_data = c_park_scs_motor, open_date = "2018-03-01")

# Regent Park CHC
c_rp_bike <- c_crime_count(crime_data = c_regent_chc_bike, open_date = "2018-04-01")
c_rp_motor <- c_crime_count(crime_data = c_regent_chc_motor, open_date = "2018-04-01")

# St. Stephens / KMOPS
c_ss_bike <- c_crime_count(crime_data = c_stephens_bike, open_date = "2018-04-01")
c_ss_motor <- c_crime_count(crime_data = c_stephens_motor, open_date = "2018-04-01")

# Street Health
c_sh_bike <- c_crime_count(crime_data = c_st_health_bike, open_date = "2018-06-01")
c_sh_motor <- c_crime_count(crime_data = c_st_health_motor, open_date = "2018-06-01")

#### Part D: Aggregate Analysis ####
# Note: Running the following codes will lead to warning messages, but these can be ignored.
d_bike <- d_aggregate(c_mp_bike, c_tw_bike, c_sr_bike,
                         c_fv_bike, c_pc_bike, c_ps_bike,
                         c_rp_bike, c_ss_bike, c_sh_bike)
d_motor <- d_aggregate(c_mp_motor, c_tw_motor, c_sr_motor,
                      c_fv_motor, c_pc_motor, c_ps_motor,
                      c_rp_motor, c_ss_motor, c_sh_motor)

#### Part E: ITS Analysis ####
# | 1. Regression and plots ####
# Bike thefts
e_bike_100m <- e_its("crime_100m", d_bike, c(0, 15))
e_bike_200m <- e_its("crime_200m", d_bike, c(0, 50))
e_bike_500m <- e_its("crime_500m", d_bike, c(0, 180))
e_bike_d200m <- e_its("crime_d200m", d_bike, c(0, 50))
e_bike_d500m <- e_its("crime_d500m", d_bike, c(0, 180))
# Motor vehicle thefts
e_motor_100m <- e_its("crime_100m", d_motor, c(0, 8))
e_motor_200m <- e_its("crime_200m", d_motor, c(0, 35))
e_motor_500m <- e_its("crime_500m", d_motor, c(25, 125))
e_motor_d200m <- e_its("crime_d200m", d_motor, c(0, 35))
e_motor_d500m <- e_its("crime_d500m", d_motor, c(25, 125))

# | 2. Predictive changes ####
# Bicycle theft
e_bike_100m_5y <- e_pred_changes("crime_100m", d_bike); e_bike_100m_5y
e_bike_200m_5y <- e_pred_changes("crime_200m", d_bike); e_bike_200m_5y
e_bike_500m_5y <- e_pred_changes("crime_500m", d_bike); e_bike_500m_5y
e_bike_d200m_5y <- e_pred_changes("crime_d200m", d_bike); e_bike_d200m_5y
e_bike_d500m_5y <- e_pred_changes("crime_d500m", d_bike); e_bike_d500m_5y
# Theft from motor vehicles
e_motor_100m_5y <- e_pred_changes("crime_100m", d_motor); e_motor_100m_5y
e_motor_200m_5y <- e_pred_changes("crime_200m", d_motor); e_motor_200m_5y
e_motor_500m_5y <- e_pred_changes("crime_500m", d_motor); e_motor_500m_5y
e_motor_d200m_5y <- e_pred_changes("crime_d200m", d_motor); e_motor_d200m_5y
e_motor_d500m_5y <- e_pred_changes("crime_d500m", d_motor); e_motor_d500m_5y