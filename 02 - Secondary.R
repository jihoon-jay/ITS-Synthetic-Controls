# Jihoon Lim
# 02 - Secondary Analysis
# September 17, 2024

library(cancensus)
library(sf)
library(dplyr)
library(foreign); library(tsModel); library("lmtest"); library("Epi");
library("splines"); library("vcd"); library(sandwich); library("reshape2"); library(tidyr); library(plyr)
library(Synth); library(SCtools); library(janitor); library(dplyr); library(gdata)
library(rlang); library(purrr); library(tidyselect); library(car); library(Epi)

#### Part A: Toronto Neighbourhoods ####
# 1. Toronto neighbourhood boundaries
a_to_140 <- read_sf("C:/Users/limji/Desktop/Research Associate/CITS/geo_project/Neighbourhoods/Neighbourhoods.shp")
a_to_140 <- st_transform(a_to_140, crs = 26917)

# 2. SCS locations in Toronto
a_scs_to <- read.csv("C:/Users/limji/Desktop/Research Associate/CITS/SCS_coordinates.csv", 
                     header=T, sep=",")
a_scs_to <- st_as_sf(a_scs_to, coords = c("Longitude", "Latitude"), crs = 4326) 
a_scs_to <- st_transform(a_scs_to, crs = 26917)

#### Part B: Neighbourhoods/Census Tracts ####
options(cancensus.api_key ="CensusMapper_22257118738618fffdf6d8407a9882e1")
# 1. 2021 Census
## a. Include relevant variables
b_to_2021 <- get_census(dataset = 'CA21',
                        regions = list(CSD=c("3520005")),
                        vectors = c("v_CA21_1", "v_CA21_6", # Population
                                    "v_CA21_8", "v_CA21_9", "v_CA21_10", # Sex Distribution
                                    "v_CA21_11", "v_CA21_68", "v_CA21_251", # Age Distribution
                                    "v_CA21_386", "v_CA21_452", "v_CA21_499","v_CA21_507", # Family/Household
                                    "v_CA21_566", "v_CA21_1040", # Income
                                    "v_CA21_4262", "v_CA21_4260", # Unsuitable Housing
                                    "v_CA21_4410", "v_CA21_4875", # Minority/Immigrants
                                    "v_CA21_5817", "v_CA21_5820", # Education
                                    "v_CA21_6495", "v_CA21_6501"), # Unemployed
                        level='CT',
                        use_cache =FALSE, geo_format = "sf")

## b. Create new variables appropriate for analysis
b_to_2021 <- b_to_2021 %>% mutate(
  pop = .[[17]],
  dens = .[[18]],
  female = .[[21]] / .[[19]],
  age00_14 = .[[22]] / .[[19]],
  age15_64 = .[[23]] / .[[19]],
  age65plus = .[[24]] / .[[19]],
  avg_age = .[[25]],
  hh_size = .[[26]],
  median_inc = .[[29]],
  limat = .[[30]],
  one_parent = .[[28]] / .[[27]],
  immigrants = .[[33]] / .[[19]],
  minority = .[[34]] / .[[19]],
  noeduc = .[[36]] / .[[35]],
  chomage = .[[38]] / .[[37]]
)
b_to_2021 <- b_to_2021[, -c(17:38)]

## c. Merge neighbourhoods and census tracts by geographic points
if (st_crs(a_to_140) != st_crs(b_to_2021)) {
  b_to_2021 <- st_transform(b_to_2021, st_crs(a_to_140))
}
b_to_centroid <- st_centroid(b_to_2021)
b_toct_2021 <- st_join(a_to_140, b_to_centroid, join = st_intersects, left = F)

## d. Streamline dataset
b_toct_2021 <- b_toct_2021[, -c(1:5, 7, 9:25, 27:32)]
names(b_toct_2021)[names(b_toct_2021) == "FIELD_6"] <- "nbr_num"
names(b_toct_2021)[names(b_toct_2021) == "FIELD_8"] <- "nbr_chr"

## e. Compute weighted average
variables <- c("dens", "female", "age00_14", "age15_64", "age65plus", "avg_age",
               "hh_size", "median_inc", "limat", "one_parent", "immigrants", "minority",
               "noeduc", "chomage")
b_demo_2021 <- b_toct_2021 %>%
  dplyr::group_by(nbr_num) %>%
  dplyr::summarise(
    total_pop = sum(pop, na.rm = TRUE),
    dplyr::across(all_of(variables), ~ sum(.x * pop, na.rm = TRUE) / sum(pop, na.rm = TRUE), 
                  .names = "{col}")
  )

## 2. 2016 Census
## a. Include relevant variables
b_to_2016 <- get_census(dataset = 'CA16',
                        regions = list(CSD=c("3520005")),
                        vectors = c("v_CA16_401", "v_CA16_406", # Population
                                    "v_CA16_1", "v_CA16_2", "v_CA16_3", # Sex Distribution
                                    "v_CA16_4", "v_CA16_61", "v_CA16_244", # Age Distribution
                                    "v_CA16_379", "v_CA16_425", "v_CA16_484", "v_CA16_488", # Family/Household
                                    "v_CA16_2398", "v_CA16_2540", # Income
                                    "v_CA16_4861", "v_CA16_4859", # Unsuitable Housing
                                    "v_CA16_3411", "v_CA16_3957", # Minority/Immigrants
                                    "v_CA16_5051", "v_CA16_5054", # Education
                                    "v_CA16_5600", "v_CA16_5606"), # Unemployed
                        level='CT',
                        use_cache =FALSE, geo_format = "sf")

## b. Create new variables appropriate for analysis
b_to_2016 <- b_to_2016 %>% mutate(
  pop = .[[14]],
  dens = .[[15]],
  female = .[[19]] / .[[17]],
  age00_14 = .[[20]] / .[[17]],
  age15_64 = .[[21]] / .[[17]],
  age65plus = .[[22]] / .[[17]],
  avg_age = .[[23]],
  hh_size = .[[16]],
  median_inc = .[[26]],
  limat = .[[27]],
  one_parent = .[[25]] / .[[24]],
  immigrants = .[[30]] / .[[17]],
  minority = .[[31]] / .[[17]],
  noeduc = .[[33]] / .[[32]],
  chomage = .[[35]] / .[[34]]
)
b_to_2016 <- b_to_2016[, -c(14:35)]

## c. Merge neighbourhoods and census tracts by geographic points
if (st_crs(a_to_140) != st_crs(b_to_2016)) {
  b_to_2016 <- st_transform(b_to_2016, st_crs(a_to_140))
}
b_to_centroid <- st_centroid(b_to_2016)
b_toct_2016 <- st_join(a_to_140, b_to_centroid, join = st_intersects, left = F)

## d. Streamline dataset
b_toct_2016 <- b_toct_2016[, -c(1:5, 7, 9:22, 24:29)]
names(b_toct_2016)[names(b_toct_2016) == "FIELD_6"] <- "nbr_num"
names(b_toct_2016)[names(b_toct_2016) == "FIELD_8"] <- "nbr_chr"

## e. Compute weighted average
variables <- c("dens", "female", "age00_14", "age15_64", "age65plus", "avg_age",
               "hh_size", "median_inc", "limat", "one_parent", "immigrants", "minority",
               "noeduc", "chomage")
b_demo_2016 <- b_toct_2016 %>%
  dplyr::group_by(nbr_num) %>%
  dplyr::summarise(
    total_pop = sum(pop, na.rm = TRUE),
    dplyr::across(all_of(variables), ~ sum(.x * pop, na.rm = TRUE) / sum(pop, na.rm = TRUE), 
                  .names = "{col}")
  )

## 3. 2011 Census
## a. Include relevant variables
b_to_2011 <- get_census(dataset = 'CA11',
                        regions = list(CSD=c("3520005")),
                        vectors = c("v_CA11F_1", "v_CA11F_4", # Population
                                    "v_CA11F_5", "v_CA11F_6", "v_CA11F_7", # Sex Distribution
                                    "v_CA11F_8", "v_CA11F_11", "v_CA11F_14", 
                                    "v_CA11F_17", "v_CA11F_35", "v_CA11F_38",
                                    "v_CA11F_41", "v_CA11F_44", "v_CA11F_47",
                                    "v_CA11F_50", "v_CA11F_53", "v_CA11F_56",
                                    "v_CA11F_59", "v_CA11F_62", "v_CA11F_65", 
                                    "v_CA11F_68", "v_CA11F_71", "v_CA11F_74", # Age Distribution
                                    "v_CA11F_77", "v_CA11F_217", "v_CA11F_129", "v_CA11F_115", # Family/Household
                                    "v_CA11N_2564", "v_CA11N_2606", # Income
                                    "v_CA11N_2276", "v_CA11N_2274", # Unsuitable Housing
                                    "v_CA11N_460", "v_CA11N_22", # Minority/Immigrants
                                    "v_CA11N_1774", "v_CA11N_1771", # Education
                                    "v_CA11N_2008"), # Unemployed
                        level='CT',
                        use_cache =FALSE, geo_format = "sf")

## b. Create new variables appropriate for analysis
b_to_2011 <- b_to_2011 %>% mutate(
  pop = .[[16]],
  dens = .[[16]] / .[[17]],
  female = .[[20]] / .[[18]],
  age00_14 = (.[[21]] + .[[22]] + .[[23]]) / .[[18]],
  age15_64 = (.[[24]] + .[[25]] + .[[26]] + .[[27]] 
              + .[[28]] + .[[29]] + .[[30]] + .[[31]] 
              + .[[32]] + .[[33]]) / .[[18]],
  age65plus = (.[[34]] + .[[35]] + .[[36]] + .[[37]] + .[[38]]) / .[[18]],
  avg_age = .[[39]],
  hh_size = .[[40]],
  median_inc = .[[43]],
  limat = .[[44]],
  one_parent = .[[41]] / .[[42]],
  immigrants = .[[48]] / .[[18]],
  minority = .[[47]] / .[[18]],
  noeduc = .[[49]] / .[[50]],
  chomage = .[[51]]
)
b_to_2011 <- b_to_2011[, -c(16:51)]

## c. Merge neighbourhoods and census tracts by geographic points
if (st_crs(a_to_140) != st_crs(b_to_2011)) {
  b_to_2011 <- st_transform(b_to_2011, st_crs(a_to_140))
}
b_to_centroid <- st_centroid(b_to_2011)
b_toct_2011 <- st_join(a_to_140, b_to_centroid, join = st_intersects, left = F)

## d. Streamline dataset
b_toct_2011 <- b_toct_2011[, -c(1:5, 7, 9:18, 20:31)]
names(b_toct_2011)[names(b_toct_2011) == "FIELD_6"] <- "nbr_num"
names(b_toct_2011)[names(b_toct_2011) == "FIELD_8"] <- "nbr_chr"

## e. Compute weighted average
variables <- c("dens", "female", "age00_14", "age15_64", "age65plus", "avg_age",
               "hh_size", "median_inc", "limat", "one_parent", "immigrants", "minority",
               "noeduc", "chomage")
b_demo_2011 <- b_toct_2011 %>%
  dplyr::group_by(nbr_num) %>%
  dplyr::summarise(
    total_pop = sum(pop, na.rm = TRUE),
    dplyr::across(all_of(variables), ~ sum(.x * pop, na.rm = TRUE) / sum(pop, na.rm = TRUE), 
                  .names = "{col}")
  )

#### Part C: Data Setup ####
# 1. Create monthly count data
## Primary outcomes
c_demo_assault <- c_synth_dataorg1(b_demo_2021, "Assault")
names(c_demo_assault)[names(c_demo_assault) == "total_events"] <- "AS"
c_demo_auto <- c_synth_dataorg1(b_demo_2021, "Auto Theft")
names(c_demo_auto)[names(c_demo_auto) == "total_events"] <- "AT"
c_demo_bne <- c_synth_dataorg1(b_demo_2021, "Break and Enter")
names(c_demo_bne)[names(c_demo_bne) == "total_events"] <- "BE"
c_demo_robbery <- c_synth_dataorg1(b_demo_2021, "Robbery")
names(c_demo_robbery)[names(c_demo_robbery) == "total_events"] <- "RB"
c_demo_t5000 <- c_synth_dataorg1(b_demo_2021, "Theft Over")
names(c_demo_t5000)[names(c_demo_t5000) == "total_events"] <- "TH"
## Secondary outcomes
c_demo_bike <- c_synth_dataorg2(b_bike)
names(c_demo_bike)[names(c_demo_bike) == "total_events"] <- "BK"
c_demo_bike <- c_demo_bike %>% dplyr::select(-geometry)
c_demo_motor <- c_synth_dataorg2(b_motor)
names(c_demo_motor)[names(c_demo_motor) == "total_events"] <- "MV"
c_demo_motor <- c_demo_motor %>% dplyr::select(-geometry)
c_demo_mhaa <- c_synth_dataorg2(b_mhaa)
names(c_demo_mhaa)[names(c_demo_mhaa) == "total_events"] <- "MH"

# 2. Join MCI monthly count
c_mci <- c_demo_assault %>%
  inner_join(c_demo_auto, by = c("HOOD_140", "year", "month")) %>%
  inner_join(c_demo_bne, by = c("HOOD_140", "year", "month")) %>%
  inner_join(c_demo_robbery, by = c("HOOD_140", "year", "month")) %>%
  inner_join(c_demo_t5000, by = c("HOOD_140", "year", "month")) %>%
  inner_join(c_demo_bike, by = c("HOOD_140", "year", "month")) %>%
  inner_join(c_demo_motor, by = c("HOOD_140", "year", "month")) %>%
  inner_join(c_demo_mhaa, by = c("HOOD_140", "year", "month"))
names(c_mci)[names(c_mci) == "HOOD_140"] <- "nbr_num"

# 3. Separate data by time
c_03a <- c_mci %>% filter(year %in% c(2014:2015))
c_03b <- c_mci %>% filter(year %in% c(2016:2020))
c_03c <- c_mci %>% filter(year %in% c(2021:2024))

# 4. Join census covariates
c_04a <- c_03a %>% 
  left_join(b_demo_2011, by = "nbr_num") %>% 
  dplyr::select(-geometry)
c_04b <- c_03b %>% 
  left_join(b_demo_2016, by = "nbr_num") %>% 
  dplyr::select(-geometry)
c_04c <- c_03c %>% 
  left_join(b_demo_2021, by = "nbr_num") %>% 
  dplyr::select(-geometry)

# 5. Append data
c_05 <- rbind(c_04a, c_04b, c_04c)
c_05 <- c_05 %>% arrange(nbr_num, year, month)

# 6. Create binary variable for treated vs. control
# Create a sequence from 1 to 140
nbr_seq <- c(1:140)
nbr_trt <- c(70, 72, 73, 74, 75, 77, 78, 81, 82, 85, 86)
nbr_ctrl <- setdiff(nbr_seq, nbr_trt)
c_06 <- c_05 %>%
  dplyr::mutate(group = case_when(
    !is.na(nbr_num) & nbr_num %in% nbr_trt ~ 1, 
    TRUE ~ 0
  ))

# 7. Add time variable
c_07 <- c_06 %>%
  dplyr::group_by(nbr_num) %>%          # Group by neighborhood
  dplyr::mutate(time = row_number()) %>% # Generate sequential time variable
  dplyr::ungroup()                      # Ungroup after creating the time variable

# 8. Add neighbourhood names
# Unique Toronto neighbourhoods
to_140 <- read_sf("C:/Users/limji/Desktop/Research Associate/CITS/geo_project/Neighbourhoods/Neighbourhoods.shp")
to_140 <- to_140 %>% as_tibble() %>% dplyr::select(6:7, -geometry)
names(to_140)[names(to_140) == "FIELD_6"] <- "nbr_num"
names(to_140)[names(to_140) == "FIELD_7"] <- "nbr_chr"
c_08 <- c_07 %>%
  dplyr::left_join(to_140 %>% dplyr::select(nbr_num, nbr_chr), by = "nbr_num") %>%
  dplyr::select(everything(), nbr_chr)
c_08 <- c_08 %>% mutate(nbr = nbr_num)
c_08 <- as.data.frame(c_08)

#### Part D: Synthetic Control Analysis ####
# | 1. Demographic variable setup ####
# Data set with neighbourhood demographic (predictor) values only
demo_fields <- c_08 %>%
  dplyr::select(nbr_num, nbr_chr, time, total_pop, dens, female, 
                age00_14, age15_64, age65plus, avg_age, hh_size, 
                median_inc, limat, one_parent, immigrants, minority, 
                noeduc, chomage)
# Retain values for each PHU for January 2017 only
demo_sub <- filter(demo_fields, time==25) #change time to reflect time of interest for unweighted sample mean
# Restructure and clean data set
demo_sub <- subset(demo_sub, select = -c(nbr_chr, time))
demo_wide <- dcast(melt(demo_sub, id.vars='nbr_num'), variable ~ nbr_num)
demo_wide <- demo_wide %>% clean_names

# Table of demographic values for exposed PHUs only
demo_trt <- subset(demo_wide, select=c(x70, x72, x73, x74, x75, x77, x78, x81, x82, x85, x86))
# Table of demographic values for synthetic control PHU pool 
demo_sc <- subset(demo_wide, select=-c(variable, x70, x72, x73, x74, x75, x77, x78, x81, x82, x85, x86))

# | 2. Function input setup ####
# Neighbourhood names
x70 <- "x70"; assign(x70, as.name(x70))
x72 <- "x72"; assign(x72, as.name(x72))
x73 <- "x73"; assign(x73, as.name(x73))
x74 <- "x74"; assign(x74, as.name(x74))
x75 <- "x75"; assign(x75, as.name(x75))
x77 <- "x77"; assign(x77, as.name(x77))
x78 <- "x78"; assign(x78, as.name(x78))
x81 <- "x81"; assign(x81, as.name(x81))
x82 <- "x82"; assign(x82, as.name(x82))
x85 <- "x85"; assign(x85, as.name(x85))
x86 <- "x86"; assign(x86, as.name(x86))

# Outcome names
AS <- "AS"; assign(AS, as.name(AS)) # Assault
AT <- "AT"; assign(AT, as.name(AT)) # Auto theft
BE <- "BE"; assign(BE, as.name(BE)) # Break and enter
RB <- "RB"; assign(RB, as.name(RB)) # Robbery
TH <- "TH"; assign(TH, as.name(TH)) # Theft over $5000
BK <- "BK"; assign(BK, as.name(BK)) # Bicycle theft
MV <- "MV"; assign(MV, as.name(MV)) # Theft from vehicles
MH <- "MH"; assign(MH, as.name(MH)) # Mental health apprehension

# Miscellaneous names - For data_sub_wide dataset
x70.x <- "x70.x"; assign(x70.x, as.name(x70.x))
x72.x <- "x72.x"; assign(x72.x, as.name(x72.x)) 
x73.x <- "x73.x"; assign(x73.x, as.name(x73.x))
x74.x <- "x74.x"; assign(x74.x, as.name(x74.x))
x75.x <- "x75.x"; assign(x75.x, as.name(x75.x))
x77.x <- "x77.x"; assign(x77.x, as.name(x77.x))
x78.x <- "x78.x"; assign(x78.x, as.name(x78.x))
x81.x <- "x81.x"; assign(x81.x, as.name(x81.x))
x82.x <- "x82.x"; assign(x82.x, as.name(x82.x)) 
x85.x <- "x85.x"; assign(x85.x, as.name(x85.x))
x86.x <- "x86.x"; assign(x86.x, as.name(x86.x))
y70.y <- "y70.y"; assign(y70.y, as.name(y70.y))
y72.y <- "y72.y"; assign(y72.y, as.name(y72.y)) 
y73.y <- "y73.y"; assign(y73.y, as.name(y73.y))
y74.y <- "y74.y"; assign(y74.y, as.name(y74.y))
y75.y <- "y75.y"; assign(y75.y, as.name(y75.y))
y77.y <- "y77.y"; assign(y77.y, as.name(y77.y))
y78.y <- "y78.y"; assign(y78.y, as.name(y78.y))
y81.y <- "y81.y"; assign(y81.y, as.name(y81.y))
y82.y <- "y82.y"; assign(y82.y, as.name(y82.y)) 
y85.y <- "y85.y"; assign(y85.y, as.name(y85.y))
y86.y <- "y86.y"; assign(y86.y, as.name(y86.y))

# | 2. Synthetic control analysis ####
nbr_list <- list(
  list(nbr_trt_num = 70, x_nbr = x70, to_nbr = "South Riverdale", num_x = "x70.x", num_y = "y70.y", nbr_short = "sr", interv_time = 47),
  list(nbr_trt_num = 72, x_nbr = x72, to_nbr = "Regent Park", num_x = "x72.x", num_y = "y72.y", nbr_short = "rp", interv_time = 52),
  list(nbr_trt_num = 73, x_nbr = x73, to_nbr = "Moss Park", num_x = "x73.x", num_y = "y73.y", nbr_short = "mp", interv_time = 44),
  list(nbr_trt_num = 74, x_nbr = x74, to_nbr = "North St.James", num_x = "x74.x", num_y = "y74.y", nbr_short = "ns", interv_time = 54),
  list(nbr_trt_num = 75, x_nbr = x75, to_nbr = "Church-Yonge", num_x = "x75.x", num_y = "y75.y", nbr_short = "cy", interv_time = 44),
  list(nbr_trt_num = 77, x_nbr = x77, to_nbr = "Waterfront Communities", num_x = "x77.x", num_y = "y77.y", nbr_short = "wc", interv_time = 51),
  list(nbr_trt_num = 78, x_nbr = x78, to_nbr = "Kensington-Chinatown", num_x = "x78.x", num_y = "y78.y", nbr_short = "kc", interv_time = 51),
  list(nbr_trt_num = 81, x_nbr = x81, to_nbr = "Trinity-Bellwoods", num_x = "x81.x", num_y = "y81.y", nbr_short = "tb", interv_time = 51),
  list(nbr_trt_num = 82, x_nbr = x82, to_nbr = "Niagara", num_x = "x82.x", num_y = "y82.y", nbr_short = "ni", interv_time = 51),
  list(nbr_trt_num = 85, x_nbr = x85, to_nbr = "South Parkdale", num_x = "x85.x", num_y = "y85.y", nbr_short = "sp", interv_time = 51),
  list(nbr_trt_num = 86, x_nbr = x86, to_nbr = "Roncesvalles", num_x = "x86.x", num_y = "y86.y", nbr_short = "ro", interv_time = 51)
)

## || a. Loop through each neighborhood to create aggregate data ####
# Assault
agg_AS <- NULL
for (nb in nbr_list) {
  # Synthetic control - Engine
  d_sca(input_data = c_08, interv_time = nb$interv_time, dep_qu1 = "AS", dep_qu0 = AS, 
        nbr_trt_num = nb$nbr_trt_num, x_nbr = nb$x_nbr, to_nbr = nb$to_nbr, mci = "Assault", 
        num_x = nb$num_x, num_y = nb$num_y, nbr_short = nb$nbr_short)
  # Change variable names
  mv("nbr_sc_pair", paste0("sc_as_", nb$nbr_short))
  # Create aggregate data
  if (is.null(agg_AS)) {
    agg_AS <- get(paste0("sc_as_", nb$nbr_short))  # Initialize aggregate data
  } else {
    agg_AS <- merge(x = agg_AS, y = get(paste0("sc_as_", nb$nbr_short)), by = 'int.time', all = TRUE)
  }
}

# Auto Theft
agg_AT <- NULL
for (nb in nbr_list) {
  # Synthetic control - Engine
  d_sca(input_data = c_08, interv_time = nb$interv_time, dep_qu1 = "AT", dep_qu0 = AT, 
        nbr_trt_num = nb$nbr_trt_num, x_nbr = nb$x_nbr, to_nbr = nb$to_nbr, mci = "Auto theft", 
        num_x = nb$num_x, num_y = nb$num_y, nbr_short = nb$nbr_short)
  # Change variable names
  mv("nbr_sc_pair", paste0("sc_at_", nb$nbr_short))
  # Create aggregate data
  if (is.null(agg_AT)) {
    agg_AT <- get(paste0("sc_at_", nb$nbr_short))  # Initialize aggregate data
  } else {
    agg_AT <- merge(x = agg_AT, y = get(paste0("sc_at_", nb$nbr_short)), by = 'int.time', all = TRUE)
  }
}

# Break and Enter
agg_BE <- NULL
for (nb in nbr_list) {
  # Synthetic control - Engine
  d_sca(input_data = c_08, interv_time = nb$interv_time, dep_qu1 = "BE", dep_qu0 = BE, 
        nbr_trt_num = nb$nbr_trt_num, x_nbr = nb$x_nbr, to_nbr = nb$to_nbr, mci = "B&E", 
        num_x = nb$num_x, num_y = nb$num_y, nbr_short = nb$nbr_short)
  # Change variable names
  mv("nbr_sc_pair", paste0("sc_be_", nb$nbr_short))
  # Create aggregate data
  if (is.null(agg_BE)) {
    agg_BE <- get(paste0("sc_be_", nb$nbr_short))  # Initialize aggregate data
  } else {
    agg_BE <- merge(x = agg_BE, y = get(paste0("sc_be_", nb$nbr_short)), by = 'int.time', all = TRUE)
  }
}

# Robbery
agg_RB <- NULL
for (nb in nbr_list) {
  # Synthetic control - Engine
  d_sca(input_data = c_08, interv_time = nb$interv_time, dep_qu1 = "RB", dep_qu0 = RB, 
        nbr_trt_num = nb$nbr_trt_num, x_nbr = nb$x_nbr, to_nbr = nb$to_nbr, mci = "Auto theft", 
        num_x = nb$num_x, num_y = nb$num_y, nbr_short = nb$nbr_short)
  # Change variable names
  mv("nbr_sc_pair", paste0("sc_rb_", nb$nbr_short))
  # Create aggregate data
  if (is.null(agg_RB)) {
    agg_RB <- get(paste0("sc_rb_", nb$nbr_short))  # Initialize aggregate data
  } else {
    agg_RB <- merge(x = agg_RB, y = get(paste0("sc_rb_", nb$nbr_short)), by = 'int.time', all = TRUE)
  }
}

# Theft over $5000
agg_TH <- NULL
for (nb in nbr_list) {
  # Synthetic control - Engine
  d_sca(input_data = c_08, interv_time = nb$interv_time, dep_qu1 = "TH", dep_qu0 = TH, 
        nbr_trt_num = nb$nbr_trt_num, x_nbr = nb$x_nbr, to_nbr = nb$to_nbr, mci = "Thefts >$5000", 
        num_x = nb$num_x, num_y = nb$num_y, nbr_short = nb$nbr_short)
  # Change variable names
  mv("nbr_sc_pair", paste0("sc_th_", nb$nbr_short))
  # Create aggregate data
  if (is.null(agg_TH)) {
    agg_TH <- get(paste0("sc_th_", nb$nbr_short))  # Initialize aggregate data
  } else {
    agg_TH <- merge(x = agg_TH, y = get(paste0("sc_th_", nb$nbr_short)), by = 'int.time', all = TRUE)
  }
}

# Bicycle Thefts
agg_BK <- NULL
for (nb in nbr_list) {
  # Synthetic control - Engine
  d_sca(input_data = c_08, interv_time = nb$interv_time, dep_qu1 = "BK", dep_qu0 = BK, 
        nbr_trt_num = nb$nbr_trt_num, x_nbr = nb$x_nbr, to_nbr = nb$to_nbr, mci = "Bicycle thefts", 
        num_x = nb$num_x, num_y = nb$num_y, nbr_short = nb$nbr_short)
  # Change variable names
  mv("nbr_sc_pair", paste0("sc_bk_", nb$nbr_short))
  # Create aggregate data
  if (is.null(agg_BK)) {
    agg_BK <- get(paste0("sc_bk_", nb$nbr_short))  # Initialize aggregate data
  } else {
    agg_BK <- merge(x = agg_BK, y = get(paste0("sc_bk_", nb$nbr_short)), by = 'int.time', all = TRUE)
  }
}

# Theft from vehicles
agg_MV <- NULL
for (nb in nbr_list) {
  # Synthetic control - Engine
  d_sca(input_data = c_08, interv_time = nb$interv_time, dep_qu1 = "MV", dep_qu0 = MV, 
        nbr_trt_num = nb$nbr_trt_num, x_nbr = nb$x_nbr, to_nbr = nb$to_nbr, mci = "Thefts from vehicles", 
        num_x = nb$num_x, num_y = nb$num_y, nbr_short = nb$nbr_short)
  # Change variable names
  mv("nbr_sc_pair", paste0("sc_mv_", nb$nbr_short))
  # Create aggregate data
  if (is.null(agg_MV)) {
    agg_MV <- get(paste0("sc_mv_", nb$nbr_short))  # Initialize aggregate data
  } else {
    agg_MV <- merge(x = agg_MV, y = get(paste0("sc_mv_", nb$nbr_short)), by = 'int.time', all = TRUE)
  }
}

# Mental health apprehensions
agg_MH <- NULL
for (nb in nbr_list) {
  # Synthetic control - Engine
  d_sca(input_data = c_08, interv_time = nb$interv_time, dep_qu1 = "MH", dep_qu0 = MH, 
        nbr_trt_num = nb$nbr_trt_num, x_nbr = nb$x_nbr, to_nbr = nb$to_nbr, mci = "Mental health apprehensions", 
        num_x = nb$num_x, num_y = nb$num_y, nbr_short = nb$nbr_short)
  # Change variable names
  mv("nbr_sc_pair", paste0("sc_mh_", nb$nbr_short))
  # Create aggregate data
  if (is.null(agg_MH)) {
    agg_MH <- get(paste0("sc_mh_", nb$nbr_short))  # Initialize aggregate data
  } else {
    agg_MH <- merge(x = agg_MH, y = get(paste0("sc_mh_", nb$nbr_short)), by = 'int.time', all = TRUE)
  }
}

#### Part E: Aggregate Analysis ####
# | 1. Aggregate data - Trt and SC ####
e_agg_AS <- e_aggregate(agg_AS); e_agg_AT <- e_aggregate(agg_AT)
e_agg_BE <- e_aggregate(agg_BE); e_agg_RB <- e_aggregate(agg_RB)
e_agg_TH <- e_aggregate(agg_TH); e_agg_BK <- e_aggregate(agg_BK)
e_agg_MV <- e_aggregate(agg_MV); e_agg_MH <- e_aggregate(agg_MH)

# | 2. Run CITS ####
e_reg_as <- e_cits_sc(e_agg_AS, "Assault")
e_reg_at <- e_cits_sc(e_agg_AT, "Auto theft")
e_reg_be <- e_cits_sc(e_agg_BE, "Break and enter")
e_reg_rb <- e_cits_sc(e_agg_RB, "Robbery")
e_reg_th <- e_cits_sc(e_agg_TH, "Thefts $5000+")
e_reg_bk <- e_cits_sc(e_agg_BK, "Bicycle theft")
e_reg_mv <- e_cits_sc(e_agg_MV, "Theft from vehicles")
e_reg_mh <- e_cits_sc(e_agg_MH, "Mental health apprehension")
