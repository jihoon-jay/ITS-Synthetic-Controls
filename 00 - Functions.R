## Jihoon Lim
## 00 - Functions
## September 17, 2024

#### Part A: Toronto Data ####
a_dataorg <- function(to_data) {
  # | 1. Remove rows with NSA in specific columns ####
  a01 <- subset(to_data, to_data$DIVISION != "NSA") # N = 392,743
  a01a <- subset(a01, a01$HOOD_140 != "NSA") # N = 390,448
  a01b <- subset(a01a, a01a$NEIGHBOURHOOD_140 != "NSA") # N = 390,448
  # | 2. Remove rows with missing dates ####
  a02 <- a01b %>% 
    filter(!is.na(OCC_YEAR) & !is.na(OCC_MONTH) & !is.na(OCC_DAY)) # N = 390,332
  # | 3. Remove events before 1/1/2014 ####
  a03 <- a02 %>% 
    filter(OCC_YEAR >= 2014 & OCC_DOY >= 1) # N = 388,932
  # | 4. Remove duplicate event ID ####
  a04 <- a03 %>%
    distinct(EVENT_UNIQUE_ID, .keep_all = TRUE) # N = 339,058
  # | 5. Remove leading and trailing spaces ####
  a05 <- data.frame(lapply(a04, function(x) {
    if (is.character(x)) trimws(x) else x
  }), stringsAsFactors = FALSE) # N = 339,058
  # | 6. Remove where REPORT_DATE<OCC_DATE ####
  a06 <- a05 %>% 
    filter(REPORT_DATE >= OCC_DATE) # N = 339,058
  a06$HOOD_140 <- as.numeric(a06$HOOD_140)
  # Check the number of unique
  length(unique(a06$HOOD_140))
  length(unique(a06$NEIGHBOURHOOD_140))
  length(unique(a06$MCI_CATEGORY))
  
  # | 7. Check if there is a mismatch in neighbourhood names ####
  a07 <- a06 %>% 
    mutate(Hood_ID2 = regmatches(a06$NEIGHBOURHOOD_140, regexpr("[0-9]+",a06$NEIGHBOURHOOD_140)))
  class(a07$HOOD_140)
  class(a07$Hood_ID2)
  a07 %>% 
    filter(HOOD_140 != Hood_ID2) %>% 
    count() # N = 0
  sum(grepl("^\\s*$", a07$HOOD_140)) # N = 0
  return(a06)
}

#### Part B: Crime Event Coordinates ####
b_map <- function(mci, map_title) {
  # Descriptive map of Toronto by MCI
  # Step 1: Subset data
  crime_data <- subset(b01, b01$MCI_CATEGORY == mci)
  crime_data <- st_as_sf(crime_data, coords = c("LONG_WGS84", "LAT_WGS84"), crs = 4326)
  crime_data <- st_transform(crime_data, crs = 26917)
  
  # Step 2: Generate plot
  crime_map <- ggplot() +
    geom_sf(data = crime_data, color = "red", shape = 21) + # Crime
    geom_sf(data = b02, color = "black", lwd = 0.7, alpha = 0.1) + # Neighbourhood
    geom_sf(data = to_scs, color = "blue", shape = 16) + # SCS
    labs(title = map_title) +
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
  
  return(crime_map)
}

#### Part C: Data Set for Each SCS ####
c_scs1 <- function(input_data, site, mci, open_date) {
  # Notes:
  # Site variable includes: "Moss Park (OPS)", "The Works", "South Riverdale CHC", "Fred Victor",
  # "Parkdale CHC", "Parkdale SCS", "Regent Park CHC", "St. Stephens / KMOPS", 
  # "Street Health", and "Casey's House"
  # MCI variable includes: "Assault", "Auto Theft", "Break and Enter", "Robbery", and "Theft Over"
  # Opening date for programming purposes: Moss Park ("2017-08-01"), The Works ("2017-08-01"),
  # South Riverdale CHC ("2017-11-01"), Fred Victor ("2018-02-01"), Parkdale CHC ("2018-03-01"), 
  # Parkdale SCS ("2018-03-01"), Regent Park CHC ("2018-04-01"), St. Stephens / KMOPS ("2018-04-01")
  # Street Health ("2018-06-01"), Casey's House ("2021-04-01")
  
  ## Step 1: Geographic coordinates for SCS
  scs_geo <- input_data
  scs_geo <- st_transform(scs_geo, crs = 4326)
  if (site == "Moss Park") {
    scs_geo$scs <- ifelse(scs_geo$event_date <= "2018-05-31", 
                          scs_sf$geometry[1,], scs_sf$geometry[2,])
    scs_geo$scs <- st_sfc(lapply(scs_geo$scs, st_point), crs = 4326) # Convert coordinates
  } else if (site == "The Works") {
    scs_geo$scs <- scs_sf$geometry[3,]
    scs_geo$scs <- st_sfc(lapply(scs_geo$scs, st_point), crs = 4326)
  } else if (site == "South Riverdale CHC") {
    scs_geo$scs <- scs_sf$geometry[4,]
    scs_geo$scs <- st_sfc(lapply(scs_geo$scs, st_point), crs = 4326)
  } else if (site == "Fred Victor") {
    scs_geo$scs <- scs_sf$geometry[5,]
    scs_geo$scs <- st_sfc(lapply(scs_geo$scs, st_point), crs = 4326)
  } else if (site == "Parkdale CHC") {
    scs_geo$scs <- scs_sf$geometry[6,]
    scs_geo$scs <- st_sfc(lapply(scs_geo$scs, st_point), crs = 4326)
  } else if (site == "Parkdale SCS") {
    scs_geo$scs <- scs_sf$geometry[7,]
    scs_geo$scs <- st_sfc(lapply(scs_geo$scs, st_point), crs = 4326)
  } else if (site == "Regent Park CHC") {
    scs_geo$scs <- scs_sf$geometry[8,]
    scs_geo$scs <- st_sfc(lapply(scs_geo$scs, st_point), crs = 4326)
  } else if (site == "St. Stephens / KMOPS") {
    scs_geo$scs <- scs_sf$geometry[9,]
    scs_geo$scs <- st_sfc(lapply(scs_geo$scs, st_point), crs = 4326)
  } else if (site == "Street Health") {
    scs_geo$scs <- scs_sf$geometry[10,]
    scs_geo$scs <- st_sfc(lapply(scs_geo$scs, st_point), crs = 4326)
  }
  
  ## Step 2: Extract the coordinates from the geometry column
  coordinates_event <- st_coordinates(scs_geo$geometry)
  coordinates_scs <- st_coordinates(scs_geo$scs)
  
  ## Step 3: Create separate longitude and latitude columns
  scs_geo$long_event <- coordinates_event[, 1]  # First column is longitude
  scs_geo$lat_event <- coordinates_event[, 2]   # Second column is latitude
  scs_geo$long_scs <- coordinates_scs[, 1]  # First column is longitude
  scs_geo$lat_scs <- coordinates_scs[, 2]   # Second column is latitude
  
  ## Step 4: Calculate distance
  scs_geo$dist <- purrr::pmap_dbl(.l = list(scs_geo$long_event,
                                            scs_geo$lat_event,
                                            scs_geo$long_scs,
                                            scs_geo$lat_scs),
                                  .f = ~distm(c(..1,..2),c(..3,..4)))
  
  ## Step 5: Major crime categories
  scs_geo_mci <- subset(scs_geo, scs_geo$MCI_CATEGORY == mci)
  
  ## Step 6: Calculate the number of events for each month
  scs_geo_mci$event_100m <- ifelse(scs_geo_mci$dist <= 100, 1, 0)
  scs_geo_mci$event_200m <- ifelse(scs_geo_mci$dist <= 200, 1, 0)
  scs_geo_mci$event_500m <- ifelse(scs_geo_mci$dist <= 500, 1, 0)
  scs_geo_mci$donut_200m <- ifelse(scs_geo_mci$dist > 100 & scs_geo_mci$dist <= 200, 1, 0)
  scs_geo_mci$donut_500m <- ifelse(scs_geo_mci$dist > 100 & scs_geo_mci$dist <= 500, 1, 0)
  scs_geo_crime <- subset(scs_geo_mci, scs_geo_mci$event_500m == 1)
  ## Set as regular data frame
  scs_geo_crime <- dplyr::select(as.data.frame(scs_geo_crime), -c(geometry, scs))
  return(scs_geo_crime)
}

# For secondary outcomes
c_scs2 <- function(input_data, site, open_date) {
  # Notes:
  # Site variable includes: "Moss Park (OPS)", "The Works", "South Riverdale CHC", "Fred Victor",
  # "Parkdale CHC", "Parkdale SCS", "Regent Park CHC", "St. Stephens / KMOPS", 
  # "Street Health", and "Casey's House"
  # MCI variable includes: "Assault", "Auto Theft", "Break and Enter", "Robbery", and "Theft Over"
  # Opening date for programming purposes: Moss Park ("2017-08-01"), The Works ("2017-08-01"),
  # South Riverdale CHC ("2017-11-01"), Fred Victor ("2018-02-01"), Parkdale CHC ("2018-03-01"), 
  # Parkdale SCS ("2018-03-01"), Regent Park CHC ("2018-04-01"), St. Stephens / KMOPS ("2018-04-01")
  # Street Health ("2018-06-01"), Casey's House ("2021-04-01")
  
  ## Step 1: Geographic coordinates for SCS
  scs_geo <- input_data
  scs_geo <- st_transform(scs_geo, crs = 4326)
  if (site == "Moss Park") {
    scs_geo$scs <- ifelse(scs_geo$event_date <= "2018-05-31", 
                          scs_sf$geometry[1,], scs_sf$geometry[2,])
    scs_geo$scs <- st_sfc(lapply(scs_geo$scs, st_point), crs = 4326) # Convert coordinates
  } else if (site == "The Works") {
    scs_geo$scs <- scs_sf$geometry[3,]
    scs_geo$scs <- st_sfc(lapply(scs_geo$scs, st_point), crs = 4326)
  } else if (site == "South Riverdale CHC") {
    scs_geo$scs <- scs_sf$geometry[4,]
    scs_geo$scs <- st_sfc(lapply(scs_geo$scs, st_point), crs = 4326)
  } else if (site == "Fred Victor") {
    scs_geo$scs <- scs_sf$geometry[5,]
    scs_geo$scs <- st_sfc(lapply(scs_geo$scs, st_point), crs = 4326)
  } else if (site == "Parkdale CHC") {
    scs_geo$scs <- scs_sf$geometry[6,]
    scs_geo$scs <- st_sfc(lapply(scs_geo$scs, st_point), crs = 4326)
  } else if (site == "Parkdale SCS") {
    scs_geo$scs <- scs_sf$geometry[7,]
    scs_geo$scs <- st_sfc(lapply(scs_geo$scs, st_point), crs = 4326)
  } else if (site == "Regent Park CHC") {
    scs_geo$scs <- scs_sf$geometry[8,]
    scs_geo$scs <- st_sfc(lapply(scs_geo$scs, st_point), crs = 4326)
  } else if (site == "St. Stephens / KMOPS") {
    scs_geo$scs <- scs_sf$geometry[9,]
    scs_geo$scs <- st_sfc(lapply(scs_geo$scs, st_point), crs = 4326)
  } else if (site == "Street Health") {
    scs_geo$scs <- scs_sf$geometry[10,]
    scs_geo$scs <- st_sfc(lapply(scs_geo$scs, st_point), crs = 4326)
  }
  
  ## Step 2: Extract the coordinates from the geometry column
  coordinates_event <- st_coordinates(scs_geo$geometry)
  coordinates_scs <- st_coordinates(scs_geo$scs)
  
  ## Step 3: Create separate longitude and latitude columns
  scs_geo$long_event <- coordinates_event[, 1]  # First column is longitude
  scs_geo$lat_event <- coordinates_event[, 2]   # Second column is latitude
  scs_geo$long_scs <- coordinates_scs[, 1]  # First column is longitude
  scs_geo$lat_scs <- coordinates_scs[, 2]   # Second column is latitude
  
  ## Step 4: Calculate distance
  scs_geo$dist <- purrr::pmap_dbl(.l = list(scs_geo$long_event,
                                            scs_geo$lat_event,
                                            scs_geo$long_scs,
                                            scs_geo$lat_scs),
                                  .f = ~distm(c(..1,..2),c(..3,..4)))
  
  ## Step 5: Calculate the number of events for each month
  scs_geo$event_100m <- ifelse(scs_geo$dist <= 100, 1, 0)
  scs_geo$event_200m <- ifelse(scs_geo$dist <= 200, 1, 0)
  scs_geo$event_500m <- ifelse(scs_geo$dist <= 500, 1, 0)
  scs_geo$donut_200m <- ifelse(scs_geo$dist > 100 & scs_geo$dist <= 200, 1, 0)
  scs_geo$donut_500m <- ifelse(scs_geo$dist > 100 & scs_geo$dist <= 500, 1, 0)
  scs_geo_crime <- subset(scs_geo, scs_geo$event_500m == 1)
  ## Set as regular data frame
  scs_geo_crime <- dplyr::select(as.data.frame(scs_geo_crime), -c(geometry, scs))
  return(scs_geo_crime)
}

c_crime_count <- function (crime_data, open_date) {
  ## Step 1: Extract year and month from the date column using lubridate
  scs_geo_mci <- crime_data %>%
    dplyr::mutate(year_month = floor_date(event_date, "month"))  # Create a "year-month" variable
  
  ## Step 2: Define the range of year_month for `complete()` before summarizing
  min_month <- floor_date(as.Date("2014-01-01"), "month")
  max_month <- floor_date(as.Date("2024-06-01"), "month")
  year_month_range <- seq(min_month, max_month, by = "month")
  
  ## Step 3: Count the number of events for each month
  mo_counts <- scs_geo_mci %>%
    dplyr::group_by(year_month) %>%
    dplyr::summarise(
      count_100m = sum(event_100m, na.rm = TRUE),
      count_200m = sum(event_200m, na.rm = TRUE),
      count_500m = sum(event_500m, na.rm = TRUE),
      dcount_200m = sum(donut_200m, na.rm = TRUE),
      dcount_500m = sum(donut_500m, na.rm = TRUE)
    )
  
  ## Step 3: Ensure no missing months by filling in any missing 'year_month' values
  mo_counts <- mo_counts %>% 
    tidyr::complete(year_month = year_month_range, 
                    fill = list(count_100m = 0, count_200m = 0, count_500m = 0, 
                                dcount_200m = 0, dcount_500m = 0))
  
  ## Add a check to validate counts
  mo_counts$check <- with(mo_counts, count_100m <= count_200m & count_200m <= count_500m)
  sum(!mo_counts$check) # If 0, logic is satisfied.
  
  ## Step 4: Define the reference start date
  start_date <- as.Date(open_date)
  
  ## Step 5: Create time, level, and trend variables
  mo_counts$time <- interval(start_date, mo_counts$year_month) %/% months(1)
  mo_counts <- dplyr::select(as.data.frame(mo_counts), -c(check))
  
  return(mo_counts)
}

# Data organization for synthetic control analysis
c_synth_dataorg1 <- function(b_data, mci) {
  # Note: The 'a_data' dataset comes from 01 - Analysis.
  # Merge data
  c_demo <- full_join(a_mci_ready, b_data, by = c("HOOD_140" = "nbr_num"))
  c_demo <- c_demo[,c(13:15, 25, 28:29, 32:47)]
  c_demo$event_date <- lubridate::ymd(paste0(c_demo$OCC_YEAR, c_demo$OCC_MONTH, c_demo$OCC_DAY))
  c_demo <- dplyr::select(as.data.frame(c_demo), -c(OCC_YEAR, OCC_MONTH, OCC_DAY))
  c_demo$year <- lubridate::year(c_demo$event_date)
  c_demo$month <- lubridate::month(c_demo$event_date)
  
  # Summing events grouped by neighbourhood and month
  mci_vector <- c("Assault", "Auto Theft", "Break and Enter", "Robbery", "Theft Over")
  if (mci %in% mci_vector) {
    c_demo_subset <- subset(c_demo, c_demo$MCI_CATEGORY == mci)
  } else {
    c_demo_subset <- c_demo
  }
  c_demo_grouped <- c_demo_subset %>%
    dplyr::group_by(HOOD_140, year, month) %>%
    dplyr::summarise(total_events = dplyr::n(), .groups = 'drop')
  
  # Ensure that all neighbourhoods and months are included, even with 0 events
  c_demo_complete <- c_demo_grouped %>%
    complete(
      HOOD_140, 
      year, 
      month = full_seq(month, 1),   # Fills missing months
      fill = list(total_events = 0) # Fills missing counts with 0
    )
  # Exclude if between 07/2024 and 12/2024
  month_v <- c(7, 8, 9, 10, 11, 12)
  c_demo_complete <- c_demo_complete %>%
    filter(!(year == 2024 & month %in% month_v))
  return(c_demo_complete)
}

c_synth_dataorg2 <- function(b_data) {
  b_data$year <- lubridate::year(b_data$event_date)
  b_data$month <- lubridate::month(b_data$event_date)
  c_demo <- b_data %>%
    dplyr::group_by(HOOD_140, year, month) %>%
    dplyr::summarise(total_events = dplyr::n(), .groups = 'drop')
  
  # Ensure that all neighbourhoods and months are included, even with 0 events
  c_demo <- c_demo %>%
    complete(
      HOOD_140, 
      year, 
      month = full_seq(month, 1),   # Fills missing months
      fill = list(total_events = 0) # Fills missing counts with 0
    )
  # Exclude if between 07/2024 and 12/2024
  month_v <- c(7, 8, 9, 10, 11, 12)
  c_demo <- c_demo %>%
    filter(!(year == 2024 & month %in% month_v))
  return(c_demo)
}

#### Part D: Aggregate Analysis ####
d_aggregate <- function(data01, data02, data03, data04, data05, data06, data07,
                        data08, data09) {
  # Step 1: Aggregate data
  d_aggr <- data01
  d_aggr <- merge(x=d_aggr, y=data02, by='time', all=TRUE)
  d_aggr <- merge(x=d_aggr, y=data03, by='time', all=TRUE)
  d_aggr <- merge(x=d_aggr, y=data04, by='time', all=TRUE)
  d_aggr <- merge(x=d_aggr, y=data05, by='time', all=TRUE)
  d_aggr <- merge(x=d_aggr, y=data06, by='time', all=TRUE)
  d_aggr <- merge(x=d_aggr, y=data07, by='time', all=TRUE)
  d_aggr <- merge(x=d_aggr, y=data08, by='time', all=TRUE)
  d_aggr <- merge(x=d_aggr, y=data09, by='time', all=TRUE)
  d_aggr <- d_aggr[complete.cases(d_aggr), ]
  
  # Step 2: Sum the counts for each distance
  d_aggr$crime_100m <- rowSums(d_aggr[startsWith(names(d_aggr), "count_100m")])
  d_aggr$crime_200m <- rowSums(d_aggr[startsWith(names(d_aggr), "count_200m")])
  d_aggr$crime_500m <- rowSums(d_aggr[startsWith(names(d_aggr), "count_500m")])
  d_aggr$crime_d200m <- rowSums(d_aggr[startsWith(names(d_aggr), "dcount_200m")])
  d_aggr$crime_d500m <- rowSums(d_aggr[startsWith(names(d_aggr), "dcount_500m")])
  d_aggr <- d_aggr[, c("time", "crime_100m", "crime_200m", "crime_500m", 
                       "crime_d200m", "crime_d500m")]
  
  # Step 3: Add level and trend
  d_aggr$level <- ifelse(d_aggr$time <= 0, 0, 1)
  d_aggr$trend <- d_aggr$time * d_aggr$level
  
  return(d_aggr)
}

#### Part E: ITS Analysis ####
# Regression
e_its <- function(crime_var, agg_data, y_limits = NULL) {
  # 1. Generates a plot of observed and counterfactuals.
  # 2. Outputs ITS regression and Newey-West standard errors with lag = 3
  
  # Crime_var: "crime_100m", "crime_200m", or "crime_500m"
  # Agg_data: d_assault, d_auto, d_bne, d_robbery, d_t5000
  # mci_type: "Assault", "Auto Theft", "Break and Enter", "Robbery", "Theft over $5000"
  # scs_dist: 100, 200, 500
  
  ## a. Run preliminary regression
  crime.glm <- glm.nb(agg_data[[crime_var]] ~ time + level + trend, data = agg_data)
  
  ## b. Plot observed and counterfactual
  ## Observed data
  agg_data$pred <- predict(crime.glm, newdata = agg_data, se.fit = TRUE)$fit
  agg_data$se <- predict(crime.glm, newdata = agg_data, se.fit = TRUE)$se.fit
  ## Counterfactual data
  agg_data_new <- agg_data %>% dplyr::select(time, level, trend)
  agg_data_new$level <- rep(0); agg_data_new$trend <- rep(0)
  agg_data$cf <- predict(crime.glm, agg_data_new, se.fit = TRUE)$fit
  agg_data$cf.se <- predict(crime.glm, agg_data_new, se.fit = TRUE)$se.fit
  ## Plot
  its_plot <- ggplot(agg_data, aes(time, .data[[crime_var]])) + 
    geom_vline(xintercept = 0, linetype = "dotted", size = 1) + 
    # Counterfactual
    geom_ribbon(aes(ymin = exp(cf - (1.96*cf.se)), ymax = exp(cf + (1.96*cf.se))), fill = "lightskyblue1", alpha = 0.2) +
    geom_line(aes(time, exp(cf)),lty=2, size=1, color="lightskyblue") +
    # Observed
    geom_ribbon(aes(ymin = exp(pred - (1.96*se)), ymax = exp(pred + (1.96*se))), fill = 'lightblue', alpha = 1) +
    geom_line(aes(time, exp(pred)), lty=1, size=1, color="blue") + 
    #ggtitle(paste(mci_type, "cases before and after implementation of SCS")) +
    #xlab("Time since intervention") + ylab(paste("Number of events within", scs_dist, "metres")) +
    # Remove axis labels
    labs(x = NULL, y = NULL) + 
    # Make points black
    geom_point(alpha = 1, color = "blue", shape = 2) +
    theme_minimal()
    # Optional: Customize shapes
    #scale_shape_manual(values = c(16, 2)) +
    # Optional: Customize colours
    #scale_color_manual(values = c("0" = "red", "1" = "blue")) 
  
  # Optional: Set fixed y-axis limits across crime variables if provided
  if (!is.null(y_limits)) {
    its_plot <- its_plot + coord_cartesian(ylim = y_limits)
  }
  print(its_plot)
  
  ## c. Newey-West estimator
  newey.reg <- coeftest(crime.glm, vcov=NeweyWest(crime.glm, lag=3))
  newey.est <- matrix(data = NA, nrow = 4, ncol = 5)
  for (i in 1:nrow(newey.est)) {
    ar.vcov <- NeweyWest(crime.glm, lag=3)
    newey.se <- sqrt(ar.vcov[i, i])
    newey.point <- round(exp(coef(newey.reg)[i]), 2)
    newey.lower <- round(exp(coef(newey.reg)[i] - newey.se*1.96), 2)
    newey.upper <- round(exp(coef(newey.reg)[i] + newey.se*1.96), 2)
    newey.pval <- round(newey.reg[,4][[i]], 4)
    newey.est[i,] <- cbind(newey.point, newey.se, newey.lower, newey.upper, newey.pval)
  }
  
  return(newey.est)
}

# Predictive Changes
e_pred_changes <- function(crime_var, agg_data) {
  # 1. Generates a plot of observed and counterfactuals.
  # 2. Outputs ITS regression and Newey-West standard errors with lag = 3
  
  # Crime_var: "crime_100m", "crime_200m", or "crime_500m"
  # Agg_data: d_assault, d_auto, d_bne, d_robbery, d_t5000
  
  # a. Run preliminary regression
  crime.glm <- glm.nb(agg_data[[crime_var]] ~ time + level + trend, data = agg_data)
  pred5y <- fitted(crime.glm)["114"]
  
  # b. Then estimate the counterfactual at the same time point
  newdata <- agg_data %>% dplyr::select(time, level, trend)
  newdata$level <- rep(0); newdata$trend <- rep(0)
  counterfactual <- predict(crime.glm, type = "response", newdata)
  cfac5y <- counterfactual["114"]
  
  # c. Changes after 5 years
  abs5y <- pred5y - cfac5y # Absolute change
  rel5y <- ((pred5y - cfac5y) / cfac5y)*100 # Relative change (%)
  
  return(list(pred5y = pred5y, cfac5y = cfac5y, abs5y = abs5y, rel5y = rel5y))
}

#### Synthetic Control Analysis ####
d_sca <- function(input_data, interv_time, dep_qu1, dep_qu0, nbr_trt_num, x_nbr, 
                  to_nbr, mci, num_x, num_y, nbr_short) {
  ## Function Dictionary ##
  # interv_time = Number indicating the month when intervention came into effect (e.g., 78)
  # dep_qu1 = Dependent variable (in double quotation marks)
  # dep_qu0 = Dependent variable (no quotation marks)
  # nbr_trt_num = Count of health outcome of interest (no quotation marks)
  # x_nbr = Census data for treated neighbourhoods (x70, x72, x73, x74, x75, x77, x78, 
  # x81, x82, x85, x86)
  # to_nbr = Neighbourhood names ("South Riverdale (70)", "Regent Park (72)", "Moss Park (73)", 
  # "North St.James Town (74)", "Church-Yonge Corridor (75)", "Waterfront Communities-The Island (77)", 
  # "Kensington-Chinatown (78)", "Trinity-Bellwoods (81)", "Niagara (82)", "South Parkdale (85)", 
  # "Roncesvalles (86)")
  # mci = MCI categories ("Assault", "Auto theft", "Break and enter", "Robbery", "Theft > $5000",
  # "Bicycle theft", "Theft from vehicles", "Mental health apprehensions")
  # num_x = nbr number.x ("x70.x", "x72.x", "x73.x", "x74.x", "x75.x", "x77.x", "x78.x", 
  # "x81.x", "x82.x", "x85.x", "x86.x")
  # num_y = nbr number.y ("y70.y", "y72.y", "y73.y", "y74.y", "y75.y", "y77.y", "y78.y", 
  # "y81.y", "y82.y", "y85.y", "y86.y")
  
  # | 1. SC Setup ####
  ## || 1.1. Identify synthetic control for the treated unit ####
  prep.out <- dataprep(foo = input_data, 
                       predictors = c("total_pop", "dens", "female", 
                                      "age00_14","age15_64", "age65plus", 
                                      "avg_age", "hh_size"),
                       predictors.op = "mean", 
                       time.predictors.prior = c(1:interv_time),
                       special.predictors = list(
                         list("median_inc", seq(1, interv_time, 12),"mean"),
                         list("limat", seq(1, interv_time, 12),"mean"),
                         list("one_parent", seq(1, interv_time, 12),"mean"),
                         list("immigrants", seq(1, interv_time, 12),"mean"),
                         list("minority", seq(1, interv_time, 12),"mean"),
                         list("noeduc", seq(1, interv_time, 12),"mean"),
                         list("chomage", seq(1, interv_time, 12),"mean")
                       ),
                       dependent = dep_qu1, 
                       unit.variable = "nbr_num", 
                       unit.names.variable = "nbr_chr",
                       time.variable = "time",
                       # Treated nbr
                       treatment.identifier = nbr_trt_num,
                       # Comparison in donor pool
                       controls.identifier = nbr_ctrl,
                       time.optimize.ssr = c(1:interv_time), 
                       time.plot = c(1:126))
  
  ## || 1.2. Summarize SC analysis results ####
  synth.out <- synth(prep.out, Margin.ipop = 5e-3)
  # print(synth.out$loss.v) # MSPE
  # print(round(sqrt(synth.out$loss.v), 2)) # RMSPE
  
  # | 2. SC Output ####
  synth.tables <- synth.tab(dataprep.res = prep.out, synth.res = synth.out)
  ## || 2.1. Weights ####
  unit.wt <- round(synth.out$solution.w, 2) # SC pooled weight
  pred.wt <- t(round(synth.out$solution.v, 2)) # SC predictor weights
  wt.balance <- round(synth.tables$tab.pred, 2) # Balance table trt vs. SC
  ## || 2.2. Save treatment and synthetic control for subsequent data analysis & figures ####
  nbr.group <- prep.out$Y1plot # Count of specified outcome for the treated unit
  nbr.synth <- round(prep.out$Y0plot %*% synth.out$solution.w,1) # Count of specified outcome for the SC
  
  # | 3. Create demographics summary values for the treated and the SC units ####
  ## || 3.1. Convert data.frames to matrices and remove the column names using demographic tables generated earlier
  demo_sc_m <- as.matrix(demo_sc); demo_sc_m <- unname(demo_sc_m)
  ## || 3.2. Check dimensions of matrix ####
  dim(demo_sc_m); dim(unit.wt)
  ## || 3.3. Multiply demo values by unit.weights to get treated unit SC values for 2016 ####
  nbr.sc <- round(demo_sc_m %*% unit.wt, 1)
  nbr.demo <- demo_wide %>% dplyr::select(variable, all_of(x_nbr))
  
  ## || 3.4. Build summary table of exposed nbr and SC demographics ####
  demo_summary <- cbind(nbr.demo, nbr.sc)
  names(demo_summary)[names(demo_summary) == "w.weight"] <- paste(to_nbr, "sc", sep = ".")
  
  # | 4. Extract synthetic control and create a new data frame to run the ITS models ####
  ## || 4.1. Filter data frame for easier spreading to wide format ####
  data_sub <- input_data %>%
    dplyr::select(nbr_num, year, total_pop, dep_qu0, month, time, group)
  # Rearrange column order for easier spreading to wide format
  data_sub_outcome <- data_sub[c("time", "year", "month", "nbr_num", dep_qu1)] # ED
  data_sub_outcome_wide <- spread(data_sub_outcome, nbr_num, dep_qu0) # ED
  data_sub_pop <- data_sub[c("time", "nbr_num", "total_pop")] # population
  data_sub_pop_wide <- spread(data_sub_pop, nbr_num, total_pop) # population
  # Merge where .x represents outcome and .y represents population
  data_sub_wide <- merge(data_sub_outcome_wide, data_sub_pop_wide, by="time") 
  # Add in 'Effective' to denote the implementation of SCS 0 == time <= intervention; 1 == time > intervention
  data_sub_wide <- data_sub_wide %>% mutate(level = ifelse(time <= interv_time, 0, 1))
  data_sub_wide <- data_sub_wide %>% 
    dplyr::rename_with(~ paste0("x", .), .cols = matches("^\\d+\\.x")) %>% 
    dplyr::rename_with(~ paste0("y", .), .cols = matches("^\\d+\\.y"))
  # Add results from the synthetic control analyses
  nbr.group <- as.data.frame(nbr.group); names(nbr.group)[1] <- "nbr.group"
  nbr.synth <- as.data.frame(nbr.synth); names(nbr.synth)[1] <- "nbr.synth"
  # Merge
  data_outcome_wide <- cbind(data_sub_wide, nbr.group, nbr.synth)
  
  ## || 4.2. Prepare new data frame in the long format (i.e., Treated & Synthetic control appended) ####
  combined <- data_outcome_wide %>% 
    dplyr::select(time, year, month, level, nbr.group, nbr.synth)
  ## Split to gather & linking variable & then merge [NB messy code] - Reshape data frame
  combined <- gather(combined, group, outcome, c(nbr.group, nbr.synth), factor_key=TRUE)
  ## Tidy data frame: 'Group' variable indicates "1" = Treated & "0" = Comparison nbrs
  combined <- combined %>% mutate(group = ifelse(group == "nbr.group", 1, 0))
  
  # | 5. Controlled interrupted time series ####
  ## || 5.1. Run regressions to obtain effect measures ####
  combined$trend <- ifelse(combined$time <= interv_time, 0, combined$time - interv_time)
  combined$group.time <- ifelse(combined$group == 1, combined$time, 0)
  combined$group.level <- ifelse(combined$group == 1 & combined$level == 1, 1, 0)
  combined$group.trend <-ifelse(combined$group.level == 1, combined$time - interv_time, 0)
  combined$new.time <- combined$time - interv_time
  
  ## Adjusted estimates - Include harmonic function
  cits.adj <- glm.nb(outcome ~ new.time + group + group.time 
                     + level + trend + group.level + group.trend, 
                     data = combined)
  
  ## || 5.2. Newey-West Standard Errors ####
  newey.reg <- coeftest(cits.adj, vcov=NeweyWest(cits.adj, lag=3))
  newey.est <- matrix(data = NA, nrow = 8, ncol = 5)
  for (i in 1:8) {
    ar.vcov <- NeweyWest(cits.adj, lag=3)
    newey.se <- sqrt(ar.vcov[i, i])
    newey.point <- round(exp(coef(newey.reg)[i]), 2)
    newey.lower <- round(exp(coef(newey.reg)[i] - newey.se*1.96), 2)
    newey.upper <- round(exp(coef(newey.reg)[i] + newey.se*1.96), 2)
    newey.pval <- round(newey.reg[,4][[i]], 4)
    newey.est[i,] <- cbind(newey.point, newey.se, newey.lower, newey.upper, newey.pval)
  }
  
  # | 6. Plot & save Figure for manuscript ####
  ## Create counterfactual data
  newdata <- combined %>% 
    dplyr::select(new.time, year, month, group, outcome, group.time, group.level, group.trend)
  newdata$level <- rep(0); newdata$trend <- rep(0)
  newdata$group.level <- rep(0); newdata$group.trend <- rep(0)
  ## Subset by Trt vs. SC
  newdata1 <- subset(newdata, group == 1)
  newdata0 <- subset(newdata, group == 0)
  ## Calculate the counterfactual values
  combined$ob <- predict(cits.adj, newdata1, se.fit = TRUE)$fit
  combined$ob.se <- predict(cits.adj, newdata1, se.fit = TRUE)$se.fit
  combined$cf <- predict(cits.adj, newdata0, se.fit = TRUE)$fit
  combined$cf.se <- predict(cits.adj, newdata0, se.fit = TRUE)$se.fit
  
  ## Plot
  Legend <- c("Observed" = "blue", "Counterfactual" = "red")
  sc_plot <- ggplot(combined, aes(new.time, outcome)) + 
    # Intervention line
    geom_vline(xintercept = 0, linetype = "dotted") +
    # Counterfactual
    geom_ribbon(aes(ymin = exp(cf - (1.96*cf.se)), ymax = exp(cf + (1.96*cf.se))), fill = "pink", alpha = 0.5) +
    geom_line(aes(new.time, exp(cf), color="Counterfactual"),lty=2, size=1) +
    # Observed
    geom_ribbon(aes(ymin = exp(ob - (1.96*ob.se)), ymax = exp(ob + (1.96*ob.se))), fill = 'lightblue', alpha = 0.5) +
    geom_line(aes(new.time, exp(ob), color="Observed"), lty=1, size=1) + 
    ggtitle(paste(mci, "count over time in", to_nbr)) +
    xlab("Months before/after intervention") + ylab(paste(mci, "count")) +
    geom_line(aes(new.time, exp(ob)), color='black', lty=1) + 
    geom_point(alpha=0.3) +
    scale_color_manual(values = Legend)
  print(sc_plot)
  
  # | 7. Building multiple baseline dataset and re-running CITS analyses ####
  # Create data points
  ## Treated
  nbr.outcome.obs <- with(data_outcome_wide, nbr.group)
  nbr.outcome.datanew <- data.frame(level=rep(c(0,1),c(interv_time-1, (max(combined$time)-interv_time+1))),
                                    time= 1:max(combined$time), month = c(rep(1:12, 10), rep(1:6,1)),
                                    year = c(rep(2014:2023, each = 12), rep(2024, 6)))
  ## Synthetic control
  nbr.sc.outcome.obs <- with(data_outcome_wide, nbr.synth)
  nbr.sc.outcome.datanew <- data.frame(level=rep(c(0,1),c(interv_time-1, (max(combined$time)-interv_time+1))),
                                       time= 1:max(combined$time), month = c(rep(1:12, 10), rep(1:6,1)),
                                       year = c(rep(2014:2023, each = 12), rep(2024, 6)))
  
  ## || 7.1. Rewrite names of columns removing spaces and dashes ####
  data_sub_pop_wide_nn <- data_sub_pop_wide %>% clean_names()
  data_sub_outcome_wide_nn <- data_sub_outcome_wide %>% clean_names
  
  ## || 7.2. Remove columns of neighbourhoods not in pool for synthetic control ####
  data_sub_pop_pool <- subset(data_sub_pop_wide_nn, 
                              select = -c(time, x70, x72, x73, x74, x75, x77, 
                                          x78, x81, x82, x85, x86))
  data_sub_outcome_wide <- subset(data_sub_outcome_wide_nn, 
                                  select = -c(time, year, month, x70, x72, x73, x74, 
                                              x75, x77, x78, x81, x82, x85, x86))
  
  ## || 7.3. Convert data.frames to matrices and remove the column names ####
  data_sub_pop_pool_m <- as.matrix(data_sub_pop_pool)
  data_sub_pop_pool_m <- unname(data_sub_pop_pool_m)
  data_sc_outcome_m <- as.matrix(data_sub_outcome_wide)
  data_sc_outcome_m <- unname(data_sc_outcome_m)
  unit.wt <- unname(unit.wt)
  
  ## || 7.4. Check dimensions of matrix ####
  dim(data_sub_pop_pool_m); dim(data_sc_outcome_m); dim(unit.wt)
  
  ## || 7.5. Multiply populations of each treated nbr by unit.weights ####
  nbr.sc.pop <- round(data_sub_pop_pool_m %*% unit.wt) # SC population per time interval
  nbr.sc.outcome <- round(data_sc_outcome_m %*% unit.wt) # SC event count per time interval
  
  ## || 7.6. Create table of just treated unit and SC outcome counts and populations ####
  nbr.outcome <- data_sub_wide %>% dplyr::select(num_x)
  nbr.pop <- data_sub_wide %>% dplyr::select(num_y)
  int.time <- nbr.outcome.datanew %>% dplyr::select(time)
  int.time <- int.time - interv_time
  names(int.time)[names(int.time) == "time"] <- "int.time"
  nbr_sc_pair <- cbind(nbr.outcome.datanew, int.time, nbr.outcome, nbr.pop, nbr.sc.outcome, nbr.sc.pop)
  names(nbr_sc_pair)[names(nbr_sc_pair) == "nbr.sc.outcome"] <- paste(nbr_short, "sc.x", sep = ".")
  names(nbr_sc_pair)[names(nbr_sc_pair) == "nbr.sc.pop"] <- paste(nbr_short, "sc.y", sep = ".")
  
  # | 8. Output data ####
  prep.out <<- prep.out
  synth.out <<- synth.out
  synth.tables <<- synth.tables
  unit.wt <<- unit.wt
  pred.wt <<- pred.wt
  wt.balance <<- wt.balance
  demo_sc <<- demo_sc
  demo_sc_m <<- demo_sc_m
  demo_summary <<- demo_summary
  nbr.group <<- nbr.group
  nbr.synth <<- nbr.synth
  nbr.sc <<- nbr.sc
  nbr.demo <<- nbr.demo
  combined <<- combined
  data_sub_outcome <<- data_sub_outcome
  data_sub_outcome_wide <<- data_sub_outcome_wide
  data_sub_pop <<- data_sub_pop
  data_sub_pop_wide <<- data_sub_pop_wide
  data_sub_wide <<- data_sub_wide
  data_outcome_wide <<- data_outcome_wide
  combined <<- combined
  nbr.pop <<- nbr.pop
  nbr.outcome <<- nbr.outcome
  nbr.sc.pop <<- nbr.sc.pop
  nbr.sc.outcome <<- nbr.sc.outcome
  int.time <<- int.time
  nbr_sc_pair <<- nbr_sc_pair
}

#### SC Data Aggregation ####
e_aggregate <- function(agg_df) {
  # 1. Aggregate event counts and population size in each neighbourhood for both trt and SC.
  # 2. Calculate rate per 100,000 population.
  # 3. Create a dataset suitable for CITS.
  ## Trt - Counts & population
  agg_df$trt_count <- agg_df$x70.x + agg_df$x72.x + agg_df$x73.x + agg_df$x74.x + 
    agg_df$x75.x + agg_df$x77.x + agg_df$x78.x + agg_df$x81.x + agg_df$x82.x + 
    agg_df$x85.x + agg_df$x86.x
  agg_df$trt_pop <- agg_df$y70.y + agg_df$y72.y + agg_df$y73.y + agg_df$y74.y + 
    agg_df$y75.y + agg_df$y77.y + agg_df$y78.y + agg_df$y81.y + agg_df$y82.y + 
    agg_df$y85.y + agg_df$y86.y
  agg_df$trt_rate <- (agg_df$trt_count/agg_df$trt_pop)*100000 # Rate per 100,000 population
  ## SC - Counts & population
  agg_df$sc_count <- agg_df$sr.sc.x + agg_df$rp.sc.x + agg_df$mp.sc.x + agg_df$ns.sc.x + 
    agg_df$cy.sc.x + agg_df$wc.sc.x + agg_df$kc.sc.x + agg_df$tb.sc.x + agg_df$ni.sc.x + 
    agg_df$sp.sc.x + agg_df$ro.sc.x
  agg_df$sc_pop <- agg_df$sr.sc.y + agg_df$rp.sc.y + agg_df$mp.sc.y + agg_df$ns.sc.y + 
    agg_df$cy.sc.y + agg_df$wc.sc.y + agg_df$kc.sc.y + agg_df$tb.sc.y + agg_df$ni.sc.y + 
    agg_df$sp.sc.y + agg_df$ro.sc.y
  agg_df$sc_rate <- (agg_df$sc_count/agg_df$sc_pop)*100000 # Rate per 100,000 population
  
  ## Treated
  agg_tr <- agg_df %>% dplyr::select(int.time, trt_count, trt_pop, trt_rate)
  agg_tr$group <- 1
  agg_tr$group.time <- ifelse(agg_tr$group == 1, agg_tr$int.time, 0)
  agg_tr$trend <- ifelse(agg_tr$int.time <= 0, 0, agg_tr$int.time)
  agg_tr$level <- ifelse(agg_tr$int.time <= 0, 0, 1)
  agg_tr$group.level <-ifelse(agg_tr$group == 1 & agg_tr$level == 1, 1, 0)
  agg_tr$group.trend <-ifelse(agg_tr$group.level == 1, agg_tr$int.time, 0)
  names(agg_tr)[names(agg_tr) == "trt_count"] <- "count"
  names(agg_tr)[names(agg_tr) == "trt_pop"] <- "pop"
  names(agg_tr)[names(agg_tr) == "trt_rate"] <- "rate"
  ## SC
  agg_sc <- agg_df %>% dplyr::select(int.time, sc_count, sc_pop, sc_rate)
  agg_sc$group <- 0
  agg_sc$group.time <- ifelse(agg_sc$group == 1, agg_sc$int.time, 0)
  agg_sc$trend <- ifelse(agg_sc$int.time <= 0, 0, agg_sc$int.time)
  agg_sc$level <- ifelse(agg_sc$int.time <= 0, 0, 1)
  agg_sc$group.level <-ifelse(agg_sc$group == 1 & agg_sc$level == 1, 1, 0)
  agg_sc$group.trend <-ifelse(agg_sc$group.level == 1, agg_sc$int.time, 0)
  names(agg_sc)[names(agg_sc) == "sc_count"] <- "count"
  names(agg_sc)[names(agg_sc) == "sc_pop"] <- "pop"
  names(agg_sc)[names(agg_sc) == "sc_rate"] <- "rate"
  
  agg_final <- rbind.fill(agg_tr, agg_sc)
  agg_final <- agg_final[complete.cases(agg_final), ] # Total time (43mo pre, 72mo post)
  return(agg_final)
}

#### SC Regression and Plotting ####
e_cits_sc <- function(agg_data, mci) {
  CITS <- glm(rate ~ int.time + group + group.time + level + trend + group.level + group.trend, 
              data = agg_data, family = quasipoisson(link = "log"))
  newey.reg <- coeftest(CITS, vcov=NeweyWest(CITS, lag=3)) # Set lag = 3
  newey.est <- matrix(data = NA, nrow = 8, ncol = 5)
  for (i in 1:nrow(newey.est)) {
    ar.vcov <- NeweyWest(CITS, lag=3)
    newey.se <- sqrt(ar.vcov[i, i])
    newey.point <- round(exp(coef(newey.reg)[i]), 2)
    newey.lower <- round(exp(coef(newey.reg)[i] - newey.se*1.96), 2)
    newey.upper <- round(exp(coef(newey.reg)[i] + newey.se*1.96), 2)
    newey.pval <- round(newey.reg[,4][[i]], 4)
    newey.est[i,] <- cbind(newey.point, newey.se, newey.lower, newey.upper, newey.pval)
  }
  
  # Plot CITS results
  agg_new <- agg_data %>% 
    dplyr::select(int.time, group, level, trend, rate, 
                  group.time, group.level, group.trend)
  agg_new$group.level <- rep(0); agg_new$group.trend <- rep(0)
  
  ## Subset by Trt vs. SC
  agg_trt <- subset(agg_new, group == 1)
  agg_new1 <- subset(agg_data, group == 1)
  agg_new0 <- subset(agg_data, group == 0)
  ## Calculate the counterfactual values
  agg_data$tr.cf <- predict(CITS, agg_trt, se.fit = TRUE)$fit
  agg_data$tr.cf.se <- predict(CITS, agg_trt, se.fit = TRUE)$se.fit
  agg_data$tr <- predict(CITS, agg_new1, se.fit = TRUE)$fit
  agg_data$tr.se <- predict(CITS, agg_new1, se.fit = TRUE)$se.fit
  agg_data$sc <- predict(CITS, agg_new0, se.fit = TRUE)$fit
  agg_data$sc.se <- predict(CITS, agg_new0, se.fit = TRUE)$se.fit
  
  # ITS plot
  ## Plot
  agg_plot <- ggplot(agg_data, aes(int.time, rate)) + 
    # Intervention line
    geom_vline(xintercept = 0, linetype = "dotted", size = 1) +
    # Synthetic control
    geom_ribbon(aes(ymin = exp(sc - (1.96*sc.se)), ymax = exp(sc + (1.96*sc.se))), fill = "pink", alpha = 0.5) +
    geom_line(aes(int.time, exp(sc)),lty=1, size=1, color="red") +
    # Counterfactual
    geom_ribbon(aes(ymin = exp(tr.cf - (1.96*tr.cf.se)), ymax = exp(tr.cf + (1.96*tr.cf.se))), fill = 'lightskyblue1', alpha = 0.2) +
    geom_line(aes(int.time, exp(tr.cf)), lty=2, size=1, color="lightskyblue") + 
    # Treated
    geom_ribbon(aes(ymin = exp(tr - (1.96*tr.se)), ymax = exp(tr + (1.96*tr.se))), fill = 'lightblue', alpha = 0.5) +
    geom_line(aes(int.time, exp(tr)), lty=1, size=1, color="blue") + 
    # ggtitle(paste(mci ,"rate over time")) +
    # xlab("Months before or after intervention") + 
    # ylab("Rate (per 100,000 population)") +
    geom_line(aes(int.time, exp(tr)), color='black', lty=1) +
    # Remove axis labels
    labs(x = NULL, y = NULL) + 
    # Add points with different shapes based on the treatment variable
    geom_point(aes(shape = factor(group), color = factor(group)), alpha = 1, show.legend = FALSE) + 
    # Optional: Customize shapes
    scale_shape_manual(values = c(16, 2)) +
    # Optional: Customize colours
    scale_color_manual(values = c("0" = "red", "1" = "blue")) +
    theme_minimal()
  print(agg_plot)
  return(newey.est)
}
