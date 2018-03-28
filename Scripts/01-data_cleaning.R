
# Introduction ------------------------------------------------------------

# File to load and run data


# Load libraries and data -------------------------------------------------

library(dplyr)  # load dplyr

library(tidyr) # load tidyr

library(lubridate) # load lubridate package

# load data
sleep <- read.csv("Data/my_sleep_data_raw1.csv", stringsAsFactors = FALSE,
                  na.strings = "-")


# Data Summary ------------------------------------------------------

# structure of the data
str(sleep) # check column data for definitions of variables

# names are too long
names(sleep) <- c("sl.no", "date_expected_sleep", "day_expected_sleep", 
                  "time_expected_sleep", "date_actual_sleep", "day_actual_sleep", 
                  "time_actual_sleep", "date_wake_up", "day_wake_up", 
                  "time_expected_wake_up", "time_actual_wake_up", "duration_sleep",
                  "feeling_15_min_wake_up", "perception_work_day", 
                  "perception_day_general")

# function to check missing values
miss_calc <- function(x) {
  
  stopifnot(!missing(x), (is.data.frame(x)|is.matrix(x)))
  
  miss_df <- data.frame(variables = names(x),
                        missing_vals = vector("numeric", ncol(x)))
  
  miss_df$missing_vals <- vapply(x, function(x) sum(is.na(x)), 
                                 FUN.VALUE = c(miss = 0))
  
  miss_df
}

# output of miss_clac
miss_calc(sleep)

# Feeling within 15 minutes of sleeping has 33 missing values. 
# That's around 72%, so, I'll delete that entire column.
sleep <- sleep %>% select(-starts_with("feeling"))

# Deleting missing values from the dependent variable. Imputing these 
# would introduce biases that I don't want.
sleep <- sleep[-(1:4), ]

# removing id variable
sleep <- sleep %>% select(-sl.no)

# correcting date entry errors
sleep[44, 4] <-  "06-03-2018"
sleep[52, 7] <- "15-03-2018"
sleep[53, 7] <- "16-03-2018"

# parsing the dates properly
sleep <- sleep %>%
  mutate(date_expected_sleep = dmy(date_expected_sleep),
         date_actual_sleep = dmy(date_actual_sleep),
         date_wake_up = dmy(date_wake_up))

# removing remaining missing value
sleep <- sleep[-which(is.na(sleep$perception_day_general), arr.ind = T), ]


# drop perception_work_day because of change in measurement unit
sleep <- sleep %>% select(-perception_work_day)

# write data to disk
# write.csv(sleep, "Derived/sleep_cleaned.csv")