
# This file contains the code for the analyses conducted in this exploratory project.

# -------------------------------------------------------------
# R version 3.4.2 (2017-09-28)
# Platform: x86_64-w64-mingw32/x64 (64-bit)
# Running under: Windows >= 8 x64
# 
# attached base packages:
#   [1] stats     graphics  grDevices utils     datasets  methods  
# [7] base     
# 
# other attached packages:
#   [1] lubridate_1.7.1 magrittr_1.5    dplyr_0.7.4     tidyr_0.7.2    
# [5] ggplot2_2.2.1  
# 
# loaded via a namespace (and not attached):
#   [1] Rcpp_0.12.13     assertthat_0.2.0 R6_2.2.2        
# [4] grid_3.4.2       plyr_1.8.4       gtable_0.2.0    
# [7] scales_0.5.0     stringi_1.1.5    rlang_0.1.4     
# [10] lazyeval_0.2.1   bindrcpp_0.2     tools_3.4.2     
# [13] stringr_1.2.0    glue_1.2.0       purrr_0.2.4     
# [16] munsell_0.4.3    yaml_2.1.14      compiler_3.4.2  
# [19] pkgconfig_2.0.1  colorspace_1.3-2 bindr_0.1       
# [22] tibble_1.3.4  .


# Libraries and Data ------------------------------------------------------

# get required pacakges
packages <- c("ggplot2", "tidyr", "dplyr", "magrittr", "ggthemes", "lubridate")

#load them in one go
lapply(packages, library, character.only = TRUE)

# get data
sleep <- read.csv("Derived/sleep_cleaned.csv", stringsAsFactors = FALSE)

# data structure
str(sleep)


# Data Prep ---------------------------------------------------------------

# convert duration of sleep to a continuous variable, measured in hours to facilitate analysis
sleep <- sleep %>% 
  separate(duration_sleep, c("hours", "unit1", "minutes", "unit2" ), sep = " ", remove = TRUE, convert = TRUE) %>% 
  mutate(minutes = ifelse(is.na(minutes), 00, minutes),
         total_sleep = hours + (minutes / 60)) %>% 
  select(-c(unit1, unit2, hours, minutes))  # ignore the error

# to gain insight into the differences in sleep times across different perceptions of the day
sleep %>% 
  group_by(perception_day_general) %>% 
  summarise(
    mean_sleep_time = mean(total_sleep),
    median_sleep_time = median(total_sleep)
  ) %>% 
  gather(mean_sleep_time:median_sleep_time, key = type, value = sleep_times) %>% 
  ggplot() + aes(perception_day_general, sleep_times) + geom_line(aes(col = type))

# The plot shows that, for best results (perception_day_general > 3), 8 to 8.5 hours (not inclusive) of sleep is needed. 
# That's based on the mean. Looking at the median values, around 8 hours of sleep is needed (Maybe what they say is true). 

# Permutation test --------------------------------------------------------

# function to get difference based on a particular rule
get_diff <- function(x, y, rule) {
  rule(x) - rule(y)
}

# function to compute permuted differences in means
perm_fun <- function(x, n1, n2) {
  n <- n1 + n2 # get total size
  idx_a <- sample(1:n, n1)  # get sample of size n1
  idx_b <- setdiff(1:n, idx_a)  # get sample of size n2
  mean_diff <- get_diff(x[idx_a], x[idx_b], mean)  # get mean difference
  mean_diff
}

# function to combine the entire thing
perms <- function(x, n1, n2, rep = 1000) {
  
  perm_diffs <- vector("numeric", rep)  # numeric vector to store computed differences
  
  for (i in seq(rep)) {
    perm_diffs[[i]] <- perm_fun(x, n1, n2)  # compute mean differences "rep" number of times
  }
  
  perm_diffs  # return computed difference vector
}

# compute permuted differences for all possible combinations ===========================================

# vector of sizes n1
upper <- list(one = 5, two = 5, three = 5, four = 24, five = 24, six = 21) 

# vector of sizes n2
lower <- list(one = 24, two = 21, three = 5, four = 24, five = 5, six = 5)

set.seed(1234)  # set random seed

# compute differences 
perm_differences <- purrr::map2(upper, lower, perms, x = sleep$total_sleep, rep = 10000)

# calculate observed differences =====================================================================

# function to calculate observed differences
calc_obs_diffs <- function(df, comb1, comb2) {
  get_diff(df[["total_sleep"]][df[["perception_day_general"]] == comb1],
           df[["total_sleep"]][df[["perception_day_general"]] == comb2], 
           rule = mean)
}

# vectors to store all possible combinations
combinations_upper <- c(2, 2, 2, 3, 3, 4)
combinations_lower <- c(3, 4, 5, 4, 5, 5)

# computing observed differences
observed_diffs <- purrr::map2_dbl(combinations_upper, combinations_lower,
                                  calc_obs_diffs, df = sleep)

# naming observed differences vector. number1_number2 indicates that the value is the observed difference in sleep times between 
# perception == number1 and perception == number2.
names(observed_diffs) <- c("two_three", "two_four", "two_five", "three_four",
                           "three_five", "four_five")

# Getting the outputs --------------------------------------------------------------------------------------------------------------------

# converting perms_differences to a data.frame
 perm_differences <- as.data.frame(perm_differences)
 
# changing names of perm_differences
names(perm_differences) <- names(observed_diffs)

# function to plot histogram of permuted difference data
get_plots <- function(x, obs_diffs, x_labs) {
  ggplot(as.data.frame(perm_differences)) + aes(x) + 
    geom_histogram(binwidth = 0.1) + geom_vline(xintercept = obs_diffs)  + 
    xlab(x_labs) + ggtitle("Perception of day: Permuted differences", subtitle = x_labs) + theme_economist_white()
}

# vector to store subtitles and x axis names
x_labs <- c("two and three", "two and four", "two and five", "three and four",
            "three and five", "four and five")

# storing the plots in a list for easy access
plt_list <- purrr::pmap(list(as.data.frame(perm_differences), 
                             obs_diffs = observed_diffs, x_labs = x_labs), 
                        get_plots)
