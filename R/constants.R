# package constants

# storage capacity variable used
# "MAXIMUM_STORAGE_CAPACITY" or "REPRESENTATIVE_STORAGE_CAPACITY"
capacity_variable <- "MAXIMUM_STORAGE_CAPACITY"

# cutoff_year; defines earliest year of data to use...
#... in target and release rule inferrence
cutoff_year <- 1995

# number of points per week for fitting storage curve harmonic
n_points <- 3

# minimum allowable days of storage data (three years ~ 1095 days)
min_allowable_days_of_storage <- 1095

# minimum allowable data points to use release and inflow without any back-calculating
min_r_i_datapoints <- 260 # 5 years

# tolerance for r-squared value of release residual model.
# Models with lower r-squared value than r_sq_tol are discarded.
r_sq_tol <- 0.2

# release constraint quantile
r_st_min_quantile <- 0.005
r_st_max_quantile <- 0.995


# unit conversion
m3_to_Mm3 <- 1e-6
seconds_per_day <- 86400
weeks_per_year <- 365.25 / 7


HUC_replacements <-
  tibble::tribble(
    ~STATE, ~HUC4_replacement,
    "Indiana", "05XX",
    "Michigan", "07XX",
    "Minnesota", "09XX",
    "New York", "02XX",
    "Vermont", "02XX",
    "Ohio", "05XX",
    "Wisconsin", "07XX"
    )

