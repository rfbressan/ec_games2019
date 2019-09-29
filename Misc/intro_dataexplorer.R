
# Introdução ao DataExplorer ----------------------------------------------

# Script seguindo a vinheta do pacote DataExplorer


# Data --------------------------------------------------------------------

library(tidyr)
library(dplyr)
library(DataExplorer)
library(nycflights13)

data_list <- list(Airlines = airlines, 
                  Airports = airports, 
                  Flights = flights, 
                  Planes = planes, 
                  Weather = weather)
plot_str(data_list)

final_data <- flights %>% 
  left_join(airlines, by = "carrier") %>% 
  left_join(planes, by = "tailnum", suffix = c("_flights", "_planes")) %>% 
  left_join(airports, by = c("origin" = "faa"), suffix = c("_carrier", "_origin")) %>% 
  left_join(airports, by = c("dest" = "faa"), suffix = c("_origin", "_dest"))

introduce(final_data) %>% pivot_longer(cols = everything(), names_to = "Dados")
plot_intro(final_data)
plot_missing(final_data)
profile_missing(final_data)

final_data <- final_data %>% 
  select(-speed)

plot_bar(final_data %>% select(manufacturer))

final_data <- final_data %>% 
  mutate(manufacturer = manufacturer %>% 
           replace(manufacturer == "AIRBUS INDUSTRIE", "AIRBUS") %>% 
           replace(manufacturer == "CANADAIR LTD", "CANADAIR") %>% 
           replace(manufacturer %in% c("MCDONNELL DOUGLAS AIRCRAFT CO", 
                                       "MCDONNELL DOUGLAS CORPORATION"), 
                   "MCDONNELL DOUGLAS"))

# Feature dst_origin and tzone_origin contains only 1 value, so we should drop them:

final_data <- select(final_data, -c(dst_origin, tzone_origin, year_flights, tz_origin))

plot_bar(final_data, with = "arr_delay")
plot_histogram(final_data)

final_data <- update_columns(final_data, "flight", as.factor)
qq_data <- final_data %>% 
  select(c(arr_delay, air_time, distance, seats))

plot_qq(qq_data, sampled_rows = 1000L)
log_qq_data <- update_columns(qq_data, 2:4, function(x) log(x + 1))
plot_qq(log_qq_data, sampled_rows = 1000L)

qq_data <- final_data %>% 
  select(c(name_origin, arr_delay, air_time, distance, seats))
plot_qq(qq_data, by = "name_origin", sampled_rows = 1000L)

plot_correlation(na.omit(final_data), maxcat = 5L)

pca_df <- na.omit(final_data %>% 
                    select(origin, dep_delay, arr_delay, air_time, 
                           year_planes, seats))

plot_prcomp(pca_df, variance_cap = 0.9, nrow = 2L, ncol = 2L)


# Feature Engineering -----------------------------------------------------

final_df <- set_missing(final_data, list(0L, "unknown"))

final_df <- group_category(data = final_data, feature = "manufacturer", 
                           threshold = 0.2, update = TRUE)
plot_bar(final_df$manufacturer)
final_df <- group_category(data = final_data, feature = "name_carrier", 
                           threshold = 0.2, measure = "distance", update = TRUE)
plot_bar(final_df$name_carrier)

teste <- update_columns(final_data, "month", as.factor)
dummify(teste %>% select(month))

