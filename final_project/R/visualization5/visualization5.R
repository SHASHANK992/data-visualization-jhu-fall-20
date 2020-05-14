py_df <- read.csv('python_melt.csv') # Load data from python
lag_df <- read.csv('lag_data.csv')

subset_countries <- unique(lag_df$Country.Region) # country names

### Fig. 8
### The delta_confirmed() function takes in the number of
### daily confirmed cases from a country and calculates the
### change in number of cases along with the growth rate.
### The change in number of cases is the difference in
### cases between day i and day i-1. The growth rate is the
### change in cases on day i divided by change in cases on
### day i-1. When the growth is NaN, it defaults to 0.
delta_confirmed <- function(df_cases = us_sum$confirmed) {
  N <- length(df_cases); change_matrix <- matrix(0, nrow = N)
  growth_matrix <- matrix(0, nrow = N) # Initialize variables
  
  for (i in 2:N) { # calculate change in cases
    change_matrix[i] <- df_cases[i] - df_cases[i-1]
  }
  
  for (j in 3:(N-1)) { # calculate growth rate of cases
    growth <- (change_matrix[j] / change_matrix[j-1])
    if (is.nan(growth)) {
      growth_matrix[j] <- 0
    } else {
      growth_matrix[j] <- growth
    }
  }
  
  change_df <- data.frame(days = 1:N, # save to df
                          cases = df_cases,
                          delta_cases = change_matrix,
                          growth_cases = growth_matrix)
  return(change_df)
}

country_list <- split(py_df, py_df$Country.Region) # split countries to list

# calculate the delta dataframes for each of the countries
delta_list <- lapply(country_list, function(x) {
  delta_confirmed(df_cases = x$Confirmed.Cases)
})

### The growth_plot() function is to plot the growth rate
### which is a function of the change in number of cases per
### day. By taking deltaN_d / deltaN_(d-1) the growth is
### calculated. This function will plot this growth rate
### against the number of days of cases. Additionally, it
### will draw a horizontal line at 1 to show when it may
### soon hit the inflection point and taper off.
growth_plot <- function(df_growth = outside_china_delta, location = 'Outside-China') {
  growth_capped <- ifelse(df_growth$growth_cases < 5,
                          ifelse(df_growth$growth_cases >= 0, df_growth$growth_cases, 0), 5)
  N <- nrow(df_growth)
  plot(1:N, growth_capped, pch = 19,
       xlab = 'Days of Cases', ylab = 'Growth Rate',
       main = paste(location, ': Growth Rate vs. Days of Cases'))#,
  # sub = '(Growth capped at 5 for perspective purposes.)')
  abline(h = 1); abline(h = 0)
  segments(x0 = 1:N, y0 = 0, x1 = 1:N, y1 = growth_capped)
}

par(mfrow = c(3,3)) # plot the grid of growth factors
lapply(seq_along(delta_list), function(x) {
  growth_plot(df_growth = delta_list[[x]], location = subset_countries[[x]])
  
  inf_growth <- which((delta_list[[x]]$growth_cases == Inf))
  sapply(inf_growth, function(y) {
    abline(v = y, col = 'red')
  })
})
dev.off()

# Below certain individual and group plots are created
# from the previous grid of plots.
### The group_plot function is used to plot an individual
### country from the previous grid of plots.
group_plot <- function(delta_df, country_name) {
  growth_plot(df_growth = delta_df, location = country_name)
  
  inf_growth <- which((delta_df$growth_cases == Inf))
  sapply(inf_growth, function(y) {
    abline(v = y, col = 'red')
  })
}
country_order <- unique(py_df$Country.Region) # order of countries

# China
china_num <- which(country_order == 'China')
group_plot(delta_df = delta_list[[china_num]], country_name = 'China')

# Positive trends
positive_trend <- c('China', 'France', 'Germany', 'Spain')
positive_index <- sapply(positive_trend, function(x) which(country_order == x))
par(mfrow = c(2,2))
for(i in 1:length(positive_index)) {
  group_plot(delta_df = delta_list[[i]], country_name = positive_trend[i])
}

# Negative trends
negative_trend <- country_order[-positive_index]
negative_index <- sapply(negative_trend, function(x) which(country_order == x))
par(mfrow = c(3,4))
for(i in 1:length(negative_index)) {
  group_plot(delta_df = delta_list[[i]], country_name = negative_trend[i])
}