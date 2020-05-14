py_df <- read.csv('python_melt.csv') # Load data from python
lag_df <- read.csv('lag_data.csv')

### Fig. 5
### Plot log cases with linear regression fit
subset_countries <- unique(lag_df$Country.Region) # country names

### The plog_log_cases function will plot the log number of cases
### against the days of cases. It will fit a line using linear regression
### and the plot will include Adj. R-squared, slope, and intercept.
plot_log_cases <- function(log_df = outside_china_log,
                           location_name = 'Outside China') {
  # Fit model
  # Reference: https://www.r-bloggers.com/that-damn-r-squared/
  model_log_df <- lm(formula = log_confirmed~days, data = log_df)
  adj_r_squared <- summary(model_log_df)$r.squared # adj. R-squared
  intercept <- summary(model_log_df)$coefficients[1]
  slope <- summary(model_log_df)$coefficients[2]
  date_count <- 1:nrow(log_df)
  
  # Plot data
  plot(date_count, log_df$log_confirmed, # plot log of cases
       type = 'l', ylab = 'log confirmed cases', xlab = 'Lag Days',
       main = paste(location_name, 'Log Cases vs. Lag Days'),
       sub = paste('Adj. R-squared:', round(adj_r_squared, 4)))
  abline(model_log_df, col = 'blue', lty = 2)
  return(c(slope, intercept))
}

### The log_plot_grid() function is a helper function to
### make the data loaded from python suitable for the
### variables in the plot_log_cases function. It also
### makes sure that the log cases with -Inf are changed
### to 0.
log_plot_grid <- function(df = lag_df, country_name) {
  # Reference: https://stackoverflow.com/questions/7531868/how-to-rename-a-single-column-in-a-data-frame
  names(df)[names(df) == 'Log.Confirmed.Cases'] <- 'log_confirmed'
  names(df)[names(df) == 'Lag.Days'] <- 'days'
  df$log_confirmed <- ifelse(df$log_confirmed < 0, 0, df$log_confirmed)
  plot_log_cases(log_df = df, location_name = country_name)
}

par(mfrow = c(3,5)) # plot grid of log cases
for (i in subset_countries) {
  ith_country <- lag_df[lag_df$Country.Region == i,]
  log_plot_grid(df = ith_country, country_name = i)  
}
dev.off()

### Fig. 6
par(mfrow = c(1,2))
china_lag <- lag_df[lag_df$Country.Region == 'China',]
log_plot_grid(df = china_lag, country_name = 'China')

# Use the code from plot_log_cases to plot a custom
# plot for China
china <- py_df[py_df$Country.Region == 'China',]
names(china)[names(china) == 'Log.Confirmed.Cases'] <- 'log_confirmed'
names(china)[names(china) == 'Days'] <- 'days'
location_name <- 'China'

log_df <- china[1:20,] # Subset first 20 days of cases
model_log_df <- lm(formula = log_confirmed~days, data = log_df)
adj_r_squared <- summary(model_log_df)$r.squared # adj. R-squared
intercept <- summary(model_log_df)$coefficients[1]
slope <- summary(model_log_df)$coefficients[2]
date_count <- 1:nrow(log_df)

# Plot data
plot(date_count, log_df$log_confirmed, # plot log of cases
     type = 'l', ylab = 'log confirmed cases', xlab = 'Days of Cases',
     main = paste(location_name, 'Log Cases vs. Days of Cases'),
     sub = paste('Adj. R-squared:', round(adj_r_squared, 4)))
abline(model_log_df, col = 'blue', lty = 2)
