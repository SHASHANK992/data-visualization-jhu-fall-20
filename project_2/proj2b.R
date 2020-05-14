library(ggplot2); library(ggmap); library(gridExtra); library(writexl); library(ks)
# Source: https://www.kaggle.com/camnugent/california-housing-prices#housing.csv
housing <- read.csv(file = 'housing.csv')

# check NA's
sum(is.na(housing)) # 207
# Reference: https://stackoverflow.com/questions/4862178/remove-rows-with-all-or-some-nas-missing-values-in-data-frame
housing <- housing[complete.cases(housing),]
rownames(housing) <- NULL

# Reference: http://blog.chapagain.com.np/r-calculate-mean-median-mode-variance-standard-deviation/
# Reference: https://community.rstudio.com/t/export-rstudio-data-to-excel/7579/4
descriptive_statistics <- function(x, file_name) {
  iqr_data <- summary(x)
  std_dev <- sd(x)
  unique_x <- unique(x)
  mode <- unique_x[which.max(tabulate(match(x, unique_x)))]
  desc_stat <- as.data.frame(matrix(c(iqr_data, std_dev, mode), ncol = 8))
  colnames(desc_stat) <- c("Minimum", "1st Quantile", "Median", "Mean",
                           "3rd Quantile", "Max", "Std. Deviation", "Mode")
  write_xlsx(x = desc_stat, path = paste(file_name, '.xlsx'), col_names = TRUE)
}

# colnames(housing)
# longitude - A measure of how far west a house is; a higher value is farther west
# quantitative, interval
longitude <- housing[,1]
par(mfrow = c(1,2))
hist(longitude, main = 'Histogram of Longitude', xlab = 'Longitude')
boxplot(longitude, main = 'Boxplot of Longitude', ylab = 'Longitude')
median(longitude)
descriptive_statistics(x = longitude, file_name = 'longitude')

# latitude - A measure of how far north a house is; a higher value is farther north
# quantitative, interval
latitude <- housing[,2]
hist(latitude, main = 'Histogram of Latitude', xlab = 'Latitude')
boxplot(latitude, main = 'Boxplot of Latitude', ylab = 'Latitude')
median(latitude)
descriptive_statistics(x = latitude, file_name = 'latitude')

# housing_median_age - Median age of a house within a block; a lower number is a newer building
# quantitative, ratio
housing_median_age <- housing[,3]
hist(housing_median_age, main = 'Histogram of Median Age of Homes', xlab = 'Median Age')
boxplot(housing_median_age, main = 'Boxplot of Median Age of Homes', ylab = 'Median Age')
min(housing_median_age)
max(housing_median_age)
descriptive_statistics(x = housing_median_age, file_name = 'housing_median_age')

# total_rooms - Total number of rooms within a block
# quantitative, ratio
total_rooms <- housing[,4]
hist(total_rooms, main = 'Histogram of Total Rooms', xlab = 'Total Rooms')
boxplot(total_rooms, main = 'Boxplot of Total Rooms', ylab = 'Total Rooms')
min(total_rooms); max(total_rooms); median(total_rooms)
descriptive_statistics(x = total_rooms, file_name = 'total_rooms')

# total_bedrooms - Total number of bedrooms within a block
# quantitative, ratio
total_bedrooms <- housing[,5]
hist(total_bedrooms, main = 'Histogram of Total Bedrooms', xlab = 'Total Bedrooms')
boxplot(total_bedrooms, main = 'Boxplot of Total Bedrooms', ylab = 'Total Bedrooms')
min(total_bedrooms); max(total_bedrooms); median(total_bedrooms)
descriptive_statistics(x = total_bedrooms, file_name = 'total_bedrooms')

# population - Total number of people residing within a block
# quantitative, ratio
population <- housing[,6]
hist(population, main = 'Histogram of Total Residents', xlab = 'Total Residents')
boxplot(population, main = 'Boxplot of Total Residents', ylab = 'Total Residents')
min(population); max(population); median(population)
descriptive_statistics(x = population, file_name = 'population')

# households - Total number of households, a group of people
# residing within a home unit, for a block
# quantitative, ratio
households <- housing[,7]
hist(households, main = 'Histogram of Total Households', xlab = 'Total Households')
boxplot(households, main = 'Boxplot of Total Households', ylab = 'Total Households')
min(households); max(households); median(households)
descriptive_statistics(x = households, file_name = 'households')

# median_income - Median income for households within a block
# of houses (measured in tens of thousands of US Dollars)
# quantitative, ratio
median_income <- housing[,8]
hist(median_income, main = 'Histogram of Median Income', xlab = 'Median Income')
boxplot(median_income, main = 'Boxplot of Median Income', ylab = 'Median Income')
min(median_income); max(median_income); median(median_income)
descriptive_statistics(x = median_income, file_name = 'median_income')

# median_house_value - Median house value for households within
# a block (measured in US Dollars)
# quantitative, ratio
median_house_value <- housing[,9]
hist(median_house_value, main = 'Histogram of Median House Value', xlab = 'Median House Value')
boxplot(median_house_value, main = 'Boxplot of Median House Value', ylab = 'Median House Value')
sum(median_house_value == max(median_house_value))
descriptive_statistics(x = median_house_value, file_name = 'median_house_value')

# ocean_proximity - Location of the house w.r.t ocean/sea
# qualitative, nominal
ocean_proximity <- housing[,10]
# Reference: https://stackoverflow.com/questions/21639392/make-frequency-histogram-for-factor-variables
# barplot(prop.table(table(ocean_proximity)), main = 'Percentage Histogram of Ocean Proxmity')
barplot(prop.table(table(ocean_proximity))[order(prop.table(table(ocean_proximity)))], main = 'Percentage Histogram of Ocean Proxmity')
descriptive_statistics(x = prop.table(table(ocean_proximity)), file_name = 'ocean_proximity')

### Q1
# Refrence: https://bookdown.org/egarpor/NP-UC3M/kde-ii-mult.html
density_estimate <- kde(x = cbind(longitude, latitude))
plot(density_estimate, display = 'persp', ticktype = 'detailed', phi = 25, theta = 10)

# Reference: https://www.littlemissdata.com/blog/maps
# map API: ************************************
ggmap::register_google(key = "************************************")
ca_map <- ggmap(get_googlemap(center = c(lon = -119.4179, lat = 36.7783),
                              zoom = 6, scale = 2,
                              maptype ='terrain',
                              color = 'color'))
ca_points <- ca_map + geom_point(mapping = aes(x = longitude, y = latitude),
                                 data = housing, alpha = 0.3, size = 0.5)

# heatmap
# Reference: https://stackoverflow.com/questions/32148564/heatmap-plot-by-value-using-ggmap
ca_heatmap <- ca_map + stat_density2d(
  mapping = aes(x = longitude, y = latitude, fill = ..level.., alpha = ..level..),
  data = housing, geom = 'polygon', size = 0.01, bins = 100) +
  scale_fill_gradient(low = 'red', high = 'green') +
  scale_alpha(range = c(0, 0.3), guide = FALSE)

# Reference: https://stackoverflow.com/questions/32946539/maps-printing-one-by-one-and-not-in-one-row-using-ggmap-loop-mfrow
grid.arrange(ca_points, ca_heatmap, ncol = 2)

### Q2
indices_958 <- which(housing[,"median_house_value"] == max(median_house_value))
med_val_958 <- housing[indices_958,]

ca_958 <- ca_map + geom_point(mapping = aes(x = longitude, y = latitude),
                                 data = med_val_958, alpha = 1, size = 0.5)
ca_958_heatmap <- ca_map + stat_density2d(
  mapping = aes(x = longitude, y = latitude, fill = ..level.., alpha = ..level..),
  data = med_val_958, geom = 'polygon', size = 0.01, bins = 100) +
  scale_fill_gradient(low = 'red', high = 'green') +
  scale_alpha(range = c(0, 0.3), guide = FALSE)

sf_map <- ggmap(get_googlemap(center = c(lon = -122.2913, lat = 37.8272),
                              zoom = 9, scale = 2,
                              maptype ='terrain',
                              color = 'color'))
sf_map2 <- sf_map + geom_point(mapping = aes(x = longitude, y = latitude),
                    data = med_val_958, alpha = 1, size = 0.7, color = 'red')

la_map <- ggmap(get_googlemap(center = c(lon = -118.2437, lat = 34.0522),
                              zoom = 9, scale = 2,
                              maptype ='terrain',
                              color = 'color'))
la_map2 <- la_map + geom_point(mapping = aes(x = longitude, y = latitude),
                    data = med_val_958, alpha = 1, size = 0.7, color = 'red')

grid.arrange(sf_map2, la_map2, ncol = 2)

par(mfrow = c(2,2))
plot(kde(med_val_958$total_rooms), display = 'persp')
plot(kde(med_val_958$median_income), display = 'persp')
plot(kde(med_val_958$housing_median_age), display = 'persp')
barplot(prop.table(table(med_val_958$ocean_proximity))[order(prop.table(table(med_val_958$ocean_proximity)))], main = 'Percentage Histogram of Ocean Proxmity')

### Q3
hist(median_income)
low_to_high_inc <- housing[order(housing$median_income),]
bottom_18 <- round(nrow(housing) * 0.18)
top_5 <- round(nrow(housing) * 0.05)
lower_class <- low_to_high_inc[1:bottom_18,]
lower_class$class <- 0
upper_class <- low_to_high_inc[(nrow(housing) - 1 - top_5):nrow(housing),]
upper_class$class <- 2
middle_class <- low_to_high_inc[(bottom_18 + 1):(nrow(housing) - 2 - top_5),]
middle_class$class <- 1
housing_class <- rbind(lower_class, middle_class, upper_class)
write_xlsx(x = housing_class, path = 'housing_class.xlsx', col_names = TRUE)

ca_map + geom_point(mapping = aes(x = longitude, y = latitude),
                    data = lower_class, alpha = 0.5, size = 0.2, color = 'red') +
  geom_point(mapping = aes(x = longitude, y = latitude),
                    data = upper_class, alpha = 0.5, size = 0.2, color = 'blue')

par(mfrow = c(1,2))
barplot(prop.table(table(lower_class$ocean_proximity))[order(prop.table(table(lower_class$ocean_proximity)))])
barplot(prop.table(table(upper_class$ocean_proximity))[order(prop.table(table(upper_class$ocean_proximity)))])
prop.table(table(lower_class$ocean_proximity))
prop.table(table(upper_class$ocean_proximity))

par(mfrow = c(2,2))
hist(lower_class$population, main = 'Histogram of Lower Class Population', xlab = 'Residents within a Block')
boxplot(lower_class$population, main = 'Boxplot of Lower Class Population')
hist(upper_class$population, main = 'Histogram of Upper Class Population', xlab = 'Residents within a Block')
boxplot(upper_class$population, main = 'Boxplot of Upper Class Population')
median(lower_class$population)
median(upper_class$population)

