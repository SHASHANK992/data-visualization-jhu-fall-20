### Edit data from Python for sunburst
sunburst <- read.csv('sunburst.csv') # load sunburst data
sunburst_sub <- sunburst[,colnames(sunburst)[-4]] # remove ISO column

# NA the blank columns
sunburst_sub[sunburst_sub$Province.State == '',]['Province.State'] = 'other'

# set NA to country
# Reference: https://stackoverflow.com/questions/2851015/convert-data-frame-columns-from-factors-to-characters
sunburst_sub$Province.State <- as.character(sunburst_sub$Province.State,
                                            stringsAsFactors=FALSE) # remove factors
for (i in 1:nrow(sunburst_sub)) { # set Na's to country names instead
  if (is.na(sunburst_sub[i,1])) {
    sunburst_sub[i,1] = as.character(sunburst_sub[i,2])
  }
}

# Reference: https://stackoverflow.com/questions/14262741/combining-duplicated-rows-in-r-and-adding-new-column-containing-ids-of-duplicate
sunburst_final <- aggregate(sunburst_sub[3], sunburst_sub[-3], sum)
# replace double names in province/country with NaN
for (i in 1:nrow(sunburst_final)) {
  if (sunburst_final[i,1] == sunburst_final[i,2]) {
    sunburst_final[i,1] <- NaN
  }
}
write.csv(sunburst_final, file = 'sunburst_edit.csv', row.names = FALSE)
