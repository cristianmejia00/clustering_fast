# Fitting a bass model
print("###################### reports/11_bass_model.R")
# This need a file having columns representing years and rows representing observations (i.e. clusters)
# The values of the matrix correspond to frequencies (i.e. integers)

###########################################
# Libraries:
library(netdiffuseR)
library(dplyr)

###########################################
# File and parameters:
yearly_trend <- cluster_year_frequencies

# Parameters for the bass model
# The number of years to consider to compute the bass model: 
years_in_study <- 5
# Wheater the most recent year contains complete data
last_year_is_complete <- FALSE

###########################################
total_years_found <- if (!last_year_is_complete) {ncol(yearly_trend) - 1} else {ncol(yearly_trend)} # We remove 1 year beacuse data is not completed there. For example: Data upto 2018 is completed but 2019 is not completed because today is march 2019
yearly_trend_b <- yearly_trend[,c((total_years_found - years_in_study + 1) : total_years_found)]
yearly_trend_b <- yearly_trend_b + 1 #To avoid zeros

total_count <- sum(yearly_trend_b)

bass_model <- lapply(c(1:nrow(yearly_trend_b)), function(x){
  test <- yearly_trend_b[x,] %>% unname %>% as.numeric %>% cumsum()
  test <- test / test[years_in_study]
  print(x)
  print(test)
  ans <- fitbass(test)
  return(as.data.frame(t(data.frame(coef(ans)))))
}) %>% bind_rows()

bass_model$cluster <- c(1:nrow(yearly_trend_b))
bass_model$cluster_code <- rcs[order(rcs$cluster),c(2)]

write.csv(bass_model, file = "bass_model.csv", row.names = FALSE)