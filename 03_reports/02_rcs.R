#####################################################
#####################################################
print("###################### reports/02_rcs.R")

# Load utils
source("zz_utils/zz_auxiliary_functions.R")

# INPUTS
# myDataCorrect

# Computed
myDataCorrect$PY <- as.character(myDataCorrect$PY) %>% as.integer()
id_com <- sort(unique(myDataCorrect$"X_C"))
network_year <- round(mean(myDataCorrect$PY, na.rm = TRUE), digits = 1)


# Create a summary with information of each cluster
# per each cluster a list of data frames is created (one data frame per core patent)
values <- lapply(id_com, function(cluster) {
  cluster_data <- myDataCorrect[myDataCorrect$"X_C" == cluster, ]
  cluster_size <- length(cluster_data$"X_C")
  cluster_year <- round(mean(cluster_data$PY, na.rm = TRUE), digits = 1)
  cluster_code <- names(table(cluster_data$cluster_code))

  sum_cites <- sum(cluster_data$"Z9", na.rm = TRUE)
  ave_cites <- sum_cites / cluster_size

  degrees <- cluster_data$X_E
  dmax <- max(degrees, na.rm = TRUE)
  max_degrees <- cluster_data$"X_N"[which(degrees == dmax)][1] # get the id of the documents with highest degree #Remove "[1]" at the end to obtain multiple core papers per cluster
  rows <- lapply(max_degrees, function(y) {
    hub_year_temp <- cluster_data$"PY"[which(cluster_data$"X_N" == y)]
    hub_year <- if (is.na(hub_year_temp)) {
      cluster_year
    } else {
      hub_year_temp
    }
    hub_title <- cluster_data$"TI"[which(cluster_data$"X_N" == y)]
    hub_type1 <- if (settings$params$dataset_source != "wos") {
      "ARTICLE"
    } else {
      toupper(cluster_data$"DT"[which(cluster_data$"X_N" == y)])
    } # Whether a paper was classified as Review by WOS
    hub_type2 <- if (grepl("overview|review|survey", tolower(hub_title))) {
      "REVIEW"
    } else {
      "ARTICLE"
    } # Whether it is a review by the title
    hub_ID <- y
    row <- data.frame(
      cluster, cluster_code,
      network_year, cluster_year,
      hub_year, hub_title, hub_ID, hub_type1, hub_type2,
      dmax, sum_cites, ave_cites, cluster_size
    )
    return(row)
  })

  valid_fields <- settings$rp$categorical_long_reports %>% unlist()
  valid_fields <- valid_fields[valid_fields %in% colnames(cluster_data)]

  tt <- c()
  for (i in c(1:length(valid_fields))) {
    tt[i] <- paste(names(TopSomething(cluster_data, coll = valid_fields[i])), collapse = "; ") %>% tolower()
  }
  names(tt) <- valid_fields
  tops <- data.frame(as.list(tt))

  return(cbind(rows, tops))
})

# convert lists of data frames into single data frame
rcs <- rbindlist(values) %>% as.data.frame()

# Calculate participation
# Participation is the proportion of papers within the cluster connected to the hub
rcs$participation <- round(rcs$dmax / rcs$cluster_size, 3)


# Calculate RCS Labels
rcs$X <- rcs$cluster_year - rcs$network_year
rcs$Y <- rcs$hub_year - rcs$cluster_year
rcs$X[is.na(rcs$X)] <- 0
rcs$Y[is.na(rcs$Y)] <- 0

# Add labels
labels <- sapply(1:nrow(rcs), function(z) {
  x <- rcs$X[z]
  y <- rcs$Y[z]
  name <- if (x > 0 & y > 0) {
    "Change Maker"
  } else if (x > 0 & y < 0) {
    "Incremental"
  } else if (x < 0 & y > 0) {
    "Breaktrough"
  } else if (x < 0 & y < 0) {
    "Matured"
  }
  # else {"OTHER"}})
  # The appearance of the following may imply error in calculations
  else if (x == 0 & y > 0) {
    "B & CM"
  } else if (x == 0 & y < 0) {
    "M & I"
  } else if (x > 0 & y == 0) {
    "CM & I"
  } else if (x < 0 & y == 0) {
    "B & M"
  } else {
    "CENTERED"
  }
})
rcs$label <- labels


## Needs the PY prop table computed in cluster reports
# Find growing clusters
growth_finder <- function(a_prop_matrix, a_range) {
  a_mat <- a_prop_matrix[, c((ncol(a_prop_matrix) - a_range + 1):ncol(a_prop_matrix))]
  b_mat <- a_prop_matrix[, c((ncol(a_prop_matrix) - a_range):(ncol(a_prop_matrix) - 1))]
  c_mat <- a_mat - b_mat
  scores <- rowSums(c_mat) / (a_range - 1)
  return(scores)
}

# By best speed use proportions, by best amount of publication use frequencies
# By comparing the growing rate of the last 4 years "growth_rate" = growth_rate
cluster_year_proportions <- read.csv(file.path(output_folder_level, "report_PY_proportions.csv"))
if ((max(myDataCorrect$PY, na.rm = TRUE) - min(myDataCorrect$PY, na.rm = TRUE)) > 4) {
  rcs$growth_rate <- growth_finder(cluster_year_proportions, 4)
} else {
  rcs$growth_rate <- 0
}


# Write RCS
write.csv(rcs, file = rn$PROJECTrcs, row.names = FALSE)

# cleaning up
rm("id_com", "network_year", "values", "labels", "cluster_year_proportions")
