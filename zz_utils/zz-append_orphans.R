# add orphans as cluster 99 for level 0

# Add X_N based on the row index after "dataset".
if (any(orphans$X_N %in% dataset$X_N)) {
  tmp <- max(dataset$X_N, na.rm = TRUE) + 1
  orphans$X_N <- c(tmp:(tmp + nrow(orphans)))
}

# Add relevant columns to the orphans dataset

colls <- setdiff(colnames(dataset_minimal), colnames(orphans))
#c("level0", "level1", "subcluster_label1", "level2", "subcluster_label2", "level3", "subcluster_label3", "cl99", "cl_99", "X_C")
for (i in colls) {
  orphans[i] <- 0
}

# Inherit from the correspondent value
if (settings$addons$include_orphans == "99") {
  print("...appending orphans as cluster 99")
  idx <- max(which(dataset_minimal$level0 == 99), na.rm = TRUE)
  orphans[, colls] <- dataset[idx, colls]
}
if (settings$addons$include_orphans == "999") {
  print("...appending orphans as cluster 999")
  orphans$X_C <- 999
  orphans[, colls] <- c(999)#, 0, "999-0", 0, "999-0-0", 0, "999-0-0-0", TRUE, FALSE)
}

# Add the remaining values
orphans$X_D <- orphans$X_E <- 1

# Append orphans to dataset
dataset_minimal <- rbind.fill(dataset_minimal, orphans[, c("X_N", "X_C", "UT", colls)])
orphans$level0 <- NULL
dataset <- rbind.fill(dataset, orphans)
