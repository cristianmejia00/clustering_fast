###################################################################
# Data Scope
myDataCorrect <- dataset
# if (scope == "all") {myDataCorrect <- dataset}
# if (scope == "cl99") {myDataCorrect <- subset(dataset, dataset$cl99 == FALSE)}
# if (scope == "cl_99") {myDataCorrect <- subset(dataset, dataset$cl_99 == FALSE)}

# Remove small clusters
if (!("level0" %in% colnames(myDataCorrect))) {
  myDataCorrect$level0 <- myDataCorrect$X_C
}

# min_selector <- table(myDataCorrect$level0)
# min_selector <- names(min_selector)[min_selector >= settings$cno$size_lower_limit]
# # myDataCorrect <- myDataCorrect[myDataCorrect$level0 %in% min_selector]


# Add simple ID
n_cluster_listed <- c(1:length(unique(myDataCorrect$level0)))
names(n_cluster_listed) <- names(table(myDataCorrect$level0))
myDataCorrect$X_C <- n_cluster_listed[as.character(myDataCorrect$level0)]
if ('X_C_label' %in% colnames(myDataCorrect)) {
  myDataCorrect$cluster_code <- myDataCorrect$X_C_label
} else {
  myDataCorrect$cluster_code <- myDataCorrect$level0 %>% as.character()
}


# Cleaning up
rm('n_cluster_listed')