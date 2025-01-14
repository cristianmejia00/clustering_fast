###################################################################
# Data Scope
if (scope == "all") {
    myDataCorrect <- dataset
}
if (scope == "cl99") {
    myDataCorrect <- subset(dataset, dataset$cl99 == FALSE)
}
if (scope == "cl_99") {
    myDataCorrect <- subset(dataset, dataset$cl_99 == FALSE)
}

# Remove small clusters
min_selector <- table(myDataCorrect$subcluster_label3)
min_selector <- names(min_selector)[min_selector >= settings$cno$size_lower_limit]
# myDataCorrect <- myDataCorrect[myDataCorrect$subcluster_label3 %in% min_selector]


# Add simple ID
n_cluster_listed <- c(1:length(unique(myDataCorrect$subcluster_label3)))
names(n_cluster_listed) <- myDataCorrect$subcluster_label3[order(
    myDataCorrect$level0,
    myDataCorrect$level1,
    myDataCorrect$level2,
    myDataCorrect$level3
)] %>% .[!duplicated(.)]
myDataCorrect$X_C <- n_cluster_listed[myDataCorrect$subcluster_label3]
myDataCorrect$cluster_code <- myDataCorrect$subcluster_label3

# # verify ID is working
# table(myDataCorrect$subcluster_label3)[1:20]
# table(myDataCorrect$cluster_ID)[1:20]
