###################################################################
# Data Scope
myDataCorrect <- dataset
# if (scope == "all") {myDataCorrect <- dataset}
# if (scope == "cl99") {myDataCorrect <- subset(dataset, dataset$cl99 == FALSE)}
# if (scope == "cl_99") {myDataCorrect <- subset(dataset, dataset$cl_99 == FALSE)}

# Remove small clusters
min_selector <- table(myDataCorrect$subcluster_label1)
min_selector <- names(min_selector)[min_selector >= settings$cno$size_lower_limit]
# myDataCorrect <- myDataCorrect[myDataCorrect$subcluster_label1 %in% min_selector]


# Add simple ID
n_cluster_listed <- c(1:length(unique(myDataCorrect$subcluster_label1)))
names(n_cluster_listed) <- myDataCorrect$subcluster_label1[order(
    myDataCorrect$level0,
    myDataCorrect$level1
)] %>% .[!duplicated(.)]
myDataCorrect$X_C <- n_cluster_listed[myDataCorrect$subcluster_label1]
cluster_ID_label1_backup <- myDataCorrect$X_C
myDataCorrect$cluster_code <- myDataCorrect$subcluster_label1

# # verify ID is working
# table(myDataCorrect$subcluster_label3)[1:20]
# table(myDataCorrect$cluster_ID)[1:20]
