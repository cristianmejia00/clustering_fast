# Compute the Rindex, which an index to compare two clustering solutions.


# For Adjusted Rand Index, we can use the mclust package
if (!requireNamespace("mclust", quietly = TRUE)) {
  install.packages("mclust")
}
library(mclust)

table(dataset_oa_01$level0)
table(dataset_oa_01$fukan_c)


?rename

ttt_cl <- merge(dataset_oa_00 %>% select(UT, level0),
                dataset %>% select(UT, level0) %>% dplyr::rename('level0_full' = level0),
                by = 'UT',
                all.x = TRUE,
                all.y = FALSE)

adjusted_rand_index <- adjustedRandIndex(ttt_cl$level0, ttt_cl$level0_full)


print(paste("Adjusted Rand Index:", adjusted_rand_index))

########### Subcluster
ttt_scl <- merge(dataset_oa_01 %>% select(UT, level1),
                dataset %>% select(UT, level1) %>% dplyr::rename('level1_full' = level1),
                by = 'UT',
                all.x = TRUE,
                all.y = FALSE)

adjusted_rand_index <- adjustedRandIndex(ttt_scl$level1, ttt_scl$level1_full)


print(paste("Adjusted Rand Index:", adjusted_rand_index))

