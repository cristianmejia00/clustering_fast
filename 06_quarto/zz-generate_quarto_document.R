print("###################### zz-generate_quarto_document.R")

# 20230717

# Create a Quarto publishable document

# Libraries
library(glue)
library(quarto)
# require(devtools)
# install_version("knitr", version = "1.42", repos = "http://cran.us.r-project.org")

# Inputs
# analysis_metadata <- analysis_metadata
# params <- params

bibliometrics_folder <- settings$analysis_metadata$bibliometrics_folder
project_folder <- settings$analysis_metadata$project_folder
analysis_folder <- settings$analysis_metadata$analysis_folder
level_folder <- "level1"

dataset <- dataset
dataset_bibliography <- dataset_bibliography
if(settings$params$type_of_analysis == 'topic_model') {
  dataset_bibliography$X_E <- round(dataset_bibliography$new_score,3)
}
  
# # Initialization
# adjust types
rcs_merged$cluster_code <- as.character(rcs_merged$cluster_code)
dataset$X_C <- as.character(dataset$X_C)

# main_path
main_path <- file.path(
  bibliometrics_folder,
  project_folder,
  analysis_folder,
  level_folder
)

# Utils
glue_code <- function(text) {
  glue(text, .open = "<<", .close = ">>", .literal = FALSE, .comment = "##")
}

###################################
###################################
qt <- list()

###################################
###################################
# Article metadata
###################################
document_title <- settings$analysis_metadata$project_name %>% toTitleCase()

qt$yml <- glue('---
title: "{document_title}"
author: Cristian Mejia
date: {Sys.Date()}
bibliography: index_files/bibliography.bib
format:
  html:
    toc: true
    toc-title: Contents
    toc-depth: 4
editor: visual
---')

###################################
###################################
# Article initialization
###################################
qt$load_data <- glue_code('
```{r, message=FALSE}
#| echo: false

library(dplyr)
library(data.table)
library(DT)

load("settings.rdata")

bibliometrics_folder <- settings$analysis_metadata$bibliometrics_folder
project_folder <- settings$analysis_metadata$project_folder
analysis_folder <- settings$analysis_metadata$analysis_folder

main_path <- file.path(bibliometrics_folder,
                       project_folder,
                       analysis_folder,
                       "<<level_folder>>")

rcs_merged <- read.csv(file.path(main_path, "rcs_merged.csv"))
# if(nrow(rcs_merged) > 0) {
#   stop(is.na(rcs_merged$cluster_name))
# }
```
')
qt$load_data

###################################
###################################
# Data
###################################
if (grepl("academic-landscape.com", settings$analysis_metadata$fukan_url)) {
  fukan_url <- glue("[Link]({settings$analysis_metadata$fukan_url})")
} else {
  fukan_url <- "Not apply."
}
qt$data <- glue("
| Query           | {settings$analysis_metadata$query}         |
|-----------------|-----------------------------------|
| Database        | {toupper(settings$params$dataset_source)}  |
| Documents       | {settings$analysis_metadata$downloaded_documents}|
| Date retrieved  | {settings$analysis_metadata$date}          |
| Fukan Analysis  | {fukan_url}                       |
| ID              | {settings$analysis_metadata$query_id}  |
: Metadata
")

###################################
###################################
# Methods
###################################
methods <- ''
for (i in settings$rp$methods) {
  methods <- glue('{methods}
                  - {i}  
                  ')
}
qt$methods <- methods

###################################
###################################
# Results
###################################

# INTRO
displayable_charts <- intersect(settings$rp$categorical_long_reports, colnames(dataset))
rm(chart)
dataset_barcharts <- ''
dataset_trends <- ''
for (chart in displayable_charts) {
  dataset_barcharts <- glue_code('<<dataset_barcharts>>
  
                                ![](index_files/charts/fig_<<chart>>.svg)
                                ')
  this_chart_label <- settings$rp$column_labels[chart]
  dataset_trends <- glue_code('<<dataset_trends>>
  
                              ## <<this_chart_label>>

                              ![](index_files/charts/fig_yearly_trends_<<chart>>.svg)
                              ')
}


## Overview
qt$dataset_overview <- glue_code('  

::::: panel-tabset

## Years

![Documents per year](index_files/charts/fig_yearly_trends.svg){#fig-docs-per-year}


## Stats
  

:::: {#fig-panel-dataset-bars layout-ncol=3}

<<dataset_barcharts>>

Dataset stats.
::::  

## Stats yearly trends

:::: panel-tabset

<<dataset_trends>>

::::  

:::::


')



## Clusters
# List of cluster
cluster_names_list <- ''
tmp <- paste(rcs_merged$cluster_code, '. ', rcs_merged$cluster_name, sep = '')
for (i in tmp) {
  cluster_names_list <- glue('{cluster_names_list}
                  {i}  
                  ')
}
qt$cluster_names <- cluster_names_list

# Cluster general figures
qt$clusters_figures <- glue_code('

:::: panel-tabset

## Clusters

![](index_files/charts/fig_scatter_clusters_PY_x_Z9.svg)

## Size  

![](index_files/charts/fig_cluster_size_v.svg)

## Trends

![](index_files/charts/fig_yearly_trends_clusters.svg)

## Years

![](index_files/charts/fig_clusters_PY_boxplot.svg)

## Citations

![](index_files/charts/fig_clusters_Z9_boxplot.svg)

## Scatterplots

::: panel-tabset

## Year x Size

![](index_files/charts/fig_scatter_clusters_PY_x_size.svg)

## Year x Citations

![](index_files/charts/fig_scatter_clusters_PY_x_Z9.svg)

## Size x Citations

![](index_files/charts/fig_scatter_clusters_size_x_Z9.svg)

:::

::::
                                 
                                 ')


# Cluster table
qt$results_table <- '
```{r}
#| echo: false
tmp <- rcs_merged[,c("cluster_code", "cluster_name", "documents", "documents_percent", "PY_Median", "PY_Mean", "Z9_Median", "Z9_Mean", "rcs_label")]
tmp$cluster_code <- gsub("---", "", tmp$cluster_code)
if(all(is.na(rcs_merged$cluster_name))|all(rcs_merged$cluster_name == "")) {
  tmp$cluster_name <- sapply(rcs_merged$frequent_keywords, function(x) {
    k <- strsplit(x, "; ") %>% unlist()
    k <- k[c(1:min(5, length(k)))]
    k <- paste(k, collapse = "; ")
  })
  tmp$cluster_name[tmp$cluster_code %in% c(99,999)] <- "Others"
}
tmp$PY_Mean <- round(tmp$PY_Mean, 1)
tmp$Z9_Mean <- round(tmp$Z9_Mean, 1)
setnames(tmp,
         c("cluster_code","cluster_name", "documents", "documents_percent", "PY_Median", "PY_Mean", "Z9_Median", "Z9_Mean", "rcs_label"),
         c("Cluster","Name", "Documents", "Documents %", "Year Median", "Year Mean", "Cites Median", "Cites Mean", "Label"))
# Not to display the Cluster name in the table because space.
tmp$Name <- NULL # uncomment this to see the names in the table
datatable(tmp)
```
'
# Write cluster descriptions and paper summaries
list_of_clusters <- dataset$X_C %>%
  unique() %>%
  sort()

# We need a conversion vector to translate from cluster_code to cluster_id
cluster_conversion <- rcs_merged$cluster
names(cluster_conversion) <- rcs_merged$cluster_code


# We need the length of digits of the largest cluster id so we can add a padding to match the name 
char_size <- nchar(as.character(length(cluster_conversion)))


cluster_chart_panel <- list()

for (cluster in list_of_clusters) {
  cluster_id <- cluster_conversion[cluster]
  cluster_barcharts <- ''
  for (chart in displayable_charts) {
    cluster_barcharts <- glue_code('<<cluster_barcharts>>
  
                                    ![](index_files/charts/by_clusters/fig_<<str_pad(cluster_id, char_size, "left", "0")>>_<<chart>>.svg)
                                    ')}
  
  cluster_chart_panel[[cluster_id]] <- glue_code('
  

::: {#fig-panel-<<cluster>> layout-ncol=3}

<<cluster_barcharts>>

Cluster <<cluster>> stats.

:::

')
  
}

qt$clusters <- ""
for (cluster in list_of_clusters) {
  cluster_main_description <- glue("
  #### Cluster {cluster}: {rcs_merged$cluster_name[rcs_merged$cluster_code == cluster]}
  `Documents: {rcs_merged$documents[rcs_merged$cluster_code == cluster]}; Ave. Year: {round(rcs_merged$PY_Mean[rcs_merged$cluster_code == cluster],1)}; Ave. Citations: {round(rcs_merged$Z9_Mean[rcs_merged$cluster_code == cluster],1)}`  
  {rcs_merged$description[rcs_merged$cluster_code == cluster]}
  ")
  cluster_data <- subset(dataset_bibliography, X_C == cluster)
  cluster_papers_description <- list()
  for (i in c(1:nrow(cluster_data))) {
    cluster_papers_description[[i]] <- glue("{cluster_data$summary[i]} [@{cluster_data$citation_key[i]}]  `degree: {cluster_data$X_E[i]}` `citations: {cluster_data$Z9[i]}`  ", .literal = TRUE)
  }
  cluster_papers_description <- paste(cluster_papers_description, collapse = "\n")
  qt$clusters <- glue("{qt$clusters}
                      {cluster_main_description}
                      {cluster_chart_panel[[min(length(list_of_clusters), cluster_conversion[cluster])]]}
                      **Articles:**
                      {cluster_papers_description}
                      ---

                      ")
}

###################################
###################################
# DOCUMENT
###################################
quarto_document <- glue('
{qt$yml}
{qt$load_data}
## Data and Methods

### Data
{qt$data}

### Methods
{qt$methods}

## Results
### Dataset Overview
{qt$dataset_overview}


### Clusters
{qt$cluster_names}  

{qt$clusters_figures}

{qt$results_table}

{qt$clusters}

')

if (nchar(quarto_document) == 0) {
  stop('The generated quarto ducument is blank.')
}


# Write the file
fileConn <- file(file.path(output_folder_level, "index.qmd"))
writeLines(quarto_document, fileConn)
close(fileConn)

# Render the file to HTML
# settings.R and bibliography.bib must be on the same directory as index.qmd
quarto_render(file.path(output_folder_level, "index.qmd"))


# Render the file as PDF
# write.csv(rcs_merged, file = 'rcs_merged.csv', row.names = FALSE)

# Upload files to repo for online viewing
