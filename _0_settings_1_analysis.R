# Analysis Settings file
# This is a directive settings file
# We use it as input

# Call necessary libraries
source("zz_utils/02_libraries.R")
source("zz_utils/00_system_paths.R")

# Initialize settings
settings <- list()

###########################################################
## Metadata
settings$metadata <- list(
  
  project_folder = "Q001_robotics", # <- change here.
  
  filtered_folder = "f01", #do not change!
  analysis_id = "a01_cn__f01_dc__c01_lv", #do not change! 
  bibliometrics_folder = output_folder_path #do not change! 
)

###########################################################
## General Parameters
settings$params <- list(
  recursive_level = 1, # Reports will be generated to this level. 0 means clusters, 1 subclusters, 2 subclusters of subclusters
  dataset_source = "wos",
  unit_of_analysis = "cluster", # topic, cluster, facet, firm, country, institution, author, etc.
  type_of_analysis = "citation_network", # "topic_model", "citation_network", or "both"
  seed = 100 # The seed for random initialization. Needed for reproducibility
)

########################################################### for 00_clustering.R
## Citation network options
if (settings$params$type_of_analysis %in% c("citation_network", "both")) {
  settings$cno <- list(
    # Shall we use the network file from Fukan System (i.e. mission.pairs.tsv is available)
    # if False, we create our own network file from scratch.
    using_mission_pairs_from_fukan = FALSE,

    # Shall we use the initial clustering solution from Fukan System?
    # If TRUE, we use the column "_C" in mission.facet.all, and hence we can use the figure from Fukan System
    # If FALSE, we compute a new "_C" column based on the algorithm of choice.
    using_initial_column_C_from_fukan = FALSE,

    # Type of network
    network_type = "direct_citation",

    # The component subsetting
    # By default we take the largets comp
    # 'all' = everything
    # 'top' = The top n components by size
    # 'component' = Only the selected component
    # 'min_vertices' = The min size of vertex in comp to be considered. For example value = 2, will remove all floating isolated nodes.
    component = list(
      strategy = "top",
      value = 1
    ),

    # Clustering
    clustering = list(
      algorithm = "louvain" # "louvain", OR "newman" OR "infomap"
    ),

    # Thresholding.
    # i.e. Creation of the `dataset_minimal`. Recursive clustering and aggregation of small clusters

    thresholding = list(
      # Either...
      # - Proportion of articles that determines the number of level0 clusters (<1)(e.g. #largest clusters contain 90%, 0.9, of articles )
      # - Number of cluster to consider from the Fukan System solution (1+)
      threshold = 0.90,

      # Top max clusters to analyze per iteration
      max_clusters = 1000, # When clustering level 0 has more than 100 clusters pick a large number

      # Cluster scope
      scope = "all", # "all" OR "cl99" OR "cl_99"

      ### Options for clustering or recursive clustering.
      ### The following options are useful for any of these conditions
      ### - We want recursive clustering
      ### - we want clustering at level0, either because:
      ###   - we don't want to use Fukan System X_C
      ###   - We have a WOS dataset without X_C

      # Subcluster only if having this or more
      size_limit = 350,

      # Include cluster having collecting a minimum of __ articles
      size_lower_limit = 45,

      # When recursive clustering there is a label "-0" that might be annoying. TRUE to remove it. However, Excel will think they are dates.
      remove_zero = FALSE
    )
  )
}


## Topic Model options
if (settings$params$type_of_analysis %in% c("topic_model", "both")) {
  settings$tmo <- list(
    # embeds to use within filtered folder
    embeds_folder = 'e01',
    
    # The integer column with the Year of publication
    year_column = "PY",

    # The number of topics to get. Use 0 to infer the topics with HDBScan
    n_topics = 59,

    # The minimum size for a topic
    min_topic_size = 10
  )
}


# add-ons
settings$addons <- list(
  "include_orphans" = "NO", # NO, 99, 999
  # Sentiment analysis is computed outside R, with Python
  # The dataset must contain the columns:
  # - `sentiment` NUMERIC. with a score between -1 and 1
  # - `sentiment_factor` STRING ENUM[positive, neutral, negative] with the sentiment label
  "sentiment_analysis" = FALSE
)

###########################################################
## For LLM
settings$llm <- list(
  "theme" = "robotics",
  "description" = "the study and practice of designing, constructing, operating, and using robots. Robots are machines that perform tasks that humans traditionally do. Robotics is an interdisciplinary field that involves mechanical engineering, computer science, and other areas.",
  "compute" = c("old_paper_summaries", "representative_docs_summaries", "cluster_title", "cluster_description", "cluster_enhanced_description")
)

########################################################### for 00_reports.R
## Reporting
settings$rp <- list(
  most_recent_year = 2024, # This is needed so the charts do not plot 2024, or future years where data is incomplete
  top_documents = 0, # 0 means ALL # Select the number of top documents to show in the article report
  top_items = 20, ## 0 means ALL # Select the number of top `documents`field`` to show in the clusters report
  text_columns = c("TI", "AB"), # Column(s) with text contents to merge and analyze
  article_report_columns = list("X_C", "cluster_code", "AU", "PY", "DI", "TI", "AB", "Z9", "X_E", "DE", "SO", "WC", "Countries", "UT", "sentiment", "sentiment_factor"),
  categorical_long_reports = list("AU", "WC", "SO", "Countries", "Institutions", "DE", "sentiment_factor", "ID", "issues"), # Columns in the dataset for long-form summary. These are also used for RCS.
  categorical_simple_wide_reports = list("PY", "sentiment_factor"), # Columns in the dataset without ';' for matrix type summary
  categorical_multi_wide_reports = list("WC", "Countries", "Institutions", "issues"), # Columns in the dataset with ';' for matrix type summary
  numerical_reports = list("PY", "Z9", "sentiment", "score") # Numeric columns in the dataset for summary (min, max, mean, median, sd)
  # methods = c("Data collection from WOS", "Created citation network", "Extracted Maximum Component", "Clustering using the Louvain method", "Cluster description")
)

# Column labels are used to format RCS columns and charts' labels
if (settings$params$dataset_source == "wos") {
  settings$rp$column_labels <- list(
    "X_C" = "Cluster Index",
    "cluster_code" = "Cluster Code",
    "TI" = "Title",
    "AB" = "Abstract",
    "AU" = "Authors",
    "PY" = "Publication Years",
    "X_E" = "Degree",
    "SO" = "Journals",
    "Countries" = "Countries",
    "Institutions" = "Institutions",
    "DI" = "DOI",
    "WC" = "Categories",
    "DE" = "Author Keywords",
    "ID" = "WOS Keywords",
    "Z9" = "Citations",
    "score" = "Score",
    "sentiment" = "Sentiment score",
    "sentiment_factor" = "Sentiment",
    "UT" = "ID"
  )
}

if (settings$params$dataset_source == "derwent") {
  settings$rp$column_labels <- list(
    "X_C" = "Cluster",
    "TI" = "Title",
    "AB" = "Abstract",
    "AU" = "Inventors",
    "PY" = "Publication Years",
    "X_E" = "Degree",
    "SO" = "Firms",
    "Countries" = "Countries",
    "Institutions" = "Asignees",
    "DI" = "DOI",
    "WC" = "IPC",
    "DE" = "Author Keywords",
    "Z9" = "Citations",
    "score" = "Score",
    # "sentiment" = "Sentiment score",
    # "sentiment_factor" = "Sentiment",
    "UT" = "Patent Number"
  )
}

if (settings$params$dataset_source == "factiva") {
  settings$rp$column_labels <- list(
    "X_C" = "Cluster",
    "TI" = "Headline",
    "AB" = "Main paragraph",
    "X_E" = "Score",
    "PY" = "Publication Years",
    "SO" = "Newspapers",
    "AU" = "Factiva Types",
    "Countries" = "Regions",
    "Institutions" = "Entities",
    # "WC" = "Categories",
    "DE" = "Categories",
    "ID" = "Entities",
    "score" = "Score",
    "sentiment" = "Sentiment score",
    "sentiment_factor" = "Sentiment",
    "UT" = "ID",
    "issues" = "Issues",
    "Keywords" = "Keywords"
  )
}

# Activate stopwords
settings$stopwords <- list()
settings$stopwords$article_StopWords <- list(
  "analysis", "paper", "na", "say", "will", "can", "article", "use", "press", "release",
  "all", "rights", "reserved", "elsevier", "scopus", "doi", "int", "ieee", "cover", "story",
  # Stemmed
  "use", "structur", "result", "method", "system", "effect", "studi", "measur", "model", "show", "high",
  "observ", "increas", "also", "propos", "two", "base", "investig", "properti", "process", "differ", "obtain",
  "found", "chang"
)

# patent_StopWords <- c("patent", "claim", "device", "data", "module", "network", "control" ,
#                   "base","method", "methods","terminal", "information",
#                   "connect", "connects", "connection", "communication", "internet", "things", "thing")
#
settings$stopwords$news_Stopwords <- list(
  "said", "country", "year", "according", "people", "work", "say", "says", "said",
  "need", "one", "number", "well", "part", "end", "report", "support", "per", "cent", "percent",
  "one", "two", "three", "four", "five", "six", "seven", "eight", "nine", "ten", "billion", "million", "thousand",
  "million", "time", "living", "make", "including", "however", "reached", "provide", "expected", "day",
  "set", "important", "come", "many", "made", "way", "take", "total", "want", "com", "now", "like", "able", "get",
  "order", "continue", "aim", "since", "monday", "tuesday", "wednesday", "thursday", "friday", "saturday", "sunday", "week",
  "noted", "see", "addition", "put", "present", "month", "received", "taken",
  "january", "february", "march", "april", "may", "june", "july", "august", "september", "november", "october", "december",
  "timescontent", "especially", "know", "look", "give", "consider", "much", "asked", "lot", "less",
  "yesterday", "tomorrow", "publish", "reprint", "yet", "ago"
)

settings$stopwords$myStopWords <- list(
  settings$stopwords$article_StopWords,
  settings$stopwords$news_Stopwords
) %>% unlist()


source("_1_entry_1_analysis.R")

