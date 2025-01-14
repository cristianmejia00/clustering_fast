##########################################################
# Load libraries
source("zz_utils/02_libraries.R")

# Load settings from the project we are interested in
# source("settings.R")


##########################################################
##########################################################
##########################################################
library(httr)
library(jsonlite)

claude_api_key <- readr::read_file(file.path("05_assets", "credentials", "claude.key"))

#' @description
#' Get answers from OpenAI's GPT. Here used for ARTICLE summarization.
#' @param prompt LIST. A prompt in the format of OpenAI. See the code `zz-prompts.R` for details.
#' @param model STRING {gpt-3.5-turbo-0613} the OpenAI Moodel to use. Options: gpt-3.5-turbo-0613, gpt-4, 'gpt-4-0613'
#' @param temperature NUMBER. Between 0 and 2. 0 means less randomness and 2 more creative.
#' @param max_tokens INTEGER. The approx MAX size possible for the reply from ChatGPT.
#' @param n INTEGER. Number of reply variations to get.
#' @returns The JSON reply from OpenAI in R's LIST form. The actual reply text is located at `x$choices[[1]]$message$content`
ask_claude <- function(system_prompt,
                       user_prompt,
                       api_key = claude_api_key,
                       model = "claude-3-5-sonnet-latest", #' claude-3-opus-20240229',
                       temperature = 0.1,
                       max_tokens = 500) {
  # Set up API endpoint and headers
  api_url <- "https://api.anthropic.com/v1/messages"

  headers <- c(
    "anthropic-version" = "2023-06-01",
    "content-type" = "application/json",
    "x-api-key" = api_key
  )

  # Set up the request payload
  payload <- list(
    system = system_prompt,
    model = model,
    temperature = temperature,
    max_tokens = max_tokens,
    messages = list(
      list(
        "role" = "user",
        "content" = user_prompt
      )
    )
  )
  # Send the API request
  response <- httr::POST(
    url = api_url,
    add_headers(headers),
    body = toJSON(payload, auto_unbox = TRUE)
  )

  # Check the response status
  if (response$status_code == 200) {
    # Parse the JSON response
    result <- fromJSON(httr::content(response,
      as = "text",
      encoding = "UTF-8"
    ))
    # Extract and print the assistant's response
    assistant_response <- result$content$text
    return(assistant_response)
  } else {
    return(paste("Error:", response$status_code))
  }
}


##########################################################
##########################################################
##########################################################
# Libraries
library(reticulate)
library(glue)

# One time operation to generate a python env
# reticulate::conda_create(envname = 'openai_env', packages = 'openai', python_version = '3.9')

# Activate enviroment
# reticulate::use_condaenv('openai_env')

# Attach key.
# In VSCode create a file `openai.key`
# Is only one line with the OpenAi key.
# `credentials/openai.key` was added to .gitignore so is not committed to the repo.
# import Openai Python library
# openai <- reticulate::import('openai')
# client = openai$OpenAI(api_key = readr::read_file('05_assets/credentials/openai.key'))


# utils
#' @description
#' Get answers from OpenAI's GPT. Here used for ARTICLE summarization.
#' @param prompt LIST. A prompt in the format of OpenAI. See the code `zz-prompts.R` for details.
#' @param model STRING {gpt-3.5-turbo-0613} the OpenAI Moodel to use. Options: gpt-3.5-turbo-0613, gpt-4, 'gpt-4-0613'
#' @param temperature NUMBER. Between 0 and 2. 0 means less randomness and 2 more creative.
#' @param max_tokens INTEGER. The approx MAX size possible for the reply from ChatGPT.
#' @param n INTEGER. Number of reply variations to get.
#' @returns The JSON reply from OpenAI in R's LIST form. The actual reply text is located at `x$choices[[1]]$message$content`
ask_gpt <- function(system_prompt,
                    user_prompt,
                    model = "gpt-3.5-turbo",
                    temperature = 0.1,
                    max_tokens = 500,
                    n = 1) {
  response <- client$chat$completions$create(
    model = model,
    temperature = temperature,
    max_tokens = as.integer(max_tokens),
    n = as.integer(n),
    messages = list(
      list(
        "role" = "system",
        "content" = system_prompt
      ),
      list(
        "role" = "user",
        "content" = user_prompt
      )
    )
  )
  return(response$choices[[1]]$message$content)
}


##########################################################
##########################################################
##########################################################

#' @description
#' Function to get a subset of the cluster containing the combination of
#' top 5 most linked (X_E), most cited (Z9), and Most linked of the most recent
#' @param dataset DATAFRAME. the dataset
#' @param cluster INTEGER. the cluster number to subset. Compatible with X_C, meaning sypport for cluster 99.
#' @returns DATAFRAME. The largest possible is of `top * 3` when all 3 conditions are different
get_cluster_data <- function(dataset_, cluster_, top = 5) {
  cluster_data <- dataset_ %>%
    filter(X_C == cluster_) %>%
    select(all_of(c("X_C", "TI", "AB", "AU", "PY", "UT", "Z9", "X_E", "summary")))
  if (nrow(cluster_data) > top) {
    selected_papers <- c(
      # Most connected
      cluster_data$UT[order(cluster_data$X_E, decreasing = TRUE)][1:top],
      # Most cited
      cluster_data$UT[order(cluster_data$Z9, decreasing = TRUE)][1:top] # ,
      # Newest most connected (X_E is preferred over Z9 because most of paper wont have citations)
      # cluster_data$UT[order(cluster_data$PY, cluster_data$X_E, decreasing = TRUE)][1:top]
    ) %>% unique()
    # Only retain selected papers
    cluster_data <- cluster_data[cluster_data$UT %in% selected_papers, ]
  }
  cluster_data$text <- paste(cluster_data$TI, cluster_data$AB, sep = " ")
  return(cluster_data)
}

##########################################################
##########################################################
##########################################################

#' @description
#' AskGPT to summarize each article in the given dataset. Each summary is appended to column `summary`
#' @param dataset DATAFRAME. the dataset
#' @returns DATAFRAME. the same dataset with the column summary appended.
get_papers_summary <- function(cl_dataset) {
  # cl_dataset$summary <- ''
  starting <- 1
  ending <- nrow(cl_dataset)
  while (starting < ending) {
    for (idx in c(starting:ending)) {
      print(paste(cl_dataset$X_C[idx], as.character(idx), cl_dataset$TI[idx], sep = "; "))
      article_summary <- tryCatch(
        {
          if (nchar(cl_dataset$summary[idx]) == 0) {
            prompt_summary <- prompt_summarize_a_paper(
              topic = MAIN_TOPIC,
              topic_description = MAIN_TOPIC_DESCRIPTION,
              article_text = cl_dataset$text[idx]
            )
            article_summary <- ask_claude(
              system_prompt = prompt_summary$system,
              user_prompt = prompt_summary$user,
              temperature = 0.7
            )
            cl_dataset$summary[idx] <- article_summary
          }
        },
        error = function(err) {
          message(glue("error found in {idx}"))
          message(err)
        },
        finally = {
          starting <- idx
          Sys.sleep(5)
        }
      )
    }
    # starting <- idx
  }
  return(cl_dataset)
}
