# 20230717
library(glue)


###################################
###################################
# GLOBALS
###################################

# The topic used to infer the query
MAIN_TOPIC <- settings$llm$theme

# It means that you know about ...
MAIN_TOPIC_DESCRIPTION <- settings$llm$description



###################################
###################################
# Article summary
###################################
prompt_summarize_a_paper <- function(topic, topic_description, article_text){
  list('system' = glue('You are a researcher with a great record of publications and that understands what good academic writing is.
                   Your writing style is that of authors in reputable journals like Nature or Science.
                   Your answers are concise and avoid adverbs.
                   Your research of expertice is on <<{topic}>>, meaning that you know about {topic_description}'),
       'user' = glue('Be sure that your summary is shorter than the text provided. Summarize in one sentence of less than 50 words, and focussing on "{topic}" the following text: {article_text}'))
}


###################################
###################################
# Cluster description
###################################
prompt_cluster_description <- function(topic, topic_description, cluster_text) {
  list('system' = glue('You are a policy consultant with expertise on <<{topic}>>, meaning that you know about {topic_description}
      You will be given multiple texts (a.k.a cluster) separated by #####. Your task is to read the texts and find their common topic. Ideally, the common topic should be framed in the context of <<{topic}>>.
      To find the topic name you will follow the next steps:
  
      Step 1: You understand the main topic of each text
      Step 2: You find common keywords across the texts relevant to <<{topic}>>
      Step 3: You summarize all the text in a single paragraph taking into consideration the common keywords and themes.
      Step 4: You conclude by giving a name for the common topic shared by the articles. 
      
      Your answers are concise.'), 
    'user' = glue('What is the main topic of the following cluster: {cluster_text}'))
}

###################################
###################################
# Cluster description enhanced
###################################
prompt_cluster_description_enhanced <- function(topic, cluster_description) {
  list('system' = glue('Please synthesize the following text in a single cohesive paragraph. Do not mention the texts one by one, but in an integrated and coherent manner. Do not use the word "delve" and do not define <<{topic}>> because the reader already know their meaning'),
       'user' = glue('Please synthesize what this cluster is about: {cluster_description}'))
}


###################################
###################################
# Cluster name
###################################
prompt_cluster_name <- function(topic, topic_description, cluster_description) {
  list('system' = glue('You are a policy consultant with expertise on <<{topic}>>, meaning that you know about {topic_description}
      You will be given the decription of a cluster of documents. 
      Either extract the cluster name given in the description, or give a short name based on what you read. 
      Your answers are concise and include only the name of the cluster without any further explanation or introduction.'), 
    'user' = glue('What is the name of the following cluster?: {cluster_description}'))
}

###################################
###################################
# Cluster figure caption
###################################
prompt_figure_caption <- function(topic){
  list('system' = 'You are a researcher with a great record of publications and that understands what good academic writing is.
                   Your writing style is that of authors in reputable journals like Nature or Science.
                   Your answers are concise.',
       'user' = 'Please provide some bullet points with good practices for figure captions based on standard practices in scientific writing.',
       'assistant' = "I can provide some general guidelines for writing figure captions based on standard practices in scientific writing:
        
  1. **Caption Length:** Keep the caption concise but descriptive enough to understand the figure without reading the main text. Typically, a few sentences should be sufficient.
  2. **Start with a Title:** The caption should start with a brief title that explains what the figure is about.
  3. **Description:** After the title, you should provide a detailed explanation of what the figure is displaying. Include necessary details such as the experiment's conditions, what each part of the figure indicates (e.g., axes of graphs), and the significance of the results.
  4. **Avoid Redundancy:** The caption should not just repeat information that's already in the text. Instead, it should complement the text and provide extra information to help readers understand the figure.
  5. **Key Details:** Any symbols, abbreviations, and units of measurement used in the figure should be clearly defined in the caption.
  6. **Statistical Information:** If your figure includes any statistical data, make sure to include information about the statistical tests used, the number of observations, and any relevant significance values.
  7. **Avoid Interpretation:** Try not to include any conclusions or interpretations in the figure caption. The role of the caption is to describe, while interpretation of the data should be left for the main body of the text.
  8. **Referencing the Figure:** When you refer to the figure in your text, make sure to use the same terminology as in the caption. For example, if your caption refers to panels A, B, and C, your text should use the same terms.
  9. **Readability:** Make sure your caption is easily understandable and accessible to the readership of the journal. Avoid overly technical jargon where possible and consider the background knowledge of your audience.
  10. **Proofread:** Finally, make sure to proofread your caption carefully. Errors in your caption can confuse readers and potentially undermine the credibility of your work.",
    'user' = glue("Please write concisely a figure caption explaining the following:
  -	The figure represents the citation network of {topic}
  -	Each cluster is represented with different colors.
  -	Clusters are labeled from the largest by the number of documents.
  -	Clusters that are near each other tend to be topically related and those apart are topically divergent.")
  )
}


###################################
###################################
# Cluster {SENTIMENT} description
# POSITIVE or NEGATIVE
###################################
prompt_cluster_sentiment_description <- function(topic, topic_description, sentiment, cluster_text) {
  list(
    list(
      'role' = 'system',
      'content' = 'You are a policy consultant with expertise on <<{topic}>>, meaning that you know about {topic_description}
      You will be given multiple texts (a.k.a cluster) separated by #####. 
      These are {sentiment} news about <<{topic}>>
      Your task is to read the texts and find their common topic. Ideally, the common topic should be framed in the context of <<{topic}>>.
      To analyze the cluster you will follow these steps:
  
      Step 1: You understand the main topic of each text highlighting their {sentiment} connotation or impact if any.
      Step 2: You find common keywords across the texts relevant to <<{topic}>>
      Step 3: You explain why all these news are considered to be {sentiment}
      Step 4: You summarize all the text in a single paragraph taking into consideration the common keywords and themes.

      Your response must not show the steps. Instead, you reply with a paragraph  synthesizing your analysis emphasizing why this cluster is {sentiment}.
      ' 
    ),
    list(
      'role' = 'user',
      'content' = glue('What is the main topic of the following cluster: {cluster_text}')
    )
  )
}

