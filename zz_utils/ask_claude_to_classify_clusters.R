library(readr)

# Load the human augmentation dataset
a318 <- read_csv("~/Library/CloudStorage/GoogleDrive-cristianmejia00@gmail.com/My Drive/Bibliometrics_Drive/Q318_human_augmentation/a01_cn__f01_dc__c01_lv/level1/cluster_summary.csv")


# Write the genering prompt

a318$relevant <- sapply(c(1:nrow(a318)), function(x) {
  cluster_text <- paste(a318$cluster_name[x], a318$description[x], sep = ". ")
  print(cluster_text)
  system_prompt <- "You are a researcher in the field of human augmentations with robotics technologies"
  my_question <- glue("The following text describes a research topic from a clustering analysis. Please assess if the contents or technologies mentioned in the text is directly related to haptics, sensing technologies, vision or visual enhancement technologics, wearables, or mechanics. If so, simple return 'TRUE', or 'FALSE' otherwise. Here's the text: {cluster_text}")
  ask_claude(system_prompt = system_prompt,
             user_prompt = my_question)
})

# Split the column following Claude's convention " - "
a318 <- a318 %>% separate(relevant,
                          sep = " - ",
                          into = c("relevant", "relevant_explanation"),
                          extra = "merge",
                          fill = "right")

# Adjustments for non conventional replies
a318$relevant_explanation[is.na(a318$relevant_explanation)] <- a318$relevant[is.na(a318$relevant_explanation)]
a318$relevant <- grepl("TRUE", a318$relevant)

# Save the file
write.csv(a318, "cluster_summary_assessment.csv", row.names = FALSE)
