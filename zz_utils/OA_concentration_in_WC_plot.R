# Code to compute and plot the share of open access papers per WC.
# I created this code for the STI2024 conference. 

# This was developed for the STI project. Hence, we need a dataset having a column "OA_is2", or 
# anyonther boolean column. OA_is2 is a Boolean column indicating when the paper is OA according the 
# Categories set by the Web of Science, and ourselves. The WoS has the String column OA  with the type
# of OA per paper, OA_is2, simply says if not NA == TRUE.


library(ggrepel)
library(tidyr)

result <- dataset %>%
  select(WC, OA_is2) %>%
  mutate(categories = strsplit(WC, "; ")) %>%
  unnest(categories) %>%
  group_by(categories) %>%
  summarise(
    size = n(),
    OA = mean(OA_is2 == "Open Access"),
    not_OA = mean(OA_is2 == "Not Open Access")
  ) %>% 
  filter(size > 10) %>%
  arrange(desc(OA))


table(dataset$OA_is2)
# View the result
print(result)


# Create the scatter plot
plot <- ggplot(result, aes(x = OA, y = size, label = categories)) +
  geom_point() +
  geom_text_repel(box.padding = 0.5) +
  labs(y = "Articles",
       x = "OA coverage") +
  theme_minimal()

# Display the plot
print(plot)
