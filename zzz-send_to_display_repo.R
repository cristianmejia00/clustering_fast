# 2024/06/06
library(fs)

# Copy the Quarto document to the GitHub repo `display`
index_folder <- file.path(output_folder_level, "index_files")
index_file <- file.path(output_folder_level, "index.html")

# Create folder
display_folder <- file.path("C:\\Users\\crist\\Desktop\\GitHub\\display", settings$analysis_metadata$project_folder)
display_folder_index_files <- file.path(display_folder, "index_files")
#dir.create(display_folder_index_files)

# Copy files
fs::dir_copy(index_folder, display_folder)
file.copy(index_file, display_folder)

# Create a `.nojekyll` blank file to prevent GitHub to render with Jekyll.
file.create(file.path(display_folder, ".nojekyll"))
