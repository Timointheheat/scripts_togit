# Define the paths
base_path <- "C:/Users/tbo102/OneDrive - Vrije Universiteit Amsterdam/Study 1/Analysis/Analysis REAL/JAP/cleaned"

# Function to remove unwanted files for a single participant
remove_unwanted_files <- function(participant_id) {
  participant_folder <- file.path(base_path, paste0("p", participant_id))
  
  # List all files in the participant folder
  files <- list.files(participant_folder, full.names = TRUE)
  
  # Check if the folder is empty
  if (length(files) == 0) {
    return(NULL)
  }
  
  # Define unwanted patterns
  unwanted_patterns <- c("ha", "raw", "summary", "wbgt", "po", "core")
  
  # Identify files to remove
  files_to_remove <- files[sapply(files, function(file) any(sapply(unwanted_patterns, function(pattern) grepl(pattern, basename(file)))))]
  
  # Remove unwanted files
  file.remove(files_to_remove)
}

# Process participants 1 to 43
for (i in 1:43) {
  remove_unwanted_files(i)
}
