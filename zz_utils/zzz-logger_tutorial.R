# 20230723
# Logger tutorial


# install.packages('logger')
library(logger)

# Name of the log file to be created
t <- "log.txt"

# Use `tee` to print in the console and save to log file
log_appender(appender_tee(t))

# Print warnings throughout the code
log_warn('Attention!...')
log_info('Loading data ...')
log_info('New message')

# Unlink the log file at the end of the process
unlink(t)

# Read the contents of log.txt
readLines(t)

