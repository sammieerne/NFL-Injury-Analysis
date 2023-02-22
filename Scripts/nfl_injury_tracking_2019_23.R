if(require(pacman)== FALSE) install.packages("pacman")
pacman::p_load(tidyverse, 
               tidycensus, # for getting the census data
               httr, jsonlite, # pkgs that we might use for API,
               janitor,
               lubridate,# for making a column name from row 1
               magrittr, rvest, httr )

# * * Reading through a singular page -------------------------------------


'''
url <- "https://www.prosportstransactions.com/football/Search/SearchResults.php?Player=&Team=&BeginDate=2019-07-15&EndDate=2023-02-07&InjuriesChkBx=yes&submit=Search"

# Read the HTML content from the URL
html_content <- read_html(url)

# Extract the table from the HTML content
table_html <- html_content %>%
  html_node("table") %>%
  html_table()

# Specify the column names
colnames(table_html) <- c("Date", "Team", "Relinquished", "Notes")

# Store the table data in a data.frame
df <- as.data.frame(table_html)
'''


# * * Reading through multiple pages --------------------------------------


# Load the required library
#library(rvest)

# Define the URL template
url_template <- "https://www.prosportstransactions.com/football/Search/SearchResults.php?Player=&Team=&BeginDate=2019-07-15&EndDate=2023-02-07&InjuriesChkBx=yes&submit=Search&start=%d"

# Specify the starting value for the start parameter
start_value <- 0

# Initialize an empty data.frame to store the data
df_all <- data.frame()

# Repeat the loop until there are no more pages
while (TRUE) {
  # Define the URL
  url <- sprintf(url_template, start_value)
  
  # Read the HTML content from the URL
  html_content <- read_html(url)
  
  # Extract the table from the HTML content
  table_html <- html_content %>%
    html_node("table") %>%
    html_table()
  
  # Check if the table is empty (no more pages)
  if (nrow(table_html) == 0) {
    break
  }
  
  # Specify the column names
  colnames(table_html) <- c("Date", "Team", "Relinquished", "Notes")
  
  # Store the table data in a data.frame
  df <- as.data.frame(table_html)
  
  # Append the data to the all-data data.frame
  df_all <- rbind(df_all, df)
  
  # Update the start value
  start_value <- start_value + 25
}

write_csv(df_all, 'nfl_injuries_2019_23.csv')
