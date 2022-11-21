# Necessary packages (a subset of those required in 2022_10_12_scrape_rent_ledger)
library(pacman) # p_load()
p_load(
  here,
  lubridate,
  janitor,
  readxl,
  purrr,
  tidyverse
)

# Obtain the file path for the folder containing household data Excel files
hh_data_folder <- file.path(here::here(), "PovertySolution_2022", "data", "Household Composition Info",
                            "DHC Owned and Managed")

# Get the names of files (Excel only)
hh_filenames <- list.files(hh_data_folder, pattern = '.xlsx')

# Get building names
building_names = word(hh_filenames, 1, sep = '_')

# Load every file into R

 ### My inelegant, base-R-only implementation ###
# Create empty list with a space for each Excel file's data
#my_households <- vector("list", length(hh_filenames))
#for (i in 1:length(hh_filenames)) {
#  this_file_path <- paste0(hh_data_folder, "/", hh_filenames[i])
#  my_households[[i]] <- read_xlsx(this_file_path)
#}

# Pre-defining col types fails bc Excel sheets have different number of columns
# Better implementation based on Soumik's code
my_households = file.path(hh_data_folder, hh_filenames) %>%
  map(., read_xlsx, col_types = NULL) # function will guess data type from Excel

# Examine structure of data
str(my_households)

# Excel files are dated from Feb 9, 2021
excel_last_modified <- ymd("2021-02-09")
# read_xlsx converted dates into days since 1899-12-30
excel_origin_date <- "1899-12-30"

# Define a function that runs on each tibble in the list we just made
clean_households <- function(hh_data) {
  # Column names are a little inconsistent between files
  dob_colname <- as.character(hh_data[1,][grep("DOB", hh_data[1,])])
  m_num_colname <- as.character(hh_data[1,][grep("Number", hh_data[1,])])
  
  # Remove mostly-empty columns that aren't relevant to analysis
  idx_lname_col <- grep("Last", hh_data[1,])
  idx_fname_col <- grep("First", hh_data[1,])
  #idx_addr_col <- grep("Address", ignore.case = T, hh_data[1,])
  idx_na_col <- (1:ncol(hh_data))[is.na(hh_data[1,])]
  
  print(paste0("Cleaning data for property ID: '", hh_data[2, 1], "'"))
  new_data <- hh_data %>% 
    row_to_names(1) %>% # Turn the first row into a header
    # Q: OK to get rid of First Name columns?
    # Get rid of useless columns
    # Names are not perfectly consistent across Excel files
    #select(-c("First Name", "Last Name", "Member LastName (3b)", 
    #          "Member FirstName (3c)")) %>%
    select(-all_of(c(idx_na_col, idx_lname_col, idx_fname_col))) %>%
    rename(Customer = Tenant, dob = dob_colname, mem_number = m_num_colname) %>%
    mutate(Tenant = as.factor(Customer)) %>%
    mutate(dob = as.Date(as.numeric(dob), origin = excel_origin_date)) %>%
    mutate(dob = ymd(dob)) %>%
    mutate(age = interval(dob, excel_last_modified) %>% 
             as.numeric('years') %>%
             round(digits = 1))
    
  new_data # Returns the cleaned data
}

# Run clean-up function on all household data files
my_households = my_households %>% map(., clean_households)

# Merge building data with the household data rows that have a tenant ID
# TODO decide what to do with the household rows with no tenant ID
combined <- vector(mode = "list", length = 12L)
for (i in 1:12) {
  combined[[i]] <- merge(my_buildings[[i]], my_households[[i]], by = "Customer")
}
#save(combined, file = "combined.Rdata")
