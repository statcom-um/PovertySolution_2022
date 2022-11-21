#--- Source Necessary Packages -------------------------------------------------


library(pacman)

`%notin%` <- Negate(`%in%`)

p_load(
  #--- Packages to Fit Models
  
  MASS,
  logistf,
  survival,
  
  #--- Packages to Produce Tables
  
  gtsummary,
  flextable,
  janitor,
  broom,
  officer,
  kableExtra,
  
  #--- Packages to Produce Figures
  
  ggsci,
  ggridges,
  ggthemes,
  ggforce,
  ggpubr,
  patchwork,
  grid,
  gridExtra,
  survminer,
  viridis,
  ggridges,
  hrbrthemes,
  stickylabeller,
  
  #--- Packages for Data Retrieval & Pre-Processing
  
  readxl,
  here,
  rdrop2,
  lubridate,
  zoo,
  tidyverse,
  purrr
)

#directory based programming
data_folder = file.path(here::here(),
                        'data', 'Receivable\ Analytics',
                        'DHC\ Owned\ and\ Managed')
my_files = list.files(data_folder, pattern = '.xlsx')

#get building names
building_names = word(my_files, 1, sep = '_')

#read in all excel files
rent_ledger = file.path(data_folder, my_files) %>%
  map(., read_xlsx, col_type = 'text')

#data cleaning as a function as written in 2022_02_25.R
#replaced |> pipe with %>% pipe because I'm a bad programmer who is
#behind in R versions
clean_buildings = function(dat) {
  new_dat = dat %>%
    #-- Slice Off Unnecessary Header Rows and Rename Columns
    slice(-1, -3, -4) %>%
    row_to_names(1) %>%
    #-- String Match Start of New Resident Records and Fix Variables
    mutate(
      NewRes = grepl("\\(", Property),
      # modified to deal with Resident issues in Greenbrooke
      #id those where there is a name instead of 'resident'
      Identified = case_when(
        grepl("\\(", Property) &
          !grepl('Resident', Property) ~ 'Identified',
        grepl("\\(", Property) &
          grepl('Resident', Property) ~ 'Deidentified',
        TRUE ~ NA_character_
      ),
      Customer = if_else(
        NewRes,
        gsub(
          "\\(([^()]+)\\)",
          "\\1",
          str_extract(Property, "\\(([^()]+)\\)")
        ),
        NA_character_
      ) %>% factor(),
      Transaction = Transaction %>% as.numeric() %>% as_date(origin = "1899-12-30 UTC"),
      across(Charges:Balance, as.numeric)
    ) %>%
    #--- Impute Resident T-Codes
    fill(Customer, Identified) %>%
    #--- Drop Extra Rows and Columns
    filter(!NewRes) %>%
    #drop_na(Property) %>%
    select(-(Job:`Cost Code`), -Post, -NewRes) %>%
    #--- Impute 'Balance Forward' with First Transaction Date
    fill(Transaction, .direction = "up") %>%
    #logically identify problem rows and
    #replace nonmissing Tenant information that is not Resident with Resident
    mutate(
      Identified = case_when(Tenant != 'Resident' ~ 'Identified',
                             TRUE ~ Identified),
      Tenant = case_when(Tenant != 'Resident' ~ 'Resident', TRUE ~
                           Tenant)
    ) %>%
    #return new data
    return(new_dat)
}
#apply function to list of buildings
my_buildings = rent_ledger %>% map(., clean_buildings)
my_tenants = rent_ledger %>% map_dfr(., clean_buildings)