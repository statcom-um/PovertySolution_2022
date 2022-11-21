# What this script does:
# - Roughly calculate proportion/percent of payments per building that are late
#   (And output histograms to PDF if you uncomment the relevant code)
# - Determine which date should be "time zero" for longitudinal analysis
#   (We decided that June 01, 2016 is a good start date)
# - Creates new columns for each transaction: 
#     -time elapsed since June 01, 2016 in days and in months
#     -a TRUE/FALSE column indicating whether this was after start of pandemic

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

# This loads a .Rdata file containing the building rent payment ledgers 
# generated from 2022_10_12_scrape_rent_ledger.R
load(file.path(here(), 
               "PovertySolution_2022", 
               "data", 
               "Receivable Analytics", 
               "DHC Owned and Managed",
               "loaded_my_buildings.Rdata"))
customers <- unique(my_buildings[[1]]$Customer)

# Calculate the ratio of "late" to "rent" charges per customer per building
#logic - look at the charges - rent vs. late charge
library(data.table)
prop_late <- list()
for (i in 1:12) {
  customers <- unique(my_buildings[[i]]$Customer)
  late_over_rent <- vector()
  for (j in 1:length(customers)) {
    # Grab all the data for customer j in this building's ledger
    cust_data <- my_buildings[[i]][my_buildings[[i]]$Customer == customers[j],]
    # Look only at "legitimate" charges (i.e. no credits or adjustment-type)
    rent_and_late_charges <- cust_data[(cust_data$Charge %like% "am_rent" | 
                                          cust_data$Charge %like% "am_late") & 
                                         cust_data$Charges > 0 & 
                                         !cust_data$Notes %like% "ADJ" ,1:10]
    #only looking at charges that were considered rent or late charges and > 0
    late_charges <- cust_data[(cust_data$Charge %like% "am_late") & cust_data$Charges > 0 ,1:10]
    rent_charges <- cust_data[(cust_data$Charge %like% "am_rent") & cust_data$Charges > 0 ,1:10]
    late_over_rent[j] <- dim(late_charges)[1]/dim(rent_charges)[1]
  }
  prop_late[[i]] <- late_over_rent
  print(paste("Step",i,"done"))
}

#some individuals have more late charges than rent charges, which implies 
# missing data - I'll just equate them to the most truant renters in each building
for (i in 1:12) {
    prop_late[[i]][which(prop_late[[i]] > 1)] <- max(prop_late[[i]][prop_late[[i]] < 1])
}

# Obtain the file path for the folder containing household data Excel files
hh_data_folder <- file.path(here::here(), "PovertySolution_2022", "data", "Household Composition Info",
                            "DHC Owned and Managed")

# Get the names of files (Excel only)
hh_filenames <- list.files(hh_data_folder, pattern = '.xlsx')

# Get building names
building_names = word(hh_filenames, 1, sep = '_')

# Uncomment to save to pdf
#pdf("Percent_Late_payments.pdf") 
# Make 3 x 4 grid of the next 12 plots we create
par(mfrow = c(3, 4))
for (i in 1:12) {
  #these are VERY skewed distributions.
  hist(100*prop_late[[i]], 
       main = building_names[i], 
       xlim = c(0, 100),
       xlab = "% late rent payments")
#I think we need to use kruskal testing)
}
# Run this to stop outputting plots to pdf
#dev.off()

building_no <- vector()
proportion <- vector()
counter <- 0
for (i in 1:12) {
  for (j in 1:length(prop_late[[i]])) {
    counter <- counter + 1
    building_no[counter] <- i
    proportion[counter] <- prop_late[[i]][j]
  }
}
kruskal_table <- data.frame(as.factor(building_no), proportion)

kruskal.test(prop_late)
#with a test-statistic of 86.766 and a p-value of 7.153e-14, there is 
# evidence to suggest that the late payment patterns of individual buildings are not the same.

#Find the earliest time for a security deposit or am_rent
times <- list()
for (i in 1:12) {
  customers <- unique(my_buildings[[i]]$Customer)
  earliest <- vector(mode = "character")
  for (j in 1:length(customers)) {
    cust_data <- my_buildings[[i]][my_buildings[[i]]$Customer == customers[j],]
    rent_and_sdep_charges <- cust_data[(cust_data$Charge %like% "am_rent" | cust_data$Charge %like% "am_sdep"), 5]
    #only looking at security deposits or rent charges
    earliest[j] <- as.character(sort(rent_and_sdep_charges)[1], na.rm = TRUE)
  }
  times[[i]] <- earliest
}

earliest_time <- vector()
building_id <- vector()
counter <- 0
for (i in 1:12) {
  for (j in 1:length(times[[i]])) {
    counter <- counter + 1
    earliest_time[counter] <- times[[i]][j]
    building_id[counter] <- i
  }
}
time_table <- data.frame(building_id, earliest_time)
sort(time_table$earliest_time)[1:15] 
#choosing June 2016 as the 0 time (first time there is less than a month jump to the next time)

library(lubridate)
# Calculate time passed since June 01, 2016 (in DAYS)
my_buildings2 <- my_buildings
for (i in 1:12) {
  my_buildings2[[i]]$Transaction2 <- ymd(my_buildings2[[i]]$Transaction) - ymd("2016-06-01") #time difference in days of the transaction date from the "0 time"
}

# Calculate time passed since June 01, 2016 (in MONTHS)
my_buildings2 <- my_buildings
for (i in 1:12) {
  my_buildings2[[i]]$Transaction_m <- interval(ymd(my_buildings2[[i]]$Transaction),  
                                              ymd("2016-06-01")) %>% 
    as.numeric("months") #time difference in days of the transaction date from the "0 time"
}
ymd("2020-03-01") - ymd("2016-06-01") #1369 days
for (i in 1:12) {
  my_buildings2[[i]]$COVID <- my_buildings2[[i]]$Transaction2 > (ymd("2020-03-01") - ymd("2016-06-01"))
} #separating out the dates before COVID and the dates after COVID