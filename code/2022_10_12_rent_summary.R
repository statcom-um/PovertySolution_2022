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

source(file.path(here(), "code", "2022_10_12_scrape_rent_ledger.R"))

## find out how many times tenant had late payment
clean_rent_late_count = function(dat) {
  return(left_join(dat %>% 
                     filter(grepl("Tenant Rent", Charge)) %>%
                     filter(Charges > 0) %>%
                     rowwise() %>%
                     mutate(late_by = day(Transaction)) %>%
                     group_by(Customer) %>%
                     summarise(total = n()), 
                   dat %>% 
                     filter(grepl("Tenant Late Charge", Charge)) %>%
                     rowwise() %>%
                     mutate(late_by = day(Transaction)) %>%
                     group_by(Customer) %>%
                     summarise(late = n()) 
  ))
}

late_count <- my_buildings %>% 
  map_dfr(., clean_rent_late_count) %>%
  mutate(late = ifelse(is.na(late), 0, late))

tenants = my_buildings[[1]]
for(i in 2:12){
  tenants = tenants %>% 
    add_row(my_buildings[[i]])
}

## find out when tenant paid in same month after late fees added
clean_rent_late_count = function(id) {
  
  dat <- tenants %>%
    filter(Customer == id) %>%
    mutate(index = seq_along(Charge)) 
  
  pos <- dat %>% 
    filter(grepl("Tenant Late Charge", Charge)) %>%
    pull(index) + 1
  
  month <- dat %>% 
    filter(grepl("Tenant Late Charge", Charge)) %>%
    select(c(index, Transaction, Receipts, Charge, Balance)) %>%
    mutate(month = lubridate::month(Transaction)) %>%
    select(c(index, Transaction, month, Charge)) %>%
    rename(month_rent = month)
  
  payment <- dat %>% 
    select(c(index, Transaction, Receipts)) %>%
    filter(Receipts < 0) %>%
    mutate(index = index - 1, 
           month_pay = lubridate::month(Transaction))
  
  same <- left_join(payment, month, by = "index") %>%
    filter(month_pay == month_rent) %>% 
    mutate(late_duration = as.numeric(Transaction.x - Transaction.y), 
           rent = -Receipts) %>%
    select(c(index, late_duration, rent, Transaction.x)) %>%
    rename(date = Transaction.x)
  
  if(nrow(same) == 0){
    return(c(id, 0, 0, 0))
  }else{
    return(c(id, nrow(same), median(same$late_duration), mean(same$rent)))
  }
}

same = clean_rent_late_count(as.character(unique(tenants$Customer)[1]))
for(id in unique(tenants$Customer)[-1]){
  same = rbind(same, 
               clean_rent_late_count(as.character(id)))
}

dat <- full_join(as_tibble(same) %>%
                   mutate(V2 = as.numeric(V2), 
                          V3 = as.numeric(V3), 
                          V4 = as.numeric(V4)) %>%
                   rename(same_times = V2, 
                          median_late_duration = V3, 
                          mean_late_amount = V4, 
                          tid = V1), 
                 late_count %>% rename(tid = Customer)) %>%
  rename(late_same = same_times, 
         late_duration = median_late_duration, 
         late_amount = mean_late_amount) %>%
  select(c(tid, total, late, late_same, late_duration, late_amount))

dat <- left_join(tenants %>% 
                   rename(tid = Customer, location = Property) %>% 
                   select(c(location, tid)) %>% 
                   group_by(tid) %>% 
                   summarise(tid = unique(tid), 
                             location = unique(location)) %>%
                   filter(location != "Grand Total") %>%
                   drop_na(), 
                 dat) %>%
  drop_na()

loc_code = dat %>% pull(location) %>% unique()
loc_name = building_names

dat <- left_join(dat, 
                 as_tibble(cbind(location = loc_code, 
                                 name = loc_name)))

## dat %>% write_csv(file.path(here(), "data", "created_data", "2022_10_21_rent_summary.csv"))



## boxplot of lateness proportion by building.
dat  %>%
  mutate(prop = ifelse(late >= total, 1, late/total)) %>%
  ggplot(aes(y = name, x = prop, fill = name)) +
  geom_density_ridges() +
  scale_y_discrete(expand = expansion(add = c(0, 2))) + 
  scale_fill_futurama() +
  theme_bw() +
  theme(legend.position = "none") + 
  xlab("Proportion of late rent payments") + 
  ylab("") + 
  labs(title = "Proportion of late rent payments by tenants, stratified by location.")

## density plots of delay in rent payment, building.
dat  %>%
  filter(late_same > 0, late_amount > 0, late_duration > 0) %>% 
  ggplot(aes(y = name, x = late_duration, fill = name)) +
  geom_density_ridges() +
  scale_y_discrete(expand = expansion(add = c(0, 1.8))) + 
  scale_fill_futurama() +
  theme_bw() +
  theme(legend.position = "none") + 
  xlab("Median delay in rent payment (# of days)") + 
  ylab("") + 
  labs(title = "Median delay in rent payment (# of days) by tenants, stratified by location.") 

## density plots of delay in rent payment, building.
dat  %>%
  filter(late_same > 0, late_amount > 0, late_duration > 0) %>% 
  ggplot(aes(y = name, x = late_amount, fill = name)) +
  geom_density_ridges() +
  scale_y_discrete(expand = expansion(add = c(0, 1.8))) + 
  scale_fill_futurama() +
  theme_bw() +
  theme(legend.position = "none") + 
  xlab("Mean delayed rent amount (USD)") + 
  ylab("") + 
  labs(title = "Mean delayed rent amount (USD) per tenant, stratified by location.")

## scatterplot of delay in rent payment vs amount of rent due
dat  %>%
  filter(late_same > 0, late_amount > 0, late_duration > 0) %>% 
  ggplot(aes(x = late_duration, y = late_amount, color = name)) + 
  geom_point() +
  scale_fill_futurama() +
  theme_bw() +
  theme(legend.position = "none") + 
  xlab("Median delay in rent payment (# of days)") + 
  ylab("Mean delayed rent amount (USD)") + 
  labs(title = "Scatterplot of mean amount of delayed rent and  median delay in paying rent (in days) stratified by location.")



