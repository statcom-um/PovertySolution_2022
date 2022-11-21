library(pacman)

`%notin%` <- Negate(`%in%`)

p_load(
  #--- Packages to Fit Models
  MASS, logistf, survival,
  #--- Packages to Produce Tables
  gtsummary, flextable, janitor, broom, officer, kableExtra, reactable, 
  #--- Packages to Produce Figures
  crayon, ggsci, ggridges, ggthemes, ggforce, ggpubr, patchwork, grid, gridExtra, plotly,  survminer, viridis, ggridges, hrbrthemes, stickylabeller, latex2exp, scales, glue, 
  #--- Packages for Data Retrieval & Pre-Processing
  readxl, here, rdrop2, lubridate, zoo, tidyverse, purrr, data.table, stringr, tidyr
)

### load old stuff, might need to redefine proportion code
source(file.path(here(), "code", "2022_10_12_scrape_rent_ledger.R"))

run = FALSE ### save running time
if(run == TRUE){
  ## find out how many times tenant had late payment (modifed from last time, had processing error)
  clean_rent_late_count = function(temp) {
    
    due <- temp %>% filter(Charges > 0) %>% filter(grepl("Tenant Rent", Charge)) %>% 
      mutate(y = year(Transaction), 
             m = month(Transaction)) %>% 
      select(Customer, y, m, Charges) %>% rename(rentFees = Charges)
    
    late <- temp %>% filter(Charges > 0) %>% filter(grepl("Tenant Late Charge", Charge)) %>% 
      mutate(y = year(Transaction), 
             m = month(Transaction), 
             d = day(Transaction)) %>% 
      select(Customer, y, m, d, Charges) %>% rename(lateFees = Charges)
    
    return(due %>% left_join(late))
    
  }
  
  rent_late <- group_split(my_tenants %>% group_by(Customer)) %>% 
    map_dfr(., clean_rent_late_count) 
}else{
  rent_late <- read_csv(file.path(here(), "data", "created_data", "2022_11_21.csv"))
}

prop <- rent_late %>% 
  group_by(Customer) %>% 
  summarise(tid = unique(Customer), total = n(), late = sum(!is.na(lateFees))) %>% 
  select(tid, total, late)

covariates <- read_csv(file.path(here(), "data", "created_data", "2022_07_31.csv"))

data <- covariates %>% 
  left_join(prop) %>% 
  left_join((my_tenants %>% select(Customer, Property) %>% rename(tid = Customer) %>% unique() %>% drop_na())) %>% 
  drop_na() 

data <- data %>% select(colnames(data)[c(1, 2, 3, 4, 5, 6, 7, 11, 12, 13, 14)])  
colnames(data) <- c("tid", "age", "sex", "num_hh", "num_dep", "num_br", "income", "rent", "total_trans", "late_trans", "property")

modified_p <- function(x, n){
  prop.test(x, n, conf.level = 0.50)$conf.int[2]
}

data <- data %>% 
  rowwise() %>% 
  mutate(mod_p = modified_p(late_trans, total_trans))

data %>% 
  ggplot(aes(x = property, y = mod_p, fill = property)) + 
  geom_violin()
