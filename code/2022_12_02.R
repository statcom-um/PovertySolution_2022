library(pacman)

`%notin%` <- Negate(`%in%`)

p_load(
  #--- Packages to Fit Models
  MASS, logistf, survival, fastR2, 
  #--- Packages to Produce Tables
  gtsummary, flextable, janitor, broom, officer, kableExtra, reactable, broom.mixed, lmerTest,
  #--- Packages to Produce Figures
  crayon, ggsci, ggridges, ggthemes, ggforce, ggpubr, patchwork, grid, gridExtra, plotly,  survminer, viridis, ggridges, hrbrthemes, stickylabeller, latex2exp, scales, glue, 
  #--- Packages for Data Retrieval & Pre-Processing
  readxl, here, rdrop2, lubridate, zoo, tidyverse, purrr, data.table, stringr, tidyr, table1
)

y <- read_csv(file.path(here(), "data", "created_data", "2022_11_21.csv")) %>% 
  group_by(Customer) %>% 
  summarise(tid = unique(Customer), total = n(), late = sum(!is.na(lateFees))) %>% 
  select(tid, total, late) %>% 
  rename(n_total = "total", 
         n_late = "late")

covariates <- read_csv(file.path(here(), "data", "created_data", "2022_07_31.csv")) %>% 
  rename(age = "Age on effective date of action", 
         sex = "Sex", 
         n_hh = "Total number in household", 
         n_dep = "Number of dependents", 
         n_br = "Number of bedrooms in unit", 
         income = "Adjusted annual income: 8a minus 8x (if 8x is larger, put 0)", 
         rent = "Tenant rent: 10d minus 10en If positive or 0, put tenant rent If negative, credit tenant") %>% 
  select(c(tid, age, sex, n_hh, n_dep, n_br, income, rent))

## some weird duplication issue, dropping duplicates for now
covariates <- covariates %>% 
  filter(tid %notin% 
           (covariates %>% group_by(tid) %>% summarise(n = n()) %>% filter(n > 1) %>% pull(tid)))

buildings <- read_csv(file.path(here(), "data", "created_data", "2022_03_21.csv")) %>% 
  select(c(tid, name, late_duration, late_amount))

data <- covariates %>% inner_join(y) %>% inner_join(buildings) %>% rowwise() %>% 
  mutate(n_hh = case_when(n_hh > 3 ~ factor("> 3",  levels = c("1", "2", "3", "> 3")),
                          TRUE ~ factor(n_hh, levels = c("1", "2", "3", "> 3"))), 
         n_dep = case_when(TRUE ~ factor(n_dep, levels = c("0", "1", "2"))),
         n_br = case_when(n_br >= 3 ~ factor(">=3",  levels = c("1", "2", ">=3")),
                          TRUE ~ factor(n_br, levels = c("1", "2", ">=3"))))

data <- data %>% drop_na() %>% 
  rowwise() %>%
  mutate(prop = wilson.ci(x = n_late, n = n_total, conf.level = 0.50)[1]) %>% 
  mutate(prop = case_when(prop < 0 ~ 0, 
                          prop > 1 ~ 1, 
                          TRUE ~ prop))

table1(~ age + sex + n_hh + n_dep + n_br + income| name, data %>% drop_na(), 
       render.continuous = c(.="Mean (SD)", 
                             .="Median (IQR)", 
                             .="[Min, Max]")) 

ggplotly(data %>%  
  ggplot(aes(y = name, x = age, fill = name)) + 
  geom_violin(width=1.4) +
  geom_boxplot(width=0.1, color="black") +
  theme_bw() + 
  xlab("Age (years)") + 
  ylab("") + 
  theme(legend.position = "bottom") + 
  labs(fill = "Building"))

data %>% 
  group_by(name, sex) %>% 
  summarize(n = n()) %>%
  drop_na() %>% 
  ggplot(aes(fill = sex, x = name, y = n)) + 
  geom_bar(position="fill", stat="identity") +
  theme_bw() + 
  ylab("Fraction") + 
  xlab("") + 
  theme(legend.position = "bottom",
        legend.box="vertical", legend.margin=margin(),
        axis.text.x = element_text(face = "bold", size = 18),
        axis.text.y = element_text(face = "bold", size = 18),
        plot.title = element_text(face = "bold", size = 18),
        plot.subtitle = element_text(size = 14),
        #axis.text = element_text(size = 12),
        axis.title = element_text(face = "bold", size = 18),
        legend.title = element_text(face = "bold", size = 18),
        legend.text = element_text(size = 18)) + 
  labs(fill = "Sex") + 
  coord_flip()


data %>% 
  group_by(name, n_hh) %>% 
  summarize(n = n()) %>% 
  ungroup() %>% 
  drop_na() %>% 
  ggplot(aes(fill = n_hh, x = name, y = n)) + 
  geom_bar(position="fill", stat="identity") +
  theme_bw() + 
  ylab("Fraction") + 
  xlab("") + 
  theme(legend.position = "bottom",
        legend.box="vertical", legend.margin=margin(),
        axis.text.x = element_text(face = "bold", size = 18),
        axis.text.y = element_text(face = "bold", size = 18),
        plot.title = element_text(face = "bold", size = 18),
        plot.subtitle = element_text(size = 14),
        #axis.text = element_text(size = 12),
        axis.title = element_text(face = "bold", size = 18),
        legend.title = element_text(face = "bold", size = 18),
        legend.text = element_text(size = 18)) + 
  labs(fill = "Sex") + 
  coord_flip()

op <- tidy(lmerTest::lmer(prop ~ -1 + age + (1 | name), data = data))
op[-c(nrow(op)-1, nrow(op)), ] %>% kbl() %>% kable_paper("hover", full_width = T)


