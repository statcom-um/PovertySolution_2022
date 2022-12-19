library(pacman)

`%notin%` <- Negate(`%in%`)

p_load(
  #--- Packages to Fit Models
  MASS, logistf, survival, ISLR2, tree, randomForest, gbm, 
  #--- Packages to Produce Tables
  gtsummary, flextable, janitor, broom, officer, kableExtra, reactable, 
  #--- Packages to Produce Figures
  crayon, ggsci, ggridges, ggthemes, ggforce, ggpubr, patchwork, grid, gridExtra, plotly,  survminer, viridis, ggridges, hrbrthemes, stickylabeller, latex2exp, scales, glue, 
  #--- Packages for Data Retrieval & Pre-Processing
  readxl, here, rdrop2, lubridate, zoo, tidyverse, purrr, data.table, stringr, tidyr, table1
)

data <- read_csv(file.path(here(), "data", "created_data", "2022_12_02.csv"))


## categorising proportion based on #of times/year. 
data <- data %>% 
  mutate(prop_cat = factor(case_when(prop <= 3/12 ~ "stage1", 
                                     prop <= 1 ~ "stage2"), 
                           levels = c("stage1", "stage2")), 
         sex = as.factor(sex), 
         n_br = as.factor(n_br), 
         n_dep = as.factor(n_dep), 
         name = as.factor(name))

## v basic tree fitting
tree.naive.model <- tree(prop_cat ~ age + sex + n_dep + n_br + rent + name, data = data)
summary(tree.naive.model) #lmao this is dogshit

## pruning the tree above
cv.tree.naive.model <- cv.tree(tree.naive.model, FUN = prune.misclass)
cv.tree.naive.model

## random forest 
rf.naive.model <- randomForest(prop_cat ~ age + sex + n_dep + n_br + rent + name, 
                               data = data, 
                               ntree = 30, 
                               importance = TRUE)
rf.naive.model  


## boosted trees
data.boost <- data %>% mutate(prop_cat = case_when(prop_cat == "stage1" ~ 0, 
                                                   prop_cat == "stage2" ~ 1))
b.naive.model <- gbm(prop_cat ~ age + sex + n_dep + n_br + rent + name, 
                     data = data.boost, 
                     distribution = "bernoulli", 
                     n.trees = 5000, interaction.depth = 3)
summary(b.naive.model)
