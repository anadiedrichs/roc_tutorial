
# fuente https://sydykova.com/post/2019-03-12-make-roc-curves-tidyverse/

# load tidyverse and tidymodels packages
library(tidyverse)
library(broom)
library(yardstick)

# set the second level of truth to positive in yardstick
options(yardstick.event_first = FALSE)

# load cowplot to change plot theme
library(cowplot)

# get `biopsy` dataset from `MASS`
data(biopsy, package = "MASS")

# change column names from `V1`, `V2`, etc. to informative variable names
colnames(biopsy) <-
  c(
    "ID",
    "clump_thickness",
    "uniform_cell_size",
    "uniform_cell_shape",
    "marg_adhesion",
    "epithelial_cell_size",
    "bare_nuclei",
    "bland_chromatin",
    "normal_nucleoli",
    "mitoses",
    "outcome"
  )


# fit a logistic regression model to predict tumor types
glm_out1 <- glm(
  formula = outcome ~ clump_thickness +
    uniform_cell_shape +
    marg_adhesion +
    bare_nuclei +
    bland_chromatin +
    normal_nucleoli,
  family = binomial,
  data = biopsy
) %>%
  augment() %>%
  mutate(model = "m1") # name the model

# fit a different logistic regression model to predict tumor types
glm_out2 <- glm(outcome ~ clump_thickness,
                family = binomial,
                data = biopsy
) %>%
  augment() %>%
  mutate(model = "m2") # name the model

# combine the two datasets to make an ROC curve for each model
glm_out <- bind_rows(glm_out1, glm_out2)

glm_out %>%
  group_by(model) %>% # group to get individual ROC curve for each model
  roc_curve(truth = outcome, .fitted) %>% # get values to plot an ROC curve
  ggplot(
    aes(
      x = 1 - specificity, 
      y = sensitivity, 
      color = model
    )
  ) + # plot with 2 ROC curves for each model
  geom_line(size = 1.1) +
  geom_abline(slope = 1, intercept = 0, size = 0.4) +
  scale_color_manual(values = c("#48466D", "#3D84A8")) +
  coord_fixed() +
  theme_cowplot()


glm_out %>%
  group_by(model) %>% # group to get individual AUC value for each model
  roc_auc(truth = outcome, .fitted)


