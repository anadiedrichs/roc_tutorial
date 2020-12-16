library(tidymodels)

data(two_class_example)

head(two_class_example)

pr_curve(two_class_example, truth, Class1)


library(ggplot2)
library(dplyr)
pr_curve(two_class_example, truth, Class2) %>%
  ggplot(aes(x = recall, y = precision)) +
  geom_path() +
  coord_equal() +
  theme_bw()


autoplot(pr_curve(two_class_example, truth, Class2))

# Ejemplo multiclase

hpc_cv %>%
  filter(Resample == "Fold01") %>%
  pr_curve(obs, VF:L) %>%
  autoplot()


hpc_cv %>%
  group_by(Resample) %>%
  pr_curve(obs, VF:L) %>%
  autoplot()
