library(tidyverse)

view(iris)

by_species <- iris %>%
  group_by(Species) %>%
  nest()

View(by_species)

specific_models <- tibble(
  Species = unique(iris$Species),
  model =list(function(d) lm(Sepal.Length~Petal.Width+Petal.Length, d),
              function(d) lm(Sepal.Length~Petal.Width, d),
              function(d) lm(Sepal.Length~Sepal.Width+Petal.Length, d)))

bob <- left_join(by_species, specific_models) %>% 
  rowwise() %>% 
  summarise(fit=list(model(data)),
            fit=list(broom::tidy(fit))) %>% unnest()

?rowwise


     
          