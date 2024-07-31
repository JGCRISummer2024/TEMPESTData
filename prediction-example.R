# Example of seeing how many individuals we need to predict others
# We use the built-in 'iris' dataset (see ?iris) and predicting
# petal length from petal width

library(dplyr)
library(tidyr)

set.seed(123)

# Try using 5, 10, ...45 individuals to predict the others
results <- list()
for (n in seq(from = 3, to = 45, by = 4)) {
  message(n)
  training_set <- sample(1:50, n, replace = FALSE)
  
  # Build one linear model for each species using the training data
  # and predict the validation (non-training) individuals
  for(sp in unique(iris$Species)) {
    message("\tModeling ", sp)
    iris_sp <- iris %>% filter(Species == sp)
    iris_sp$ID <- seq_len(nrow(iris_sp)) # Create ID column
    iris_sp$train <- iris_sp$ID %in% training_set
    training <- filter(iris_sp, ID %in% training_set)
    validation <- filter(iris_sp, !ID %in% training_set)

    # Fit models and predict
    m <- lm( Petal.Length ~ Sepal.Length, data = training)
    validation$Petal.Length.predicted <- predict(m, newdata = validation)
    validation$R2 <- summary(m)$adj.r.squared
    validation$n <- n
    results[[paste(n, sp)]] <- validation
  }
}

# Combine all our predicted data
results %>% 
  bind_rows() -> results

# Plot results
library(ggplot2)
p1 <- ggplot(results, aes(Petal.Length, Petal.Length.predicted, color = Species)) + geom_point() + facet_wrap(~n) + geom_abline()
print(p1)

results %>%
  group_by(Species, n) %>%
  summarise(R2 = summary(lm(Petal.Length ~ Petal.Length.predicted))$adj.r.squared) %>% 
  ggplot(aes(n, R2, color = Species)) + geom_point() + geom_line() ->
  p2
print(p2)


