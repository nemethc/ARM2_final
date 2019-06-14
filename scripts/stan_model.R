library(brms)
library(tidybayes)

source("scripts/leg_data.R")

m1priors <- c(
  prior(student_t(3, 0, 2.5), class = "Intercept"),
  prior(student_t(3, 0, 2.5), class = "b")
)

m1 <- brm(
  billpassed ~ Party,
  data = bill_data,
  #prior = m1priors,
  family = "bernoulli",
  seed = 123 # Adding a seed makes results reproducible.
) 

summary(m1)

parameters <- m1 %>% gather_draws(b_PartyR) %>% median_hdi()

print(exp(parameters[c(".value", ".lower", ".upper")]))

plot(marginal_effects(m1))

m2 <- lm(billpassed ~ Party, data = bill_data)

summary(m2)

full_brms <- brm()


