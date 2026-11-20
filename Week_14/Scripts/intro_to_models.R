### Intro to Models
### Created by: Wei Shen Lim
### Created on: 2026-11-19
##########################################################################

### Load Libraries
library(tidyverse)
library(here)
library(palmerpenguins)
library(broom) #for clean model output
library(performance) #to check model assumptions
library(modelsummary) #to show model output
library(tidymodels) #for tidy models
library(parsnip) #for tidy models
library(flextable) #to look at model results in a nice table
library(wesanderson) #color palettes for modelplot
library(pushoverr) #sends push notifications to your phone from R!

########################### Basic Linear Model ################################
mod <- lm(y ~ x, data = df)

# Multiple regression
mod <- lm(y ~ x1 + x2, data = df)

# Interaction term
mod <- lm(y ~ x1 * x2, data = df)

######################### Model the penguin dataset ###########################
penguins |> 
  ggplot(aes(y = bill_length_mm, x = bill_depth_mm, color = species)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(color = "Species",
       y = "Bill Length (mm)",
       x = "Bill Depth (mm)") +
  theme_bw()

# Linear model of bill length ~ bill depth, with an interaction by species
Peng_mod <- lm(bill_length_mm ~ bill_depth_mm * species, data = penguins)

# Always check model assumptions with {performance}
# This covers everything!
check_model(Peng_mod)

### View results: base R
# ANOVA table (for significant effects)
# anova() uses Type I test, so order affects results
# Type II or III tests from car::Anova(Peng_mod, type = 2) don't depend on order
anova(Peng_mod)

# Summary output (for effect sizes)
summary(Peng_mod)

### View results: with {broom}
# Tidy coefficients
coeffs <- tidy(Peng_mod) #just put tidy() around the model
coeffs #this is the output from summary

# tidy() also works on an ANOVA table
sigeffect <- tidy(anova(Peng_mod)) #put tidy() around the anova
sigeffect #this is a clean output from the anova

# glance() extracts R-squared, AIC, and other model-level statistics
# Model-level statistics (from summary)
results <- glance(Peng_mod)
results

# augment() adds residuals and predicted values to your original data
# Residuals and fitted values
resid_fitted <- augment(Peng_mod)
resid_fitted

# Always check how many observations your model actually used
nobs(Peng_mod)

### View results: with {modelsummary}
# New model without the interaction
Peng_mod_noX <- lm(bill_length_mm ~ bill_depth_mm, data = penguins)

# Make a named list of models
models <- list("Model with interaction" = Peng_mod,
               "Model with no interaction" = Peng_mod_noX)

# Save the results as a .docx
modelsummary(models, output = here("Week_14", "Output", "table.docx"))

# Canned coefficient modelplots, uses {wesanderson} package
modelplot(models) +
  labs(x = "Coefficients",
       y = "Term names") +
  scale_color_manual(values = wes_palette("Darjeeling1"))

################### Many models with purrr, dplyr, and broom ##################
models <- penguins |> 
  nest(.by = species) |> #nest all the data by species
  mutate(fit = map(data, ~lm(bill_length_mm ~ body_mass_g, data = .x))) #one model per species
models

# Show the list of models
models$fit #show you each of the 3 models

# View results
# Mutate models to have a tidy coefficient dataframe first (using tidy())
# And a tidy model-results dataframe (using glance())
# Then select what we want to show and unnest it back into a dataframe
results <- models |> 
  mutate(coeffs = map(fit, tidy), #coefficients
         modelresults = map(fit, glance)) |> #R2 and others
  select(species, coeffs) |> #only keep the coeffcients
  unnest(cols = coeffs) #back to a dataframe
results

############################### {tidymodels} ##################################
lm_mod <- linear_reg() |> #specify the functional form
  set_engine("lm") |> #set the engine for the type of lm
  fit(bill_length_mm ~ bill_depth_mm * species, data = penguins) |> #model fit
  tidy(conf.int = TRUE) |> #adds 95% confidence intervals
  filter(term != "(Intercept)") |> #the intercept dwarfs the other terms
  ggplot(aes(x = estimate, y = term)) +
  geom_vline(xintercept = 0, linetype = "dashed") + # to see if relationship is +ve or -ve
  geom_point() +
  geom_errorbar(aes(xmin = conf.low, xmax = conf.high), width = 0.1)
lm_mod

### {pushoverr} sends push notifications to your phone from R!
# Will have to follow direcitons on the webiste to install the app on your phone
# But then one line of code does it
pushover("Wei Shen - your code is done.")