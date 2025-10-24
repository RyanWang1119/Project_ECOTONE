library(mgcv)
library(dplyr)
library(gamlss)
library(gamlss.add) 
dat <- read.csv(here("data", "bc_hour_complete.csv"))
gam_data_clean$is_positive <- as.numeric(gam_data_clean$count > 0)

# Create a second dataset *only* for the positive counts
gam_data_positive <- filter(gam_data_clean, is_positive == 1)

# --- Fit Model 1: The Hurdle (Binomial) ---
hurdle_model_1 <- gam(
  is_positive ~ s(hour, bs = "cc") + day_of_week_f,
  family = binomial(),
  data = gam_data_clean,
  method = "REML"
)

# --- Fit Model 2: The Count (Truncated NB) ---
# We use family=nb() on the subsetted data. 
# This is a simple way to approximate a truncated NB.
hurdle_model_2 <- gam(
  count ~ s(hour, bs = "cc") + day_of_week_f + lag1 + lag2,  # <--- CORRECTED
  family = nb(),
  data = gam_data_positive,  # Using ONLY positive data
  method = "REML"
)

# --- To get the final fitted values ---
# 1. Predict the probability of a non-zero count
prob_positive <- predict(hurdle_model_1, newdata = gam_data_clean, type = "response")
# 2. Predict the average count *if* it's positive
#    (We need to be careful with 'lag' predictors in newdata)
mean_if_positive <- predict(hurdle_model_2, newdata = gam_data_clean, type = "response")
# 3. Final fitted mean = Prob(Positive) * Mean(If Positive)
final_fitted_hurdle <- prob_positive * mean_if_positive

ts.plot(dat_train,
        main = 'Hurdle GAM Fitted vs Actual',
        ylab = 'Count',
        col = "blue",
        lwd = 1)
lines(final_fitted_hurdle, col = "red", lty = 2, lwd = 2)
legend("topleft",
       lwd = c(1, 2),
       lty = c(1, 2),
       col = c("blue", "red"),
       legend = c("Actual Raw Data", "Fitted (Hurdle GAM)"))



# --- Use your 'gam_data_clean' data.frame ---
# (It should have hour, day_of_week_f, lag1, lag2)

gamlss_model <- gamlss(
  # Formula 1: For the Mean (mu)
  formula = count ~ pbc(hour) + day_of_week_f + lag1 + lag2, 
  
  # Formula 2: For the Dispersion (sigma)
  sigma.formula = ~ pbc(hour) + day_of_week_f,
  
  # --- THE FIX ---
  # Use the correct, exported family name: NBII()
  family = NBII(), 
  # ---
  
  data = gam_data_clean,
  n.cyc = 50 
)

# --- 1. Check the Summary ---
summary(gamlss_model)

# --- 2. Plot the Fitted Values ---
fitted_gamlss <- c(
  rep(NA, nrow(gam_data) - nrow(gam_data_clean)), 
  fitted(gamlss_model, what = "mu") # Plot the mean
)

# Plot
ts.plot(dat_train,
        main = 'Distributional GAMLSS Fitted vs Actual',
        ylab = 'Count',
        col = "blue",
        lwd = 1)
lines(fitted_gamlss, col = "red", lty = 2, lwd = 2)
legend("topleft",
       lwd = c(1, 2),
       lty = c(1, 2),
       col = c("blue", "red"),
       legend = c("Actual Raw Data", "Fitted (GAMLSS)"))
