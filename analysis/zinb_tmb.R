library(glmmTMB)

gam_data <- data.frame(
  count = as.numeric(dat_train),      
  time_index = 1:n_train             
)

gam_data$hour <- (gam_data$time_index - 1) %% 24
gam_data$lag1 <- lag(gam_data$count, 1)
gam_data$lag2 <- lag(gam_data$count, 2)
gam_data_clean <- na.omit(gam_data)

zinb_tmb_model <- glmmTMB(
  count ~ lag1 + lag2 + s(hour, bs = "cc"),   # Count model
  ziformula = ~ s(hour, bs = "cc"),          # Zero model
  data = gam_data_clean,
  family = nbinom2(link = "log")
)

summary(zinb_tmb_model)
AIC(zinb_tmb_model)
fitted_tmb_counts <- predict(zinb_tmb_model, type = "conditional")

fitted_zinb_counts_padded <- c(
  rep(NA, nrow(gam_data) - nrow(gam_data_clean)), 
  fitted_tmb_counts
)

tsplot(x = dat_time, y = dat_train,
       main = 'glmmTMB ZINB "Count" Component vs Actual',
       ylab = 'Count', col = "blue", lwd = 1)
lines(x = dat_time, y = fitted_zinb_counts_padded, col = "red", lwd = 2)

