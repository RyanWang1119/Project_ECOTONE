library(pscl)

gam_data <- data.frame(
  count = as.numeric(dat_train),      
  time_index = 1:n_train             
)

gam_data$hour <- (gam_data$time_index - 1) %% 24
gam_data$lag1 <- lag(gam_data$count, 1)
gam_data$lag2 <- lag(gam_data$count, 2)
gam_data_clean <- na.omit(gam_data)



zinb_model <- zeroinfl(
  count ~ lag1 + lag2 | factor(hour),  
  data = gam_data_clean,      
  dist = "negbin"
)

summary(zinb_model)
AIC(zinb_model)

fitted_counts <- predict(zinb_model, type = "count")
fitted_zinb_counts <- c(
  rep(NA, nrow(gam_data) - nrow(gam_data_clean)), 
  fitted_counts
)

fitted_zeros_prob <- predict(zinb_model, type = "zero")
fitted_zinb_zeros <- c(
  rep(NA, nrow(gam_data) - nrow(gam_data_clean)), 
  fitted_zeros_prob
)

tsplot(x = dat_time, y = dat_train,
       main = 'ZINB "Count" Component vs Actual',  # New title
       ylab = 'Count',
       col = "blue",
       lwd = 1)

lines(x = dat_time, y = fitted_zinb_counts, col = "red", lwd = 2)
legend("topleft",
       lwd = c(1, 2),
       lty = c(1, 2),
       col = c("blue", "red"),
       legend = c("Actual Raw Data", "Fitted (ZINB Count Model)"))

plot(x = dat_time, y = fitted_zinb_zeros, 
     type = "l",
     col = "darkgreen",
     lwd = 2,
     main = 'ZINB "Zero" Component: Probability of Structural Zero',
     ylab = "Probability",
     xlab = "Time",
     ylim = c(0, 1) # Force y-axis to be from 0 to 1
)

