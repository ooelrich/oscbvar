############################################################
### EXPLORING DIFFERENT SMOOTHING SPACES                 ###
############################################################

# Three obvious versions that do not require more work: time, the
# three continuous ones (temp, hum, windspeed), or all four.

sotw_time <- data.frame(
    t = bikes_d_log$t,
    time = bikes_d_log$t / sqrt(var(bikes_d_log$t))
)

cwl <- seq(0.01, 0.2, by = 0.01)
plt <- plot_no_in_caliper(cwl, df = sotw_time, first_t = 401, percentage = TRUE)
ggsave("temp/in_caliper_time.pdf", plt)

# Only including time leads to a constant rolling window (obviously), the
# length of which depends on the caliper width

sotw_cont <- subset(bikes_d_log, select = c(t, temp, hum, windspeed))
cwl <- seq(0.01, 0.2, by = 0.01)
plt_cont <- plot_no_in_caliper(cwl, df = sotw_cont, first_t = 401, percentage = TRUE)
ggsave("temp/in_caliper_cont.pdf", plt_cont)


sotw_all <- cbind(
    subset(bikes_d_log, select = c(t, temp, hum, windspeed)),
    time = bikes_d_log$t / sqrt(var(bikes_d_log$t))
)
cwl <- seq(0.01, 0.2, by = 0.01)
plt_all <- plot_no_in_caliper(cwl, df = sotw_all, first_t = 401, percentage = TRUE)
ggsave("temp/in_caliper_all.pdf", plt_all)
