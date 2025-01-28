library(ggplot2)

# Create the plot
cutoff <- 0.5

df_plot <- subset(avg_ref, PAN_pct >= 0.5-rd2.2$bw[1] & PAN_pct <= 0.5+rd2.2$bw[1])



ggplot(df_plot, aes(x = PAN_pct, y = change_pp)) +
  geom_point(alpha = 0.5) +
  geom_vline(xintercept = cutoff, linetype = "dashed", color = "red") +
  geom_smooth(data = subset(df_plot, PAN_pct < cutoff), method = "loess", se = FALSE, color = "blue") +
  geom_smooth(data = subset(df_plot, PAN_pct >= cutoff), method = "loess", se = FALSE, color = "blue") +
  labs(title = "Regression Discontinuity Plot",
       x = "Running Variable (x)",
       y = "Outcome Variable (y)") +
  theme_minimal()

library(rdrobust)

rd3.2 <- rdrobust(y = avg_ref3$change_pp, x = avg_ref3$PAN_pct, c = 0.5, p = 1)
summary(rd3.2)

rdplot(y = df_plot$change_pp, x = df_plot$PAN_pct, c = 0.5, p = 1, binselect = "espr", nbins = 9)
rdplot(y = df_plot$change_pp, x = df_plot$PAN_pct, c = 0.5, p = 2, binselect = "espr", nbins = 9)
rdplot(y = df_plot$change_pp, x = df_plot$PAN_pct, c = 0.5, p = 3, binselect = "espr", nbins = 9)
rdplot(y = df_plot$change_pp, x = df_plot$PAN_pct, c = 0.5, p = 4, binselect = "espr", nbins = 9)

rdplot(y = avg_ref3$change_pp, x = avg_ref3$PAN_pct, c = 0.5, p = 3, nbins = 10)

m1 <- RDestimate(change_pp ~ PAN_pct, cutpoint = 0.5, data = avg_ref3, bw = 0.082)
summary(m1)
  
m2 <- rdrobust(y = avg_ref3$change_pp, x = avg_ref3$PAN_pct, c = 0.5, p = 1, bwselect = "mserd")
summary(m2)

m3 <- rdrobust(y = avg_ref3$change_pp, x = avg_ref3$PAN_pct, c = 0.5, p = 1, bwselect = "msetwo")
summary(m3)

m4 <- rdrobust(y = avg_ref3$change_pp, x = avg_ref3$PAN_pct, c = 0.5, p = 1, bwselect = "msesum")
summary(m4)

m5 <- rdrobust(y = avg_ref3$change_pp, x = avg_ref3$PAN_pct, c = 0.5, p = 1, bwselect = "msecomb2")
summary(m5)

m6 <- rdrobust(y = avg_ref3$change_pp, x = avg_ref3$PAN_pct, c = 0.5, p = 1, bwselect = "cerrd")
summary(m6)
