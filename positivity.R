#  Positivity diagnosis 
summary(pi_hat)

# Propensity score overlap: Propensity scores by treatment group (cross-fitting)
plot1<-data.frame(pi_hat = pi_hat, A = factor(A, labels = c("No Exercise", "Exercise"))) %>%
  ggplot(aes(x = pi_hat, fill = A, color = A)) +
  geom_density(alpha = 0.3, linewidth = 0.8) +
  scale_fill_manual(values  = c("steelblue", "tomato")) +
  scale_color_manual(values = c("steelblue", "tomato")) +
  theme_classic() +
  labs(x     = expression(hat(g)(W)),
       y     = "Density",
       fill  = "Treatment",
       color = "Treatment",
       title = "A")

# Inverse weights: Distribution of inverse weights by treatment group (cross-fitting)
plot2<-data.frame(
  weight = c(1 / pi_hat[A == 1], 1 / (1 - pi_hat[A == 0])),
  A      = factor(c(rep(1, sum(A == 1)), rep(0, sum(A == 0))),
                  labels = c("No Exercise", "Exercise"))
) %>%
  ggplot(aes(x = A, y = weight, fill = A)) +
  geom_boxplot(alpha = 0.6, outlier.shape = 21) +
  scale_fill_manual(values = c("steelblue", "tomato")) +
  theme_classic() +   theme(legend.position = "none") +
  labs(x = NULL, y = "Inverse weight", fill = "Treatment",
       title = "B")