data <- read.csv("veriler.csv", stringsAsFactors = FALSE)

names(data)

depth_raw <- data[, 7]
ml_raw    <- data[, 10]

eq <- data.frame(
  Depth_km = depth_raw,
  ML       = ml_raw
)

eq <- subset(eq, !is.na(Depth_km) & !is.na(ML) & Depth_km > 0 & ML > 0)

summary(eq)


library(ggplot2)

p_scatter <- ggplot(eq, aes(x = Depth_km, y = ML)) +
  geom_point(alpha = 0.4) +
  geom_smooth(method = "lm", se = TRUE) +
  labs(
    title = "Relationship between Earthquake Depth and Local Magnitude (ML)",
    x = "Depth (km)",
    y = "Local Magnitude (ML)"
  ) +
  theme_minimal()

print(p_scatter)

ggsave("scatter_depth_vs_ML.png", plot = p_scatter, width = 7, height = 5, dpi = 300)

p_hist_ml <- ggplot(eq, aes(x = ML)) +
  geom_histogram(bins = 30, boundary = 0, closed = "left") +
  labs(
    title = "Distribution of Local Magnitude (ML)",
    x = "Local Magnitude (ML)",
    y = "Frequency"
  ) +
  theme_minimal()

print(p_hist_ml)
ggsave("hist_ML.png", plot = p_hist_ml, width = 7, height = 5, dpi = 300)

p_hist_depth <- ggplot(eq, aes(x = Depth_km)) +
  geom_histogram(bins = 30, boundary = 0, closed = "left") +
  labs(
    title = "Distribution of Earthquake Depth (km)",
    x = "Depth (km)",
    y = "Frequency"
  ) +
  theme_minimal()

print(p_hist_depth)
ggsave("hist_depth.png", plot = p_hist_depth, width = 7, height = 5, dpi = 300)

cor_result <- cor.test(eq$Depth_km, eq$ML, method = "pearson")
cor_result
