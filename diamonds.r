library(ggplot2)
library(dplyr)
data(diamonds)

corrected_diamonds <- diamonds %>%
    mutate(size = x * y * z) %>%
    select(carat, size, price, cut, color)

p <- ggplot(corrected_diamonds, aes(size, price, color = cut)) +
    geom_point(alpha = 0.3) +
    scale_x_log10()

ggsave("diamonds.png", plot = p, width = 10, height = 10)
