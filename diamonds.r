library("ggplot2")
library(dplyr)
library(ggthemes)
library(ggridges)
library(caret)
data(diamonds)

options(scipen = 999)

corrected_diamonds <- diamonds %>%
    mutate(size = x * y * z) %>%
    select(carat, size, price, cut, color, depth)

p <- ggplot(corrected_diamonds, aes(price, cut, fill = stat(x))) +
    geom_density_ridges_gradient(scale = 3) +
    scale_fill_viridis_c(name = "Price", option = "C") +
    labs(title = "Prices of Different Diamond Cuts")

ggsave("diamonds.png", plot = p, width = 10, height = 10)

# density plot feature impact comparison
p2 <- featurePlot(
    x = corrected_diamonds[, c("depth", "size", "carat")],
    y = corrected_diamonds$cut,
    plot = "density",
    layout = c(3, 1),
    scales = list(
        x = list(relation = "free"),
        y = list(relation = "free")
    ),
    adjust = 1.5,
    auto.key = list(columns = 3)
)

# print(p2)
# trellis.device("png", file = "testcaret3.png", width = 1500, height = 800)
# print(p2)

# dev.off()

# boxplot feature comparison
p3 <- featurePlot(
    x = corrected_diamonds[, c("depth", "size", "carat")],
    y = corrected_diamonds$cut,
    plot = "box",
    layout = c(3, 1),
    scales = list(
        y = list(relation = "free"),
        x = list(rot = 90)
    ),
    adjust = 1.5,
    auto.key = list(columns = 3)
)

# correlation plots
p4 <- featurePlot(
    x = corrected_diamonds[, c("depth", "size", "carat")],
    y = corrected_diamonds$price,
    plot = "scatter",
    layout = c(3, 1),
    type = c("p", "smooth"),
    span = .5
)
print(p4)
