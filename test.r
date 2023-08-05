library(gapminder)
library(ggplot2)
library(gganimate)

plot <- ggplot(gapminder, aes(gdpPercap, lifeExp, size = pop, col = country)) +
    geom_point(show.legend = FALSE, alpha = 0.75) +
    scale_x_log10() +
    scale_size(range = c(5, 24)) +
    facet_wrap(~continent) +
    labs(title = "Year: {frame_time}", x = "GDP per Capita", y = "Life Expectancy") +
    transition_time(year) +
    ease_aes("linear")

# anim_save("output.gif", plot, height = 1000, width = 1000, units = "px")
