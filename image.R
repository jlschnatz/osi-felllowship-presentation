library(tidyverse)
library(sysfonts)
library(showtext)

font_add_google("Open Sans", "os")
showtext_auto()
showtext_opts(dpi = 400)

# from Scheel et al. (2021)
pb_data <- tribble(
 ~type, ~outcome, ~n,
 "RR", "positive", 31,
 "RR", "negative", 40,
 "SR", "positive", 146,
 "SR", "negative", 6
)

data_ci <- tribble(
  ~type, ~lb, ~ub,
  "RR", 31.91, 55.95,
  "SR", 91.61, 98.54
)

pb_data |>
  mutate(n_total = sum(n), .by = type) |>
  mutate(pct = 100 * n / n_total) |>
  mutate(type = factor(type, levels = c("SR", "RR"))) |>
  ggplot(mapping = aes(x = type, y = pct, fill = outcome)) +
  geom_col(width = 0.3, alpha = .9) +
  geom_errorbar(
    mapping = aes(ymin = lb, ymax = ub, x = type), 
    data = data_ci, 
    inherit.aes = FALSE,
    linewidth = .5,
    width = 0.025
  ) +
  scale_y_continuous(
    name = "% of Papers",
    breaks = seq(0, 100, 10),
    expand = expansion()
  ) +
  scale_fill_manual(
    name = "First Hypothesis",
    values = c("#8F0047", "#377692"),
    labels = c("Not Supported", "Supported"),
    ) +
  scale_x_discrete(
    name = NULL,
    labels = c("Standard Reports", "Registered Reports")
  ) +
  sjPlot::theme_sjplot() +
  theme(
    legend.position = "bottom",
    text = element_text(family = "os")
  ) -> p

ggsave(
  filename = "img/scheel_etal_2021.png", 
  plot = p, 
  width = 5, height = 4, 
  dpi = 400,
  bg = "white"
  )

