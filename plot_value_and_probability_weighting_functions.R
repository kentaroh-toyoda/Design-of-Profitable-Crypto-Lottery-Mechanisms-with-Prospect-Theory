require("tidyverse")

fig_dir <- "./plots/"

# v(x): a value function
alpha <- 0.88
lambda <- 2.25

# grapthics parameters
ratio <- 0.9
width <- 4 * ratio
height <- 3 * ratio

g <- ggplot() +
  xlim(-100, 100) +
  geom_function(
    fun =
      function(x) ifelse(x >= 0, x^alpha, -lambda * (-x)^alpha)
  ) +
  xlab("x") +
  ylab("v(x)")

ggsave(
  filename = paste0(fig_dir, "v_x.pdf"), plot = g,
  width = width, height = height
)

# pi(p): a probability weighting function
g <- ggplot() +
  xlim(0, 1) +
  geom_function(aes(linetype = "Prelec"),
    fun = function(p) {
      alpha <- 0.65
      beta <- 1
      exp(-beta * (-log(p))^alpha)
    }
  ) +
  geom_function(aes(linetype = "Tversky and Kahneman"),
    fun = function(p) {
      delta <- 0.65
      p^delta / (p^delta + (1 - p)^delta)^(1 / delta)
    }
  ) +
  geom_function(aes(linetype = "EUT"),
    fun = function(p) p
  ) +
  xlab("p") +
  ylab("w(p)") +
  theme(
    legend.justification = c(0, 0),
    legend.position = c(0, 0.55),
    legend.title = element_blank(),
    legend.key = element_blank(),
    legend.background = element_blank()
  )

ggsave(
  filename = paste0(fig_dir, "w_p.pdf"), plot = g,
  width = width, height = height
)
