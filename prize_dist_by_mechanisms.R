require("tidyverse")

n_participants <- 100
mechanisms <- c(
  "winner-take-all",
  "top-k (linear weighting)",
  "top-k (exponential weighting)",
  "three bands"
)

prize_dist <- function(n, mechanism, k = 0.2) {
  if (mechanism == "winner-take-all") {
    c(1, rep(0, n - 1))
  } else if (mechanism == "top-k (linear weighting)") {
    n_prizes <- round(n * k)
    c((n_prizes:1) / (n_prizes * (n_prizes + 1) / 2), rep(0, n - n_prizes))
  } else if (mechanism == "top-k (exponential weighting)") {
    n_prizes <- round(n * k)
    c((1 / 2)^(1:n_prizes), rep(0, n - n_prizes))
  } else if (mechanism == "three bands") {
    if (n < 50) {
      NA
    } else {
      c(1 / 3, rep(1 / 3 / 9, 9), rep(1 / 3 / 40, 40), rep(0, n - 50))
    }
  }
}

parameters <- expand.grid(
  mechanism = mechanisms,
  n = n_participants
)
tmp <- parameters %>%
  purrr::pmap(prize_dist) %>%
  purrr::map_df(broom::tidy)

prize_distribution <- tibble(
  mechanism = rep(parameters$mechanism, each = n_participants),
  n = rep(1:n_participants, length(mechanisms)),
  prize = tmp$x
)

g <- prize_distribution %>%
  ggplot(aes(x = n, y = prize, group = mechanism)) +
  geom_bar(stat = "identity") +
  xlab("Rank") +
  ylab("Ratio") +
  scale_x_continuous(n.breaks = 10) +
  facet_wrap(. ~ mechanism, ncol = 2, scales = "free") +
  theme_bw()

ggsave(
  filename = "prize_dist_by_mechanisms.pdf", plot = g,
  width = 6, height = 4.5
)
