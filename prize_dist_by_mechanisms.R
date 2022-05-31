require("tidyverse")
save_dir <- "./plots/"

print(system("ls -al"))
n_participants <- 100
mechanisms <- c(
  "winner-take-all",
  "top-k (linear weighting)",
  "top-k (exponential weighting)",
  "three bands"
)

prize_dist <- function(n, mechanism, r = 0.2) {
  if (mechanism == "winner-take-all") {
    c(1, rep(0, n - 1))
  } else if (mechanism == "top-k (linear weighting)") {
    k <- round(n * r)
    c((k:1) / (k * (k + 1) / 2), rep(0, n - k))
  } else if (mechanism == "top-k (exponential weighting)") {
    k <- round(n * r)
    c((1 / 2)^(1:k), rep(0, n - k))
  } else if (mechanism == "three bands") {
    if (n < 50) {
      NA
    } else {
      c(1 / 3, rep(1 / 3 / 9, 9), rep(1 / 3 / 40, 40), rep(0, n - 50))
    }
  }
}

parameters <- expand.grid(
  n = n_participants,
  mechanism = mechanisms
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
  facet_wrap(. ~ mechanism, ncol = 2, scales = "free")

ggsave(
  filename = paste0(save_dir, "prize_dist_by_mechanisms.pdf"),
  plot = g, width = 6, height = 4.5
)
