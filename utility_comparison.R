require("tidyverse")
require("scales")

save_dir <- "./plots/"

pi <- function(x, func = "Tversky and Kahneman") {
  if (func == "Tversky and Kahneman") {
    delta <- 0.65
    x^delta / (x^delta + (1 - x)^delta)^(1 / delta)
  } else if (func == "Prelec") {
    alpha <- 0.65
    beta <- 1
    exp(-beta * (-log(x))^alpha)
  }
}

v <- function(x, func = "Tversky and Kahneman") {
  if (func %in% c("Tversky and Kahneman", "Prelec")) {
    alpha <- 0.88
    lambda <- 2.25
    ifelse(x >= 0, x^alpha, -lambda * (-x)^alpha)
  }
}

utility_top_k <- function(theory, n, total, fee, top_k_weighting, top_k_ratio,
                          func) {
  if (top_k_ratio <= 0 || top_k_ratio > 100) {
    stop("top_k_ratio must be between 0 and 100 [%]")
  }
  k <- ceiling(n * top_k_ratio / 100)

  if (top_k_weighting == "linear") {
    denom <- k * (k + 1) / 2
    if (theory == "EUT") {
      1 / n * sum(total * (k:1) / denom - fee) + (1 - k / n) * (-fee)
    } else if (theory == "CPT") {
      pi(1 / n, func) * sum(v(total * (k:1) / denom - fee, func)) +
        pi(1 - k / n, func) * v(-fee, func)
    }
  } else if (top_k_weighting == "exponential") {
    if (theory == "EUT") {
      1 / n * sum(total * (1 / 2)^(1:k) - fee) + (1 - k / n) * (-fee)
    } else if (theory == "CPT") {
      pi(1 / n, func) * sum(v(total * (1 / 2)^(1:k) - fee), func) +
        pi(1 - k / n, func) * v(-fee, func)
    }
  }
}

calc_utility <- function(theory, mechanism, n, r, fee, func,
                         top_k_weighting, top_k_ratio) {
  total <- n * (1 - r) * fee
  if (theory == "EUT") {
    # under expected utility maximizer
    if (mechanism == "winner-take-all") {
      1 / n * (total - fee) + (1 - 1 / n) * (-fee)
    } else if (stringr::str_detect(mechanism, "^top")) {
      utility_top_k(theory, n, total, fee, top_k_weighting, top_k_ratio, func)
    } else if (mechanism == "three bands") {
      if (n < 50) {
        NA
      } else {
        1 / n * (total / 3 - fee) +
          9 / n * (total / 3 / 9 - fee) +
          40 / n * (total / 3 / 40 - fee) +
          (1 - 50 / n) * (-fee)
      }
    }
  } else if (theory == "CPT") {
    # under cumulative prospect theory
    if (mechanism == "winner-take-all") {
      pi(1 / n, func) * v(total - fee, func) +
        pi(1 - 1 / n, func) * v(-fee, func)
    } else if (stringr::str_detect(mechanism, "^top")) {
      utility_top_k(theory, n, total, fee, top_k_weighting, top_k_ratio, func)
    } else if (mechanism == "three bands") {
      if (n < 50) {
        NA
      } else {
        pi(1 / n, func) * v(total / 3 - fee, func) +
          pi(9 / n, func) * v(total / 3 / 9 - fee, func) +
          pi(40 / n, func) * v(total / 3 / 40 - fee, func) +
          pi(1 - 50 / n, func) * v(-fee, func)
      }
    }
  }
}

################################################################################
## utility versus number of participants (with different k) ##
################################################################################
# theory: whether participants' behavior is assumed to be EUT or CPT
# mechanism: game rule that determines how rewards are given to participants
# n: number of participants
# r: how much an organizer takes
# fee: fee paid by a participant
# func: which value and weighting functions are used
# top-k: k denotes how much participants would receive prizes in %
# top-k-weighting: how prizes are split in top-k mechanisms
parameters <- expand.grid(
  theory = "CPT",
  top_k_ratio = 1:100,
  top_k_weighting = c("linear", "exponential"),
  n = 1:200,
  r = 0.1,
  fee = 1,
  func = "Tversky and Kahneman"
) %>%
  dplyr::rowwise() %>%
  dplyr::mutate(
    mechanism =
      paste0("top ", top_k_ratio, "% (", top_k_weighting, ")")
  ) %>%
  dplyr::select(
    theory, mechanism, n, r, fee, func, top_k_weighting,
    top_k_ratio
  )

tmp <- parameters %>%
  purrr::pmap(calc_utility) %>%
  unlist()
utility_participants <- bind_cols(parameters, utility = tmp)

p <- utility_participants %>%
  dplyr::filter(
    n %% 10 == 0,
    top_k_ratio %in% c(1, 15, 30, 60, 90, 100)
  ) %>%
  dplyr::mutate(
    top_k_ratio =
      factor(percent(top_k_ratio, scale = 1),
        levels = percent(sort(top_k_ratio), scale = 1)
      )
  ) %>%
  ggplot(aes(
    x = n, y = utility, shape = top_k_ratio, color = top_k_ratio,
    group = top_k_weighting
  )) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray60") +
  # geom_line() +
  geom_point() +
  scale_x_continuous(n.breaks = 5) +
  xlab("Number of participants") +
  ylab("Utility") +
  theme(
    legend.title = element_blank(),
    legend.position = "bottom",
    # legend.text = element_text(size = 6),
    legend.margin = margin(t = 0, unit = "cm")
  ) +
  guides(
    shape = guide_legend(nrow = 1, byrow = TRUE),
    color = guide_legend(nrow = 1, byrow = TRUE)
  ) +
  facet_wrap(. ~ top_k_weighting, scales = "fixed")

ggsave(
  filename = paste0(save_dir, "utility_participants_rank_weight.pdf"), plot = p,
  device = cairo_pdf, width = 4.5, height = 3
)

################################################################################
## average utility versus number of participants (with different k)           ##
################################################################################
p <- utility_participants %>%
  dplyr::group_by(top_k_ratio, top_k_weighting) %>%
  dplyr::summarize(utility = mean(utility)) %>%
  ggplot(aes(x = top_k_ratio, y = utility, color = top_k_weighting)) +
  # geom_vline(xintercept = 6, linetype = "dashed", color = "gray60") +
  # geom_vline(xintercept = 16, linetype = "dashed", color = "gray60") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray60") +
  geom_line() +
  # geom_text(x = 6, y = 2, label = "6%", color = "gray60") +
  # geom_text(x = 16, y = 2, label = "16%", color = "gray60") +
  # ylim(-7, 2.5) +
  xlab("Top-k [%]") +
  ylab("Average utility") +
  theme(
    legend.title = element_blank(),
    legend.position = "bottom",
    legend.margin = margin(t = 0, unit = "cm")
  )

ggsave(
  filename = paste0(save_dir, "utility_participants_rank_weight_average.pdf"),
  plot = p, device = cairo_pdf, width = 4, height = 3
)

# find ks that give us the highest average utility
utility_participants %>%
  dplyr::group_by(top_k_ratio, top_k_weighting) %>%
  dplyr::summarize(utility = mean(utility), .groups = "drop") %>%
  dplyr::group_by(top_k_weighting) %>%
  dplyr::slice(which.max(utility))

################################################################################
## utility versus number of participants (with different v and p, EUT vs CPT) ##
################################################################################
parameters <- expand.grid(
  theory = factor(c("CPT", "EUT"), levels = c("CPT", "EUT")),
  mechanism = c(
    "winner-take-all",
    "top 16% (linear)",
    "top 6% (exponential)",
    "three bands"
  ),
  n = 1:200,
  r = 0.1,
  fee = 1,
  func = c("Tversky and Kahneman", "Prelec")
) %>%
  # extract top_k_ratio for top-k
  dplyr::rowwise() %>%
  dplyr::mutate(
    top_k_ratio = ifelse(
      stringr::str_detect(mechanism, "^top"),
      stringr::str_extract(mechanism, "\\d+") %>% as.numeric(),
      NA
    ),
    top_k_weighting = ifelse(
      stringr::str_detect(mechanism, "^top"),
      stringr::str_match(mechanism, "\\((\\w+)\\)") %>% .[2],
      NA
    )
  )

tmp <- parameters %>%
  purrr::pmap(calc_utility) %>%
  unlist()
utility_participants <- bind_cols(parameters, utility = tmp)

p <- utility_participants %>%
  ggplot(aes(x = n, y = utility, color = mechanism, linetype = theory)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray60") +
  geom_line() +
  xlab("Number of participants") +
  ylab("Utility") +
  scale_linetype_manual(values = c("solid", "dotted")) +
  theme(
    legend.title = element_blank(),
    legend.position = "bottom",
    legend.text = element_text(size = 7),
    legend.box = "horizontal",
    legend.background = element_blank()
  ) +
  guides(
    shape = guide_legend(nrow = 2, byrow = TRUE),
    color = guide_legend(nrow = 2, byrow = TRUE)
  ) +
  facet_wrap(. ~ func, scales = "fixed")

ggsave(
  filename = paste0(save_dir, "utility_participants.pdf"), plot = p,
  device = cairo_pdf, width = 4.5, height = 3.5
)

# find the minimum number of participants to obtain a positive utility
utility_participants %>%
  dplyr::filter(
    func == "Tversky and Kahneman",
    theory == "CPT",
    utility > 0
  ) %>%
  dplyr::group_by(mechanism) %>%
  dplyr::slice(which.min(n)) %>%
  dplyr::select(mechanism, n)

################################################################################
## utility versus fee                                                         ##
################################################################################
parameters <- expand.grid(
  theory = "CPT",
  top_k_ratio = 16,
  top_k_weighting = "linear",
  n = 1:200,
  r = 0.1,
  fee = 10^(0:4),
  func = "Tversky and Kahneman"
) %>%
  dplyr::rowwise() %>%
  dplyr::mutate(
    mechanism =
      paste0("top ", top_k_ratio, "% (", top_k_weighting, ")")
  ) %>%
  dplyr::select(
    theory, mechanism, n, r, fee, func, top_k_weighting, top_k_ratio
  )

tmp <- parameters %>%
  purrr::pmap(calc_utility) %>%
  unlist()
utility_participants <- bind_cols(parameters, utility = tmp)

p <- utility_participants %>%
  dplyr::mutate(r = percent(r)) %>%
  dplyr::mutate(
    f = factor(comma(fee), levels = comma(sort(fee)))
  ) %>%
  ggplot(aes(
    x = n, y = utility, color = f
  )) +
  geom_line() +
  scale_y_continuous(
    trans = log10_trans(),
    breaks = trans_breaks("log10", function(x) 10^x),
    labels = trans_format("log10", math_format(10^.x))
  ) +
  xlab("Number of participants") +
  ylab("Utility") +
  theme(
    legend.title = element_blank(),
    legend.text = element_text(size = 8),
    legend.position = "bottom",
    legend.margin = margin(t = 0, unit = "cm")
  ) +
  guides(
    # shape = guide_legend(nrow = 1, byrow = TRUE),
    color = guide_legend(nrow = 1, byrow = TRUE)
  )

ggsave(
  filename = paste0(save_dir, "utility_f.pdf"), plot = p,
  device = cairo_pdf, width = 4, height = 3
)

################################################################################
## utility versus r                                                           ##
################################################################################
parameters <- expand.grid(
  theory = "CPT",
  top_k_ratio = 16,
  top_k_weighting = "linear",
  n = 1:200,
  r = c(0.05, 0.1, 0.3, 0.5, 0.7, 0.9),
  fee = 1,
  func = "Tversky and Kahneman"
) %>%
  dplyr::rowwise() %>%
  dplyr::mutate(
    mechanism =
      paste0("top ", top_k_ratio, "% (", top_k_weighting, ")")
  ) %>%
  dplyr::select(
    theory, mechanism, n, r, fee, func, top_k_weighting, top_k_ratio
  )


tmp <- parameters %>%
  purrr::pmap(calc_utility) %>%
  unlist()
utility_participants <- bind_cols(parameters, utility = tmp)

p <- utility_participants %>%
  dplyr::mutate(r = factor(percent(r), levels = percent(sort(r)))) %>%
  ggplot(aes(x = n, y = utility, color = r)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray60") +
  geom_line() +
  xlab("Number of participants") +
  ylab("Utility") +
  theme(
    legend.title = element_blank(),
    legend.text = element_text(size = 7),
    legend.position = "bottom",
    legend.margin = margin(t = 0, unit = "cm")
  ) +
  guides(
    # shape = guide_legend(nrow = 1, byrow = TRUE),
    color = guide_legend(nrow = 1, byrow = TRUE)
  )

ggsave(
  filename = paste0(save_dir, "utility_r.pdf"), plot = p,
  device = cairo_pdf, width = 4, height = 3
)

# find the number of participants required
utility_participants %>%
  dplyr::filter(utility > 0) %>%
  dplyr::group_by(r) %>%
  dplyr::slice(which.min(n)) %>%
  dplyr::select(r, n)

################################################################################
## Operator's profit                                                         ##
################################################################################
parameters <- expand.grid(
  theory = "CPT",
  top_k_ratio = 16,
  top_k_weighting = "linear",
  n = 1:50,
  r = c(0.05, 0.1, 0.2),
  fee = 1:2,
  func = "Tversky and Kahneman"
) %>%
  dplyr::rowwise() %>%
  dplyr::mutate(
    mechanism =
      paste0("top ", top_k_ratio, "% (", top_k_weighting, ")")
  ) %>%
  dplyr::select(
    theory, mechanism, n, r, fee, func, top_k_weighting, top_k_ratio
  )


tmp <- parameters %>%
  purrr::pmap(calc_utility) %>%
  unlist()
utility_participants <- bind_cols(parameters, utility = tmp)

p <- utility_participants %>%
  # Operator's profit is 0 if utility <= 0, otherwise r * n * fee
  dplyr::mutate(profit = ifelse(utility <= 0,
    0,
    r * n * fee
  )) %>%
  dplyr::mutate(
    f = factor(fee, levels = sort(fee)),
    r = factor(percent(r), levels = percent(sort(r)))
  ) %>%
  ggplot(aes(
    x = n, y = profit, color = r, linetype = f
  )) +
  geom_line() +
  xlab("Number of participants") +
  ylab("Operator's expected profit") +
  theme(
    # legend.title = element_blank(),
    legend.text = element_text(size = 8),
    legend.position = "bottom",
    legend.margin = margin(t = 0, unit = "cm")
  ) +
  guides(
    color = guide_legend(nrow = 1, byrow = TRUE),
    linetype = guide_legend(nrow = 1, byrow = TRUE)
  )

ggsave(
  filename = paste0(save_dir, "operator_profit.pdf"), plot = p,
  device = cairo_pdf, width = 4, height = 3
)

# find when a utility gets positive
utility_participants %>%
  dplyr::filter(utility > 0) %>%
  dplyr::group_by(fee, r) %>%
  dplyr::slice(which.min(utility)) %>%
  dplyr::select(r, fee, n)
