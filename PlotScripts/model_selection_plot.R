library(tidyverse)
library(grid)


# Colors ------------------------------------------------------------------

LIGHT_ORANGE <- rgb(242, 178, 135, maxColorValue = 255)
ORANGE <- rgb(234, 127, 61, maxColorValue = 255)
LIGHT_GREEN <- rgb(170, 208, 145, maxColorValue = 255)
GREEN <- rgb(114, 171, 77, maxColorValue = 255)
LIGHT_BLUE = "#82DDF4" # rgb(219, 227, 242, maxColorValue = 255)
BLUE <- "#3dc4ea"# rgb(96, 156, 211, maxColorValue = 255)
DARK_BLUE <- "#2b89a4"
RED <- "#ea4d3d" # rgb(189, 21, 19, maxColorValue = 255)
DARK_RED <- "#a4362b"

COLOR_PALETTE <- c(LIGHT_ORANGE, ORANGE, LIGHT_GREEN, GREEN, LIGHT_BLUE, BLUE, DARK_BLUE, RED, DARK_RED)
# Show colors
plot(
  1:length(COLOR_PALETTE), 1:length(COLOR_PALETTE),
  pch = 19, cex = 10,
  main = "COLOR_PALETTE",
  col = COLOR_PALETTE
)


# Load data ---------------------------------------------------------------

load_df <- read_csv("/faststorage/project/ctdna_nn_F2024/data/model_selection1.csv")
n_models <- load_df$model %>%
  unique() %>%
  length()
n_fold <- load_df$fold_idx %>%
  unique() %>%
  length()
alpha <- 0.05

q_fold <- qnorm(1 - alpha / (n_models * n_fold))



feature_df <- data.frame(
  removed_feature = str_split(
#    string = "seq_length|strand|umi_count|fragment_size|first_in_pair|read_index|trinucleotide_ctx|local_complexity_2|local_complexity_1|n_deletions_in_read|n_insertions_in_read|umi_errors|Full model",
    string = "fragment_size|seq_length|trinucleotide_ctx|strand|read_index|local_complexity_2|local_GC|first_in_pair|umi_count|local_complexity_1|n_deletions_in_read|umi_errors|Full model",
    pattern = "\\|"
  )[[1]],
  model = paste0("model_", 1:13)
)


model_selection_df <- load_df %>%
  filter(!is.na(fold_idx)) %>%
  mutate(
    mean_diff_loss_upper_ci = mean_diff_loss + q_fold * sd_diff_loss / sqrt(n),
    mean_diff_loss_lower_ci = mean_diff_loss - q_fold * sd_diff_loss / sqrt(n)
  ) %>%
  group_by(model) %>%
  mutate(
    avg_mean_diff_loss = mean(mean_diff_loss),
    is_lowest_fold = mean_diff_loss == min(mean_diff_loss),
    is_significant = mean_diff_loss_lower_ci > 0,
    is_all_folds_significant = all(is_significant),
    model_idx = model %>% str_extract("\\d+") %>% as.numeric()
  ) %>%
  ungroup() %>%
  left_join(feature_df, by = "model") %>%
  mutate(
    removed_feature = ifelse(is.na(removed_feature), "ref", removed_feature),
    removed_feature = fct_reorder(removed_feature, -model_idx),
    remaining_features = model_idx,
    remaining_features = factor(remaining_features),
    remaining_features = fct_reorder(remaining_features, -model_idx),
    # Rename features for plot
    removed_feature =
      fct_recode(removed_feature,
        "Strand" = "strand",
        "First in pair" = "first_in_pair",
        "Read position" = "read_index",
        "Tri-nucleotide context" = "trinucleotide_ctx",
        "Fragment length" = "fragment_size",
        "UMI group size" = "umi_count",
        "Local GC %" = "local_GC",
        "# Errors in UMI group" = "umi_errors",
        "Local 1-mer complexity" = "local_complexity_1",
        "Local 2-mer complexity" = "local_complexity_2",
        "# Deletions in read" = "n_deletions_in_read",
        "# Insertions in read" = "n_insertions_in_read",
        "Sequence length" = "seq_length",
        "Full model (FM)" = "Full model"
      )
  )

performance <-
  model_selection_df %>%
  distinct(model, removed_feature, avg_mean_diff_loss, is_all_folds_significant, remaining_features)

# Plot --------------------------------------------------------------------

model_selection_df %>%
  ggplot(
    aes(
      x = removed_feature, y = mean_diff_loss,
      group = fold_idx
    )
  ) +
  geom_point(
    position = position_dodge(width = 0.75),
    col = "grey"
  ) +
  geom_errorbar(
    aes(
      ymin = mean_diff_loss_lower_ci,
      ymax = mean_diff_loss_upper_ci
    ),
    position = position_dodge(width = 0.75),
    width = 0.6,
    col = "grey"
  ) +
  # Vertical lines (separators)
  geom_vline(
    xintercept = 2:14 - 0.5,
    col = "grey",
    lwd = 0.5
  ) +
  geom_hline(yintercept = 0, lty = 2) +
  # Mean annotation
  geom_point(
    aes(
      x = removed_feature,
      y = avg_mean_diff_loss,
      color = is_all_folds_significant
    ),
    data = performance,
    inherit.aes = FALSE,
    size = 2
  ) +
  geom_errorbarh(
    data = performance,
    aes(
      y = avg_mean_diff_loss,
      xmin = as.numeric(removed_feature) + 0.35,
      xmax = as.numeric(removed_feature) - 0.35,
      color = is_all_folds_significant
    ),
    height = 0,
    inherit.aes = FALSE,
    lwd = 1
  ) +
  # Theme etc.
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
    axis.ticks.x = element_blank(),
    panel.background = element_rect(fill = NA),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank(),
    legend.position = c(0.02, 0.98),
    legend.justification = c(0, 1),
    legend.background = element_rect(fill = "white", color = "grey"),
    legend.title = element_blank()
  ) +
  # guides(
  #   color = guide_legend(title = "Compared to 'Full model'")
  # ) +
  scale_color_manual(
    breaks = c(TRUE, FALSE),
    labels = c("Worse than FM in all folds", "Similar to FM in \u22651 CV-fold"),
    values = c(DARK_BLUE, ORANGE)
  ) +
  labs(
    x = "Removed feature",
    y = "Mean increase in validation error"
  ) +
  coord_cartesian(ylim = c(-0.1, NA), xlim = c(NA, 13 - 0.1)) +
  geom_rect(
    xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = -0.015,
    fill = "white",
    col = NA
  ) +
  geom_hline(
    yintercept = -0.015, lwd = 0.25, col = "grey20"
  ) +
  annotate(
    geom = "text",
    label = "Remaining features", x = n_models/2 + 0.5, y = -0.02,
    size = 3, fontface = "italic",
    hjust = 0.5, vjust = 1.5
  ) +
  geom_text(
    aes(
      x = removed_feature,
      y = -.1,
      label = remaining_features
    ),
    inherit.aes = FALSE,
    vjust = 1,
    hjust = 0.5,
    data = performance %>% filter(!is.na(removed_feature)),
    size = 3
  )

# Save plot ---------------------------------------------------------------

ggsave(
  filename = "/faststorage/project/ctdna_nn_F2024/Plots/model_selection_alt.png",
  width = 17/2,
  height = 12,
  units = "cm"
)



# Stat test ---------------------------------------------------------------

model_selection_df %>%
  group_by(model) %>%
  summarise(
    t.test(x = mean_diff_loss, alternative = "greater")$p.value * 13
  )
