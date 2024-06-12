library(tidyverse)

# Load data

feature_importance_files <- c("/faststorage/project/ctdna_nn_F2024/data/feature_importance_1.csv")

load_df <- read_csv(feature_importance_files, id = "file")
#remove duplicate umi-errors
#load_df <- filter(load_df, ...1 > 5)

# if V1 or V2:
#load_df <- filter(load_df, model != "triCtx")

# If LocalGC outlier should not be included:
#load_df <- filter(load_df, mean_diff_loss<0.2)


n_features <- load_df$model %>%
  unique() %>%
  length() 
n_fold <- load_df$fold_idx %>%
  unique() %>%
  length() #- 1

alpha <- 0.05

q_fold <- qnorm(1 - alpha / (2 * n_features * n_fold))
#q_fold <- qnorm(1 - alpha / (2 * n_features))
#q_fold <- qnorm(1 - alpha / 2)



# Data wrangling ----------------------------------------------------------

feature_importance_df <- load_df %>%
  filter(!is.na(fold_idx)) %>%
  mutate(
    mean_diff_loss_upper_ci = mean_diff_loss + q_fold * sd_diff_loss / sqrt(n),
    mean_diff_loss_lower_ci = mean_diff_loss - q_fold * sd_diff_loss / sqrt(n)
  ) %>%
  group_by(model) %>%
  mutate(
    avg_mean_diff_loss = mean(mean_diff_loss),
    is_lowest_fold = mean_diff_loss == min(mean_diff_loss),
    is_significant = mean_diff_loss_lower_ci <= 0,
    is_lowest_fold_and_significant =
      case_when(
        (is_lowest_fold & is_significant) ~ TRUE,
        (is_lowest_fold & !is_significant) ~ FALSE,
        TRUE ~ NA
      )#,
  ) %>%
  ungroup() %>%
  mutate(
    model = fct_reorder(model, avg_mean_diff_loss),
    model_plot_name = fct_recode(model,
      "Strand" = "strand",
      "First in pair" = "firstInPair",
      "Read position" = "readIndex",
      "Tri-nucleotide context" = "triCtx2",
      "Fragment length" = "fragmentSize",
      "UMI group size" = "umiCount",
      "Local GC %" = "localGC",
      "# Errors in UMI group" = "umiErrors",
      "Local 1-mer complexity" = "localComp1",
      "Local 2-mer complexity" = "localComp2",
      #"# Other errors in read" = "umiErrors",
      "# Deletions in read" =  "deletionsRead",
      "# Insertions in read" = "insertionsRead",
      "Sequence length" = "seqLength"
    )
  )

# Ranking -----------------------------------------------------------------



# Ranking of features
ranking <- feature_importance_df %>%
  group_by(model_plot_name, model) %>%
  summarise(
    avg_mean_diff_loss = mean(mean_diff_loss)
  ) %>%
  arrange(-avg_mean_diff_loss)
ranking

features <- ranking$model
for (i in 1:length(features)) {
  cat(paste0("\"model_", i, "\": \"", paste0(features[1:i], collapse = "|"), "\",\n"))
}




# Plot --------------------------------------------------------------------

feature_importance_df %>%
  ggplot(
    aes(
      x = model_plot_name,
      y = mean_diff_loss,
      group = fold_idx
    )
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
  geom_point(
    position = position_dodge(width = 0.75),
    col = "grey"
  ) +
  geom_hline(yintercept = 0, lty = 2) +
  geom_vline(
    xintercept = 2:14 - 0.5,
    col = "grey",
    lwd = 0.5
  ) +
  # Mean annotations
  geom_point(
    aes(
      x = model_plot_name,
      y = avg_mean_diff_loss
    ),
    data = ranking,
    inherit.aes = FALSE,
    size = 2
  ) +
  geom_errorbarh(
    data = ranking,
    aes(
      y = avg_mean_diff_loss,
      xmin = as.numeric(model_plot_name) + 0.35,
      xmax = as.numeric(model_plot_name) - 0.35
    ),
    height = 0,
    inherit.aes = FALSE,
    lwd = 1
  ) +
  labs(
    x = "Feature of interest",
    y = "Mean increase in validation error"
  ) +
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
    axis.ticks.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank()
  ) +
  guides(
    shape = guide_legend(title = "CV fold index"),
    size = guide_legend(title = "CV fold index")
  ) +
  coord_cartesian(ylim = c(NA, 0.09), xlim = c(NA, 13 - 0.1)) +
  annotate(
    geom = "text", label = "\U2190 Least important",
    x = -Inf, y = Inf, hjust = -0.1, vjust = 2,
    size = 3
  ) +
  annotate(
    geom = "text", label = "Most important \U2192",
    x = Inf, y = Inf, hjust = 1.1, vjust = 2,
    size = 3
  )


# Save plot ---------------------------------------------------------------

ggsave(
  filename = "/faststorage/project/ctdna_nn_F2024/Plots/feature_importanc_final_alt.png",
  width = 17/2,
  height = 12,
  units = "cm"
)
