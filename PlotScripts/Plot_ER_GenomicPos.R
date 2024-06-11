# Script for making plots of Error rate for Genomic Position. Both for observed and predicted values.
# Also includes making a count plot of Genomic Position

library(tidyverse)
library(ggplot2)

### For observed:
data <- read_csv("~/ctdna_nn_F2024/data/training_data_indels.csv",
                 name_repair = function(x) gsub("data\\.", "", x))


# Read dataset with predictions
gp <- readRDS(file = "~/ctdna_nn_F2024/data/indels_data_with_pred.RData")


# Range of genomic_pos
gp2 <- subset(gp, genomic_pos >= 7675400 & genomic_pos <= 7675500)


### Get counts
# Count of observations
count <- table(gp2$genomic_pos)

# Count of mismatches in observations
mismatches <- subset(gp2, obs %in% c("D", "I"))
mm_count <- table(mismatches$genomic_pos)

# Count of mismatches of predictions
pred_mismatches <- subset(gp2, pred %in% c("D", "I"))
pred_mm_count <- table(pred_mismatches$genomic_pos)

# Combine to dataset
df <- data.frame(
  gp = as.integer(names(count)),
  count = as.numeric(count),
  mm = as.numeric(mm_count[match(names(count), names(mm_count))]),
  pred_mm = as.numeric(pred_mm_count[match(names(count), names(pred_mm_count))])
)

# Change NAs to 0
df$mm[is.na(df$mm)] <- 0
df$pred_mm[is.na(df$pred_mm)] <- 0

# Get beta for failrate of original dataset
beta <- data$info.beta[1]

# Calculate errors
df$error_p <- df$mm / df$count
df$pred_error <- df$pred_mm / df$count

# Calculate error rate of original dataset
df$error_beta <- (beta * df$error_p) / (beta * df$error_p - df$error_p + 1)
df$pred_error_beta <- (beta * df$pred_error) / (beta * df$pred_error - df$pred_error + 1)

# Avoid error rate of 1 by subsetting
df2 <- subset(df, count != mm & count != pred_mm)

# Plot and save
png(file="~/ctdna_nn_F2024/Plots/ER_gp_final3.png",
width=1000, height=700)
ggplot(df2, aes(x=gp)) +
  geom_line(aes(y=error_beta, color = "Observed"), linewidth = 0.7) +
  geom_line(aes(y=pred_error_beta, color = "Predicted"), linewidth = 0.7) +
  scale_color_manual(values = c("Observed" = "dodgerblue2", "Predicted" = "green")) +
  theme_bw() +
  ggtitle("Error Rate for Genomic Positions") +
  xlab("Genomic Position") +
  ylab("Error Rate") +
  theme(plot.title=element_text(size=37, hjust = 0.5, margin=margin(0,0,20,0)), axis.title=element_text(size=25), legend.text=element_text(size=25), legend.title = element_blank(), axis.text=element_text(size=17), legend.position = c(0.8, 0.9))
dev.off()



# Count Plot

# Make dataset longer
df_long <- pivot_longer(df2, cols = c(count, mm), names_to = "variable", values_to = "value")

png(file="~/ctdna_nn_F2024/Plots/ER_gp_count.png",
width=1000, height=700)
ggplot(df_long, aes(x=gp, y=value, fill=variable)) +
  geom_col(position="identity", stat="identity") +
  theme_bw() +
  scale_fill_manual(values = c("count" = "steelblue2", "mm" = "lightcoral"), labels = c("Number of Total Observations", "Number of Mismatches")) +
  ggtitle("Count for Genomic Positions") +
  xlab("Genomic Position") +
  ylab("Count") +
  theme(plot.title=element_text(size=37, hjust = 0.5, margin=margin(0,0,20,0)), axis.title=element_text(size=25), legend.text=element_text(size=25), legend.title = element_blank(), axis.text=element_text(size=17), legend.position = c(0.8, 0.9))
dev.off()




# Plot both together

options(repr.plot.width = 12, repr.plot.height = 6)

plot1 <- ggplot(df2, aes(x=gp)) +
  geom_line(aes(y=error_beta, color = "Observed"), linewidth = 0.7) +
  geom_line(aes(y=pred_error_beta, color = "Predicted"), linewidth = 0.7) +
  scale_color_manual(values = c("Observed" = "dodgerblue2", "Predicted" = "green")) +
  theme_bw() +
  #ggtitle("Error Rate for Genomic Positions") +
  xlab("Genomic Position") +
  ylab("Error Rate") +
  theme(plot.title=element_text(size=37, hjust = 0.5, margin=margin(0,0,20,0)), axis.title=element_text(size=25), legend.text=element_text(size=25), legend.title = element_blank(), axis.text=element_text(size=17), legend.position = c(0.8, 0.9))

plot2 <- ggplot(df_long, aes(x=gp, y=value, fill=variable)) +
  geom_col(position="identity", stat="identity") +
  theme_bw() +
  scale_fill_manual(values = c("count" = "steelblue2", "mm" = "lightcoral"), labels = c("Number of Total Observations", "Number of Mismatches")) +
  #ggtitle("Count for Genomic Positions") +
  #xlab("Genomic Position") +
  ylab("Count") +
  theme(plot.title=element_text(size=37, hjust = 0.5, margin=margin(0,0,20,0)), axis.title.y=element_text(size=25), axis.title.x = element_blank(), legend.text=element_text(size=25), legend.title = element_blank(), axis.text.y=element_text(size=17), axis.text.x = element_blank(), legend.position = c(0.75, 0.9))

figure <- ggarrange(plot2,plot1,nrow=2,ncol=1, align = "v")
ann <- annotate_figure(figure,
               top = text_grob("Count of Observations and Error Rate of Observations and Predictions", size = 30, hjust = 0.5))

ggsave("~/ctdna_nn_F2024/Plots/ER_gp_both_wide.png", ann, width = 17, height = 14)


