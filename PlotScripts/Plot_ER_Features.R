# Script for plotting Error rate of features. Change to wanted feature at # CHANGE.
# Can also make plots of count of features by using the geom_bar instead of geom_line and setting y = count.

library(tidyverse)
library(ggplot2)

data <- read_csv("~/ctdna_nn_F2024/data/training_data_indels.csv",
                 name_repair = function(x) gsub("data\\.", "", x))
                 

# Select feature at all # CHANGE
feature <- data$umi_count                                       # CHANGE

### Get counts
# Count of observations
count <- table(feature)

# Count of mismatches in observations
mismatches <- subset(data, obs %in% c("D", "I"))
mm_count <- table(mismatches$umi_count)                          # CHANGE

# Combine to data frame
df <- data.frame(
  feature = as.numeric(names(count)),
  count = as.numeric(count),
  mm = as.numeric(mm_count[match(names(count), names(mm_count))])
)

# Change NAs to 0
df$mm[is.na(df$mm)] <- 0

# Get beta for failrate of original dataset
beta <- data$info.beta[1]

# Calculate errors
df$error_p <- df$mm / df$count

# Calculate error rate of original dataset
df$error_beta <- (beta * df$error_p) / (beta * df$error_p - df$error_p + 1)

# Avoid error rate of 1 and make feature limitations by subsetting
df2 <- subset(df, count != mm & feature < 100)


# Plot and save
plot1 <- ggplot(df2, aes(x=feature, y=error_beta)) +    # change y=count for count plot
  geom_line(color = "dodgerblue2", linewidth = 0.7) +
  #geom_bar(stat="identity", fill = "lightblue") +      # for count plot
  theme_bw() +
  ggtitle("Error Rate of UMI Count") +                            # CHANGE
  xlab("UMI Count") +                                              # CHANGE
  ylab("Error Rate") +
  theme(plot.title=element_text(size=37, hjust = 0.5, margin=margin(0,0,20,0)), axis.title=element_text(size=25), legend.text=element_text(size=25), legend.title=element_text(size=20), axis.text=element_text(size=17), legend.position = c(0.8, 0.9))
  
ggsave("~/ctdna_nn_F2024/Plots/ER_umi_wide.png", plot1, width = 12, height = 8)









png(file="~/ctdna_nn_F2024/Plots/ER_localComp2_final.png",                     # CHANGE
width=1000, height=700)
ggplot(df2, aes(x=feature, y=error_beta)) +
  geom_line(color = "dodgerblue2", linewidth = 0.7) +
  #geom_bar(stat="identity", fill = "lightblue") +
  #geom_vline(xintercept = 167, linetype="dashed") +
  #geom_vline(xintercept = 321, linetype="dashed") +
  theme_bw() +
  ggtitle("Error Rate of Local 2-mer Complexity") +                            # CHANGE
  xlab("Local 2-mer Complexity") +                                              # CHANGE
  ylab("Error Rate") +
  theme(plot.title=element_text(size=37, hjust = 0.5, margin=margin(0,0,20,0)), axis.title=element_text(size=25), legend.text=element_text(size=25), legend.title=element_text(size=20), axis.text=element_text(size=13), legend.position = c(0.8, 0.9))
dev.off()