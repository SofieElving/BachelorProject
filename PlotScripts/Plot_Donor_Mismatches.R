# Script for plotting count of matches and mismatches for each donor (anonymized).

library(tidyverse)
library(ggplot2)

data <- read_csv("~/ctdna_nn_F2024/data/training_data_indels.csv",
                 name_repair = function(x) gsub("data\\.", "", x))
                 

# Regular expression pattern to match everything before the first underscore
pattern <- '([^_]*)_.+'
data$qname <- sapply(data$qname, function(x) sub(pattern, '\\1', x))


# Make table of counts for each donor
count <- table(data$qname)

# Make mismatch subset and count these for each donor
mismatches <- subset(data, obs %in% c("D", "I"))
mm_count <- table(mismatches$qname)

# Combine in data frame
df <- data.frame(
  qname = as.character(names(count)),
  count = as.numeric(count),
  mm = as.numeric(mm_count[match(names(count), names(mm_count))])
)


### Donor anonymization ###
# Extract donor numbers
df$qname <- gsub("Donor ", "", df$qname)
df$qname <- as.numeric(df$qname)

# Rename donors to sequential numbering
df$qname <- paste("Donor", seq_along(df$qname))

# Convert qname to factor with custom levels
df$qname <- factor(df$qname, levels = unique(df$qname))


# Reshape the data into long format
df_long <- pivot_longer(df, cols = c(count, mm), names_to = "variable", values_to = "value")

# Plot and save
png(file="~/ctdna_nn_F2024/Plots/donor_mm_count_anonymous_trans.png",                    
width=1000, height=700)

ggplot(df_long, aes(fill=variable, y=value, x=qname, alpha=ifelse(variable != "mm", 1, 0.9))) + 
    geom_col(position="identity", stat="identity") +
    labs(x = "Donor", y = "Value", fill = "") +
    ggtitle("Distribution of Mismatches for each Donor") +
    scale_fill_manual(values = c("count" = "darkslategray3", "mm" = "lightcoral"), labels = c("Number of Total Observations", "Number of Mismatches")) + # Customize colors
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1), plot.title=element_text(size=37, hjust = 0.5, margin=margin(0,0,20,0)), axis.title=element_text(size=25), legend.text=element_text(size=25), legend.title=element_text(size=20), axis.text=element_text(size=13), legend.position = c(0.8, 0.9)) # Rotate x-axis labels for better readability

dev.off()

