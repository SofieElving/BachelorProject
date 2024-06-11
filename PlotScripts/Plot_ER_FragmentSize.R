# Script for plotting Error rate and count of Fragment Size with Mono- and Di-Chromatosome indication.

library(tidyverse)
library(ggplot2)
library(ggpubr)

data <- read_csv("~/ctdna_nn_F2024/data/training_data_indels.csv",
                 name_repair = function(x) gsub("data\\.", "", x))
                 

feature <- data$fragment_size                                       

### Get counts
# Count of observations
count <- table(feature)

# Count of mismatches in observations
mismatches <- subset(data, obs %in% c("D", "I"))
mm_count <- table(mismatches$fragment_size)                          

# Combine to dataset
df <- data.frame(
  feature = as.integer(names(count)),
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

# Avoid error rate of 1 by subsetting
df2 <- subset(df, count != mm)


# Plot and save

options(repr.plot.width = 12, repr.plot.height = 6)

# Error rate plot
plot1 <- ggplot(df2, aes(x=feature, y=error_beta)) +
  geom_line(linewidth = 0.7, color = "dodgerblue2") +
  geom_vline(xintercept = 167, linetype="dashed", color ="red", linewidth = 0.8) +
  annotate(geom="label", x=167, y=0.0007, label="Mono-Chromatosome", angle=0, size=7, color="black", fill = "lightpink") +
  geom_vline(xintercept = 321, linetype="dashed", color="green", linewidth = 0.8) +
  annotate(geom="label", x=321, y=0.0007, label="Di-Chromatosome", angle=0, size=7, color="black", fill = "palegreen") +
  theme_bw() +
  scale_x_continuous(limits=c(0,800)) +
  coord_cartesian(ylim = c(0, 0.00075)) +
  #ggtitle("Error Rate of Fragment Size") +                            
  xlab("Fragment Size") +                                              
  ylab("Error Rate") + 
  theme(plot.title=element_text(size=37, hjust = 0.5, margin=margin(0,0,20,0)), axis.title=element_text(size=25), legend.text=element_text(size=25), legend.title=element_text(size=20), axis.text=element_text(size=17), legend.position = c(0.8, 0.9))

# Count plot  
plot2 <- ggplot(df2, aes(x=feature, y=count)) +
  geom_bar(stat="identity", fill = "lightblue") +
  geom_vline(xintercept = 167, linetype="dashed", color ="red", linewidth = 0.8) +
  geom_vline(xintercept = 321, linetype="dashed", color="green", linewidth = 0.8) +
  theme_bw() +
  scale_x_continuous(limits=c(0,800)) +
  #ggtitle("Count of Fragment Size") +                           
  xlab("Fragment Size") +                                          
  ylab("Count") + 
  theme(plot.title=element_text(size=37, hjust = 0.5, margin=margin(0,0,20,0)), axis.title.y=element_text(size=25), axis.title.x=element_blank(), legend.text=element_text(size=25), legend.title=element_text(size=20), axis.text.y=element_text(size=17), axis.text.x=element_blank(), legend.position = c(0.8, 0.9))

figure <- ggarrange(plot2,plot1,nrow=2,ncol=1, align = "v")
ann <- annotate_figure(figure,
               top = text_grob("Count and Error Rate of Fragment Size", size = 35, hjust = 0.5))

ggsave("~/ctdna_nn_F2024/Plots/ER_fragmentSize_both_new.png", ann, width = 19, height = 15)