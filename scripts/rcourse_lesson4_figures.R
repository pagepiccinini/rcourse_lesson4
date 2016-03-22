## READ IN DATA ####
source("scripts/rcourse_lesson4_cleaning.R")


## LOAD PACKAGES ####
library(ggplot2)


## ORGANIZE DATA ####
data_figs = data_clean %>%
  # Change order and text of labels
  mutate(series = factor(series, levels=c("tos", "tng"),
                         labels = c("The Original Series", "The Next Generation")))

# Summarise data by series and alignment
data_figs_sum = data_figs %>%
  # Get percentages of extinect for each level of series and alignment 
  group_by(series, alignment) %>%
  summarise(perc_extinct = mean(extinct) * 100) %>%
  ungroup()
  

## MAKE FIGURES ####
extinct.plot = ggplot(data_figs_sum, aes(x = series, y = perc_extinct, fill = alignment)) +
  geom_bar(stat = "identity", position = "dodge") +
  # Set y-axis to range from 0 to 100
  ylim(0, 100) +
  # Add line for chance (50%)
  geom_hline(yintercept = 50) +
  # Manually set the colors for the bars
  scale_fill_manual(values = c("red", "yellow")) +
  # Add a title
  ggtitle("Percentage of Possibly Extinct Aliens\nby Series and Alignment") +
  # Customize the x-axis
  xlab("Star Trek series") +
  # Customize the y-axis
  ylab("Percentage of species\nlikely to become extinct") +
  # Remove dark background
  theme_bw() +
  # Additional paramaters for displaying plot
  theme(text=element_text(size=18), title=element_text(size=18),
        panel.border = element_blank(),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"),
        legend.position="top", legend.key=element_blank(),
        strip.background = element_rect(color="white", fill="white"))

# Write figure to a pdf in the 'figures' folder
pdf("figures/extinct.pdf")  
extinct.plot
# Close pdf call
dev.off()


