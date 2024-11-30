
##################################################
############### Difference Plots #################
##################################################

# Load packages.
library(ggplot2)
library(dplyr)
library(stringr)
library(scales)

# Read in the difference data by sex-and-age class and add class identifier.
fawn <- read.csv("Results_Diff_Fawns.csv") %>%
  mutate(class = "Fawns")

sub <- read.csv("Results_Diff_Sublegal.csv") %>%
  mutate(class = "Sublegal antlered")

female <- read.csv("Results_Diff_Females.csv") %>%
  mutate(class = "Females")

legal <- read.csv("Results_Diff_Legal.csv") %>%
  mutate(class = "Legal antlered")

# Combine data for all sex-and-age classes.
deer <- rbind(fawn, sub, female, legal)

# Rename the treatment and control areas (for clarity).
deer <- deer %>%
  mutate(trt_name = recode(trt_name, "APR" = "Antler point restriction", "Non-APR" = "No antler point restriction"))

# Plot 2022 - 2019 difference data for all sex-and-age classes in a single figure, stacked, and with a reference line at zero.
deer.diff <- ggplot(data = deer, aes(x = factor(class, level = c('Fawns', 'Females', 'Sublegal antlered', 'Legal antlered')), # Hack (order things backwards before the axis flip).
                                     y = mean, color = factor(trt_name, levels = c("No antler point restriction", "Antler point restriction")))) + # Hack (order things backwards before the axis flip).
  geom_hline(yintercept = 0, linetype = "dashed", size = 0.5, color = "black") + # Vertical reference line at zero.
  geom_errorbar(data = deer, size = 5, width = 0, alpha = 0.5, position = position_dodge(width = 0.5), mapping = aes(ymin = l95, ymax = u95)) +
  geom_point(position = position_dodge(width = 0.5), size = 4) +
  labs(x = "Sex-and-age class", y = "Change (2022 - 2019) in site-level deer abundance") + 
  theme_classic() +
  ylim(NA, 3) + # Add extra space for the legend (plus space for the deer silhouettes added later in PowerPoint).
  theme(axis.title.x = element_text(face = "bold", size = 14, margin = margin(t = 15)),
        axis.title.y = element_text(face = "bold", size = 14, margin = margin(r = 15))) +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 10)) + # Wrap text for the longer labels.
  theme(axis.text = element_text(size = 12), 
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 12, face = "bold"),
        legend.position = c(0.85, 0.3), # Manually position the legend.
        legend.key.height = unit(2.25, "lines")) + # Increase the spacing between legend items.
  scale_color_manual(values = c("#B99771", "#6D8163"), 
                     name = "Harvest treatment",
                     labels = c("No antler point\nrestriction", "Antler point\nrestriction")) +
  scale_fill_manual(values = c("#B99771", "#6D8163"), 
                    name = "Harvest treatment",
                    labels = c("No antler point\nrestriction", "Antler point\nrestriction")) +
  guides(color = guide_legend(reverse = TRUE)) + # Hack (reverse the order of legend items before the axis flip).
  coord_flip() # Lastly, flip the axes to make it a stacked plot.

# Take a look at the final figure.
deer.diff

# Save the plot (and scale it to fit in a Word doc nicely). Note that the resolution was reduced to accommodate GitHub size restrictions.
tiff(filename = "Figure_03.tiff", res = 300, units="in", width=6.5, height=5) 
print(deer.diff)
dev.off()