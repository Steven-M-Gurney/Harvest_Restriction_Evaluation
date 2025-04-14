
#############################################
######### Marginal-Effects Plots ############
#############################################

# Load packages.
library(ggplot2)
library(dplyr)
library(stringr)
library(scales)

# Load marginal-effects data by sex-and-age class and add class identifier.
legal <- read.csv("Results_ME_Legal.csv") %>%
  mutate(class = "Legal-antlered")

sublegal <- read.csv("Results_ME_Sublegal.csv") %>%
  mutate(class = "Sublegal-antlered")

female <- read.csv("Results_ME_Females.csv") %>%
  mutate(class = "Females")

fawns <- read.csv("Results_ME_Fawns.csv") %>%
  mutate(class = "Fawns")

# Combine data for all sex-and-age classes.
deer <- rbind(legal, sublegal, female, fawns)

# Rename the treatment and control areas (for clarity).
deer <- deer %>%
  mutate(trt_name = recode(trt_name, "APR" = "Antler point restriction", "Non-APR" = "No antler point restriction"))

# Plot marginal-effects for all sex-and-age classes in a single figure.
deer.me <- ggplot(data = deer, aes(x = as.factor(year), y = mean, color = trt_name, group = trt_name)) +
  geom_ribbon(aes( ymin = l95, ymax = u95, fill = trt_name), alpha = 0.33, size = 0, position = position_dodge(width = 0), color = NA) +
  geom_line(size = 1.15, linetype = "solid", position = position_dodge(width = 0)) + # Add error and lines first so the points land on top.
  facet_wrap(~factor(class, levels = c('Legal-antlered', 'Sublegal-antlered', 'Females', 'Fawns'))) # Reorder the panels for consistency.

# Make the plot prettier.
deer.me <- deer.me + labs(x= "Year", y= "Site-level deer abundance") + 
  theme_bw() +
  theme(axis.title.x = element_text(face="bold", size = 14, margin = margin(t = 15)),
        axis.title.y = element_text(face="bold", size = 14, margin = margin(r = 15))) +
  theme(axis.text=element_text(size=12)) + 
  theme(legend.text = element_text(size=12)) + 
  theme(legend.title = element_text(size=12, face = "bold")) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + # Remove the grid lines.
  theme(strip.text.x = element_text(size=12, face="bold", color="black")) +
  theme(strip.text.y = element_text(size=12, face="bold", color="black")) +
  scale_color_manual(values = c("#6D8163", "#B99771"), name = "Harvest treatment") + # Customize the colors.
  scale_fill_manual(values = c("#6D8163", "#B99771"), name = "Harvest treatment") # Do the color thing again.

# Re-position the legend.
deer.me <- deer.me + theme(legend.position = "top", legend.justification = "top")

# Take a look at the final figure.
deer.me

# Save the plot (and scale it to fit in a Word doc nicely). Note that the resolution was reduced to accommodate GitHub size restrictions.
tiff(filename = "Figure_02.tiff", res = 300, units="in", width=6.5, height=6.5) 
print(deer.me)
dev.off()
