
##################################################
################# Harvest Plots ##################
##################################################

# Load packages.
library(ggplot2)
library(dplyr)
library(stringr)
library(scales)

# Read in harvest data (provided by MDNR).
harvest <- read.csv("MDNR_Harvest_Estimates.csv")

# Pull out 95% confidence intervals from the data.
harvest <- mutate(harvest, l95 = Harvest - CL95)
harvest <- mutate(harvest, u95 = Harvest + CL95)

# Rename the treatment and control areas (for clarity).
harvest <- harvest %>%
  mutate(Zone = recode(Zone, "APR" = "Antler point restriction", "Non-APR" = "No antler point restriction"))

# Plot antlerless and antlered harvest side by side; and highlight the baseline year before the treatment.
harvest.plot <- ggplot(harvest, aes(x = Year, y = Harvest, color = Zone)) +
  geom_rect(xmin = 2017.7, xmax = 2018.3, ymin = 4750, ymax = 17750, 
            fill = "gray", alpha = 0.045, color = NA) + # Add a vertical gray rectangle to highlight baseline year.
  geom_errorbar(aes(ymin = l95, ymax = u95), width = 0, position = position_dodge(width = 0.35), size = 1) + 
  geom_point(position = position_dodge(width = 0.35), size = 3) +
  labs(x = "Year", y = "Estimated deer harvest") + 
  theme_bw() +
  theme(axis.title.x = element_text(face = "bold", size = 14, margin = margin(t = 15)),
        axis.title.y = element_text(face = "bold", size = 14, margin = margin(r = 15)),
        axis.text = element_text(size = 12),
        legend.text = element_text(size = 11),
        legend.title = element_text(size = 11, face = "bold"),
        legend.position = "top", 
        legend.justification = "top",
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        strip.text.x = element_text(size = 12, face = "bold", color = "black"),
        strip.text.y = element_text(size = 12, face = "bold", color = "black")) +
  scale_y_continuous(label = scales::comma, 
                     breaks = c(5000, 7500, 10000, 12500, 15000, 17500),  # Modify y breaks.
                     limits = c(5000, 17500)) +  # Specify y limits.
  scale_x_continuous(expand = expansion(add = c(0.5, 0.5)),  # Add some extra space.
                     breaks = c(2018, 2019, 2020, 2021)) +  # Specify x breaks.
  scale_color_manual(values = c("#6D8163", "#B99771"), # Customize colors.
                     name = "Harvest treatment") +
  scale_fill_manual(values = c("#6D8163", "#B99771"), 
                    name = "Harvest treatment") +
  facet_wrap(~factor(Class)) 

# Take a look at the final figure.
harvest.plot

# Save the plot (and scale it to fit in a Word doc nicely). Note that the resolution was reduced to accommodate GitHub size restrictions.
tiff(filename = "Figure_04.tiff", res = 300, units="in", width=6.5, height=4) 
print(harvest.plot)
dev.off()