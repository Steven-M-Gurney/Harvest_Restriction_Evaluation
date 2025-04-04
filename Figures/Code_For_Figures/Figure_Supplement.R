library(here)
library(tidyverse)

setwd(here::here("Results"))
load("fawns_year_factor_no_itx.RData")
fawn <- posterior
load("females_year_factor_no_itx.RData")
females <- posterior
load("sublegal_year_factor_no_itx.RData")
sublegal <- posterior
load("legal_year_factor_no_itx.RData")
legal <- posterior

all <- dplyr::full_join(fawn, females) |> 
  dplyr::full_join(legal) |> 
  dplyr::full_join(sublegal)

diffs <- all |> 
  dplyr::ungroup() |> 
  dplyr::select(class, iter, year.lab, trt.lab, lambda) |> 
  dplyr::filter(year.lab == 2019 | year.lab == 2022) |> 
  tidyr::pivot_wider(names_from = year.lab, values_from = lambda) |> 
  dplyr::mutate(diff = `2022` - `2019`) |> 
  dplyr::group_by(class, trt.lab) |> 
  dplyr::summarise( mean = mean(diff), 
                    l95 = quantile(diff, c(0.025)), 
                    u95 = quantile(diff, c(0.975))) |> 
  dplyr::mutate(class = ifelse(class == "Sublegal", "Sublegal\nantlered", 
                               ifelse(class == "Legal", "Legal\nantlered", class))) |> 
  dplyr::mutate(class = factor(class, 
                               levels = c("Fawns", "Females", "Sublegal\nantlered", "Legal\nantlered"))) |> 
  dplyr::mutate(trt.lab = ifelse(trt.lab == "APR", "Antler point\nrestriction", 
                                 "No antler point\nrestriction"))

ggplot( diffs, aes(x = mean, y = class, color = trt.lab)) +
  geom_vline(xintercept = 0, linetype = "dashed", linewidth = 0.5, color = "black") +
  geom_errorbar(aes(xmin = l95, xmax = u95), width = 0,
                linewidth = 5, alpha = 0.5, position = position_dodge(width = 0.4)) +
  geom_point(size = 6, position = position_dodge(width = 0.4)) +
  labs(x = "Change (2022 - 2019) in site-level deer abundance", 
       y = "Sex-and-age class",
       color = "Harvest treatment") +
  theme_classic() +
  theme(axis.text = element_text(size = 10), 
        legend.text = element_text(size = 9),
        legend.title = element_text(size = 10, face = "bold"),
        legend.position = c(0.85, 0.3), # Manually position the legend.
        legend.key.height = unit(2.25, "lines")) + # Increase the spacing between legend items.
  scale_color_manual(values = c("#B99771", "#6D8163"), 
                     name = "Harvest treatment") +
  scale_fill_manual(values = c("#B99771", "#6D8163"), 
                    name = "Harvest treatment") +
  guides(color = guide_legend(reverse = TRUE))

setwd(here::here("Figures"))
ggsave(
  filename = "figure_03_supplemental.png",
  width = 4.5,
  height = 4.5,
  units = "in",
  dpi = 600)

all.sum <- all |> 
  dplyr::group_by(class, year.lab, trt.lab) |> 
  dplyr::summarise( mean = mean(lambda), 
                    l95 = quantile(lambda, c(0.025)), 
                    u95 = quantile(lambda, c(0.975))) |> 
  dplyr::mutate(class = ifelse(class == "Sublegal", "Sublegal antlered",
                               ifelse(class == "Legal", "Legal antlered", class))) |>
  dplyr::mutate(class = factor(class, 
                               levels = c("Fawns", "Females", "Sublegal antlered", "Legal antlered"))) |> 
  dplyr::mutate(class = factor(class, levels = rev(levels(class)))) |> 
  dplyr::mutate(trt.lab = ifelse(trt.lab == "APR", "Antler point restriction", 
                                 "No antler point restriction"))

ggplot(all.sum, aes(x = year.lab, y = mean, color = trt.lab)) + 
  facet_wrap(~class) +
  geom_errorbar(aes(ymin = l95, ymax = u95), width = 0, linewidth = 1.75, position = position_dodge(width = 0.3)) +
  geom_point(position = position_dodge(width = 0.3), size = 3) +
  theme_bw() +
  scale_color_manual(values = c("#6D8163", "#B99771"), name = "Harvest treatment") +
  labs( x = "Year", 
        y = "Site-level deer abundance") +
theme(legend.position = "top",
      axis.title.x = element_text(face = "bold", size = 10, color = "black", margin = margin(t = 10)), 
      axis.title.y = element_text(face = "bold", size = 10, color = "black", margin = margin(r = 10)), 
      axis.text = element_text(size = 9, color = "black"), 
      strip.text = element_text(size = 10, face = "bold", color = "black"),
      legend.title = element_text(face = "bold", color = "black", size = 10), 
      legend.text = element_text(color = "black", size = 9)) 

# setwd(here::here("Figures"))
# ggsave(
#   filename = "figure_02_supplemental.png", 
#   width = 5, 
#   height = 5, 
#   units = "in", 
#   dpi = 600)

all |> filter(year.lab == 2022) |> filter(class == "Fawns") |> group_by(trt.lab) |> 
  summarise(mean = mean (lambda), 
            l95 = quantile(lambda, c(0.025)), 
            u95 = quantile(lambda, c(0.975)))

all |> filter(year.lab == 2022) |> filter(class == "Sublegal") |> 
  dplyr::ungroup() |> 
  dplyr::select(class, iter, trt.lab, lambda) |> 
  tidyr::pivot_wider(names_from = trt.lab, values_from = lambda) |> 
  dplyr::mutate(diff = `APR` - `Non-APR`) |> 
  dplyr::summarise( mean = mean(diff), 
                    l95 = quantile(diff, c(0.025)), 
                    u95 = quantile(diff, c(0.975)))
