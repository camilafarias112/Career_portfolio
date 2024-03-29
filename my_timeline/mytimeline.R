# This is the script to create a waffle plot for my timeline.

# Libraries:
library(lubridate)
library(forcats)
library(patchwork)
library(ggbreak)
library(png)
#library(waffle)
#library(ggwaffle)
library(extrafont)
library(prismatic)
library(ggthemes)

library(tidyverse)

loadfonts(device = "pdf", quiet = TRUE)
pallete <- c(#"#1d4497", #dark blue
             "#5773c0","#7da7ea",
             "#8cc8bc","#edb144","#c8570d",
             "#b83326","#dd7867","#D783BD")

# Prep data ----
my_data <- read_delim("my_data.txt", delim = "\t", 
                      escape_double = FALSE, trim_ws = TRUE) %>%
  mutate(era = factor(era, levels = c("undergrad","masters","phd","postdoc","ra")),
         year = as.factor(year)) 

# Prep some stuff for annotations ----
initial_text <- function(x, y, label, size, colour, family = initial_annotations_font_family, fontface, ...) {
  annotate("text", x = x, y = y, label = label, size = size, colour = colour, family = family, fontface = "bold", ...)
}
initial_segment <- function(x, xend, y, yend, colour, ...) {
  geom_segment(aes(x = x, xend = xend, y = y, yend = yend), colour = colour)
}

# Import images ----
tree <- readPNG("images/tree.png", native = TRUE)
upenn <- readPNG("images/UPENN.png", native = TRUE)
ufba <- readPNG("images/UFBA.png", native = TRUE)
me <- readPNG("images/me.png", native = TRUE)

# Plot ----
timeline <- my_data %>%
ggplot(., aes(year,row, fill = era)) + 
  geom_tile(color="white", linewidth=0.3) + 
  scale_fill_manual(values = c(pallete,"#8B140C")) + 
  theme_classic() +
  theme(
    legend.position = "none",
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.line = element_blank(),
    plot.margin=unit(c(0,3,0,5),"cm"),
    NULL) +
  coord_equal(clip = 'off', ratio=1, expand = FALSE) +

  ## Annotations guide:
  initial_text(x = 0.2, y = 1, size = 6, label = "2008", colour = "#3B3B3B", angle = 90) +
  initial_text(x = 16.8, y = 1, size = 6, label = "2023", colour = "#3B3B3B", angle = 90) +
  initial_text(x = 2.5, y = 0.2, size = 6, label = "age, 1 block = 1 year", colour = "gray") +
  geom_segment(aes(x = 0.5, xend = 2, y = -0.2, yend = -0.2), arrow = arrow(length = unit(0.05, "npc")),
               colour = "gray", size=1.5) +
  
  ### Brazil
  initial_text(x = 5.7, y = -1.5, size = 8, label = "Salvador/Brazil", colour = pallete[8]) +
  geom_segment(aes(x = 0.5, xend = 10.3, y = -1.2, yend = -1.2), colour = pallete[8], size=1.5) + # long

  ### Philadelphia
  initial_text(x = 14, y = -1.5, size = 8, label = "Philadelphia/USA", colour = pallete[8]) +
  geom_segment(aes(x = 10.6, xend = 16.5, y = -1.2, yend = -1.2), colour = pallete[8], size=1.5) + # long

  ### B.S, Masters and Ph.D:
  initial_text(x = 2.5, y = 1.9, size = 8, label = "B.S ", colour = pallete[1]) +
  initial_text(x = 5.5, y = 1.9, size = 8, label = "M.S", colour = pallete[2]) +
  initial_text(x = 8.5, y = 1.9, size = 8, label = "Ph.D", colour = pallete[3]) +
  initial_text(x = 5.5, y = 4, size = 8, label = "Biomedicine, Biology, Immunology and\nHealth Sciences\n", colour = pallete[6]) +
  geom_segment(aes(x = 0.5, xend = 10.3, y = 3.1, yend = 3.1), colour = pallete[6], size=1.5) + # long

### fellow postdoc:
  #geom_curve(aes(x = 2016, xend = 2017, y =2, yend = 1.5), arrow = arrow(length = unit(0.01, "npc")), colour = "#A979AF", curvature = -0.3) +
  initial_text(x = 13.5, y = 4.3, size = 8, label = "Bioinformatics\nData Science\nComputational Biology\n", colour = pallete[6]) +
  initial_text(x = 13.5, y = 1.9, size = 8, label = "Postdoc", colour = pallete[4]) +
  geom_segment(aes(x = 10.7, xend = 16.2, y = 3.1, yend = 3.1), colour = pallete[6], size=1.5) +
  
### Research Associate:
  initial_text(x = 16, y = 2.3, size = 8, label = "Research\nAssociate", colour = pallete[5]) +

### DIYtranscriptomics:
  initial_text(x = 14, y = -0.2, size = 8, label = "Teaching Assistant\nDIYtranscriptomics.com", colour = "purple") +
  geom_segment(aes(x = 14, xend = 14, y = 0.8, yend = 0.3), colour = "purple", size=1.5) +
  geom_segment(aes(x = 11, xend = 16, y = 0.8, yend = 0.8), colour = "purple", size=1.5) +
  geom_point(aes(x=11, y=0.8),size=5, color="purple") +
  geom_point(aes(x=16, y=0.8),size=5, color="purple") +
  
### Logos:
  annotation_raster(upenn, 13, 15, -3, -1.6) +
  annotation_raster(ufba, 5, 7, -2.9, -1.8) +
  annotation_raster(me, 1, 15.5, 11, 7)

#timeline

# Export ----
ggsave("images/timeline.png", plot = timeline, device = "png", dpi = 300,
       height = 35, width = 35, units = "cm")
ggsave("../../camilafarias112.github.io/images/content/timeline.png", plot = timeline, device = "png", dpi = 300,
       height = 35, width = 35, units = "cm")
