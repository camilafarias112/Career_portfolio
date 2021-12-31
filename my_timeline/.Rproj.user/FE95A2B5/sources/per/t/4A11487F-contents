# This is the script to create a waffle plot for my timeline.

# Libraries:
library(lubridate)
library(forcats)
library(patchwork)
library(png)
#library(waffle)
library(ggwaffle)
library(extrafont)
library(prismatic)
library(ggthemes)

library(tidyverse)

loadfonts(device = "pdf", quiet = TRUE)

# Prep data ----
my_data <- read_delim("my_data.txt", delim = "\t", 
                      escape_double = FALSE, trim_ws = TRUE)

# Prep some stuff for annotations ----
initial_text <- function(x, y, label, size, colour, family = initial_annotations_font_family, fontface, ...) {
  annotate("text", x = x, y = y, label = label, size = size, colour = colour, family = family, fontface = "bold", ...)
}

initial_segment <- function(x, xend, y, yend, colour, ...) {
  geom_segment(aes(x = x, xend = xend, y = y, yend = yend), colour = colour)
}

# Import images ----
tree <- readPNG("images/tree.png", native = TRUE)

# Plot ----
timeline <- my_data %>%
  mutate(era = factor(era, levels = c("childhood","highschool","undergrad","masters","phd","postdoc"))) %>%
  
  ## Row of years:
ggplot(., aes(year,row, fill = era)) + 
  geom_waffle(size = 0.3) + 
  #coord_equal() + 
  scale_fill_manual(values = pallete) + 
  theme_classic() +
  theme(
    legend.position = "none",
    axis.title = element_blank(), axis.text = element_blank(), axis.ticks = element_blank(), axis.line = element_blank()
    #plot.margin=unit(c(2,2,2,2),"cm")
    ) +
  coord_cartesian(xlim = c(1988, 2023),ylim = c(-4,3),
                  clip = 'off') +   # This keeps the labels from disappearing
  
  ## Annotations guide:
  initial_text(x = 1989, y = 0.7, size = 4, label = "1990", colour = "#3B3B3B", angle = 90) +
  initial_text(x = 2023, y = 0.7, size = 4, label = "2022", colour = "#3B3B3B", angle = 90) +
  initial_text(x = 1990.5, y = 0.4, size = 5, label = "age", colour = "gray") +
  geom_segment(aes(x = 1991.5, xend = 1993, y = 0.38, yend = 0.38), arrow = arrow(length = unit(0.01, "npc")), colour = "gray") +
  
  initial_text(x = 1990, y = 2.2, size = 5, label = "1 block = 1 year", colour = "gray") +
  geom_curve(aes(x = 1988, xend = 1990, y = 2, yend = 1.3), arrow = arrow(length = unit(0.01, "npc")), colour = "gray", curvature = 0.6) +
  
  ### Brazil
  initial_text(x = 2004, y = 1.7, size = 5, label = "Salvador/Brazil", colour = "gray") +
  geom_segment(aes(x = 1990, xend = 2001, y = 1.7, yend = 1.7), colour = "gray") + # long
  geom_segment(aes(x = 1990, xend = 1990, y = 1.6, yend = 1.8), colour = "gray") +
  geom_segment(aes(x = 2007, xend = 2016.2, y = 1.7, yend = 1.7), colour = "gray") + # long
  geom_segment(aes(x = 2016.2, xend = 2016.2, y = 1.6, yend = 1.8), colour = "gray") +
  
  ### Philadelphia
  initial_text(x = 2019.5, y = 1.7, size = 5, label = "Philly/USA", colour = "gray") +
  geom_segment(aes(x = 2016.7, xend = 2017.5, y = 1.7, yend = 1.7), colour = "gray") + # long
  geom_segment(aes(x = 2016.7, xend = 2016.7, y = 1.6, yend = 1.8), colour = "gray") +
  geom_segment(aes(x = 2021.5, xend = 2022.2, y = 1.7, yend = 1.7), colour = "gray") + # long
  geom_segment(aes(x = 2022.2, xend = 2022.2, y = 1.6, yend = 1.8), colour = "gray") +

  ## Annotations era:
  initial_text(x = 1998, y = 0.3, size = 5, label = "Childhood", colour = pallete[1]) +
  initial_text(x = 2003.2, y = 0.3, size = 5, label = "Highschool", colour = pallete[2]) +
  geom_curve(aes(x = 2005, xend = 2006, y = 0.3, yend = 0.5), arrow = arrow(length = unit(0.01, "npc")), colour = pallete[2], curvature = 0.6) +
  initial_text(x = 2009.5, y = 0, size = 6, label = "Undergrad\nB.S Biomedical\nSciences", colour = pallete[3]) +

  ### Masters and Ph.D:
  geom_segment(aes(x = 2013, xend = 2013, y = -0.9, yend = 0.4), arrow = arrow(length = unit(0.01, "npc")), colour = pallete[4]) +
  initial_text(x = 2012, y = -1.1, size = 6, label = "Masters", colour = pallete[4]) +
  geom_segment(aes(x = 2015, xend = 2015, y = -0.9, yend = 0.4), arrow = arrow(length = unit(0.01, "npc")), colour = pallete[5]) +
  initial_text(x = 2015, y = -1.1, size = 6, label = "Ph.D", colour = pallete[5]) +
  initial_text(x = 2013.5, y = -1.65, size = 6, label = "Immunology and\nHealth Sciences", colour = "dark gray") +
  geom_segment(aes(x = 2010, xend = 2016.5, y = -1.3, yend = -1.3), colour = "dark gray") + # long
  geom_segment(aes(x = 2010, xend = 2010, y = -1.2, yend = -1.4), colour = "dark gray") +
  geom_segment(aes(x = 2016.5, xend = 2016.5, y = -1.2, yend = -1.4), colour = "dark gray") +

### fellow postdoc:
  geom_curve(aes(x = 2018, xend = 2017, y =2, yend = 1.4), arrow = arrow(length = unit(0.01, "npc")), colour = "#A979AF", curvature = 0.3) +
  initial_text(x = 2021, y = 2.2, size = 6, label = "Ph.D fellow award\nData Science", colour = "#A979AF") +
  initial_text(x = 2020, y = 0, size = 6, label = "Postdoc\nComputional\nBiology", colour = pallete[6]) +

### fellow postdoc:
  geom_curve(aes(x = 2016, xend = 2019, y =-0.5, yend = 0.7), colour = "#E88EB2", curvature = -0.6) +
  initial_text(x = 2020, y = -0.7, size = 5, label = "TA DIYtranscriptomics.com", colour = "#E88EB2") +
  geom_segment(aes(x = 2019, xend = 2022, y = 0.7, yend = 0.7), colour = "#E88EB2") +
  geom_point(aes(x=2019, y=0.7),size=3, color="#E88EB2") +
  geom_point(aes(x=2022, y=0.7),size=3, color="#E88EB2") +

### Additional perspective:
  initial_text(x = 2006, y = -2.5, size = 5, label = "Science life\nin keywords\n(start in 2009)", colour = "#3B3B3B") +
  geom_segment(aes(x = 2009, xend = 2023, y = -2.5, yend = -2.5), arrow = arrow(length = unit(0.01, "npc")), colour = "#3B3B3B") +
  geom_segment(aes(x = 2009, xend = 2009, y = -2.4, yend = -2.6), colour = "#3B3B3B") +
  initial_text(x = 2013, y = -3.3, size = 5, label = "clinic", colour = pallete[7]) +
  initial_text(x = 2011, y = -2.7, size = 5, label = "immunology", colour = pallete[7]) +
  initial_text(x = 2011, y = -3.1, size = 5, label = "wetlab", colour = pallete[7]) +
  initial_text(x = 2012, y = -2.9, size = 5, label = "HTLV-1", colour = pallete[7]) +
  initial_text(x = 2015, y = -3.1, size = 5, label = "retrovirus", colour = pallete[7]) +
  initial_text(x = 2016, y = -3.3, size = 5, label = "cellbiology", colour = pallete[7]) +
  initial_text(x = 2015, y = -2.7, size = 5, label = "leishmania", colour = pallete[7]) +
  initial_text(x = 2020, y = -2.7, size = 5, label = "parasites", colour = pallete[7]) +
  initial_text(x = 2017, y = -2.9, size = 5, label = "bioinformatics", colour = pallete[7]) +
  initial_text(x = 2020, y = -3.1, size = 5, label = "datascience", colour = pallete[7]) +
  initial_text(x = 2021.5, y = -2.9, size = 5, label = "#dataviz", colour = pallete[7]) +
  initial_text(x = 2020, y = -3.3, size = 5, label = "#compbio", colour = pallete[7]) +
  initial_text(x = 2016, y = -3.5, size = 5, label = "biomarkers", colour = pallete[7])  +
  initial_text(x = 2020, y = -3.5, size = 5, label = "therapy", colour = pallete[7]) 

timeline

# Export ----
ggsave("../../camilafarias112.github.io/images/content/timeline.png", plot = timeline, device = "png", dpi = 300)
ggsave("images/timeline.png", plot = timeline, device = "png", dpi = 300)

