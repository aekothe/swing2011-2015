library(mapcan)
library(tidyverse)
library(devtools)
library(socviz)
library(scales)
library(cowplot)
library(grid)
library(gridExtra)
library(dplyr)
library(extrafont)

font_import()

setwd("~/Desktop/r/swing")

#data
e2011 <- read_csv(file="2011.csv")
e2015 <- read_csv(file="2015.csv")

map <- mapcan(boundaries = ridings, type = standard)

#cleaning
e2011$riding_code <- e2011$`Electoral District Number`
e2011$votes11 <- e2011$`Votes Obtained`
e2011$pctvote11 <- e2011$`Percentage of Votes Obtained`
e2011$candidate11 <- e2011$Candidate

e2015$riding_code <- e2015$`Electoral District Number/Numéro de circonscription`
e2015$votes15 <- e2015$`Votes Obtained/Votes obtenus`
e2015$pctvote15 <- e2015$`Percentage of Votes Obtained /Pourcentage des votes obtenus`
e2015$candidate15 <- e2015$`Candidate/Candidat`

e2011 <- subset(e2011, select=c(riding_code, candidate11, votes11, pctvote11))
e2015 <- subset(e2015, select=c(riding_code, candidate15, votes15, pctvote15))

#swing data
swing <- full_join(e2011, e2015, by = "riding_code")

swing <- swing %>% filter(grepl('Liberal', candidate11))
swing <- swing %>% filter(grepl('Liberal', candidate15))

swing$swing <- swing$pctvote15 - swing$pctvote11

#map, don't forget to adjust legend size uni2
smap <- full_join(map, swing, by = "riding_code")

riding_binplot(riding_data = smap,
               value_col = swing, 
               #arrange = TRUE
               ) +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", 
                       midpoint=0, limits=c(-45, 45),
                       guide = guide_colorbar(frame.colour = "black", ticks.colour = "black"),
                       labels = c("45 Percent", "30 Percent", "15 Percent", "No Change", 
                                  "-15 Percent", "-30 Percent", "-45 Percent"),
                       breaks = c(45, 30, 15, 0, 
                                  -15, -30, -45)) +
  theme_mapcan() +
  theme(text = element_text(size=12, family = "Georgia")) +
  theme(legend.title=element_blank()) +
  coord_fixed() +
  theme(legend.position = "right") +
  labs(title = "Liberal Vote Swing from 2011 to 2015",
       subtitle = "source: Elections Canada")

#hex
riding_binplot(riding_data = smap,
               value_col = swing, 
               arrange = TRUE,
               shape = "hexagon") +
  scale_fill_gradient2(low = "blue", mid = "orange", high = "red", 
                       midpoint=0, limits=c(-45, 45),
                       guide = guide_colorbar(frame.colour = "black", ticks.colour = "black"),
                       labels = c("45 Percent", "30 Percent", "15 Percent", "No Change", 
                                  "-15 Percent", "-30 Percent", "-45 Percent"),
                       breaks = c(45, 30, 15, 0, 
                                  -15, -30, -45)) +
  theme_mapcan() +
  theme(text = element_text(size=12, family = "Georgia")) +
  theme(legend.title=element_blank()) +
  coord_fixed() +
  theme(legend.position = "right") +
  labs(title = "Liberal Vote Swing from 2011 to 2015",
       subtitle = "source: Elections Canada")

#
riding_binplot(riding_data = smap,
               value_col = swing, 
               shape = "hexagon") +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", 
                       midpoint=0, limits=c(-45, 45),
                       guide = guide_colorbar(frame.colour = "black", ticks.colour = "black"),
                       labels = c("45 Percent", "30 Percent", "15 Percent", "No Change", 
                                  "-15 Percent", "-30 Percent", "-45 Percent"),
                       breaks = c(45, 30, 15, 0, 
                                  -15, -30, -45)) +
  theme_mapcan() +
  theme(text = element_text(size=12, family = "Georgia")) +
  theme(legend.title=element_blank()) +
  coord_fixed() +
  theme(legend.position = c("right")) +
  labs(title = "Liberal Vote Swing from 2011 to 2015",
       subtitle = "source: Elections Canada")


