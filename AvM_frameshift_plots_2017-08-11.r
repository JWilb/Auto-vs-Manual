library(ggplot2)
library(scales)
library(reshape2)
library(colorspace)


#pal <- choose_palette()
pal(4) # "#75BF00" "#B2CF9B" "#E2C19D" "#E59E00"


my_theme =   theme(plot.title   = element_text(lineheight=3, size = 18, face = "bold",
                                               margin = margin(10,10,10,10)),
                   axis.text.x  = element_text(size = 12),
                   axis.title.x = element_text(size = 16,
                                               margin = margin(10,0,0,0)),
                   axis.ticks   = element_blank(),
                   axis.text.y  = element_text(size = 14, face = 'italic'),
                   axis.title.y = element_text(size = 16,
                                               margin = margin(0,10,0,0)),
                   legend.title = element_text(size = 14),
                   legend.text  = element_text(size = 14),
                   legend.key.size = unit(10, "mm"),
                   legend.key = element_rect(fill = "white"),
                   panel.grid.minor = element_blank(),
                   panel.grid.major.y = element_blank(),
                   panel.grid.major.x = element_line(color = "grey"),
                   panel.background = element_blank()
                   #strip.text.x = element_text(size=14, face = "italic"),
                   #strip.text.y = element_text(size=14, face = "italic", angle=90)
)

Species <- c('Anoplophora glabripennis', 'Athalia rosae',
             'Cimex lectularius', 'Ceratitis capitata', 'Frankliniella occidentalis',
             'Leptinotarsa decemlineata','Orussus abietinus',
             'Oncopeltus fasciatus')

species_ordered <- c('Ceratitis capitata', # Diptera
                   'Anoplophora glabripennis', 'Leptinotarsa decemlineata', # Coleoptera
                   'Athalia rosae', 'Orussus abietinus', # Hymenoptera
                   'Cimex lectularius', 'Oncopeltus fasciatus', # Hemiptera
                   'Frankliniella occidentalis' # Thysanoptera
)
species_order <- rev(species_ordered)

no_change <- c(483,579,445,148,537,623,444,581)
no_shift_but_change <- c(115,93,155,426,109,126,89,172)
one_shifted <- c(110,93,111,461,114,127,84,136)
two_shifted <- c(104,80,113,459,97,124,79,138)

p_no_change <- c(59.48,68.52,54,9.91,62.66,62.3,63.79,56.57)
p_no_shift_but_change <- c(14.16,11.01,18.81,28.51,12.72,12.6,12.79,16.75)
p_one_shifted <- c(13.55,11.01,13.47,30.86,13.3,12.7,12.07,13.24)
p_two_shifted <- c(12.81,9.47,13.71,30.72,11.32,12.4,11.35,13.44)


mydat <- data.frame(Species, two_shifted, one_shifted, no_shift_but_change, no_change,
                    stringsAsFactors=FALSE)
pdat <- data.frame(Species, p_two_shifted, p_one_shifted, p_no_shift_but_change, p_no_change,
                   stringsAsFactors=FALSE)

meltdat <- melt(mydat, id.vars = "Species", measure.vars =
                  c("no_change", "no_shift_but_change",
                    "one_shifted", "two_shifted"))
pmelt <- melt(pdat, id.vars = "Species", measure.vars =
                c("p_no_change", "p_no_shift_but_change",
                  "p_one_shifted", "p_two_shifted"))

fs <- ggplot(data=meltdat, aes(x = Species, y = value, fill = variable)) +
  geom_bar(stat="identity",
           position = position_stack(reverse=TRUE)) +
  my_theme +
  theme(legend.position = 'bottom') +
  labs(title = "a)",
       y = "Count",
       fill = "Type") +
  #guides(fill = guide_legend(reverse=TRUE)) +
  scale_fill_manual(labels = c("Same start - no shift", "Different start - no shift",
                               "Shift by 1", "Shift by 2"),
                    values = c("#75BF00", "#B2CF9B", "#E2C19D", "#E59E00")) +
  coord_flip() +
  scale_x_discrete(limits = species_order)

fs_p <- ggplot(data=pmelt, aes(x = Species, y = value, fill = variable)) +
  geom_bar(stat="identity",
           position = position_stack(reverse=TRUE)) +
  my_theme +
  theme(legend.position = 'bottom') +
  labs(title = "b)",
       y = "Percentage",
       fill = "Type") +
  #guides(fill = guide_legend(reverse=TRUE)) +
  scale_fill_manual(labels = c("Same start - no shift", "Different start - no shift",
                               "Shift by 1", "Shift by 2"),
                    values = c("#75BF00", "#B2CF9B", "#E2C19D", "#E59E00")) +
  coord_flip() +
  scale_x_discrete(limits = species_order)


pdf("~/Desktop/phd/02c_auto-vs-manual/plots/frameshifts_2017-08-11.pdf", width=11, height = 5)
fs
fs_p
dev.off()


