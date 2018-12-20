library(ggplot2)
library(scales)
library(reshape2)
library(colorspace)


#pal <- choose_palette()
#pal(4) # "#75BF00" "#B2CF9B" "#E2C19D" "#E59E00"


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
             'Ceratitis capitata', 'Cimex lectularius', 'Frankliniella occidentalis',
             'Leptinotarsa decemlineata','Orussus abietinus',
             'Oncopeltus fasciatus')

species_ordered <- c('Ceratitis capitata', # Diptera
                   'Anoplophora glabripennis', 'Leptinotarsa decemlineata', # Coleoptera
                   'Athalia rosae', 'Orussus abietinus', # Hymenoptera
                   'Cimex lectularius', 'Oncopeltus fasciatus', # Hemiptera
                   'Frankliniella occidentalis' # Thysanoptera
)
species_order <- rev(species_ordered)

a_coding <- c(24.868989,19.937322,21.613806,16.743624,25.523568,22.572267,17.701101,17.464875)
a_intron <- c(144.139225,42.001872,64.416234,58.271668,82.940358,251.974183,46.085909,218.667619)
a_ns <- c(105.28531,7.009359,44.069776,136.873524,151.997859,491.970518,14.744456,325.030201)
a_rest <- c(433.418669,94.889337,354.673677,438.603947,155.34207,403.724996,122.688868,537.530523)

m_coding <- c(25.167546,19.939996,22.299912,16.999631,26.026731,22.775638,17.669895,17.622689)
m_intron <- c(147.505534,42.326824,184.160305,77.996144,86.961327,260.816089,46.766065,228.687364)
m_ns <- c(105.28531,7.009359,44.069776,136.873524,151.997859,491.970518,14.744456,325.030201)
m_rest <- c(429.753803,94.561711,234.243499,418.623464,150.817938,394.679719,122.039918,527.352964)

a_dat <- data.frame(Species, a_coding, a_intron, a_rest, a_ns,
                    stringsAsFactors=FALSE)
m_dat <- data.frame(Species, m_coding, m_intron, m_rest, m_ns,
                   stringsAsFactors=FALSE)
a_dat$Type <- 'automatic'
m_dat$Type <- 'manual'

a_melt <- melt(a_dat, id.vars = c("Species", "Type"), measure.vars =
                  c("a_coding", "a_intron", "a_rest", "a_ns"))
m_melt <- melt(m_dat, id.vars = c("Species", "Type"), measure.vars =
                c("m_coding", "m_intron", "m_rest", "m_ns"))


a_plot <- ggplot(data=a_melt, aes(x = Species, y = value, fill = variable)) +
  geom_bar(stat="identity",
           position = position_stack(reverse=TRUE)) +
  my_theme +
  theme(legend.position = 'bottom') +
  labs(title = "a)",
       y = "Size [Mb]",
       fill = "Type") +
  #guides(fill = guide_legend(reverse=TRUE)) +
  scale_fill_manual(labels = c("CDSs", "Introns",
                               "Rest", "Ns"),
                    values = c("#E59E00", "#7B9FB3", "#1EA34B", "#BC506F")) +
  coord_flip() +
  scale_x_discrete(limits = species_order)

m_plot <- ggplot(data=m_melt, aes(x = Species, y = value, fill = variable)) +
  geom_bar(stat="identity",
           position = position_stack(reverse=TRUE)) +
  my_theme +
  theme(legend.position = 'bottom') +
  labs(title = "b)",
       y = "Size [Mb]",
       fill = "Type") +
  #guides(fill = guide_legend(reverse=TRUE)) +
  scale_fill_manual(labels = c("CDSs", "Introns",
                               "Rest", "Ns"),
                    values = c("#E59E00", "#7B9FB3", "#1EA34B", "#BC506F")) +
  coord_flip() +
  scale_x_discrete(limits = species_order)


pdf("~/Desktop/phd/02c_auto-vs-manual/Poster/ass_stats_2017-08-21.pdf", width=11, height = 5)
a_plot
m_plot
dev.off()


