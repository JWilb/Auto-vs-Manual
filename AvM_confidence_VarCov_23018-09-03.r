library(ggplot2)
library(reshape2)
library(dplyr)


tab = read.csv("/u/people/jeannewb/Documents/00_Projects/AvM/confid_VarCov_results.csv",
                 header = TRUE, sep = "\t", dec = ".",stringsAsFactors = FALSE,
                 strip.white = T, na.strings = c("NA", "")
)


my_theme =   theme(plot.title   = element_text(lineheight=3, size = 18, face = "bold",
                                               margin = margin(10,10,10,10)),
                   axis.text.x  = element_text(angle = 45, hjust = 1, size = 12),
                   axis.title.x = element_text(size = 14,
                                               margin = margin(10,0,0,0)),
                   axis.text.y  = element_text(size = 14),
                   axis.title.y = element_text(size = 14,
                                               margin = margin(0,10,0,0)),
                   axis.ticks   = element_line(color = "grey"),
                   legend.title = element_text(size = 14),
                   legend.text  = element_text(size = 14),
                   legend.key.size    = unit(10, "mm"),
                   legend.key         = element_rect(fill = "white"),
                   panel.grid.minor   = element_blank(),
                   panel.grid.major.y = element_line(color = "grey"),
                   panel.grid.major.x = element_line(color = "grey"),
                   panel.background   = element_blank(),
                   panel.margin     = unit(1.5, "lines"),
                   #strip.text.x = element_text(size=14, face = "italic"),
                   strip.text.y = element_text(size=11,
                                               face = "italic"#, angle=90
                   )
)

species_order <- c(
  'AGLA', 'LDEC', # Coleoptera
  'AROS', 'OABI', # Hymenoptera
  'CLEC', 'OFAS', # Hemiptera
  'FOCC' # Thysanoptera
)


### Correlations:
# Exon count per transcript vs transcript length
# Median exon GC content    vs Exon count per transcript
# Median exon length        vs Exon count per transcript
# Median intron GC content  vs Intron count per transcript
# Median intron length      vs Intron count per transcript

combos <- c("eC_tL", "eGC_eC", "eL_eC", "iGC_iC", "iL_iC")
subtit <- "boxplots: 1000 samples of approx. size 1000"
y_ax <- "Correlation coefficient of linear regression"
#y_ax <- "Spearman correlation coefficient"
x_ax <- "Annotation"
col <- "Set"


#### eC_tL
tab_melt <- melt(tab, id.vars = c("Species", "Set", "Table"),
                 measure.vars = c("eC_tL"))
tab_melt$Species_ordered = factor(tab_melt$Species, levels = species_order)
# get the special sets
extra <- tab_melt[grep("e", tab_melt$Table), ]
extra$Table[extra$Set=='new'] <- 'new'
extra$Set[extra$Set=='new'] <- 'man'
# and remove them from the overall data
tab_melt <- tab_melt[- grep("e", tab_melt$Table), ]

ec_tl <- ggplot(data = tab_melt, aes(x = Set, y = value)) +
  my_theme +
  geom_boxplot(alpha = 0.5, position = "identity") +
  geom_errorbar(data = extra, aes(ymax = value, ymin=value, color = Table)) +
  facet_grid(. ~ Species_ordered) +
  labs(title="a) Transcript length vs. exon count confidence interval", subtitle = subtit, x = x_ax, y = y_ax, color = col) +
  scale_color_manual(values = c("#899DA4", "#C93312","#E1AF00"))

#### eGC_eC
tab_melt <- melt(tab, id.vars = c("Species", "Set", "Table"),
                 measure.vars = c("eGC_eC"))
tab_melt$Species_ordered = factor(tab_melt$Species, levels = species_order)
# get the special sets
extra <- tab_melt[grep("e", tab_melt$Table), ]
extra$Table[extra$Set=='new'] <- 'new'
extra$Set[extra$Set=='new'] <- 'man'
# and remove them from the overall data
tab_melt <- tab_melt[- grep("e", tab_melt$Table), ]

eGC_eC <- ggplot(data = tab_melt, aes(x = Set, y = value)) +
  my_theme +
  geom_boxplot(alpha = 0.5, position = "identity") +
  geom_errorbar(data = extra, aes(ymax = value, ymin=value, color = Table)) +
  facet_grid(. ~ Species_ordered) +
  labs(title="b) Median exon GC content vs. exon count confidence interval", subtitle = subtit, 
       x = x_ax, y = y_ax, color = col)+
  scale_color_manual(values = c("#899DA4", "#C93312","#E1AF00"))

#### eL_eC
tab_melt <- melt(tab, id.vars = c("Species", "Set", "Table"),
                 measure.vars = c("eL_eC"))
tab_melt$Species_ordered = factor(tab_melt$Species, levels = species_order)
# get the special sets
extra <- tab_melt[grep("e", tab_melt$Table), ]
extra$Table[extra$Set=='new'] <- 'new'
extra$Set[extra$Set=='new'] <- 'man'
# and remove them from the overall data
tab_melt <- tab_melt[- grep("e", tab_melt$Table), ]

eL_eC <- ggplot(data = tab_melt, aes(x = Set, y = value)) +
  my_theme +
  geom_boxplot(alpha = 0.5, position = "identity") +
  geom_errorbar(data = extra, aes(ymax = value, ymin=value, color = Table)) +
  facet_grid(. ~ Species_ordered) +
  labs(title="c) Median exon length vs. exon count confidence interval", subtitle = subtit, 
       x = x_ax, y = y_ax, color = col)+
  scale_color_manual(values = c("#899DA4", "#C93312","#E1AF00"))

#### iGC_iC
tab_melt <- melt(tab, id.vars = c("Species", "Set", "Table"),
                 measure.vars = c("iGC_iC"))
tab_melt$Species_ordered = factor(tab_melt$Species, levels = species_order)
# get the special sets
extra <- tab_melt[grep("e", tab_melt$Table), ]
extra$Table[extra$Set=='new'] <- 'new'
extra$Set[extra$Set=='new'] <- 'man'
# and remove them from the overall data
tab_melt <- tab_melt[- grep("e", tab_melt$Table), ]

iGC_iC <- ggplot(data = tab_melt, aes(x = Set, y = value)) +
  my_theme +
  geom_boxplot(alpha = 0.5, position = "identity") +
  geom_errorbar(data = extra, aes(ymax = value, ymin=value, color = Table)) +
  facet_grid(. ~ Species_ordered) +
  labs(title="d) Median intron GC content vs. intron count confidence interval", subtitle = subtit,  
       x = x_ax, y = y_ax, color = col)+
  scale_color_manual(values = c("#899DA4", "#C93312","#E1AF00"))

#### iL_iC
tab_melt <- melt(tab, id.vars = c("Species", "Set", "Table"),
                 measure.vars = c("iL_iC"))
tab_melt$Species_ordered = factor(tab_melt$Species, levels = species_order)
# get the special sets
extra <- tab_melt[grep("e", tab_melt$Table), ]
extra$Table[extra$Set=='new'] <- 'new'
extra$Set[extra$Set=='new'] <- 'man'
# and remove them from the overall data
tab_melt <- tab_melt[- grep("e", tab_melt$Table), ]

iL_iC <- ggplot(data = tab_melt, aes(x = Set, y = value)) +
  my_theme +
  geom_boxplot(alpha = 0.5, position = "identity") +
  geom_errorbar(data = extra, aes(ymax = value, ymin=value, color = Table)) +
  facet_grid(. ~ Species_ordered) +
  labs(title="e) Median intron length vs. intron count confidence interval", subtitle = subtit,  
       x = x_ax, y = y_ax, color = col)+
  scale_color_manual(values = c("#899DA4", "#C93312","#E1AF00"))


pdf("~/Documents/00_Projects/AvM/Plots_2018-08/Parts/lm_coeff_2018-09-02.pdf", width=10, height = 5)
ec_tl
eGC_eC
eL_eC
iGC_iC
iL_iC
dev.off()

