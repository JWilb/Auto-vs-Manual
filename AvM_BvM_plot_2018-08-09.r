library(ggplot2)
library(scales)
library(plyr)
library(data.table)
library(reshape2)
library(wesanderson)


species_order <- c('Athalia rosae', 'Orussus abietinus', # Hymenoptera
                   'Oncopeltus fasciatus'# Hemiptera
)


#### Density, VarCov
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
                   #  panel.spacing     = unit(2, "lines"),
                   #strip.text.x = element_text(size=14, face = "italic"),
                   strip.text.y = element_text(size=11,
                                               face = "italic"#, angle=90
                   )
)

# Compare full set of auto an OGS with ManCur and predecessors

read_my_table <- function(file) {
  table = read.csv(paste(c("/data/home/Desktop/PhD/02c_AvM/",
                           file), collapse =''),
                   header = TRUE, sep = "\t", dec = ".",
                   stringsAsFactors = FALSE,
                   strip.white = T,
                   na.strings = c("NA", "")#,
                   # as.is = T
                   # colClasses = c(rep('numeric', 10))
  )

  return(table)
}

get_data <- function(data) {
  # Complete MAKER sets
  aros_auto_m     = read_my_table(paste("COGNATE_a-v-m_May/COGNATE_AROS_auto_compl/COGNATE_AROS_auto_compl_", data, sep=""))
  oabi_auto_m     = read_my_table(paste("COGNATE_a-v-m_May/COGNATE_OABI_auto_compl/COGNATE_OABI_auto_compl_", data, sep=""))
  ofas_auto_m     = read_my_table(paste("COGNATE_a-v-m_May/COGNATE_OFAS_auto_compl/COGNATE_OFAS_auto_compl_", data, sep=""))

  # Complete BRAKER sets
  aros_auto_b     = read_my_table(paste("COGNATE_a-v-m_04-2018/COGNATE_AROS_braker2_compl/COGNATE_AROS_braker2_compl_", data, sep=""))
  oabi_auto_b     = read_my_table(paste("COGNATE_a-v-m_04-2018/COGNATE_OABI_braker2_compl/COGNATE_OABI_braker2_compl_", data, sep=""))
  ofas_auto_b     = read_my_table(paste("COGNATE_a-v-m_04-2018/COGNATE_OFAS_braker2_compl/COGNATE_OFAS_braker2_compl_", data, sep=""))

 # Complete OGS
 # aros_ogs        = read_my_table(paste("COGNATE_a-v-m_May/COGNATE_AROS_man_compl/COGNATE_AROS_man_compl_", data, sep=""))
 # oabi_ogs        = read_my_table(paste("COGNATE_a-v-m_May/COGNATE_OABI_man_compl/COGNATE_OABI_man_compl_", data, sep=""))
 # ofas_ogs        = read_my_table(paste("COGNATE_a-v-m_May/COGNATE_OFAS_man_compl/COGNATE_OFAS_man_compl_", data, sep=""))

  # Add necessary columns
  aros_auto_m$Species     <- 'Athalia rosae'
  aros_auto_m$Annotation  <- 'MAKER'
  aros_auto_m$Assembly.size <- 163837890

  oabi_auto_m$Species     <- 'Orussus abietinus'
  oabi_auto_m$Annotation  <- 'MAKER'
  oabi_auto_m$Assembly.size <- 201220334

  ofas_auto_m$Species     <- 'Oncopeltus fasciatus'
  ofas_auto_m$Annotation         <- 'MAKER'
  ofas_auto_m$Assembly.size <- 1098693218

  aros_auto_b$Species     <- 'Athalia rosae'
  aros_auto_b$Annotation  <- 'BRAKER'
  aros_auto_b$Assembly.size <- 163837890

  oabi_auto_b$Species     <- 'Orussus abietinus'
  oabi_auto_b$Annotation  <- 'BRAKER'
  oabi_auto_b$Assembly.size <- 201220334

  ofas_auto_b$Species     <- 'Oncopeltus fasciatus'
  ofas_auto_b$Annotation  <- 'BRAKER'
  ofas_auto_b$Assembly.size <- 1098693218

#  aros_ogs$Species     <- 'Athalia rosae'
#  aros_ogs$Annotation  <- 'OGS'
#  aros_ogs$Assembly.size <- 163837890

#  oabi_ogs$Species     <- 'Orussus abietinus'
#  oabi_ogs$Annotation  <- 'OGS'
#  oabi_ogs$Assembly.size <- 201220334

#  ofas_ogs$Species     <- 'Oncopeltus fasciatus'
#  ofas_ogs$Annotation  <- 'OGS'
#  ofas_ogs$Assembly.size <- 1098693218

  #and combine into your new data frame
  SppData <- rbind(aros_auto_m, oabi_auto_m, ofas_auto_m,
                   aros_auto_b, oabi_auto_b, ofas_auto_b#,
                  # aros_ogs, oabi_ogs, ofas_ogs
  )

  return(SppData)
}

t_dat <- get_data('07-transcript_general.tsv')
e_dat <- get_data('09-transcript_exons.tsv')
i_dat <- get_data('10-transcript_introns.tsv')

c_dat <- cbind(t_dat, e_dat, i_dat)

c_dat$Species_ordered = factor(c_dat$Species, levels = species_order)


c_dat_2 <- c_dat
setnames(c_dat_2, old=c("Transcript.length..genomic.", "Protein.length",
                        "Exon.count.of.transcript", "Median.exon.length.of.transcript",
                        "Median.intron.length.of.transcript"),
         new=c("Transcript length [bp]", "Protein length [aa]",
               "Exon count per transcript",
               "Median exon length per transcript [bp]",
               "Median intron length per transcript [bp]"))


c_melt <- melt(c_dat_2, id.vars = c("Species_ordered", "Annotation"), measure.vars =
                 c("Transcript length [bp]", "Protein length [aa]",
                   "Exon count per transcript",
                   "Median exon length per transcript [bp]",
                   "Median intron length per transcript [bp]"))

# define x axis label format
point <- format_format(big.mark = ",", decimal.mark = ".", scientific = FALSE)

# Plot
con_dens <-  ggplot(c_melt, aes(x=value, color=variable, linetype=Annotation
                                )) +
  my_theme +
  theme (panel.margin = unit(1.5, "lines")) +
  geom_line(stat="density", size = 1) +
  scale_x_continuous(trans = 'log10', expand = c(0.01, 0), labels = point,
                     breaks = c(0, 1,2, 10, 100, 1000, 10000, 100000)
  ) +
  facet_grid(Species_ordered ~ .,
             labeller = labeller(Species_ordered = label_wrap_gen(15)),
             scales = "free") +
  labs(title = "a)",
       x = "Value",
       y = "Density",
       color = "Data") +
  guides(colour = guide_legend(override.aes = list(size=1.8)),
         linetype = guide_legend(override.aes = list(size=1.8))) +
  scale_linetype_manual(values = c("dashed", "solid", "dotted")) +
  scale_color_manual(values = wes_palette(name = "Cavalcanti1"))

pdf("~/Desktop/phd/02c_auto-vs-manual/plots/BvMvOGS_2017-10-17.pdf",
    width=15, height = 8)
con_dens
dev.off()



######################### VAR COVERAGE ##################
# gg_color_hue <- function(n) {
#   hues = seq(15, 375, length = n + 1)
#   hcl(h = hues, l = 65, c = 100)[1:n]
# }
# col <- gg_color_hue(2)
# 
# darker_col <- c("#F53224", "#007175")
# sets <- c("analyzed", "complete")
# names(darker_col) <- sets
# names(sets) <- darker_col
# 
# 
# 
# my_vc <- function(dat, x_dat, y_dat, ann, tit, x_tit, y_tit) {
#   ggplot(dat, aes_string(x=x_dat, y=y_dat)) +
#     my_theme +
#     geom_point(aes_string(colour = ann)) +
#     geom_point(data = dat, aes_string(x=x_dat, y=y_dat, colour = ann)) +
#     geom_smooth(method=lm, se=TRUE, fullrange=TRUE, show.legend = FALSE,
#                 aes_string(color=ann)) +
#     geom_smooth(method=lm, se=FALSE, fullrange=TRUE, show.legend = TRUE,
#                 aes_string(color=ann)) +
#     # scale_linetype_manual(, values = c(1,2)) +
#     # geom_rug(alpha=0.1) +
#     facet_grid(Species ~ ., labeller = labeller(Species = label_wrap_gen(15))) +
#     guides(size = FALSE, shape = FALSE,
#            colour = guide_legend(override.aes = list(size = 5, shape = 19, linetype=0)),
#            linetype = guide_legend(title = "Smoothed \nconditional mean",
#                                    override.aes = list(size = 1,
#                                                        color = set))
#     ) +
#     scale_linetype_manual(values=c("solid", "solid")) +
#     labs(title = tit,
#          x = bquote(paste('log'['10']*' ', .(x_tit))),
#          y = bquote(paste('log'['10']*' ', .(y_tit)))
#     )
# }
# 
# ec_v_el <- my_vc(c_dat, x_dat='log10(Exon.count.of.transcript)',
#                  y_dat='log10(Median.exon.length.of.transcript)',
#                  ann='Annotation',
#                  "Dispersion coverage (Exon count vs exon length)",
#                  "Exon count per transcript",
#                  "Median exon length per transcript [bp]")
# ggsave(plot=ec_v_el, file="~/Desktop/phd/02c_auto-vs-manual/BvM/plots/BvM_VarCov_Sept_ec_v_el.png",
#        width=5, height=15, dpi=300)
# 
# ec_v_egc <- my_vc(c_dat, x_dat='log10(Exon.count.of.transcript)',
#                   y_dat='log10(Median.exon.GC.content.of.transcript)',
#                   ann='Annotation',
#                   "Dispersion coverage (Exon count vs exon GC content)",
#                   "Exon count per transcript",
#                   "Median exon GC content per transcript [%]")
# ggsave(plot=ec_v_egc, file="~/Desktop/phd/02c_auto-vs-manual/BvM/plots/BvM_VarCov_Sept_ec_v_egc.png",
#        width=5, height=15, dpi=300)
# 
# ic_v_il <- my_vc(c_dat, x_dat='log10(Intron.count.of.transcript)',
#                  y_dat='log10(Median.intron.length.of.transcript)',
#                  ann='Annotation',
#                  "Dispersion coverage (Intron count vs intron length)",
#                  "Intron count per transcript",
#                  "Median intron length per transcript [bp]")
# ggsave(plot=ic_v_il, file="~/Desktop/phd/02c_auto-vs-manual/BvM/plots/BvM_VarCov_Sept_ic_v_il.png",
#        width=5, height=15, dpi=300)
# ic_v_igc <- my_vc(c_dat, x_dat='log10(Intron.count.of.transcript)',
#                   y_dat='log10(Median.intron.GC.content.of.transcript)',
#                   ann='Annotation',
#                   "Dispersion coverage (Intron count vs intron GC content)",
#                   "Intron count per transcript",
#                   "Median intron GC content per transcript [%]")
# ggsave(plot=ic_v_igc, file="~/Desktop/phd/02c_auto-vs-manual/BvM/plots/BvM_VarCov_Sept_ic_v_igc.png",
#        width=5, height=15, dpi=300)
# 
# 
# 
# i_v_gc <- my_vc(c_dat, x_dat='log10(Median.intron.length.of.transcript)',
#                 y_dat='log10(Median.intron.GC.content.of.transcript)',
#                 ann='Annotation',
#                 "Dispersion coverage (Intron length vs intron GC content)",
#                 "Median intron length per transcript [bp]",
#                 "Median intron GC content per transcript [%]")
# ggsave(plot=i_v_gc, file="~/Desktop/phd/02c_auto-vs-manual/BvM/plots/BvM_VarCov_Sept_iiGC.png",
#        width=8, height=10, dpi=300)
# 
# e_v_gc <- my_vc(c_dat, x_dat='log10(Median.exon.length.of.transcript)',
#                 y_dat='log10(Median.exon.GC.content.of.transcript)',
#                 ann='Annotation',
#                 "Dispersion coverage (Exon length vs exon GC content)",
#                 "Median exon length per transcript [bp]",
#                 "Median exon GC content per transcript [%]")
# ggsave(plot=e_v_gc, file="~/Desktop/phd/02c_auto-vs-manual/BvM/plots/BvM_VarCov_Sept_eeGC.png",
#        width=8, height=10, dpi=300)
# 
# t_v_gc <- my_vc(c_dat, x_dat='log10(Transcript.length..genomic.)',
#                 y_dat='log10(Transcript.GC.content.without.ambiguity)',
#                 ann='Annotation',
#                 "Dispersion coverage (Transcript length vs GC content)",
#                 "Transcript length per transcript [bp]",
#                 "GC content per transcript [%]")
# ggsave(plot=t_v_gc, file="~/Desktop/phd/02c_auto-vs-manual/BvM/plots/BvM_VarCov_Sept_tGC.png",
#        width=8, height=10, dpi=300)
# 
# t_v_p <- my_vc(c_dat, x_dat='log10(Transcript.length..genomic.)',
#                y_dat='log10(Protein.length)',
#                ann='Annotation',
#                "Dispersion coverage (Transcript vs protein length)",
#                "Transcript length per transcript [bp]",
#                "Protein length per transcript [aa]")
# ggsave(plot=t_v_p, file="~/Desktop/phd/02c_auto-vs-manual/BvM/plots/BvM_VarCov_Sept_tP.png",
#        width=8, height=10, dpi=300)
# 
# e_v_ec <- my_vc(c_dat, x_dat='log10(Median.exon.length.of.transcript)',
#                 y_dat='log10(Exon.count.of.transcript)',
#                 ann='Annotation',
#                 "Dispersion coverage (Median exon length vs exon count)",
#                 "Median exon length per transcript [bp]",
#                 "Exon count per transcript")
# ggsave(plot=e_v_ec, file="~/Desktop/phd/02c_auto-vs-manual/BvM/plots/BvM_VarCov_Sept_eE.png",
#        width=8, height=10, dpi=300)
# 
# i_v_ic <- my_vc(c_dat, x_dat='log10(Median.intron.length.of.transcript)',
#                 y_dat='log10(Intron.count.of.transcript)',
#                 ann='Annotation',
#                 "Dispersion coverage (Median intron length vs intron count)",
#                 "Median intron length per transcript [bp]",
#                 "Intron count per transcript")
# ggsave(plot=i_v_ic, file="~/Desktop/phd/02c_auto-vs-manual/BvM/plots/BvM_VarCov_Sept_iI.png",
#        width=8, height=10, dpi=300)
# 
# egc_v_ec <- my_vc(c_dat, x_dat='log10(Median.exon.GC.content.of.transcript)',
#                   y_dat='log10(Exon.count.of.transcript)',
#                   ann='Annotation',
#                   "Dispersion coverage (Median exon GC content vs exon count)",
#                   "Median exon GC content per transcript [%]",
#                   "Exon count per transcript")
# ggsave(plot=egc_v_ec, file="~/Desktop/phd/02c_auto-vs-manual/BvM/plots/BvM_VarCov_Sept_egcEc.png",
#        width=8, height=10, dpi=300)
# 
# igc_v_ic <- my_vc(c_dat, x_dat='log10(Median.intron.GC.content.of.transcript)',
#                   y_dat='log10(Intron.count.of.transcript)',
#                   ann='Annotation',
#                   "Dispersion coverage (Median intron GC content  vs intron count)",
#                   "Median intron GC content  per transcript [bp]",
#                   "Intron count per transcript")
# ggsave(plot=igc_v_ic, file="~/Desktop/phd/02c_auto-vs-manual/BvM/plots/BvM_VarCov_Sept_igcIc.png",
#        width=8, height=10, dpi=300)
# 
# el_v_il <-  my_vc(c_dat, x_dat='log10(Median.exon.length.of.transcript)',
#                   y_dat='log10(Median.intron.length.of.transcript)',
#                   ann='Annotation',
#                   "Dispersion coverage (Median exon length vs intron length)",
#                   "Median exon length per transcript [bp]",
#                   "Median intron length per transcript [bp]")
# ggsave(plot=el_v_il, file="~/Desktop/phd/02c_auto-vs-manual/BvM/plots/BvM_VarCov_Sept_eI.png",
#        width=8, height=10, dpi=300)
# 
# t_v_ec <- my_vc(c_dat, x_dat='log10(Transcript.length..genomic.)',
#                 y_dat='log10(Exon.count.of.transcript)',
#                 ann='Annotation',
#                 "Dispersion coverage (Transcript length vs exon count)",
#                 "Transcript length [bp]",
#                 "Exon count per transcript")
# ggsave(plot=t_v_ec, file="~/Desktop/phd/02c_auto-vs-manual/BvM/plots/BvM_VarCov_Sept_tE.png",
#        width=8, height=10, dpi=300)
# 
# p_v_ec <- my_vc(c_dat, x_dat='log10(Protein.length)',
#                 y_dat='log10(Exon.count.of.transcript)',
#                 ann='Annotation',
#                 "Dispersion coverage (Protein length vs exon count)",
#                 "Protein length [aa]",
#                 "Exon count per transcript")
# ggsave(plot=p_v_ec, file="~/Desktop/phd/02c_auto-vs-manual/BvM/plots/BvM_VarCov_Sept_pE.png",
#        width=8, height=10, dpi=300)
# 
# cov_v_pl <- my_vc(c_dat, x_dat='log10(Protein.length)',
#                   y_dat='log10(Exon.coverage.of.transcript)',
#                   ann='Annotation',
#                   "Dispersion coverage (Exon coverage vs protein length)",
#                   "Protein length [aa]",
#                   "Exon coverage per transcript")
# ggsave(plot=cov_v_pl, file="~/Desktop/phd/02c_auto-vs-manual/BvM/plots/BvM_VarCov_Sept_covp.png",
#        width=8, height=10, dpi=300)
# 
# 
# 
# 
# 
# 
# 
