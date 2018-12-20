library(ggplot2)
library(scales)
library(reshape2)
library(data.table)
#library("RColorBrewer")
library(wesanderson)

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
  # Complete sets
  aros_auto_c     = read_my_table(paste("COGNATE_a-v-m_May/COGNATE_AROS_auto_compl/COGNATE_AROS_auto_compl_", data, sep=""))
  aros_b_c        = read_my_table(paste("COGNATE_a-v-m_04-2018/COGNATE_AROS_braker2_compl/COGNATE_AROS_braker2_compl_", data, sep=""))
  aros_man_c      = read_my_table(paste("COGNATE_a-v-m_May/COGNATE_AROS_man_compl/COGNATE_AROS_man_compl_", data, sep=""))
  oabi_auto_c     = read_my_table(paste("COGNATE_a-v-m_May/COGNATE_OABI_auto_compl/COGNATE_OABI_auto_compl_", data, sep=""))
  oabi_b_c        = read_my_table(paste("COGNATE_a-v-m_04-2018/COGNATE_OABI_braker2_compl/COGNATE_OABI_braker2_compl_", data, sep=""))
  oabi_man_c      = read_my_table(paste("COGNATE_a-v-m_May/COGNATE_OABI_man_compl/COGNATE_OABI_man_compl_", data, sep=""))
  ofas_auto_c     = read_my_table(paste("COGNATE_a-v-m_May/COGNATE_OFAS_auto_compl/COGNATE_OFAS_auto_compl_", data, sep=""))
  ofas_b_c        = read_my_table(paste("COGNATE_a-v-m_04-2018/COGNATE_OFAS_braker2_compl/COGNATE_OFAS_braker2_compl_", data, sep=""))
  ofas_man_c      = read_my_table(paste("COGNATE_a-v-m_May/COGNATE_OFAS_man_compl/COGNATE_OFAS_man_compl_", data, sep=""))

 # Analyzed sets
  aros_auto     = read_my_table(paste("COGNATE_a-v-m_May/COGNATE_AROS_auto2/COGNATE_AROS_auto2_", data, sep=""))
  aros_man      = read_my_table(paste("COGNATE_a-v-m_May/COGNATE_AROS_man2_nonew/COGNATE_AROS_man2_nonew_", data, sep=""))
  aros_b        = read_my_table(paste("COGNATE_a-v-m_04-2018/COGNATE_AROS_ManCvB/COGNATE_AROS_ManCvB_", data, sep=""))
  oabi_auto     = read_my_table(paste("COGNATE_a-v-m_May/COGNATE_OABI_auto2/COGNATE_OABI_auto2_", data, sep=""))
  oabi_man      = read_my_table(paste("COGNATE_a-v-m_May/COGNATE_OABI_man2_nonew/COGNATE_OABI_man2_nonew_", data, sep=""))
  oabi_b        = read_my_table(paste("COGNATE_a-v-m_04-2018/COGNATE_OABI_ManCvB/COGNATE_OABI_ManCvB_", data, sep=""))
  ofas_auto     = read_my_table(paste("COGNATE_a-v-m_May/COGNATE_OFAS_auto/COGNATE_OFAS_auto_", data, sep=""))
  ofas_man      = read_my_table(paste("COGNATE_a-v-m_May/COGNATE_OFAS_man_nonew/COGNATE_OFAS_man_nonew_", data, sep=""))
  ofas_b        = read_my_table(paste("COGNATE_a-v-m_04-2018/COGNATE_OFAS_ManCvB/COGNATE_OFAS_ManCvB_", data, sep=""))

  # Add necessary columns
  aros_auto_c$Species     <- 'Athalia rosae'
  aros_auto_c$Annotation  <- 'automatic MAKER'
  aros_auto_c$Set         <- 'complete'
  aros_auto_c$Assembly.size <- 163837890
  aros_b_c$Species      <- 'Athalia rosae'
  aros_b_c$Annotation   <- 'automatic BRAKER'
  aros_b_c$Set          <- 'complete'
  aros_b_c$Assembly.size <- 163837890
  aros_man_c$Species      <- 'Athalia rosae'
  aros_man_c$Annotation   <- 'manual'
  aros_man_c$Set          <- 'complete'
  aros_man_c$Assembly.size <- 163837890

  oabi_auto_c$Species     <- 'Orussus abietinus'
  oabi_auto_c$Annotation  <- 'automatic MAKER'
  oabi_auto_c$Set         <- 'complete'
  oabi_auto_c$Assembly.size <- 201220334
  oabi_b_c$Species      <- 'Orussus abietinus'
  oabi_b_c$Annotation   <- 'automatic BRAKER'
  oabi_b_c$Set          <- 'complete'
  oabi_b_c$Assembly.size <- 201220334
  oabi_man_c$Species      <- 'Orussus abietinus'
  oabi_man_c$Annotation   <- 'manual'
  oabi_man_c$Set          <- 'complete'
  oabi_man_c$Assembly.size <- 201220334

  ofas_auto_c$Species     <- 'Oncopeltus fasciatus'
  ofas_auto_c$Annotation  <- 'automatic MAKER'
  ofas_auto_c$Set         <- 'complete'
  ofas_auto_c$Assembly.size <- 1098693218
  ofas_b_c$Species      <- 'Oncopeltus fasciatus'
  ofas_b_c$Annotation   <- 'automatic BRAKER'
  ofas_b_c$Set          <- 'complete'
  ofas_b_c$Assembly.size <- 1098693218
  ofas_man_c$Species      <- 'Oncopeltus fasciatus'
  ofas_man_c$Annotation   <- 'manual'
  ofas_man_c$Set          <- 'complete'
  ofas_man_c$Assembly.size <- 1098693218

  aros_auto$Species     <- 'Athalia rosae'
  aros_auto$Annotation  <- 'automatic MAKER'
  aros_auto$Set         <- 'analyzed'
  aros_auto$Assembly.size <- 163837890
  aros_man$Species      <- 'Athalia rosae'
  aros_man$Annotation   <- 'manual'
  aros_man$Set          <- 'analyzed'
  aros_man$Assembly.size <- 163837890
  aros_b$Species     <- 'Athalia rosae'
  aros_b$Annotation  <- 'automatic BRAKER'
  aros_b$Set         <- 'analyzed'
  aros_b$Assembly.size <- 163837890

  oabi_auto$Species     <- 'Orussus abietinus'
  oabi_auto$Annotation  <- 'automatic MAKER'
  oabi_auto$Set         <- 'analyzed'
  oabi_auto$Assembly.size <- 201220334
  oabi_man$Species      <- 'Orussus abietinus'
  oabi_man$Annotation   <- 'manual'
  oabi_man$Set          <- 'analyzed'
  oabi_man$Assembly.size <- 201220334
  oabi_b$Species     <- 'Orussus abietinus'
  oabi_b$Annotation  <- 'automatic BRAKER'
  oabi_b$Set         <- 'analyzed'
  oabi_b$Assembly.size <- 201220334

  ofas_auto$Species     <- 'Oncopeltus fasciatus'
  ofas_auto$Annotation  <- 'automatic MAKER'
  ofas_auto$Set         <- 'analyzed'
  ofas_auto$Assembly.size <- 1098693218
  ofas_man$Species      <- 'Oncopeltus fasciatus'
  ofas_man$Annotation   <- 'manual'
  ofas_man$Set          <- 'analyzed'
  ofas_man$Assembly.size <- 1098693218
  ofas_b$Species     <- 'Oncopeltus fasciatus'
  ofas_b$Annotation  <- 'automatic BRAKER'
  ofas_b$Set         <- 'analyzed'
  ofas_b$Assembly.size <- 1098693218

  #and combine into your new data frame
  SppData <- rbind(aros_auto_c, aros_b_c, aros_man_c,
                   oabi_auto_c, oabi_b_c, oabi_man_c,
                   ofas_auto_c, ofas_b_c, ofas_man_c,
                   aros_auto, aros_man, aros_b,
                   oabi_auto, oabi_man, oabi_b,
                   ofas_auto, ofas_man, ofas_b
         )

  return(SppData)
}

t_dat <- get_data('07-transcript_general.tsv')
e_dat <- get_data('09-transcript_exons.tsv')
i_dat <- get_data('10-transcript_introns.tsv')

c_dat <- cbind(t_dat, e_dat, i_dat)


############################# DENSITIES ####################################
species_order <- c('Athalia rosae', 'Orussus abietinus', # Hymenoptera
                  'Oncopeltus fasciatus'# Hemiptera
                  )

c_dat$Species_ordered = factor(c_dat$Species, levels = species_order)

# condense density plot == Poster plot!
#library(data.table)
c_dat_2 <- c_dat
setnames(c_dat_2, old=c("Transcript.length..genomic.", "Protein.length",
                      "Exon.count.of.transcript", "Median.exon.length.of.transcript",
                      "Median.intron.length.of.transcript"),
                new=c("Transcript length [bp]", "Protein length [aa]",
                      "Exon count per transcript",
                      "Median exon length per transcript [bp]",
                      "Median intron length per transcript [bp]"))


c_melt <- melt(c_dat_2, id.vars = c("Species_ordered", "Annotation", "Set"), measure.vars =
                 c("Transcript length [bp]", "Protein length [aa]",
                   "Exon count per transcript",
                   "Median exon length per transcript [bp]",
                   "Median intron length per transcript [bp]"))

# define x axis label format
point <- format_format(big.mark = ",", decimal.mark = ".", scientific = FALSE)


### Condensed densities 

con_dens <-  ggplot(c_melt, aes(x=value, color=variable, linetype=Annotation)) +
  my_theme +
  geom_line(stat="density", size=1) +
  scale_x_continuous(trans = 'log10', expand = c(0.01, 0), labels = point,
                     breaks = c(0, 1,2, 10, 100, 1000, 10000, 100000)
  ) +
  facet_grid(Species_ordered ~ Set, labeller = labeller(Species_ordered = label_wrap_gen(15))) +
  labs(title = "a)",
       x = "Value",
       y = "Density",
       color = "Data") +
  guides(colour = guide_legend(override.aes = list(size=1.8)),
         linetype = guide_legend(keywidth = 4, override.aes = list(size=1.8))) +
  scale_color_manual(values = wes_palette(name = "Cavalcanti1")) +
  scale_linetype_manual(breaks = c("automatic MAKER", "automatic BRAKER", "manual"),
                        values = c("dashed", "solid", "dotted"))

pdf("~/Documents/00_Projects/AvM/ManCvB2_2018-08/ManCvBvM_densities_2018-08-09.pdf",
    width=14, height = 10)
con_dens
dev.off()

## ridges
con_dens_ridge <- ggplot(c_melt, aes(x=value, color=variable, y=Annotation)) +
  my_theme +
  geom_density_ridges(alpha=0, scale=0.9, size =1) +
  scale_x_continuous(trans = 'log10', expand = c(0.01, 0), labels = point,
                     breaks = c(0, 1,2, 10, 100, 1000, 10000, 100000)
  ) +
  facet_grid(Species_ordered ~ Set, labeller = labeller(Species_ordered = label_wrap_gen(15))) +
  labs(title = "a)",
       x = "Value",
       y = "Density",
       color = "Data") +
  guides(colour = guide_legend(override.aes = list(size=1.8)),
         linetype = guide_legend(override.aes = list(size=1.8))) +
  scale_color_manual(values = wes_palette(name = "Cavalcanti1")) +
  scale_linetype_manual(breaks = c("automatic MAKER", "automatic BRAKER", "manual"),
                        values = c("dashed", "solid", "dotted"))

pdf("~/Documents/00_Projects/AvM/Plots_2018-08/Parts/condensed_densities_ridges_BvM_2018-08-16.pdf", 
    width=15, height = 8)
con_dens_ridge
dev.off()


# cumulative distribution function
complete <- c_melt[grep("complete", c_melt$Set), ]
analyzed <- c_melt[grep("analyzed", c_melt$Set), ]

cum_dist_compl <-  ggplot(complete, aes(x=value, color=Annotation, linetype = Annotation)) +
  my_theme +
  stat_ecdf(pad = FALSE, na.rm = TRUE, size = 1) +
  facet_grid(Species_ordered ~ variable, labeller = labeller(Species_ordered = label_wrap_gen(15), 
                                                             variable = label_wrap_gen(15)), scales= "free") +
  scale_x_continuous(trans = 'log10', expand = c(0.01, 0), labels = point,
                     breaks = c(0, 1, 10, 100, 1000, 10000, 100000, 1000000)
  ) +
  labs(title = "a) BRAKER vs. MAKER empirical cumulative distribution functions",  subtitle = "'Complete' sets",
       x = "Value", y = "F (Value)", color = "Annotation") +
  guides(colour = guide_legend(override.aes = list(size=1.8))) +
  scale_color_manual(values = c("#FF0000", "#00A08A","black"))+
  scale_linetype_manual(values = c("solid", "dashed", "dotted"))

cum_dist_analy <-  ggplot(analyzed, aes(x=value, color=Annotation, linetype = Annotation)) +
  my_theme +
  stat_ecdf(pad = FALSE, na.rm = TRUE, size = 1) +
  facet_grid(Species_ordered ~ variable, labeller = labeller(Species_ordered = label_wrap_gen(15), 
                                                             variable = label_wrap_gen(15)), scales= "free") +
  scale_x_continuous(trans = 'log10', expand = c(0.01, 0), labels = point,
                     breaks = c(0, 1, 10, 100, 1000, 10000, 100000, 1000000)
  ) +
  labs(title = "b) BRAKER vs. MAKER empirical cumulative distribution functions",  subtitle = "'Analyzed' sets",
       x = "Value", y = "F (Value)", color = "Annotation") +
  guides(colour = guide_legend(override.aes = list(size=1.8))) +
  scale_color_manual(values = c("#FF0000", "#00A08A","black"))+
  scale_linetype_manual(values = c("solid", "dashed", "dotted"))

pdf("~/Documents/00_Projects/AvM/Plots_2018-08/Parts/BvM_cum_dist_2018-09-03.pdf", width=13, height = 6)
cum_dist_compl
cum_dist_analy
dev.off()



######################### VAR COVERAGE ##########################################
# 
# my_vc <- function(dat, x_dat, y_dat, set, ann, tit, x_tit, y_tit) {
#   ggplot(dat, aes_string(x=x_dat, y=y_dat)) +
#     my_theme +
#     theme(strip.text.y = element_blank()) + # removes species names!
#     geom_point(aes_string(colour = set)) +
#     geom_point(data = dat, aes_string(x=x_dat, y=y_dat, colour = set)) +
#     geom_smooth(method=lm, se=TRUE, fullrange=TRUE, show.legend = FALSE,
#                 aes_string(color=set)) +
#     geom_smooth(method=lm, se=FALSE, fullrange=TRUE, show.legend = TRUE,
#                 aes_string(color=set)) +
#    # scale_linetype_manual(, values = c(1,2)) +
#     # geom_rug(alpha=0.1) +
#     facet_grid(Species_ordered ~ Annotation, labeller = labeller(Species_ordered = label_wrap_gen(15))) +
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
#     ) +
#     scale_color_manual(values = wes_palette(name = "Royal1"))
# }
# 
# ec_v_el <- my_vc(c_dat, x_dat='log10(Exon.count.of.transcript)',
#                  y_dat='log10(Median.exon.length.of.transcript)',
#                  set='Set', ann='Annotation',
#                  "Dispersion coverage (Exon count vs exon length)",
#                  "Exon count per transcript",
#                  "Median exon length per transcript [bp]")
# ggsave(plot=ec_v_el, file="~/Desktop/phd/02c_auto-vs-manual/EG_plots/VarCov_Oct-ccap-refseq_ec_v_el.png",
#        width=5, height=15, dpi=300)
# ec_v_egc <- my_vc(c_dat, x_dat='log10(Exon.count.of.transcript)',
#               y_dat='log10(Median.exon.GC.content.of.transcript)',
#               set='Set', ann='Annotation',
#               "Dispersion coverage (Exon count vs exon GC content)",
#               "Exon count per transcript",
#               "Median exon GC content per transcript [%]")
# ggsave(plot=ec_v_egc, file="~/Desktop/phd/02c_auto-vs-manual/EG_plots/VarCov_Oct-ccap-refseq_ec_v_egc.png",
#        width=5, height=15, dpi=300)
# 
# ic_v_il <- my_vc(c_dat, x_dat='log10(Intron.count.of.transcript)',
#                  y_dat='log10(Median.intron.length.of.transcript)',
#                  set='Set', ann='Annotation',
#                  "Dispersion coverage (Intron count vs intron length)",
#                  "Intron count per transcript",
#                  "Median intron length per transcript [bp]")
# ggsave(plot=ic_v_il, file="~/Desktop/phd/02c_auto-vs-manual/EG_plots/VarCov_Oct-ccap-refseq_ic_v_il.png",
#        width=5, height=15, dpi=300)
# ic_v_igc <- my_vc(c_dat, x_dat='log10(Intron.count.of.transcript)',
#                   y_dat='log10(Median.intron.GC.content.of.transcript)',
#                   set='Set', ann='Annotation',
#                   "Dispersion coverage (Intron count vs intron GC content)",
#                   "Intron count per transcript",
#                   "Median intron GC content per transcript [%]")
# ggsave(plot=ic_v_igc, file="~/Desktop/phd/02c_auto-vs-manual/EG_plots/VarCov_Oct-ccap-refseq_ic_v_igc.png",
#        width=5, height=15, dpi=300)
# 
# 
# 
# i_v_gc <- my_vc(c_dat_2, x_dat='log10(Median.intron.length.of.transcript)',
#                        y_dat='log10(Median.intron.GC.content.of.transcript)',
#                        set='Set', ann='Annotation',
#                        "Dispersion coverage (Intron length vs intron GC content)",
#                        "Median intron length per transcript [bp]",
#                        "Median intron GC content per transcript [%]")
# ggsave(plot=i_v_gc, file="~/Desktop/phd/02c_auto-vs-manual/EG_plots/VarCov_Oct-ccap-refseq_iiGC.png",
#        width=8, height=10, dpi=300)
# 
# e_v_gc <- my_vc(c_dat_2, x_dat='log10(Median.exon.length.of.transcript)',
#                 y_dat='log10(Median.exon.GC.content.of.transcript)',
#                 set='Set', ann='Annotation',
#                 "Dispersion coverage (Exon length vs exon GC content)",
#                 "Median exon length per transcript [bp]",
#                 "Median exon GC content per transcript [%]")
# ggsave(plot=e_v_gc, file="~/Desktop/phd/02c_auto-vs-manual/EG_plots/VarCov_Oct-ccap-refseq_eeGC.png",
#        width=8, height=10, dpi=300)
# 
# t_v_gc <- my_vc(c_dat_2, x_dat='log10(Transcript.length..genomic.)',
#                 y_dat='log10(Transcript.GC.content.without.ambiguity)',
#                 set='Set', ann='Annotation',
#                 "Dispersion coverage (Transcript length vs GC content)",
#                 "Transcript length per transcript [bp]",
#                 "GC content per transcript [%]")
# ggsave(plot=t_v_gc, file="~/Desktop/phd/02c_auto-vs-manual/EG_plots/VarCov_Oct-ccap-refseq_tGC.png",
#        width=8, height=10, dpi=300)
# 
# t_v_p <- my_vc(c_dat_2, x_dat='log10(Transcript.length..genomic.)',
#                y_dat='log10(Protein.length)',
#                set='Set', ann='Annotation',
#                "Dispersion coverage (Transcript vs protein length)",
#                "Transcript length per transcript [bp]",
#                "Protein length per transcript [aa]")
# ggsave(plot=t_v_p, file="~/Desktop/phd/02c_auto-vs-manual/EG_plots/VarCov_Oct-ccap-refseq_tP.png",
#        width=8, height=10, dpi=300)
# 
# e_v_ec <- my_vc(c_dat_2, x_dat='log10(Median.exon.length.of.transcript)',
#                 y_dat='log10(Exon.count.of.transcript)',
#                 set='Set', ann='Annotation',
#                "Dispersion coverage (Median exon length vs exon count)",
#                "Median exon length per transcript [bp]",
#                "Exon count per transcript")
# ggsave(plot=e_v_ec, file="~/Desktop/phd/02c_auto-vs-manual/EG_plots/VarCov_Oct-ccap-refseq_eE.png",
#        width=8, height=10, dpi=300)
# 
# i_v_ic <- my_vc(c_dat_2, x_dat='log10(Median.intron.length.of.transcript)',
#                 y_dat='log10(Intron.count.of.transcript)',
#                 set='Set', ann='Annotation',
#                 "Dispersion coverage (Median intron length vs intron count)",
#                 "Median intron length per transcript [bp]",
#                 "Intron count per transcript")
# ggsave(plot=i_v_ic, file="~/Desktop/phd/02c_auto-vs-manual/EG_plots/VarCov_Oct-ccap-refseq_iI.png",
#        width=8, height=10, dpi=300)
# 
# egc_v_ec <- my_vc(c_dat_2, x_dat='log10(Median.exon.GC.content.of.transcript)',
#                 y_dat='log10(Exon.count.of.transcript)',
#                 set='Set', ann='Annotation',
#                 "Dispersion coverage (Median exon GC content vs exon count)",
#                 "Median exon GC content per transcript [%]",
#                 "Exon count per transcript")
# ggsave(plot=egc_v_ec, file="~/Desktop/phd/02c_auto-vs-manual/EG_plots/VarCov_Oct-ccap-refseq_egcEc.png",
#        width=8, height=10, dpi=300)
# 
# igc_v_ic <- my_vc(c_dat_2, x_dat='log10(Median.intron.GC.content.of.transcript)',
#                 y_dat='log10(Intron.count.of.transcript)',
#                 set='Set', ann='Annotation',
#                 "Dispersion coverage (Median intron GC content  vs intron count)",
#                 "Median intron GC content  per transcript [bp]",
#                 "Intron count per transcript")
# ggsave(plot=igc_v_ic, file="~/Desktop/phd/02c_auto-vs-manual/EG_plots/VarCov_Oct-ccap-refseq_igcIc.png",
#        width=8, height=10, dpi=300)
# 
# el_v_il <-  my_vc(c_dat_2, x_dat='log10(Median.exon.length.of.transcript)',
#                   y_dat='log10(Median.intron.length.of.transcript)',
#                   set='Set', ann='Annotation',
#                   "Dispersion coverage (Median exon length vs intron length)",
#                   "Median exon length per transcript [bp]",
#                   "Median intron length per transcript [bp]")
# ggsave(plot=el_v_il, file="~/Desktop/phd/02c_auto-vs-manual/EG_plots/VarCov_Oct-ccap-refseq_eI.png",
#        width=8, height=10, dpi=300)
# 
# t_v_ec <- my_vc(c_dat, x_dat='log10(Transcript.length..genomic.)',
#                 y_dat='log10(Exon.count.of.transcript)',
#                 set='Set', ann='Annotation',
#                 "Dispersion coverage (Transcript length vs exon count)",
#                 "Transcript length [bp]",
#                 "Exon count per transcript")
# ggsave(plot=t_v_ec, file="~/Desktop/phd/02c_auto-vs-manual/EG_plots/VarCov_Oct-ccap-refseq_tE.png",
#        width=8, height=10, dpi=300)
# 
# p_v_ec <- my_vc(c_dat_2, x_dat='log10(Protein.length)',
#                 y_dat='log10(Exon.count.of.transcript)',
#                 set='Set', ann='Annotation',
#                 "Dispersion coverage (Protein length vs exon count)",
#                 "Protein length [aa]",
#                 "Exon count per transcript")
# ggsave(plot=p_v_ec, file="~/Desktop/phd/02c_auto-vs-manual/EG_plots/VarCov_Oct-ccap-refseq_pE.png",
#        width=8, height=10, dpi=300)
# 
# cov_v_pl <- my_vc(c_dat_2, x_dat='log10(Protein.length)',
#                 y_dat='log10(Exon.coverage.of.transcript)',
#                 set='Set', ann='Annotation',
#                 "Dispersion coverage (Exon coverage vs protein length)",
#                 "Protein length [aa]",
#                 "Exon coverage per transcript")
# ggsave(plot=cov_v_pl, file="~/Desktop/phd/02c_auto-vs-manual/EG_plots/VarCov_Oct-ccap-refseq_covp.png",
#        width=8, height=10, dpi=300)
# 
# 
# 
# 
# 
# # pdf("~/Desktop/phd/02c_auto-vs-manual/plots/VarCov_Oct.pdf", width=7, height = 8)
# # t_v_gc
# # t_v_p
# # dev.off()
# 
# 
# ### DONE ###
# 
# 
# 





