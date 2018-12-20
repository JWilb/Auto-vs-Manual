library(ggplot2)
library(ggridges)
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
  table = read.csv(paste(c("/data/home/Desktop/PhD/02c_AvM/COGNATE_a-v-m_May/",
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

read_my_table2 <- function(file) {
  table = read.csv(paste(c("/u/people/jeannewb/Documents/00_Projects/PhD/02_AvM/COGNATE_analyzed-CRs_2018-12/",
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

read_my_table3 <- function(file) {
  table = read.csv(paste(c("/u/people/jeannewb/Documents/00_Projects/PhD/02_AvM/COGNATE_analyzed_2018-08/",
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
  ofas_auto_c     = read_my_table(paste("COGNATE_OFAS_auto_compl/COGNATE_OFAS_auto_compl_", data, sep=""))
  ofas_man_c      = read_my_table(paste("COGNATE_OFAS_man_compl/COGNATE_OFAS_man_compl_", data, sep=""))
  # Analyzed sets
  ofas_auto     = read_my_table3(paste("COGNATE_OFAS_auto_subset_", data, sep=""))
  ofas_man      = read_my_table3(paste("COGNATE_OFAS_man_subset_", data, sep=""))
  # ManCur new
  ofas_new  = read_my_table3(paste("COGNATE_OFAS_new_subset_", data, sep = ""))
  
  # Add necessary columns
  ofas_auto_c$Annotation  <- 'automatic'
  ofas_auto_c$Set         <- 'complete'
  
  ofas_man_c$Annotation   <- 'manual'
  ofas_man_c$Set          <- 'complete'
  
  # con_dens
  ofas_new$Annotation     <- 'new'
  ofas_new$Set            <- 'analyzed'

  ofas_auto$Annotation  <- 'automatic'
  ofas_auto$Set         <- 'analyzed'

  ofas_man$Annotation   <- 'manual'
  ofas_man$Set          <- 'analyzed'

  #and combine into your new data frame
  SppData <- rbind(ofas_auto_c, ofas_man_c,
                   ofas_auto, ofas_man, ofas_new
         )

  return(SppData)
}


get_data_novo <- function(data) {
  # chemoreceptors 
  ofas_cps  = read_my_table2(paste("COGNATE_OFAS_CRs_subset_", data, sep = ""))
  # cuticle proteins
  ofas_crs =  read_my_table2(paste("COGNATE_OFAS_CPs_subset_", data, sep = ""))
  #and combine into your new data frame
  SppData <- rbind(ofas_cps, ofas_crs)
  return(SppData)
}


# Data for auto, man, new
t_dat <- get_data('07-transcript_general.tsv')
e_dat <- get_data('09-transcript_exons.tsv')
i_dat <- get_data('10-transcript_introns.tsv')

c_dat <- cbind(t_dat, e_dat, i_dat)

# Data for cuticle proteins and chemoreceptors
t_dat_novo <- get_data_novo('07-transcript_general.tsv')
e_dat_novo <- get_data_novo('09-transcript_exons.tsv')
i_dat_novo <- get_data_novo('10-transcript_introns.tsv')

c_dat_novo <- cbind(t_dat_novo, e_dat_novo, i_dat_novo)



# define x axis label format
point <- format_format(big.mark = ",", decimal.mark = ".", scientific = FALSE)
############################# DENSITIES ####################################
c_dat_2 <- c_dat
setnames(c_dat_2, old=c("Transcript.length..genomic.", "Protein.length",
                        "Exon.count.of.transcript", "Median.exon.length.of.transcript",
                        "Median.intron.length.of.transcript"),
         new=c("Transcript length [bp]", "Protein length [aa]",
               "Exon count per transcript",
               "Median exon length per transcript [bp]",
               "Median intron length per transcript [bp]"))


c_melt <- melt(c_dat_2, id.vars = c("Annotation", "Set"), measure.vars =
                 c("Transcript length [bp]", "Protein length [aa]",
                   "Exon count per transcript",
                   "Median exon length per transcript [bp]",
                   "Median intron length per transcript [bp]"))




c_dat_novo_2 <- c_dat_novo
setnames(c_dat_novo_2, old=c("Transcript.length..genomic.", "Protein.length",
                      "Exon.count.of.transcript", "Median.exon.length.of.transcript",
                      "Median.intron.length.of.transcript",
                      "curation.status"),
                new=c("Transcript length [bp]", "Protein length [aa]",
                      "Exon count per transcript",
                      "Median exon length per transcript [bp]",
                      "Median intron length per transcript [bp]",
                      "Curation_status"))


c_melt_novo <- melt(c_dat_novo_2, id.vars = c("Curation_status", "Class"), measure.vars =
                 c("Transcript length [bp]", "Protein length [aa]",
                   "Exon count per transcript",
                   "Median exon length per transcript [bp]",
                   "Median intron length per transcript [bp]"))


### Condensed densities

con_dens <-  #ggplot(c_melt_novo, aes(x=value, color=variable)) +
  ggplot(c_melt, aes(x=value, color=variable)) +
  my_theme +
  geom_line(stat="density", size=1) +
  #geom_density_ridges(alpha=0, scale=0.9, size =1) +
  scale_x_continuous(trans = 'log10', expand = c(0.01, 0), labels = point,
                     breaks = c(0, 1,2, 10, 100, 1000, 10000, 100000, 1000000, 10000000)
  ) +
  #facet_grid(Curation_status ~ .) +
  #facet_grid(Curation_status ~ Class) +
  facet_grid(Set ~ Annotation) +
  #labs(title = "a) OFAS CRs and CPs: curated (n = 201) vs de novo (n = 113)",
  labs(title = "a) OFAS",
       x = "Value",
       y = "Density",
       color = "Data") +
  guides(colour = guide_legend(override.aes = list(size=1.8)),
         linetype = guide_legend(override.aes = list(size=1.8))) +
  scale_color_manual(values = wes_palette(name = "Cavalcanti1"))

pdf("~/Documents/00_Projects/PhD/02_AvM/Plots_2018-12/OFAS_condensed_densities_2018-12-14.pdf", 
    width=15, height = 6)
con_dens
dev.off()





### KS tests
cur     <- c_dat_novo[grep("curated MAKER model", c_dat_novo$Curation_status), ]
novo    <- c_dat_novo[grep("de novo model",       c_dat_novo$Curation_status), ]
ogs_pre <- c_dat[grep("manual",                   c_dat$Annotation), ]
ogs     <- ogs_pre[grep("complete",               ogs_pre$Set), ]
new_pre <- c_dat[grep("new",                      c_dat$Annotation), ]
new     <- new_pre[grep("analyzed",               new_pre$Set), ]
cut     <- c_dat_novo[grep("cuticle protein",     c_dat_novo$Class), ]
gus     <- c_dat_novo[grep("gustatory",     c_dat_novo$Class), ]
odo     <- c_dat_novo[grep("odorant",     c_dat_novo$Class), ]
ion     <- c_dat_novo[grep("ionotropic",     c_dat_novo$Class), ]

cut_cur     <- cut[grep("curated",     cut$Curation_status), ]
gus_cur     <- gus[grep("curated",   gus$Curation_status), ]
odo_cur     <- odo[grep("curated",     odo$Curation_status), ]
ion_cur     <- ion[grep("curated",  ion$Curation_status), ]
cut_nov     <- cut[grep("novo",     cut$Curation_status), ]
gus_nov     <- gus[grep("novo",     gus$Curation_status), ]
odo_nov     <- odo[grep("novo",     odo$Curation_status), ]
ion_nov     <- ion[grep("novo",   ion$Curation_status), ]

# CRs and CPs: curated vs de novo
ks.test(cur$"Transcript length [bp]", novo$"Transcript length [bp]")
# D = 0.1563, p-value = 0.05837
ks.test(cur$"Protein length [aa]", novo$"Protein length [aa]")
# D = 0.50685, p-value < 2.2e-16
ks.test(cur$"Exon count per transcript", novo$"Exon count per transcript")
# D = 0.24136, p-value = 0.0004375
ks.test(cur$"Median exon length per transcript [bp]", novo$"Median exon length per transcript [bp]")
# D = 0.29274, p-value = 8.257e-06
ks.test(cur$"Median intron length per transcript [bp]", novo$"Median intron length per transcript [bp]")
# D = 0.18983, p-value = 0.01626

# CRs and CPs: curated vs OGS
ks.test(cur$"Transcript length [bp]", ogs$"Transcript.length..genomic.")
# D = 0.19529, p-value = 5.133e-07
ks.test(cur$"Protein length [aa]", ogs$"Protein.length")
# D = 0.11002, p-value = 0.01618
ks.test(cur$"Exon count per transcript", ogs$"Exon.count.of.transcript")
# D = 0.27551, p-value = 1.528e-13
ks.test(cur$"Median exon length per transcript [bp]", ogs$"Median.exon.length.of.transcript")
# D = 0.1727, p-value = 1.402e-05
ks.test(cur$"Median intron length per transcript [bp]", ogs$"Median.intron.length.of.transcript")
# D = 0.10436, p-value = 0.02865

# CRs and CPs: novo vs OGS
ks.test(novo$"Transcript length [bp]", ogs$"Transcript.length..genomic.")
# D = 0.20581, p-value = 0.000147
ks.test(novo$"Protein length [aa]", ogs$"Protein.length")
# D = 0.48658, p-value < 2.2e-16
ks.test(novo$"Exon count per transcript", ogs$"Exon.count.of.transcript")
# D = 0.30804, p-value = 1.099e-09
ks.test(novo$"Median exon length per transcript [bp]", ogs$"Median.exon.length.of.transcript")
# D = 0.2823, p-value = 3.34e-08
ks.test(novo$"Median intron length per transcript [bp]", ogs$"Median.intron.length.of.transcript")
# D = 0.11541, p-value = 0.1377

# CRs and CPs: novo vs new
ks.test(novo$"Transcript length [bp]", new$"Transcript.length..genomic.")
# D = 0.23311, p-value = 0.001469
ks.test(novo$"Protein length [aa]", new$"Protein.length")
# D = 0.23278, p-value = 0.001499
ks.test(novo$"Exon count per transcript", new$"Exon.count.of.transcript")
# D = 0.2623, p-value = 0.0002153
ks.test(novo$"Median exon length per transcript [bp]", new$"Median.exon.length.of.transcript")
# D = 0.12241, p-value = 0.2728
ks.test(novo$"Median intron length per transcript [bp]", new$"Median.intron.length.of.transcript")
# D = 0.050575, p-value = 0.9993

# cuticle proteins vs new
ks.test(cut$"Transcript length [bp]", new$"Transcript.length..genomic.")
# D = 0.24374, p-value = 8.687e-05
ks.test(cut$"Protein length [aa]", new$"Protein.length")
# D = 0.36566, p-value = 3.043e-10
ks.test(cut$"Exon count per transcript", new$"Exon.count.of.transcript")
# D = 0.28809, p-value = 1.61e-06
ks.test(cut$"Median exon length per transcript [bp]", new$"Median.exon.length.of.transcript")
# D = 0.2071, p-value = 0.001418
ks.test(cut$"Median intron length per transcript [bp]", new$"Median.intron.length.of.transcript")
#  = 0.18204, p-value = 0.0224

# gustatory vs new
ks.test(gus$"Transcript length [bp]", new$"Transcript.length..genomic.")
# D = 0.26484, p-value = 0.0001987
ks.test(gus$"Protein length [aa]", new$"Protein.length")
# D = 0.3208, p-value = 2.679e-06
ks.test(gus$"Exon count per transcript", new$"Exon.count.of.transcript")
# D = 0.29467, p-value = 2.217e-05
ks.test(gus$"Median exon length per transcript [bp]", new$"Median.exon.length.of.transcript")
# D = 0.15953, p-value = 0.07057
ks.test(gus$"Median intron length per transcript [bp]", new$"Median.intron.length.of.transcript")
# D = 0.059189, p-value = 0.9928

# odorant vs new
ks.test(odo$"Transcript length [bp]", new$"Transcript.length..genomic.")
# D = 0.49294, p-value = 0.01342
ks.test(odo$"Protein length [aa]", new$"Protein.length")
# D = 0.30435, p-value = 0.296
ks.test(odo$"Exon count per transcript", new$"Exon.count.of.transcript")
# D = 0.37493, p-value = 0.1106
ks.test(odo$"Median exon length per transcript [bp]", new$"Median.exon.length.of.transcript")
# D = 0.36251, p-value = 0.1335
ks.test(odo$"Median intron length per transcript [bp]", new$"Median.intron.length.of.transcript")
# D = 0.16134, p-value = 0.9569

# ionotropic vs new
ks.test(ion$"Transcript length [bp]", new$"Transcript.length..genomic.")
# D = 0.47826, p-value = 0.005522
ks.test(ion$"Protein length [aa]", new$"Protein.length")
# D = 0.48137, p-value = 0.005114
ks.test(ion$"Exon count per transcript", new$"Exon.count.of.transcript")
# D = 0.6118, p-value = 0.0001299
ks.test(ion$"Median exon length per transcript [bp]", new$"Median.exon.length.of.transcript")
# D = 0.37888, p-value = 0.04955
ks.test(ion$"Median intron length per transcript [bp]", new$"Median.intron.length.of.transcript")
# D = 0.22869, p-value = 0.5768


# CRs and CPs: cuticle: curated vs de novo
ks.test(cut_cur$"Transcript length [bp]", cut_nov$"Transcript length [bp]")
# D = 0.45115, p-value = 0.4037
ks.test(cut_cur$"Protein length [aa]", cut_nov$"Protein length [aa]")
# D = 0.25862, p-value = 0.9562
ks.test(cut_cur$"Exon count per transcript", cut_nov$"Exon count per transcript")
# D = 0.3908, p-value = 0.589
ks.test(cut_cur$"Median exon length per transcript [bp]", cut_nov$"Median exon length per transcript [bp]")
# D = 0.28161, p-value = 0.9158
ks.test(cut_cur$"Median intron length per transcript [bp]", cut_nov$"Median intron length per transcript [bp]")
# D = 0.34118, p-value = 0.7532


# CRs and CPs: gustatory: curated vs de novo
ks.test(gus_cur$"Transcript length [bp]", gus_nov$"Transcript length [bp]")
# D = 0.42033, p-value = 0.006093
ks.test(gus_cur$"Protein length [aa]", gus_nov$"Protein length [aa]")
# D = 0.37857, p-value = 0.0182
ks.test(gus_cur$"Exon count per transcript", gus_nov$"Exon count per transcript")
# D = 0.24066, p-value = 0.2984
ks.test(gus_cur$"Median exon length per transcript [bp]", gus_nov$"Median exon length per transcript [bp]")
# D = 0.30934, p-value = 0.08673
ks.test(gus_cur$"Median intron length per transcript [bp]", gus_nov$"Median intron length per transcript [bp]")
# D = 0.1875, p-value = 0.6272

# CRs and CPs: odorant: curated vs de novo
ks.test(odo_cur$"Transcript length [bp]", odo_nov$"Transcript length [bp]")
# D = 0.625, p-value = 0.303
ks.test(odo_cur$"Protein length [aa]", odo_nov$"Protein length [aa]")
# D = 0.20833, p-value = 1
ks.test(odo_cur$"Exon count per transcript", odo_nov$"Exon count per transcript")
# D = 0.75, p-value = 0.1717
ks.test(odo_cur$"Median exon length per transcript [bp]", odo_nov$"Median exon length per transcript [bp]")
# D = 0.625, p-value = 0.303
ks.test(odo_cur$"Median intron length per transcript [bp]", odo_nov$"Median intron length per transcript [bp]")
# D = 0.5, p-value = 0.5636


# CRs and CPs: ionotropic: curated vs de novo
ks.test(ion_cur$"Transcript length [bp]", ion_nov$"Transcript length [bp]")
# D = 0.5, p-value = 0.4096
ks.test(ion_cur$"Protein length [aa]", ion_nov$"Protein length [aa]")
# D = 0.5, p-value = 0.4096
ks.test(ion_cur$"Exon count per transcript", ion_nov$"Exon count per transcript")
# D = 0.7, p-value = 0.1216
ks.test(ion_cur$"Median exon length per transcript [bp]", ion_nov$"Median exon length per transcript [bp]")
# D = 0.35, p-value = 0.8752
ks.test(ion_cur$"Median intron length per transcript [bp]", ion_nov$"Median intron length per transcript [bp]")
# D = 0.44444, p-value = 0.5175

## ridges
# con_dens_ridge <- ggplot(c_melt_novo, aes(x=value, color=variable, y=Curation_status)) +
#   my_theme +
#   geom_density_ridges(alpha=0, scale=0.9, size =1) +
#   scale_x_continuous(trans = 'log10', expand = c(0.01, 0), labels = point,
#                      breaks = c(0, 1,2, 10, 100, 1000, 10000, 100000)
#   ) +
#   facet_grid(. ~ Class) +
#   labs(title = "a)",
#        x = "Value",
#        y = "Density",
#        color = "Data") +
#   guides(colour = guide_legend(override.aes = list(size=1.8)),
#          linetype = guide_legend(override.aes = list(size=1.8))) +
#   scale_color_manual(values = wes_palette(name = "Cavalcanti1"))
# 
# pdf("~/Documents/00_Projects/AvM/Plots_2018-08/Parts/condensed_densities_ridges_onlyAnalyzed_2018-08-14.pdf",
#     width=9, height = 12)
# con_dens_ridge
# dev.off()


# log c_dat
#c_melt_log = c_melt
#c_melt_log$value = log10(c_melt_log$value)

# cumulative distribution function
## log or no log? :)
# if the scale is log transformed, use normal data but save as log
cur <- c_melt_novo[grep("curated MAKER model", c_melt_novo$Curation_status), ]
novo <- c_melt_novo[grep("de novo model", c_melt_novo$Curation_status), ]

cum_dist_compl <-  ggplot(cur, aes(x=value)) +
  my_theme +
  stat_ecdf(pad = FALSE, na.rm = TRUE, size = 1) +
  facet_grid(Class ~ variable, labeller = labeller(Species_ordered = label_wrap_gen(15), 
                                                             variable = label_wrap_gen(15)), scales= "free") +
  scale_x_continuous(trans = 'log10', expand = c(0.01, 0), labels = point,
                     breaks = c(0, 1, 10, 100, 1000, 10000, 100000, 1000000)
  ) +
  labs(title = "a)  empirical cumulative distribution functions",
       subtitle = "'Curated' sets", x = "Value", y = "F (Value)") +
  guides(colour = guide_legend(override.aes = list(size=1.8)))

cum_dist_analy <-  ggplot(novo, aes(x=value)) +
  my_theme +
  stat_ecdf(pad = FALSE, na.rm = TRUE, size = 1) +
  facet_grid(Class ~ variable, labeller = labeller(Species_ordered = label_wrap_gen(15), 
                                                             variable = label_wrap_gen(15)), scales= "free") +
  scale_x_continuous(trans = 'log10', expand = c(0.01, 0), labels = point,
                     breaks = c(0, 1, 10, 100, 1000, 10000, 100000, 1000000)
  ) +
  labs(title = "b) Automatic vs. manual annotation empirical cumulative distribution functions",
       subtitle = "'Analyzed' sets", x = "Value", y = "F (Value)", color = "Annotation") +
  guides(colour = guide_legend(override.aes = list(size=1.8))) +
  scale_color_manual(values = c("#899DA4", "#C93312","#E1AF00")) +
  scale_linetype_manual(values = c("solid", "dashed", "dotted"))

pdf("~/Documents/00_Projects/AvM/Plots_2018-08/Parts/AvM_cum_dist_2018-09-03.pdf", width=13, height = 12)
cum_dist_compl
cum_dist_analy
dev.off()



### DONE ###






