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
  table = read.csv(paste(c("/u/people/jeannewb/Documents/00_Projects/AvM/MCnew_2018-08/COGNATE_MCnew/",
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
  table = read.csv(paste(c("/u/people/jeannewb/Documents/00_Projects/AvM/COGNATE_analyzed_2018-08/",
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
  agla_auto_c     = read_my_table(paste("COGNATE_AGLA_auto_compl/COGNATE_AGLA_auto_compl_", data, sep=""))
  agla_man_c      = read_my_table(paste("COGNATE_AGLA_man_compl/COGNATE_AGLA_man_compl_", data, sep=""))
  aros_auto_c     = read_my_table(paste("COGNATE_AROS_auto_compl/COGNATE_AROS_auto_compl_", data, sep=""))
  aros_man_c      = read_my_table(paste("COGNATE_AROS_man_compl/COGNATE_AROS_man_compl_", data, sep=""))
  clec_auto_c     = read_my_table(paste("COGNATE_CLEC_auto_compl/COGNATE_CLEC_auto_compl_", data, sep=""))
  clec_man_c      = read_my_table(paste("COGNATE_CLEC_man_compl/COGNATE_CLEC_man_compl_", data, sep=""))
  ldec_auto_c     = read_my_table(paste("COGNATE_LDEC_auto_compl/COGNATE_LDEC_auto_compl_", data, sep=""))
  ldec_man_c      = read_my_table(paste("COGNATE_LDEC_man_compl/COGNATE_LDEC_man_compl_", data, sep=""))
  focc_auto_c     = read_my_table(paste("COGNATE_FOCC_auto_compl/COGNATE_FOCC_auto_compl_", data, sep=""))
  focc_man_c      = read_my_table(paste("COGNATE_FOCC_man_compl/COGNATE_FOCC_man_compl_", data, sep=""))
  oabi_auto_c     = read_my_table(paste("COGNATE_OABI_auto_compl/COGNATE_OABI_auto_compl_", data, sep=""))
  oabi_man_c      = read_my_table(paste("COGNATE_OABI_man_compl/COGNATE_OABI_man_compl_", data, sep=""))
  ofas_auto_c     = read_my_table(paste("COGNATE_OFAS_auto_compl/COGNATE_OFAS_auto_compl_", data, sep=""))
  ofas_man_c      = read_my_table(paste("COGNATE_OFAS_man_compl/COGNATE_OFAS_man_compl_", data, sep=""))
  # Analyzed sets
  agla_auto     = read_my_table3(paste("COGNATE_AGLA_auto_subset_", data, sep=""))
  agla_man      = read_my_table3(paste("COGNATE_AGLA_man_subset_", data, sep=""))
  aros_auto     = read_my_table3(paste("COGNATE_AROS_auto_subset_", data, sep=""))
  aros_man      = read_my_table3(paste("COGNATE_AROS_man_subset_", data, sep=""))
  clec_auto     = read_my_table3(paste("COGNATE_CLEC_auto_subset_", data, sep=""))
  clec_man      = read_my_table3(paste("COGNATE_CLEC_man_subset_", data, sep=""))
  ldec_auto     = read_my_table3(paste("COGNATE_LDEC_auto_subset_", data, sep=""))
  ldec_man      = read_my_table3(paste("COGNATE_LDEC_man_subset_", data, sep=""))
  focc_auto     = read_my_table3(paste("COGNATE_FOCC_auto_subset_", data, sep=""))
  focc_man      = read_my_table3(paste("COGNATE_FOCC_man_subset_", data, sep=""))
  oabi_auto     = read_my_table3(paste("COGNATE_OABI_auto_subset_", data, sep=""))
  oabi_man      = read_my_table3(paste("COGNATE_OABI_man_subset_", data, sep=""))
  ofas_auto     = read_my_table3(paste("COGNATE_OFAS_auto_subset_", data, sep=""))
  ofas_man      = read_my_table3(paste("COGNATE_OFAS_man_subset_", data, sep=""))

  # ManCur new
  agla_new  = read_my_table3(paste("COGNATE_AGLA_new_subset_", data, sep = ""))
  aros_new  = read_my_table3(paste("COGNATE_AROS_new_subset_", data, sep = ""))
  clec_new  = read_my_table3(paste("COGNATE_CLEC_new_subset_", data, sep = ""))
  ldec_new  = read_my_table3(paste("COGNATE_LDEC_new_subset_", data, sep = ""))
  focc_new  = read_my_table3(paste("COGNATE_FOCC_new_subset_", data, sep = ""))
  oabi_new  = read_my_table3(paste("COGNATE_OABI_new_subset_", data, sep = ""))
  ofas_new  = read_my_table3(paste("COGNATE_OFAS_new_subset_", data, sep = ""))
  
  # Add necessary columns
  agla_auto_c$Species     <- 'Anoplophora glabripennis'
  agla_auto_c$Annotation  <- 'automatic'
  agla_auto_c$Set         <- 'complete'
  agla_auto_c$Assembly.size <- 707712193
  agla_man_c$Species      <- 'Anoplophora glabripennis'
  agla_man_c$Annotation   <- 'manual'
  agla_man_c$Set          <- 'complete'
  agla_man_c$Assembly.size <- 707712193
  
  agla_new$Species        <- 'Anoplophora glabripennis'
  agla_new$Assembly.size  <- 707712193
  # con_dens
  agla_new$Annotation     <- 'new'
  agla_new$Set            <- 'analyzed'
  # var_cov
  #agla_new$Annotation     <- 'manual'
  #agla_new$Set            <- 'new'

  aros_auto_c$Species     <- 'Athalia rosae'
  aros_auto_c$Annotation  <- 'automatic'
  aros_auto_c$Set         <- 'complete'
  aros_auto_c$Assembly.size <- 163837890
  aros_man_c$Species      <- 'Athalia rosae'
  aros_man_c$Annotation   <- 'manual'
  aros_man_c$Set          <- 'complete'
  aros_man_c$Assembly.size <- 163837890
  
  aros_new$Species        <- 'Athalia rosae'
  aros_new$Assembly.size  <- 163837890
  # con_dens
  aros_new$Annotation     <- 'new'
  aros_new$Set            <- 'analyzed'
  # var_cov
  #aros_new$Annotation     <- 'manual'
  #aros_new$Set            <- 'new'

  clec_auto_c$Species     <- 'Cimex lectularius'
  clec_auto_c$Annotation  <- 'automatic'
  clec_auto_c$Set         <- 'complete'
  clec_auto_c$Assembly.size <- 650492763
  clec_man_c$Species      <- 'Cimex lectularius'
  clec_man_c$Annotation   <- 'manual'
  clec_man_c$Set          <- 'complete'
  clec_man_c$Assembly.size <- 650492763
  
  clec_new$Species        <- 'Cimex lectularius'
  clec_new$Assembly.size  <- 650492763
  # con_dens
  clec_new$Annotation     <- 'new'
  clec_new$Set            <- 'analyzed'
  # var_cov
  #clec_new$Annotation     <- 'manual'
  #clec_new$Set            <- 'new'

  ldec_auto_c$Species     <- 'Leptinotarsa decemlineata'
  ldec_auto_c$Annotation  <- 'automatic'
  ldec_auto_c$Set         <- 'complete'
  ldec_auto_c$Assembly.size <- 1170241964
  ldec_man_c$Species      <- 'Leptinotarsa decemlineata'
  ldec_man_c$Annotation   <- 'manual'
  ldec_man_c$Set          <- 'complete'
  ldec_man_c$Assembly.size <- 1170241964
  
  ldec_new$Species        <- 'Leptinotarsa decemlineata'
  ldec_new$Assembly.size  <- 1170241964
  # con_dens
  ldec_new$Annotation     <- 'new'
  ldec_new$Set            <- 'analyzed'
  # var_cov
  #ldec_new$Annotation     <- 'manual'
  #ldec_new$Set            <- 'new'

  focc_auto_c$Species     <- 'Frankliniella occidentalis'
  focc_auto_c$Annotation  <- 'automatic'
  focc_auto_c$Set         <- 'complete'
  focc_auto_c$Assembly.size <- 415803855
  focc_man_c$Species      <- 'Frankliniella occidentalis'
  focc_man_c$Annotation   <- 'manual'
  focc_man_c$Set          <- 'complete'
  focc_man_c$Assembly.size <- 415803855
  
  focc_new$Species        <- 'Frankliniella occidentalis'
  focc_new$Assembly.size  <- 415803855
  # con_dens
  focc_new$Annotation     <- 'new'
  focc_new$Set            <- 'analyzed'
  # var_cov
  #focc_new$Annotation     <- 'manual'
  #focc_new$Set            <- 'new'

  oabi_auto_c$Species     <- 'Orussus abietinus'
  oabi_auto_c$Annotation  <- 'automatic'
  oabi_auto_c$Set         <- 'complete'
  oabi_auto_c$Assembly.size <- 201220334
  oabi_man_c$Species      <- 'Orussus abietinus'
  oabi_man_c$Annotation   <- 'manual'
  oabi_man_c$Set          <- 'complete'
  oabi_man_c$Assembly.size <- 201220334
  
  oabi_new$Species        <- 'Orussus abietinus'
  oabi_new$Assembly.size  <- 201220334
  # con_dens
  oabi_new$Annotation     <- 'new'
  oabi_new$Set            <- 'analyzed'
  # var_cov
  #oabi_new$Annotation     <- 'manual'
  #oabi_new$Set            <- 'new'

  ofas_auto_c$Species     <- 'Oncopeltus fasciatus'
  ofas_auto_c$Annotation  <- 'automatic'
  ofas_auto_c$Set         <- 'complete'
  ofas_auto_c$Assembly.size <- 1098693218
  ofas_man_c$Species      <- 'Oncopeltus fasciatus'
  ofas_man_c$Annotation   <- 'manual'
  ofas_man_c$Set          <- 'complete'
  ofas_man_c$Assembly.size <- 1098693218
  
  ofas_new$Species        <- 'Oncopeltus fasciatus'
  ofas_new$Assembly.size  <- 1098693218
  # con_dens
  ofas_new$Annotation     <- 'new'
  ofas_new$Set            <- 'analyzed'
  # var_cov
  #ofas_new$Annotation     <- 'manual'
  #ofas_new$Set            <- 'new'

  agla_auto$Species     <- 'Anoplophora glabripennis'
  agla_auto$Annotation  <- 'automatic'
  agla_auto$Set         <- 'analyzed'
  agla_auto$Assembly.size <- 707712193
  agla_man$Species      <- 'Anoplophora glabripennis'
  agla_man$Annotation   <- 'manual'
  agla_man$Set          <- 'analyzed'
  agla_man$Assembly.size <- 707712193

  aros_auto$Species     <- 'Athalia rosae'
  aros_auto$Annotation  <- 'automatic'
  aros_auto$Set         <- 'analyzed'
  aros_auto$Assembly.size <- 163837890
  aros_man$Species      <- 'Athalia rosae'
  aros_man$Annotation   <- 'manual'
  aros_man$Set          <- 'analyzed'
  aros_man$Assembly.size <- 163837890

  clec_auto$Species     <- 'Cimex lectularius'
  clec_auto$Annotation  <- 'automatic'
  clec_auto$Set         <- 'analyzed'
  clec_auto$Assembly.size <- 650492763
  clec_man$Species      <- 'Cimex lectularius'
  clec_man$Annotation   <- 'manual'
  clec_man$Set          <- 'analyzed'
  clec_man$Assembly.size <- 650492763

  ldec_auto$Species     <- 'Leptinotarsa decemlineata'
  ldec_auto$Annotation  <- 'automatic'
  ldec_auto$Set         <- 'analyzed'
  ldec_auto$Assembly.size <- 1170241964
  ldec_man$Species      <- 'Leptinotarsa decemlineata'
  ldec_man$Annotation   <- 'manual'
  ldec_man$Set          <- 'analyzed'
  ldec_man$Assembly.size <- 1170241964

  focc_auto$Species     <- 'Frankliniella occidentalis'
  focc_auto$Annotation  <- 'automatic'
  focc_auto$Set         <- 'analyzed'
  focc_auto$Assembly.size <- 415803855
  focc_man$Species      <- 'Frankliniella occidentalis'
  focc_man$Annotation   <- 'manual'
  focc_man$Set          <- 'analyzed'
  focc_man$Assembly.size <- 415803855

  oabi_auto$Species     <- 'Orussus abietinus'
  oabi_auto$Annotation  <- 'automatic'
  oabi_auto$Set         <- 'analyzed'
  oabi_auto$Assembly.size <- 201220334
  oabi_man$Species      <- 'Orussus abietinus'
  oabi_man$Annotation   <- 'manual'
  oabi_man$Set          <- 'analyzed'
  oabi_man$Assembly.size <- 201220334

  ofas_auto$Species     <- 'Oncopeltus fasciatus'
  ofas_auto$Annotation  <- 'automatic'
  ofas_auto$Set         <- 'analyzed'
  ofas_auto$Assembly.size <- 1098693218
  ofas_man$Species      <- 'Oncopeltus fasciatus'
  ofas_man$Annotation   <- 'manual'
  ofas_man$Set          <- 'analyzed'
  ofas_man$Assembly.size <- 1098693218

  #and combine into your new data frame
  SppData <- rbind(agla_auto_c, agla_man_c, aros_auto_c, aros_man_c,
                  clec_auto_c, clec_man_c,
                   ldec_auto_c, ldec_man_c,
                   focc_auto_c, focc_man_c, oabi_auto_c, oabi_man_c,
                   ofas_auto_c, ofas_man_c,
                   agla_auto, agla_man, aros_auto, aros_man,
                   clec_auto, clec_man,
                   ldec_auto, ldec_man,
                   focc_auto, focc_man, oabi_auto, oabi_man,
                   ofas_auto, ofas_man,
                   agla_new, aros_new, clec_new, ldec_new,
                   focc_new, oabi_new, ofas_new
         )

  return(SppData)
}

t_dat <- get_data('07-transcript_general.tsv')
e_dat <- get_data('09-transcript_exons.tsv')
i_dat <- get_data('10-transcript_introns.tsv')

c_dat <- cbind(t_dat, e_dat, i_dat)



# define x axis label format
point <- format_format(big.mark = ",", decimal.mark = ".", scientific = FALSE)
############################# DENSITIES ####################################
species_order <- c(
                  'Anoplophora glabripennis', 'Leptinotarsa decemlineata', # Coleoptera
                  'Athalia rosae', 'Orussus abietinus', # Hymenoptera
                  'Cimex lectularius', 'Oncopeltus fasciatus', # Hemiptera
                  'Frankliniella occidentalis' # Thysanoptera
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


### Condensed densities

con_dens <-  ggplot(c_melt, aes(x=value, color=variable, linetype=Annotation)) +
  #ggplot(c_melt, aes(x=value, color=variable, y=Annotation)) +
  my_theme +
  geom_line(stat="density", size=1) +
  #geom_density_ridges(alpha=0, scale=0.9, size =1) +
  scale_x_continuous(trans = 'log10', expand = c(0.01, 0), labels = point,
                     breaks = c(0, 1,2, 10, 100, 1000, 10000, 100000)
  ) +
  facet_grid(Species_ordered ~ Set, labeller = labeller(Species_ordered = label_wrap_gen(15)), scales= "free") +
  labs(title = "a)",
       x = "Value",
       y = "Density",
       color = "Data") +
  guides(colour = guide_legend(override.aes = list(size=1.8)),
         linetype = guide_legend(override.aes = list(size=1.8))) +
  scale_color_manual(values = wes_palette(name = "Cavalcanti1"))

pdf("~/Documents/00_Projects/AvM/Plots_2018-08/Parts/condensed_densities_onlyAnalyzed_2018-08-14.pdf", 
    width=9, height = 12)
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
  scale_color_manual(values = wes_palette(name = "Cavalcanti1"))

pdf("~/Documents/00_Projects/AvM/Plots_2018-08/Parts/condensed_densities_ridges_onlyAnalyzed_2018-08-14.pdf", 
    width=9, height = 12)
con_dens_ridge
dev.off()


# log c_dat
#c_melt_log = c_melt
#c_melt_log$value = log10(c_melt_log$value)

# cumulative distribution function
## log or no log? :)
# if the scale is log transformed, use normal data but save as log
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
  labs(title = "a) Automatic vs. manual annotation empirical cumulative distribution functions",
       subtitle = "'Complete' sets", x = "Value", y = "F (Value)", color = "Annotation") +
  guides(colour = guide_legend(override.aes = list(size=1.8))) +
  scale_color_manual(values = c("#899DA4", "#C93312","#E1AF00")) +
  scale_linetype_manual(values = c("solid", "dashed", "dotted"))

cum_dist_analy <-  ggplot(analyzed, aes(x=value, color=Annotation, linetype = Annotation)) +
  my_theme +
  stat_ecdf(pad = FALSE, na.rm = TRUE, size = 1) +
  facet_grid(Species_ordered ~ variable, labeller = labeller(Species_ordered = label_wrap_gen(15), 
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






######################### VAR COVERAGE ##########################################

my_vc <- function(dat, x_dat, y_dat, set, ann, tit, x_tit, y_tit) {
  ggplot(dat, aes_string(x=x_dat, y=y_dat)) +
    my_theme +
    theme(strip.text.y = element_blank()) + # removes species names!
    geom_point(aes_string(colour = set, shape = set)) +
    geom_smooth(method=lm, se=TRUE, fullrange=TRUE, show.legend = FALSE,
                aes_string(color=set)) +
    geom_smooth(method=lm, se=FALSE, fullrange=TRUE, show.legend = TRUE,
                aes_string(color=set)) +
    scale_shape_manual(values = c(16, 2, 0)) +
    scale_x_continuous(trans = 'log10', expand = c(0.01, 0), labels = point,
                       breaks = c(0, 1, 10, 100, 1000, 10000, 100000)) +
    scale_y_continuous(trans = 'log10', expand = c(0.01, 0), labels = point,
                       breaks = c(0, 1, 10, 100, 1000, 10000, 100000) ) +
    facet_grid(Species_ordered ~ Annotation, labeller = labeller(Species_ordered = label_wrap_gen(15))) +
    #guides(size = FALSE, shape = FALSE,
    #       colour = guide_legend(override.aes = list(size = 5, shape = 19, linetype=0)),
    #       linetype = guide_legend(title = "Smoothed \nconditional mean",
    #                               override.aes = list(size = 1,
    #                                                   color = set))) +
    scale_linetype_manual(values=c("solid", "solid")) +
    labs(title = tit,
         x = x_tit,
         y = y_tit) +
    scale_color_manual(values = c("#899DA4", "#C93312","#E1AF00"))
}

uniq_c_dat <- c_dat[,!duplicated(t(c_dat))]
uniq_c_dat$Species_ordered = factor(c_dat$Species, levels = species_order)


ec_v_el <- my_vc(uniq_c_dat, x_dat='Exon.count.of.transcript',
                 y_dat='Median.exon.length.of.transcript',
                 set='Set', ann='Annotation',
                 "Dispersion coverage (Exon count vs exon length)",
                 "Exon count per transcript",
                 "Median exon length per transcript [bp]")

ggsave(plot=ec_v_el, file="~/Documents/00_Projects/AvM/Plots_2018-08/Parts/VarCov_new/VarCov_2018-08-11_ec_v_el.png",
       width=5, height=15, dpi=300)



ec_v_egc <- my_vc(uniq_c_dat, x_dat='Exon.count.of.transcript',
              y_dat='Median.exon.GC.content.of.transcript',
              set='Set', ann='Annotation',
              "Dispersion coverage (Exon count vs exon GC content)",
              "Exon count per transcript",
              "Median exon GC content per transcript [%]")
ec_v_egc <- ec_v_egc + scale_y_continuous(labels = point)
ggsave(plot=ec_v_egc, file="~/Documents/00_Projects/AvM/Plots_2018-08/Parts/VarCov_new/VarCov_2018-08-11_ec_v_egc.png",
       width=5, height=15, dpi=300)

ic_v_il <- my_vc(uniq_c_dat, x_dat='Intron.count.of.transcript',
                 y_dat='Median.intron.length.of.transcript',
                 set='Set', ann='Annotation',
                 "Dispersion coverage (Intron count vs intron length)",
                 "Intron count per transcript",
                 "Median intron length per transcript [bp]")
ggsave(plot=ic_v_il, file="~/Documents/00_Projects/AvM/Plots_2018-08/Parts/VarCov_new/VarCov_2018-08-11_ic_v_il.png",
       width=5, height=15, dpi=300)

ic_v_igc <- my_vc(uniq_c_dat, x_dat='Intron.count.of.transcript',
                  y_dat='Median.intron.GC.content.of.transcript',
                  set='Set', ann='Annotation',
                  "Dispersion coverage (Intron count vs intron GC content)",
                  "Intron count per transcript",
                  "Median intron GC content per transcript [%]")
ic_v_igc <- ic_v_igc + scale_y_continuous(labels = point)
ggsave(plot=ic_v_igc, file="~/Documents/00_Projects/AvM/Plots_2018-08/Parts/VarCov_new/VarCov_2018-08-11_ic_v_igc.png",
       width=5, height=15, dpi=300)



i_v_gc <- my_vc(uniq_c_dat, x_dat='Median.intron.length.of.transcript',
                       y_dat='Median.intron.GC.content.of.transcript',
                       set='Set', ann='Annotation',
                       "Dispersion coverage (Intron length vs intron GC content)",
                       "Median intron length per transcript [bp]",
                       "Median intron GC content per transcript [%]")
ggsave(plot=i_v_gc, file="~/Documents/00_Projects/AvM/Plots_2018-08/Parts/VarCov_new/VarCov_2018-08-11_iiGC.png",
       width=8, height=10, dpi=300)

e_v_gc <- my_vc(uniq_c_dat, x_dat='Median.exon.length.of.transcript',
                y_dat='Median.exon.GC.content.of.transcript',
                set='Set', ann='Annotation',
                "Dispersion coverage (Exon length vs exon GC content)",
                "Median exon length per transcript [bp]",
                "Median exon GC content per transcript [%]")
ggsave(plot=e_v_gc, file="~/Documents/00_Projects/AvM/Plots_2018-08/Parts/VarCov_new/VarCov_2018-08-11_eeGC.png",
       width=8, height=10, dpi=300)

t_v_gc <- my_vc(uniq_c_dat, x_dat='Transcript.length..genomic.',
                y_dat='Transcript.GC.content.without.ambiguity',
                set='Set', ann='Annotation',
                "Dispersion coverage (Transcript length vs GC content)",
                "Transcript length per transcript [bp]",
                "GC content per transcript [%]")
ggsave(plot=t_v_gc, file="~/Documents/00_Projects/AvM/Plots_2018-08/Parts/VarCov_new/VarCov_2018-08-11_tGC.png",
       width=8, height=10, dpi=300)

t_v_p <- my_vc(uniq_c_dat, x_dat='Transcript.length..genomic.',
               y_dat='Protein.length',
               set='Set', ann='Annotation',
               "Dispersion coverage (Transcript vs protein length)",
               "Transcript length per transcript [bp]",
               "Protein length per transcript [aa]")
ggsave(plot=t_v_p, file="~/Documents/00_Projects/AvM/Plots_2018-08/Parts/VarCov_new/VarCov_2018-08-11_tP.png",
       width=8, height=10, dpi=300)

e_v_ec <- my_vc(uniq_c_dat, x_dat='Median.exon.length.of.transcript',
                y_dat='Exon.count.of.transcript',
                set='Set', ann='Annotation',
               "Dispersion coverage (Median exon length vs exon count)",
               "Median exon length per transcript [bp]",
               "Exon count per transcript")
ggsave(plot=e_v_ec, file="~/Documents/00_Projects/AvM/Plots_2018-08/Parts/VarCov_new/VarCov_2018-08-11_eE.png",
       width=8, height=10, dpi=300)

i_v_ic <- my_vc(uniq_c_dat, x_dat='Median.intron.length.of.transcript',
                y_dat='Intron.count.of.transcript',
                set='Set', ann='Annotation',
                "Dispersion coverage (Median intron length vs intron count)",
                "Median intron length per transcript [bp]",
                "Intron count per transcript")
ggsave(plot=i_v_ic, file="~/Documents/00_Projects/AvM/Plots_2018-08/Parts/VarCov_new/VarCov_2018-08-11_iI.png",
       width=8, height=10, dpi=300)

egc_v_ec <- my_vc(uniq_c_dat, x_dat='Median.exon.GC.content.of.transcript',
                y_dat='Exon.count.of.transcript',
                set='Set', ann='Annotation',
                "Dispersion coverage (Median exon GC content vs exon count)",
                "Median exon GC content per transcript [%]",
                "Exon count per transcript")
ggsave(plot=egc_v_ec, file="~/Documents/00_Projects/AvM/Plots_2018-08/Parts/VarCov_new/VarCov_2018-08-11_egcEc.png",
       width=8, height=10, dpi=300)

igc_v_ic <- my_vc(uniq_c_dat, x_dat='Median.intron.GC.content.of.transcript',
                y_dat='Intron.count.of.transcript',
                set='Set', ann='Annotation',
                "Dispersion coverage (Median intron GC content  vs intron count)",
                "Median intron GC content  per transcript [bp]",
                "Intron count per transcript")
ggsave(plot=igc_v_ic, file="~/Documents/00_Projects/AvM/Plots_2018-08/Parts/VarCov_new/VarCov_2018-08-11_igcIc.png",
       width=8, height=10, dpi=300)

el_v_il <-  my_vc(uniq_c_dat, x_dat='Median.exon.length.of.transcript',
                  y_dat='Median.intron.length.of.transcript',
                  set='Set', ann='Annotation',
                  "Dispersion coverage (Median exon length vs intron length)",
                  "Median exon length per transcript [bp]",
                  "Median intron length per transcript [bp]")
ggsave(plot=el_v_il, file="~/Documents/00_Projects/AvM/Plots_2018-08/Parts/VarCov_new/VarCov_2018-08-11_eI.png",
       width=8, height=10, dpi=300)

t_v_ec <- my_vc(uniq_c_dat, x_dat='Transcript.length..genomic.',
                y_dat='Exon.count.of.transcript',
                set='Set', ann='Annotation',
                "Dispersion coverage (Transcript length vs exon count)",
                "Transcript length [bp]",
                "Exon count per transcript")
ggsave(plot=t_v_ec, file="~/Documents/00_Projects/AvM/Plots_2018-08/Parts/VarCov_new/VarCov_2018-08-11_tE.png",
       width=8, height=15, dpi=300)

p_v_ec <- my_vc(uniq_c_dat, x_dat='Protein.length',
                y_dat='Exon.count.of.transcript',
                set='Set', ann='Annotation',
                "Dispersion coverage (Protein length vs exon count)",
                "Protein length [aa]",
                "Exon count per transcript")
ggsave(plot=p_v_ec, file="~/Documents/00_Projects/AvM/Plots_2018-08/Parts/VarCov_new/VarCov_2018-08-11_pE.png",
       width=8, height=10, dpi=300)

cov_v_pl <- my_vc(uniq_c_dat, x_dat='Protein.length',
                y_dat='Exon.coverage.of.transcript',
                set='Set', ann='Annotation',
                "Dispersion coverage (Exon coverage vs protein length)",
                "Protein length [aa]",
                "Exon coverage per transcript")
ggsave(plot=cov_v_pl, file="~/Documents/00_Projects/AvM/Plots_2018-08/Parts/VarCov_new/VarCov_2018-08-11_covp.png",
       width=8, height=10, dpi=300)





# pdf("~/Desktop/phd/02c_auto-vs-manual/plots/VarCov_Oct.pdf", width=7, height = 8)
# t_v_gc
# t_v_p
# dev.off()

########################### COVARIANCES and Assembly size (a la E&G) #######################
#med_dat <- read.csv("/u/people/jeannewb/Documents/00_Projects/AvM/COGNATE_analyzed_2018-08/COGNATE_avm_batch_data_2018-08.csv",
med_dat <- read.csv("/u/people/jeannewb/Documents/00_Projects/AvM/COGNATE_analyzed_2018-08/COGNATE_avm_batch_data_2018-08_noNew.csv",
                    header = TRUE, sep = "\t", dec = ".",
                    stringsAsFactors = FALSE, strip.white = T, na.strings = c("NA", ""))

species_order <- c("AGLA", "LDEC", "AROS", "OABI", "CLEC", "OFAS", "FOCC")

species_names <- c(
                   "AGLA" = "Anoplophora glabripennis",
                   "LDEC" = "Leptinotarsa decemlinieata",
                   "AROS" = "Athalia rosae",
                   "OABI" = "Orussus abietinus",
                   "CLEC" = "Cimex lectularius",
                   "OFAS" = "Oncopeltus fasciatus",
                   "FOCC" = "Frankliniella occidentalis"
                   )

med_dat$Species_ordered = factor(med_dat$Species, levels = species_order)


# more data per facets: melt
dat_m_A <- melt(med_dat, id.vars = c("Species_ordered", "Set", "Annotation", "Assembly.size.without.Ns"),
                measure.vars = c("Median.genomic.transcript.length",
                                 "Median.protein.length",
                                 "Median.exon.count.per.transcript",
                                 "Median.median.exon.length.per.transcript",
                                 "Median.median.intron.length.per.transcript"))

dat_m_B <- melt(med_dat, id.vars = c("Species_ordered", "Set", "Annotation", "Assembly.size.without.Ns"),
                measure.vars = c("Coding.amount....",
                                 "Intron.amount....",
                                 "Gene.count..total.",
                                 "Exon.count..total.",
                                 "Assembly.GC.content.without.ambiguity"))

# relabel A
A_labels <- c("Median.genomic.transcript.length" = "Median transcript length [bp]",
              "Median.protein.length" = "Median protein length [bp]",
              "Median.exon.count.per.transcript" = "Median exon count p.t.",
              "Median.median.exon.length.per.transcript" = "Median median exon length p.t. [bp]",
              "Median.median.intron.length.per.transcript" = "Median median intron length p.t. [bp]")

dat_m_A$variable <- plyr::revalue(dat_m_A$variable, A_labels)

# relabel B
B_labels <- c("Coding.amount...." = "Coding proportion [%]",
              "Intron.amount...." = "Intronic proportion [%]",
              "Gene.count..total." = "Gene count (total)",
              "Exon.count..total." = "Exon count (total)",
              "Assembly.GC.content.without.ambiguity" = "Assembly GC content without Ns [%]")

dat_m_B$variable <- plyr::revalue(dat_m_B$variable, B_labels)

# Appearance
wes_pal <- c(wes_palette("GrandBudapest1"),wes_palette("GrandBudapest2"))

my_theme_ass = my_theme +
  theme(strip.text.y = element_text(size=11,
                                    face = "italic", angle=0)
       # strip.background.y = element_rect(fill = "white")#,
       # panel.margin = unit(1.5, "lines")
  )

# plot A
ass_A <- ggplot(dat_m_A, aes(x=Assembly.size.without.Ns/1000000,
                    y=value)) +
  my_theme_ass +
  theme(strip.text.y = element_text(face = 'plain')) +
  geom_smooth(method=lm, se=TRUE, fullrange=TRUE,
              aes(linetype = Annotation),
              colour = 'black') +
  geom_point(aes(colour = Species_ordered, shape = Annotation, size=5)) +
  scale_x_continuous(labels = point) +
  scale_y_continuous(trans = 'log10', expand = c(0.01, 0), labels = point,
                     breaks = c(0, 1, 2,3, 5, 10, 50, 100, 150, 200, 250, 
                                300, 500, 1000, 2500, 5000, 10000, 100000)) +
  guides(size = FALSE) +
  scale_linetype_manual("Smoothed \nconditional mean", values = c(1,2,3)) +
  guides(colour = guide_legend(override.aes = list(size = 5, shape = 15))) +
  guides(shape = guide_legend(override.aes = list(size = 5), order = 1)) +
  labs(title = "Assembly correlations",
       x = "Assembly size without Ns [Mb]",
       y = "Value",
       color = "Species") +
  facet_grid(variable ~ Set, scales= 'free',
             labeller = labeller(variable = label_wrap_gen(25))) +
  scale_color_manual(values = wes_pal)

# plot B
ass_B <- ggplot(dat_m_B, aes(x=Assembly.size.without.Ns/1000000,
                           y=value)) +
  my_theme_ass +
  theme(strip.text.y = element_text(face = 'plain')) +
  geom_smooth(method=lm, se=TRUE, fullrange=TRUE,
              aes(linetype = Annotation),
              colour = 'black') +
  geom_point(aes(colour = Species_ordered, shape = Annotation, size=5)) +
  scale_x_continuous(labels = point) +
  scale_y_continuous(trans = 'log10', expand = c(0.01, 0), labels = point,
                     breaks = c(0, 0.1, 0.5, 1, 2,3, 5, 10, 30, 40, 50, 100, 150, 200, 250, 
                                300, 500, 1000, 2500, 5000, 10000, 100000)) +
  guides(size = FALSE) +
  scale_linetype_manual("Smoothed \nconditional mean", values = c(1,2,3)) +
  guides(colour = guide_legend(override.aes = list(size = 5, shape = 15))) +
  guides(shape = guide_legend(override.aes = list(size = 5), order = 1)) +
  labs(title = "Assembly correlations",
       x = "Assembly size without Ns [Mb]",
       y = "Value",
       color = "Species") +
  facet_grid(variable ~ Set, scales= 'free',
             labeller = labeller(variable = label_wrap_gen(25))) +
  scale_color_manual(values = wes_pal)


#pdf("~/Documents/00_Projects/AvM/Plots_2018-08/Parts/AssCorr_2018-08-13_inclNew.pdf",
pdf("~/Documents/00_Projects/AvM/Plots_2018-08/Parts/AssCorr_2018-08-13_noNew.pdf",
    width=9, height = 10,
    useDingbats=FALSE)
ass_A
ass_B
dev.off()


### DONE ###






