library(ggplot2)
library(reshape2)
library(grid)
library(gtable)
library(wesanderson)

my_theme =   theme(plot.title   = element_blank(),
                     #element_text(lineheight=3, size = 18, face = "bold",
                      #                         margin = margin(10,10,10,10)),
                   axis.text.x  = element_text(angle = 45, hjust = 1, size = 12),
                   axis.title.x = element_text(size = 16,
                                               margin = margin(10,0,0,0)),
                   axis.text.y  = element_text(size = 14),
                   axis.title.y = element_text(size = 16,
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
                   panel.margin      = unit(1.5, "lines")
                   #strip.text.x = element_text(size=14, face = "italic"),
                   #strip.text.y = element_text(size=14, face = "italic", angle=90)
)

gg_color_hue <- function(n) {
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}
col <- gg_color_hue(2)

# colors used in frameshift plot:
# "#75BF00" "#B2CF9B" "#E2C19D" "#E59E00"
#col2 <- c("#B79F00", "#C77CFF")


#' **Create dataframe**
Number.of.projects.participated <-	c(1,2,3,4,5,6,7,8)
Number.of.curators.in.x.projects <- c(93,27,7,5,3,0,0,2)
Percentage.of.curators.in.x.projects	 <-	c(66.91,19.42,5.04,3.60,2.16,0.00,0.00,1.44)
Number.of.genes.annotated.by.these.curators	 <-	c(3496,1134,848,462,661,0,0,845)
Percentage.of.genes.reviewed.by.these.curators	 <-	c(46.95,15.23,11.39,6.20,8.88,0.00,0.00,11.35)
Genes.per.curator <- c(37.59,42.00,121.14,92.40,220.33,NA,NA,422.50)

dat = data.frame(Number.of.projects.participated, Number.of.curators.in.x.projects,
                Percentage.of.curators.in.x.projects, Number.of.genes.annotated.by.these.curators,
                Percentage.of.genes.reviewed.by.these.curators,
                Genes.per.curator)

meltdat <- melt(dat, id.vars = "Number.of.projects.participated",
                measure.vars = c("Percentage.of.curators.in.x.projects",
                    "Percentage.of.genes.reviewed.by.these.curators"))

x_axis_labs <- min(meltdat[,"Number.of.projects.participated"]):max(meltdat[,"Number.of.projects.participated"])


plot <-  ggplot(data=meltdat, aes(x=Number.of.projects.participated, y=value, fill= variable)) +
  my_theme +
  geom_bar(stat="identity",
           position = position_dodge()) +
  labs(title = "Curator participation and contributions",
       x = "Projects",
       y = "Percentage [%]",
       fill = "Type") +
  scale_fill_manual(labels = c("Curators",
                              "Genes"),
                    values = wes_palette(name = "Royal1")) +
  scale_x_continuous(labels = x_axis_labs,
                     breaks = x_axis_labs) +
  annotate("text", x = c(1,2,3,4,5,6,7,8), y = 15,
           label = Genes.per.curator, color = "blue") +
  annotate("text", x = Inf, y = 15.2, label = "Genes/curator", color="blue",
           hjust = -0.2, size=4)

# Turn off clipping to the plot panel
g = ggplotGrob(plot)
g$layout$clip[g$layout$name == "panel"] = "off"

pdf("~/Desktop/phd/02c_auto-vs-manual/plots/Curators_2017-10-12.pdf", width=7, height = 5)
grid.draw(g)
dev.off()





















