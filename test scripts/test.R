library(tidyverse)
library(here)
library(ggsci)
library(kableExtra)
library(knitr)
library(patchwork)
library(ggbeeswarm)
library(RColorBrewer)

here::here()

df_iso_total <- here::here("./data/castro_iso_total_1.csv") %>%
  read_csv(col_names = TRUE)

df_iso_fauna_summ <- here::here("./data/castro_fauna_iso_summ_1.csv") %>%
  readr::read_csv(col_names = TRUE)

theme<-theme(panel.background = element_blank(),legend.background = element_blank(), legend.key.size = unit(0.3, 'cm'), legend.box.background = element_rect(colour = "black"), legend.position=c(0.92,0.25),legend.text = element_text(size=6),text = element_text(),legend.title=element_text(size=7),axis.title.x = element_text(size=8), axis.title.y = element_text(size=8), panel.border=element_rect(fill=NA),panel.grid.major = element_blank(),panel.grid.minor = element_blank(),strip.background=element_blank(),
             axis.text.x=element_text(colour="black"),axis.text.y=element_text(colour="black"),axis.ticks=element_line(colour="black"),plot.margin=unit(c(1,1,1,1),"line"))


iso_carbnitro_mean_plot <- ggplot(df_iso_total, aes(x=dC, y=dN, shape = as.factor(Common_Name), colour= as.factor(Common_Name))) +
  geom_point(size = 3, alpha = 0.3) + labs(x = expression(paste(delta^{13}, "C (\u2030 vs. VPDB)")), y = expression(paste(delta^{15}, "N (\u2030 vs. Air)")), shape = 'Species', colour = 'Species') + scale_shape_manual(values = c(17,15,16,17,15,16,17,15,16,17,16,17)) + scale_color_manual(values=c('dodgerblue2','blue','#E31A1C','darkorange4','orchid1','palegreen2','blue1','green4','brown','deeppink1','black','darkturquoise')) + scale_y_continuous(breaks=seq(2.5,16,1)) + scale_x_continuous(limits=c(-27,-13),breaks=seq(-27,-12,1)) + annotate("text", x = -25, y = 15.5, colour = "gray44" , size = 4, label = "Pure C3 feeders") + annotate("text", x = -18.5, y = 15.5, colour = "gray44" , size = 4, label = "Addition of C4 plants") + theme + geom_vline(xintercept = -21, color='gray62', linetype="dotted", size=1) + geom_point(data = df_iso_fauna_summ,size = 3, aes(x=meandC, y=meandN)) + geom_errorbar(data = df_iso_fauna_summ,aes(x=meandC, y=meandN,ymin=meandN-stddevdN, ymax=meandN+stddevdN), width=.1) + geom_errorbarh(data = df_iso_fauna_summ,aes(x=meandC, y=meandN,xmin=meandC-stddevdC, xmax=meandC+stddevdC), width=.1)

iso_carbnitro_mean_plot



