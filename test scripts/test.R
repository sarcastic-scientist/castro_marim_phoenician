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

theme<-theme(panel.background = element_blank(),legend.background = element_blank(), legend.key.size = unit(0.3, 'cm'), legend.box.background = element_rect(colour = "black"), legend.position="right",legend.text = element_text(size=6),text = element_text(),legend.title=element_text(size=7),axis.title.x = element_text(size=8), axis.title.y = element_text(size=8), panel.border=element_rect(fill=NA),panel.grid.major = element_blank(),panel.grid.minor = element_blank(),strip.background=element_blank(), axis.text.x=element_text(colour="black"),axis.text.y=element_text(colour="black"),axis.ticks=element_line(colour="black"),plot.margin=unit(c(1,1,1,1),"line"))


iso_carbnitro_mean_plot <- ggplot(df_iso_total, aes(x=dC, y=dN, shape = as.factor(Common_Name), colour= as.factor(Common_Name))) +
  geom_point(size = 3, alpha = 0.3) + labs(x = expression(paste(delta^{13}, "C (\u2030 vs. VPDB)")), y = expression(paste(delta^{15}, "N (\u2030 vs. Air)")), shape = 'Species', colour = 'Species') + scale_shape_manual(values = c(17,15,16,17,15,16,17,15,16,17,16,17)) + scale_color_manual(values=c("#4749dc", "#00344d", "#25738b", "#e30293", "#b452a2", "#5866ad", "#4e052e", "#071576", "#158a2c", "#0b4512", "#637f4b", "#7d4400")) + scale_y_continuous(breaks=seq(2.5,16,1)) + scale_x_continuous(limits=c(-27,-13),breaks=seq(-27,-12,1)) + annotate("text", x = -25, y = 15.5, colour = "gray44" , size = 3, label = "Pure C3 feeders") + annotate("text", x = -18.5, y = 15.5, colour = "gray44" , size = 3, label = "Addition of C4 plants") + theme + geom_vline(xintercept = -21, color='gray62', linetype="dotted", size=1) + geom_point(data = df_iso_fauna_summ,size = 3, aes(x=meandC, y=meandN)) + geom_errorbar(data = df_iso_fauna_summ,aes(x=meandC, y=meandN,ymin=meandN-stddevdN, ymax=meandN+stddevdN), width=.1) + geom_errorbarh(data = df_iso_fauna_summ,aes(x=meandC, y=meandN,xmin=meandC-stddevdC, xmax=meandC+stddevdC), height=.1)

iso_carbnitro_mean_plot

ggsave('CNfauna.jpg',iso_carbnitro_mean_plot)

df_iso_hord <- here::here("./data/castro_iso_hord.csv") |>
  readr::read_csv(col_names = TRUE) |>
  mutate(DC = (-6.5-dC_corr) / (1+(dC_corr/1000)))

df_iso_bota <- here::here("./data/castro_iso_bota.csv") |>
  readr::read_csv(col_names = TRUE)


theme5<-theme(panel.background = element_blank(),legend.background = element_blank(), legend.key.size = unit(0.3, 'cm'), legend.box.background = element_rect(colour = "black"), legend.position="none",text = element_text(size=7),axis.title.x = element_text(size=6), axis.title.y = element_text(size=6), panel.border=element_rect(fill=NA),panel.grid.major = element_blank(),panel.grid.minor = element_blank(),strip.background=element_blank(),
              axis.text.x=element_text(colour="black"),axis.text.y=element_text(colour="black"),axis.ticks=element_line(colour="black"),plot.margin=unit(c(1,1,1,1),"line"))



dffinal_iso_summ <- df_iso_fauna %>%
  select(Species, Common_Name, dC, dN, dS) %>%
  group_by(Common_Name) %>%
  summarize(
    n = n(),
    mean_dC = mean(dC, na.rm = TRUE),
    std.dev_dC = sd(dC, na.rm = TRUE),
    mean_dN = mean(dN, na.rm = TRUE),
    std.dev_dN = sd(dN, na.rm = TRUE),
    mean_dS = mean(dS, na.rm = TRUE),
    std.dev_dS = sd(dS, na.rm = TRUE),
    min_dC = min(dC, na.rm = TRUE),
    max_dC = max(dC, na.rm = TRUE),
    min_dN = min(dN, na.rm = TRUE),
    max_dN = max(dN, na.rm = TRUE),
    min_dS = min(dS, na.rm = TRUE),
    max_dS = max(dS, na.rm = TRUE))


iso_sulphur_plot <- ggplot(dffinal_iso_summ, aes(x=Common_Name,y=mean_dS, color=as.factor(Common_Name), shape=as.factor(Common_Name))) +
  geom_point(size = 3, alpha = 0.65) + geom_errorbar(aes(ymin=mean_dS-std.dev_dS, ymax=mean_dS+std.dev_dS), width=.1) + theme + labs( x='',y=expression(paste(delta^{34}, "S (\u2030 vs. VCDT)")), shape = 'Species', colour = 'Species') + scale_color_manual(values = c("#b452a2","#5866ad","#4e052e","#25738b","#071576","#00344d","#e30293","#637f4b","#158a2c","#0b4512")) + scale_shape_manual(values = c(15,17,16,15,15,15,17,16,16,17)) + geom_jitter(data = df_iso_fauna, aes(x = Common_Name, y = dS), width=0.25) + scale_x_discrete(limits = c("Grey Plover","Kittiwake","Partridge","Chicken","Pig","Cattle","Goat","Sheep","Rabbit","Red Deer"))

ggsave('sulphfauna.jpg',iso_sulphur_plot)

