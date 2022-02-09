library(tidyverse)
library(plotrix)
library(ggpubr)
library(ggrepel)

data <- read.csv(file = "~/Google Drive/My Drive/2021F_VIP/clean/germ_data_no_zeros.csv", header = T)

#### summarize data to get means for plotting
sum <- data %>% group_by(PopID, IndivNum, StratDays) %>% summarise(totalgerm= sum(binom))

summary <- sum %>% group_by(PopID, StratDays) %>% 
    summarise(mean_germ = mean(totalgerm),
              n = n(),
              se = std.error(totalgerm))
summary$Abbr <- abbreviate(summary$PopID, minlength = 3)

####Plot theme from Derek####
plot_theme <- theme_minimal() +
    #GOOGLE: "R ggplot how to move axis title label away from plot"
    theme(axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0))) + # Move the axis title away from the plot
    theme(axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0))) +
    #GOOGLE: "R ggplot remove grid lines"
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_blank(), axis.line = element_line(colour = "black")) +
    #GOOGEL: "R ggplot increase text size"
    theme(axis.text = element_text(size = 14)) + 
    theme(axis.title = element_text(size = 16)) +
    theme(plot.title = element_text(size = 20)) +
    theme(plot.subtitle = element_text(size = 16))

#### plot1 has mean germination by population for each stratificaiton treatment
plot1 <- ggplot(summary, aes(x = factor(StratDays), y = mean_germ)) +
    geom_line(aes(colour = PopID, group = PopID)) +
    #theme(legend.position = "none") +
    geom_text(data = subset(summary, StratDays == 81), aes(label = Abbr, colour = PopID,   x = 5.2, y = mean_germ), show.legend = F, nudge_y = c(0, -0.3, 0, 0, -0.3, 0, 0,   0.3, 0, 0, 0, 0)) +
    geom_text(data = subset(summary, StratDays == 28 & PopID %in% c("KingHill", "HoweCrk")), aes(label = Abbr, colour = PopID,   x = 4.2, y = mean_germ), show.legend = F, nudge_y = c(0.3, 0.1)) +
    labs(title = "Population-Level Effect") +
    xlab("Cold Stratification (Days)") + 
    ylab("Mean Seeds Germinated (count)") +
    plot_theme

finalplot1 <- plot1 + theme(legend.position = "none")

trtmean <- sum %>% group_by(StratDays) %>% summarise(mean = mean(totalgerm),
                                                     n = n(),
                                                     se = std.error(totalgerm))

####Plot 2 is the overall effect fo the stratification treatment without population effect
plot2 <- ggplot(trtmean, aes(x=factor(StratDays), y=mean)) + 
    coord_cartesian(xlim = NULL, ylim = c(0, 10)) +
    geom_bar(stat = "identity") + 
    geom_errorbar(aes(ymin=mean-se, ymax=mean+se, width=0.2)) +
    labs(title = "Main Effect of Treatment") +
    xlab("Cold Stratification (Days)") + 
    ylab("Mean Seeds Germinated (count)") +
    plot_theme

####make a panel plot of plots 1 and 2
panel_plot <- ggarrange(plot2, finalplot1,
                        ncol = 2,
                        nrow = 1,
                        widths = 1,
                        heights = 1,
                        common.legend = FALSE)

### Save plot (hi-res)
png(file="my_panel_plot.png", width=6000, height=3000, units="px", res=600)
panel_plot
dev.off()