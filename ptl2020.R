library(tidyverse)
library(readxl)
library(gge)
library(GGEBiplots)
library(GGEBiplotGUI)
library(lme4)

filechose <- "C:\\Users\\User\\Desktop\\dataptl2019stb.xlsx"
dat <- read_excel(filechose)


dat %>% gather(year, yield, E1:E4) %>% 
  ggplot(aes(x= Genotype , y = yield)) + 
  geom_segment(aes(x = Genotype, xend = Genotype, y = 0, yend = yield), color = "peachpuff2", size = 1.5) + 
  geom_point(color = "peachpuff2", size = 6, color = "") + facet_grid(.~year) + coord_flip() +
  theme(
    panel.background = element_rect(fill = "papayawhip",
                                    colour = "papayawhip",
                                    size = 0.5, linetype = "solid"),
    panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                    colour = "white"), 
    panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                    colour = "white"),
    strip.background = element_rect(colour=NA, fill= "peachpuff")
  )



datmat <- data.matrix(dat)
datmat.d <- datmat[,-1]
row.names(datmat.d) <-c("CNT1", "PSL2", "RD31", "PTL", "SPR", "CRI", "CNT")

GGE1 <- GGEModel(datmat.d)
MeanStability(GGE1)
stattable(GGE1)

GGE2 <- gge(datmat.d)
GGEPlot(GGE2, type = 1)
GGEPlot(GGE2, type = 4)
GGEPlot(GGE2, type = 6)
GGEPlot(GGE2, type = 7)
GGEPlot(GGE2, type = 8, colEnv = "black", sizeEnv = 12, title = FALSE, axis_expand = 1.2, 
        axes = TRUE) + 
  theme(text = element_text(size=20))
GGEPlot(GGE2, type = 9)
GGEPlot(GGE2, type = 10)
