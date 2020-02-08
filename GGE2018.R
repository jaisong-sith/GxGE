library(tidyverse)
library(readxl)
library(gge)
library(GGEBiplots)
library(GGEBiplotGUI)
library(lme4)


path <- "C:\\Users\\User\\Google Drive\\PTL_research\\1.0plantbreeding\\DATA\\inter_yield_trial_data.xlsx"

data <- read_excel(path = path, sheet = "2018")

data <- data %>% select(-block, -year, - season) %>% mutate(station = as.factor(station),
                exp = as.factor(exp),
                designation = as.factor(designation),
                y14 = as.numeric(as.character(y14)))


data %>% filter(exp == "ex.5") %>% ggplot(aes(x= designation, y = y14, fill = station)) + 
  geom_boxplot() + coord_flip()


##### EX5 ####
ex5.data <- data %>% filter(exp == "ex.5")

ex5.data.mean <- ex5.data %>% group_by(station, designation) %>% summarise(y14bar = mean(y14, na.rm = TRUE))

#ex5.var.test <-unique(ex5.data$designation) %>% as.character()

#ex5.data$designation <- factor(ex5.data$designation, levels = ex5.var.test)
#ex5.data$var <- rep(paste0("G", rep(1:length(levels(ex5.data$designation)), each = 4)), times = 4)



wide_ex5 <- ex5.data.mean %>% group_by(station, designation) %>% spread(station, y14bar)


mat <- data.matrix(wide_ex5)[,-1]
rownames(mat) <- wide_ex5$designation

GGE2 <- gge(as.matrix(mat))
GGEPlot(GGE2, type = 6)


##### EX6 ####
ex6.data <- data %>% filter(exp == "ex.6")

ex6.data.mean <- ex6.data %>% group_by(station, designation) %>% summarise(y14bar = mean(y14, na.rm = TRUE))

#ex5.var.test <-unique(ex5.data$designation) %>% as.character()

#ex5.data$designation <- factor(ex5.data$designation, levels = ex5.var.test)
#ex5.data$var <- rep(paste0("G", rep(1:length(levels(ex5.data$designation)), each = 4)), times = 4)



wide_ex6 <- ex6.data.mean %>% group_by(station, designation) %>% spread(station, y14bar)


mat <- data.matrix(wide_ex6)[,-1]
rownames(mat) <- wide_ex6$designation

GGE2 <- gge(as.matrix(mat))
GGEPlot(GGE2, type = 6)

##### EX7 ####
ex7.data <- data %>% filter(exp == "ex.7")

ex7.data.mean <- ex7.data %>% group_by(station, designation) %>% 
  summarise(y14bar = mean(y14, na.rm = TRUE))

#ex5.var.test <-unique(ex5.data$designation) %>% as.character()

#ex5.data$designation <- factor(ex5.data$designation, levels = ex5.var.test)
#ex5.data$var <- rep(paste0("G", rep(1:length(levels(ex5.data$designation)), each = 4)), times = 4)



wide_ex7 <- ex7.data.mean %>% group_by(station, designation) %>% 
  spread(station, y14bar)


mat <- data.matrix(wide_ex7)[,-1]
rownames(mat) <- wide_ex7$designation

GGE2 <- gge(as.matrix(mat))
GGEPlot(GGE2, type = 6)

##### RYT ####
RYT.data <- data %>% filter(exp == "RYT.V")

RYT.data.mean <- RYT.data %>% group_by(station, designation) %>% 
  summarise(y14bar = mean(y14, na.rm = TRUE))

#ex5.var.test <-unique(ex5.data$designation) %>% as.character()

#ex5.data$designation <- factor(ex5.data$designation, levels = ex5.var.test)
#ex5.data$var <- rep(paste0("G", rep(1:length(levels(ex5.data$designation)), each = 4)), times = 4)



wide_RYT <- RYT.data.mean %>% group_by(station, designation) %>% 
  spread(station, y14bar)


mat <- data.matrix(wide_RYT)[,-1]
rownames(mat) <- wide_RYT$designation

GGE2 <- gge(as.matrix(mat))
GGEPlot(GGE2, type = 6)


##### intrayield trials ####
intra <- read_excel(path = "C:\\Users\\User\\Desktop\\intra2018.xlsx")

intra.long <- intra %>% gather(key = "rep", value = "y14", rep1:rep4)

intra.long %>% ggplot(aes(x= reorder(designation, y14, mean), y = y14)) + geom_boxplot() + coord_flip()

tgw <- read_csv(file = "C:\\Users\\User\\Desktop\\tgw_ex25.csv")

tgw %>% ggplot(aes(x= reorder(trt, weight, mean), y = weight)) + geom_boxplot() + coord_flip()
intra.long %>% left_join(tgw, by = c("designation" = "trt", "rep" = "rep")) %>%
  ggplot(aes(x = weight, y = y14)) + geom_point() + geom_smooth(method = lm)
                         