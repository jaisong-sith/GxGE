library(tidyverse)
library(readxl)
library(gge)
library(GGEBiplots)
library(GGEBiplotGUI)
library(lme4)

path <- "C:\\Users\\User\\Google Drive\\PTL_research\\1.0plantbreeding\\DATA\\inter_yield_trial_data.xlsx"

data <- path %>%
  excel_sheets() %>%
  set_names() %>% 
  map_df(~ read_excel(path = path, sheet = .x), .id = "sheet")

clean.data <- data %>% mutate(station = as.factor(station),
                year = as.factor(year),
                season = as.factor(season),
                exp = as.factor(exp),
                block = as.factor(block),
                designation = as.factor(designation),
                rep = as.factor(rep),
                y14 = as.numeric(as.character(y14))) %>%
  select(-sheet, -season)


mean_data <- clean.data %>% group_by(station, designation) %>% 
  summarise(xbar_y14 = mean(y14))

c_mean_data <- mean_data[complete.cases(mean_data),]

mean_data[complete.cases(mean_data),] %>% 
  ggplot(aes(x = reorder(station, xbar_y14, mean), y = xbar_y14, color = station)) + 
  geom_violin() 

c_mean_data <- c_mean_data %>% 
  mutate(designation = as.character(designation))



selected_data <- mean_data %>% spread(station, xbar_y14)

selected_data1 <- selected_data[complete.cases(selected_data),]


var.test <- as.character(selected_data1$designation)

selected_data1$var <- factor(c(paste0("G", 
                                     1:length(selected_data1$designation))))

r_var <- as.character(selected_data1$designation)
g_var <- as.character(selected_data1$var)
location <- names(selected_data1)[c(-1, -6)]
mat <- data.matrix(selected_data1[c(-1, -6)])
rownames(mat) <- g_var


clean.data %>% mutate(year = as.factor(as.character(year))) %>% 
  filter(designation %in% r_var) %>% 
  lm(y14 ~ station + designation, .) %>% anova()

means <- clean.data %>% mutate(year = as.factor(as.character(year))) %>%
  group_by(station) %>%
  summarise(mean = mean(y14, na.rm = TRUE))



clean.data %>% mutate(year = as.factor(as.character(year))) %>% 
  filter(designation %in% r_var) %>%
  ggplot(aes(x = y14, fill = station)) +
  geom_histogram(alpha = .3) + #alpha used for filling the density
  geom_vline(data = means, aes(xintercept = mean, color = station),
             linetype = "longdash", size=1)  + facet_grid(station ~.)



GGE2 <- gge(as.matrix(mat))
GGEPlot(GGE2, type = 1)
GGEPlot(GGE2, type = 4)
GGEPlot(GGE2, type = 6)
GGEPlot(GGE2, type = 7)
GGEPlot(GGE2, type = 8, colEnv = "black", sizeEnv = 12, title = FALSE, axis_expand = 1.2, 
        axes = TRUE) + 
  theme(text = element_text(size=20))
GGEPlot(GGE2, type = 9)
GGEPlot(GGE2, type = 10)
