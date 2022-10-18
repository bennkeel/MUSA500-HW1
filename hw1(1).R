#spat stats hw1
library(tidyverse)
library(gt)
library(gtExtras)
library(cowplot)
library(corrr)

data <- read.csv("C:/Users/cchue/Documents/Penn MUSA/Spatial Statistics/RegressionData.csv")


p1 <- ggplot(data)+
  geom_histogram(aes( x = MEDHVAL),bins = 100)+
  theme_minimal()+
  theme(text = element_text(size = 8))+
  labs(y = '' , x = 'Median House Value')


p2 <- ggplot(data)+
  geom_histogram(aes( x = PCTBACHMOR),bins = 100)+
  theme_minimal()+
  theme(text = element_text(size = 8))+
  
  labs(y = '', x = '% of Individuals with Bachelor’s Degrees or Higher')

p3<- ggplot(data)+
  geom_histogram(aes( x =NBELPOV100),bins = 100)+
  theme_minimal()+
  theme(text = element_text(size = 8))+
  labs(y = '', x = '# Households Living in Poverty')

p4<- ggplot(data)+
  geom_histogram(aes( x = PCTVACANT),bins = 100)+
  theme_minimal()+
  theme(text = element_text(size = 8))+
  labs(y = '', x = '% of Vacant Houses')

p5<- ggplot(data)+
  geom_histogram(aes( x =PCTSINGLES),bins = 100)+
  theme_minimal()+
  theme(text = element_text(size = 8))+
  labs(y = '', x = '% of Single House Units')

cowplot::plot_grid(p1, p2, p3, p4, p5, ncol = 5)


#log variables
data$MEDHVALlog <- log(data$MEDHVAL)
data$PCTBACHMORlog <- log(data$PCTBACHMOR + 1)
data$NBELPOV100log <- log(data$NBELPOV100 + 1)
data$PCTVACANTlog <- log(data$PCTVACANT + 1)
data$PCTSINGLESlog <- log(data$PCTSINGLES + 1)

l1 <- ggplot(data)+
  geom_histogram(aes( x = MEDHVALlog),bins = 100)+
  theme_minimal()+
  labs(y = '', x= 'log $')

l2 <- ggplot(data)+
  geom_histogram(aes( x = PCTBACHMORlog),bins = 100)+
  theme_minimal()+
  labs(y = '', x = 'log %')

l3<- ggplot(data)+
  geom_histogram(aes( x =NBELPOV100log),bins = 100)+
  theme_minimal()+
  labs(y = '', x = 'log households')

l4<- ggplot(data)+
  geom_histogram(aes( x = PCTVACANTlog),bins = 100)+
  theme_minimal()+
  labs(y = '', x = 'log %')

l5<- ggplot(data)+
  geom_histogram(aes( x =PCTSINGLESlog),bins = 100)+
  theme_minimal()+
  labs(y = '', x = 'log %')

cowplot::plot_grid(p1, p2, p3, p4, p5, l1,l2,l3,l4,l5, ncol = 5)

ggplot(data)+
  geom_freqpoly(aes( x = MEDHVAL), color = 'blue', fill = 'red', alpha = .5)+
  geom_histogram(aes( x = MEDHVALlog), color = 'red', fill = 'red', alpha = .5)+
  scale_x_continuous(limits = c(0, 25))
  
  theme_minimal()+
  labs(y = '')

  
### table]
sumtab <- data %>% select(MEDHVAL, PCTBACHMOR, NBELPOV100, PCTVACANT, PCTSINGLES) %>%
  #group_by(key)%>%
  summarise(HVAL = mean(MEDHVAL),
            pov = mean(NBELPOV100),
            pctbach = mean(PCTBACHMOR),
            vacant = mean(PCTVACANT),
            singles = mean(PCTSINGLES)) %>% 
  rbind(., data %>% select(MEDHVAL, PCTBACHMOR, NBELPOV100, PCTVACANT, PCTSINGLES) %>% 
          summarise(HVAL = sd(MEDHVAL),
                    pov = sd(NBELPOV100),
                    pctbach = sd(PCTBACHMOR),
                    vacant = sd(PCTVACANT),
                    singles = sd(PCTSINGLES))) %>% t() %>% as.data.frame() %>% 
  round(digit = 2) %>% 
  mutate(variable = c('Median House Value ($)', '# Households Living in Poverty', "% of Individuals with Bachelor's Degrees or Higher", '% of Vacant Houses', '% of Single House Units'))
  


colnames(sumtab) <- c('Mean', 'SD', 'Variable')

  
sumtab %>% gt() %>% 
  cols_move_to_start(Variable) %>% 
  tab_row_group(
    label = "Predictors",
    rows = c(2:5))%>% 
  tab_row_group(
    label = "Dependant Variable",
    rows = c(1)) %>% 
  tab_style(location = cells_row_groups(),
            style = list(cell_text(weight = 'bold'))) %>% 
  tab_style(
    locations = cells_column_labels(columns = everything()),
    style     = list(cell_text(weight = "bold"))) %>%
  gt_add_divider(columns = c('Variable', 'Mean'), style = "solid") %>% 
  cols_align(
    align = "right",
    columns = Variable) %>% 
  tab_header(title = 'Philadelphia Housing Summary Table') %>% 
  tab_footnote(footnote = 'Aggregated from 1,720 Census Tracts')

#1b

s1 <- ggplot(data)+
  geom_point(aes(y = MEDHVAL, x = NBELPOV100))+
  theme_minimal()+
  labs(x = '# Households Living in Poverty', y = '')+
  scale_y_continuous(labels = scales::dollar_format())

s2 <- ggplot(data)+
  geom_point(aes(y = MEDHVAL, x = PCTBACHMOR))+
  theme_minimal()+
  labs(x = "% of Individuals with Bachelor's Degrees or Higher", y = '')+
  scale_y_continuous(labels = scales::dollar_format())

s3 <- ggplot(data)+
  geom_point(aes(y = MEDHVAL, x = PCTVACANT))+
  theme_minimal()+
  labs(x = '% of Vacant Houses', y = '')+
  scale_y_continuous(labels = scales::dollar_format())

s4 <- ggplot(data)+
  geom_point(aes(y = MEDHVAL, x = PCTSINGLES))+
  theme_minimal()+
  labs(x = '% of Single House Units', y = '')+
  scale_y_continuous(labels = scales::dollar_format())


title <- ggdraw() + 
  draw_label(
    "Median House Value vs Independent Variables",
    fontface = 'bold',
    x = 0,
    hjust = 0
  ) +
  theme(
    # add margin on the left of the drawing canvas,
    # so title is aligned with left edge of first plot
    plot.margin = margin(0, 0, 0, 7)
  )

row1 <- plot_grid(s1,s2)
row2 <- plot_grid(s3,s4)
plot_grid(title, row1, row2, nrow = 3,   rel_heights = c(0.1, 1, 1))

#1c
correlate(data %>% select(MEDHVAL, PCTBACHMOR, NBELPOV100, PCTVACANT, PCTSINGLES)) %>%  autoplot()+
  geom_text(aes(label = round(r,digits=2)),size = 2)+
  labs(title= 'Pearson Correlation of Variables')

#3a regression 

reg1 <- lm(MEDHVALlog ~ ., data %>% select(MEDHVALlog, PCTBACHMOR, NBELPOV100log, PCTVACANT, PCTSINGLES))

summary(reg1)
anova(reg1)

data$fitted <- reg1 %>% fitted()
data$resids <- reg1 %>% residuals()
data$resids_standard <- reg1 %>% rstandard()

ggplot(data)+
  geom_point(aes(y = resids_standard, x = fitted))+
  theme_minimal()+
  labs(y = 'Standardized Residuals', x= 'Predicted Values')

ggplot(data)+
  geom_histogram(aes(x = resids_standard), bins = 100)+
  theme_minimal()+
  labs(title = "Standardized Residuals of OLS Regression Histogram", x = 'Standardized Residuals', y = "")

                 