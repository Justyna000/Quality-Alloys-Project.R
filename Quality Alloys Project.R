# Loading the dataset - Weekly Visits and Financials
# Additional column added for period - initial, pre-promotion, promotion, post-promotion

library(readxl)
alloy_df <- read_excel("C:/Users/dmows/OneDrive/Dokumenty/Class1 - R/Quality Alloys.xlsx")

View(alloy_df)

# Printing classes of all variables
sapply(alloy_df,class)

# Descriptive statistics for each column
summary(alloy_df)

# Descriptive statistics for specific columns - visits, unique visits, revenue, lbs.sold
summary(alloy_df$Visits)
summary(alloy_df$`Unique Visits`)
summary(alloy_df$Revenue)
summary(alloy_df$`Lbs. Sold`)


library(dplyr)

colnames(alloy_df)
alloy_df

group_by(alloy_df, Period) %>% 
  summarise(
    Visits_count = n(), 
    Visits_mean1 = mean(Visits, na.rm = TRUE),
    Visits_sd1 = sd(Visits, na.rm = TRUE),
    Visits_median1 = median(Visits, na.rm = TRUE),
    Visits_min1 = min(Visits, na.rm = TRUE),
    Visits_max1 = max(Visits, na.rm = TRUE)
  )

# Comparaison of means by period

library(ggplot2)

# Visits
bar1<-ggplot(alloy_df, aes(Period, Visits, fill = Period))
bar1 + stat_summary(fun.y = mean, geom = "bar") + stat_summary(fun.data = mean_cl_normal, geom = "errorbar", width = 0.2)

# Unique Visits
bar2<-ggplot(alloy_df, aes(Period, `Unique Visits`, fill = Period))
bar2 + stat_summary(fun.y = mean, geom = "bar") + stat_summary(fun.data = mean_cl_normal, geom = "errorbar", width = 0.2) 

# Revenue
bar3<-ggplot(alloy_df, aes(Period, Revenue, fill = Period))
bar3 + stat_summary(fun.y = mean, geom = "bar") + stat_summary(fun.data = mean_cl_normal, geom = "errorbar", width = 0.2) +
  ggtitle(label = "Average Revenue per Period") +
  theme(plot.title = element_text(hjust = 0.5)) 

# Lbs. Sold
bar4<-ggplot(alloy_df, aes(Period, `Lbs. Sold`, fill = Period))
bar4 + stat_summary(fun.y = mean, geom = "bar") + stat_summary(fun.data = mean_cl_normal, geom = "errorbar", width = 0.2)

# Costs
bar5<-ggplot(alloy_df, aes(Period, Costs, fill = Period))
bar5 + stat_summary(fun.y = mean, geom = "bar") + stat_summary(fun.data = mean_cl_normal, geom = "errorbar", width = 0.2)

# Inquiries
bar6<-ggplot(alloy_df, aes(Period, Inquiries, fill = Period))
bar6 + stat_summary(fun.y = mean, geom = "bar") + stat_summary(fun.data = mean_cl_normal, geom = "errorbar", width = 0.2)

# Avg. Time on Site
bar7<-ggplot(alloy_df, aes(Period, `Avg. Time on Site (secs.)`, fill = Period))
bar7 + stat_summary(fun.y = mean, geom = "bar") + stat_summary(fun.data = mean_cl_normal, geom = "errorbar", width = 0.2) +
  ggtitle(label = "Average Time Spent on Site per Period") +
  theme(plot.title = element_text(hjust = 0.5)) 

# Average Bounce Rate
bar8<-ggplot(alloy_df, aes(Period, `Bounce Rate`, fill = Period))
bar8 + stat_summary(fun.y = mean, geom = "bar") + stat_summary(fun.data = mean_cl_normal, geom = "errorbar", width = 0.2) +
  ggtitle(label = "Average Bounce Rate per Period") +
  theme(plot.title = element_text(hjust = 0.5)) 


sd(alloy_df$Visits, na.rm = TRUE)
sd(alloy_df$`Unique Visits`, na.rm = TRUE)
sd(alloy_df$Revenue, na.rm = TRUE)
sd(alloy_df$`Bounce Rate`, na.rm = TRUE)


group_by(alloy_df, Period) %>% 
  summarise(
    UVisits_count = n(), 
    UVisits_mean2 = mean(`Unique Visits`, na.rm = TRUE),
    UVisits_sd2 = sd(`Unique Visits`, na.rm = TRUE),
    UVisits_median2 = median(`Unique Visits`, na.rm = TRUE),
    UVisits_min2 = min(`Unique Visits`, na.rm = TRUE),
    UVisits_max2 = max(`Unique Visits`, na.rm = TRUE)
  )

group_by(alloy_df, Period) %>% 
  summarise(
    Rev_count = n(), 
    Rev_mean3 = mean(Revenue, na.rm = TRUE),
    Rev_sd3 = sd(Revenue, na.rm = TRUE),
    Rev_median3 = median(Revenue, na.rm = TRUE),
    Rev_min3 = min(Revenue, na.rm = TRUE),
    Rev_max3 = max(Revenue, na.rm = TRUE)
  )

group_by(alloy_df, Period) %>% 
  summarise(
    Lbs_count = n(), 
    Lbs_mean4 = mean(`Lbs. Sold`, na.rm = TRUE),
    Lbs_sd4 = sd(`Lbs. Sold`, na.rm = TRUE),
    Lbs_median4 = median(`Lbs. Sold`, na.rm = TRUE),
    Lbs_min4 = min(`Lbs. Sold`, na.rm = TRUE),
    Lbs_max4 = max(`Lbs. Sold`, na.rm = TRUE)
  )


# Correlation matrix
my_num_data <- alloy_df[, sapply(alloy_df, is.numeric)]

res <- cor(my_num_data)
round(res, 2)

library(reshape2)
melted_cormat <- melt(res)
head(melted_cormat)

library(gplots)
library(RColorBrewer)

my_palette <- colorRampPalette(c("red", "yellow", "green"))(n = 299)
col_breaks = c(seq(-1,0,length=100), # for red
               seq(0,0.8,length=100),  # for yellow
               seq(0.81,1,length=100)) # for green


cormat <- round(cor(my_num_data),2)
cormat

# Get lower triangle of the correlation matrix
get_lower_tri<-function(cormat){
  cormat[upper.tri(cormat)] <- NA
  return(cormat)
}
# Get upper triangle of the correlation matrix
get_upper_tri <- function(cormat){
  cormat[lower.tri(cormat)]<- NA
  return(cormat)
}

upper_tri <- get_upper_tri(cormat)
upper_tri

# Melt the correlation matrix
library(reshape2)
melted_cormat <- melt(upper_tri, na.rm = TRUE)
# Heatmap
library(ggplot2)
ggplot(data = melted_cormat, aes(Var2, Var1, fill = value))+
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Pearson\nCorrelation") +
  theme_minimal()+ 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 12, hjust = 1))+
  coord_fixed()

# Melt the correlation matrix
library(reshape2)
melted_cormat <- melt(upper_tri, na.rm = TRUE)
# Heatmap
library(ggplot2)
ggplot(data = melted_cormat, aes(Var2, Var1, fill = value))+
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Pearson\nCorrelation") +
  theme_minimal()+ 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 12, hjust = 1))+
  coord_fixed()


reorder_cormat <- function(cormat){
  # Use correlation between variables as distance
  dd <- as.dist((1-cormat)/2)
  hc <- hclust(dd)
  cormat <-cormat[hc$order, hc$order]
}

# Reorder the correlation matrix
cormat <- reorder_cormat(cormat)
upper_tri <- get_upper_tri(cormat)
# Melt the correlation matrix
melted_cormat <- melt(upper_tri, na.rm = TRUE)
# Create a ggheatmap
ggheatmap <- ggplot(melted_cormat, aes(Var2, Var1, fill = value))+
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Pearson\nCorrelation") +
  theme_minimal()+ # minimal theme
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 12, hjust = 1))+
  coord_fixed()

# Print the heatmap
print(ggheatmap)

ggheatmap + 
  geom_text(aes(Var2, Var1, label = value), color = "black", size = 3) +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.major = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank(),
    axis.ticks = element_blank(),
    legend.justification = c(1, 0),
    legend.position = c(0.6, 0.7),
    legend.direction = "horizontal")+
  guides(fill = guide_colorbar(barwidth = 7, barheight = 1,
                               title.position = "top", title.hjust = 0.5))

###########################################################################################################################

# Revenue vs pounds sold

scatter1<-ggplot(alloy_df, aes(`Lbs. Sold`, Revenue))
scatter1 +geom_point() +geom_smooth(method="lm", colour = "Green")

# Revenue versus visits

scatter2<-ggplot(alloy_df, aes(Visits, Revenue))
scatter2 +geom_point() +geom_smooth(method="lm", colour = "Pink")

# Analysis of Lbs Sold 2005-2010

lbs_sold <- read_excel("C:/Users/dmows/OneDrive/Dokumenty/Class1 - R/Lbs_Sold.xlsx")
lbs_sold

summary(lbs_sold$`Lbs. Sold`)
hist (lbs_sold$`Lbs. Sold`, main="Distribution of Lbs. Sold")

ggplot(lbs_sold, aes(x=Week, y=`Lbs. Sold`, group=1)) +
  geom_line()
  

# Histogram of the Visits

hist (alloy_df$Visits, main="Distribution of Visits")
ggplot(alloy_df, aes(x=`Week (2008-2009)`, y=Visits, group=1)) +
  geom_line()


ggplot(alloy_df, aes(x=`Week (2008-2009)`, y=Revenue, group=1)) +
  geom_line()

ggplot(alloy_df, aes(x=`Week (2008-2009)`, y=Profit, group=1)) +
  geom_line()


library(readxl)
library(data.table)
library(ggplot2)
library(plotly)
library(moments)
library(corrplot)
library(PerformanceAnalytics)
library(psych)
library(lmtest)
library(rsq)


hist (alloy_df$`Avg. Time on Site (secs.)`, main="Avg. Time on Site")

#################################################################################################################################

# Grouping data into months (more or less 4-5 weeks) to better see the patterns

## Profits per period

`5_weeks_prof` <- c(sum(alloy_df$Profit[2:6]),
                    sum(alloy_df$Profit[7:10]),
                    sum(alloy_df$Profit[11:15]),
                    sum(alloy_df$Profit[16:19]),
                    sum(alloy_df$Profit[20:23]),
                    sum(alloy_df$Profit[24:28]),
                    sum(alloy_df$Profit[29:32]),
                    sum(alloy_df$Profit[33:36]),
                    sum(alloy_df$Profit[37:40]),
                    sum(alloy_df$Profit[41:45]),
                    sum(alloy_df$Profit[46:49]),
                    sum(alloy_df$Profit[50:53]),
                    sum(alloy_df$Profit[54:58]),
                    sum(alloy_df$Profit[59:62]),
                    sum(alloy_df$Profit[63:66]))

prof_table <- data.table(`5_weeks` = seq(1,15,1), Profit = `5_weeks_prof`, Group = c(rep("Initial",3),
                                                                                     rep("Pre-Promo",5),
                                                                                     rep("Promotion",4),
                                                                                     rep("Post-Promo",3)))

ggplot(data = prof_table, aes(x=prof_table$`5_weeks`, y=prof_table$Profit/1000, fill = Group))  + 
  geom_bar(stat="identity") + 
  scale_x_discrete("Months") + 
  scale_y_continuous("Profit(in thousands)") + 
  scale_fill_discrete(breaks=c("Initial","Pre-Promo","Promotion", "Post-Promo")) +
  ggtitle(label = "Profits per Period, May 25, 2008 - August 29, 2009 ") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(text = element_text(size=10),
        legend.title = element_text(size=10),
        legend.text = element_text(size=10),
        legend.position = "right")

# Revenues per period

`5_weeks_rev` <- c(sum(alloy_df$Revenue[2:6]),
                   sum(alloy_df$Revenue[7:10]),
                   sum(alloy_df$Revenue[11:15]),
                   sum(alloy_df$Revenue[16:19]),
                   sum(alloy_df$Revenue[20:23]),
                   sum(alloy_df$Revenue[24:28]),
                   sum(alloy_df$Revenue[29:32]),
                   sum(alloy_df$Revenue[33:36]),
                   sum(alloy_df$Revenue[37:40]),
                   sum(alloy_df$Revenue[41:45]),
                   sum(alloy_df$Revenue[46:49]),
                   sum(alloy_df$Revenue[50:53]),
                   sum(alloy_df$Revenue[54:58]),
                   sum(alloy_df$Revenue[59:62]),
                   sum(alloy_df$Revenue[63:66]))

rev_table <- data.table(`5_weeks` = seq(1,15,1), Revenue = `5_weeks_rev`, Group = c(rep("Initial",3),
                                                                                     rep("Pre-Promo",5),
                                                                                     rep("Promotion",4),
                                                                                     rep("Post-Promo",3)))

ggplot(data = rev_table, aes(x=rev_table$`5_weeks`, y=rev_table$Revenue/1000, fill = Group))  + 
  geom_bar(stat="identity") + 
  scale_x_discrete("Months") + 
  scale_y_continuous("Revenues(in thousands)") + 
  scale_fill_discrete(breaks=c("Initial","Pre-Promo","Promotion", "Post-Promo")) +
  ggtitle(label = "Revenues per Period, May 25, 2008 - August 29, 2009 ") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(text = element_text(size=10),
        legend.title = element_text(size=10),
        legend.text = element_text(size=15),
        legend.position = "right")

# Costs per period

`5_weeks_cost` <- c(sum(alloy_df$Costs[2:6]),
                    sum(alloy_df$Costs[7:10]),
                    sum(alloy_df$Costs[11:15]),
                    sum(alloy_df$Costs[16:19]),
                    sum(alloy_df$Costs[20:23]),
                    sum(alloy_df$Costs[24:28]),
                    sum(alloy_df$Costs[29:32]),
                    sum(alloy_df$Costs[33:36]),
                    sum(alloy_df$Costs[37:40]),
                    sum(alloy_df$Costs[41:45]),
                    sum(alloy_df$Costs[46:49]),
                    sum(alloy_df$Costs[50:53]),
                    sum(alloy_df$Costs[54:58]),
                    sum(alloy_df$Costs[59:62]),
                    sum(alloy_df$Costs[63:66]))

cost_table <- data.table(`5_weeks` = seq(1,15,1), Costs = `5_weeks_cost`, Group = c(rep("Initial",3),
                                                                                    rep("Pre-Promo",5),
                                                                                    rep("Promotion",4),
                                                                                    rep("Post-Promo",3)))

ggplot(data = cost_table, aes(x=cost_table$`5_weeks`, y=cost_table$Costs/1000, fill = Group))  + 
  geom_bar(stat="identity") + 
  scale_x_discrete("Months") + 
  scale_y_continuous("Costs(in thousand)") + 
  scale_fill_discrete(breaks=c("Initial","Pre-Promo","Promotion", "Post-Promo")) +
  ggtitle(label = "Costs") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(text = element_text(size=15),
        legend.title = element_text(size=10),
        legend.text = element_text(size=10),
        legend.position = "right")


# Visits per period

`5_weeks_vist` <- c(sum(alloy_df$Visits[2:6]),
                    sum(alloy_df$Visits[7:10]),
                    sum(alloy_df$Visits[11:15]),
                    sum(alloy_df$Visits[16:19]),
                    sum(alloy_df$Visits[20:23]),
                    sum(alloy_df$Visits[24:28]),
                    sum(alloy_df$Visits[29:32]),
                    sum(alloy_df$Visits[33:36]),
                    sum(alloy_df$Visits[37:40]),
                    sum(alloy_df$Visits[41:45]),
                    sum(alloy_df$Visits[46:49]),
                    sum(alloy_df$Visits[50:53]),
                    sum(alloy_df$Visits[54:58]),
                    sum(alloy_df$Visits[59:62]),
                    sum(alloy_df$Visits[63:66]))


visit_table <- data.table(`5_weeks` = seq(1,15,1), Visit = `5_weeks_vist`, Group = c(rep("Initial",3),
                                                                                    rep("Pre-Promo",5),
                                                                                    rep("Promotion",4),
                                                                                    rep("Post-Promo",3)))

ggplot(data = visit_table, aes(x=visit_table$`5_weeks`, y=visit_table$Visit, fill = Group))  + 
  geom_bar(stat="identity") + 
  scale_x_discrete("Months") + 
  scale_y_continuous("Visits") + 
  scale_fill_discrete(breaks=c("Initial","Pre-Promo","Promotion", "Post-Promo")) +
  ggtitle(label = "Website Visits per Period, May 25, 2008 - August 29, 2009") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(text = element_text(size=10),
        legend.title = element_text(size=10),
        legend.text = element_text(size=15),
        legend.position = "right")

# Inquires per period

`5_weeks_inqu` <- c(sum(alloy_df$Inquiries[2:6]),
                    sum(alloy_df$Inquiries[7:10]),
                    sum(alloy_df$Inquiries[11:15]),
                    sum(alloy_df$Inquiries[16:19]),
                    sum(alloy_df$Inquiries[20:23]),
                    sum(alloy_df$Inquiries[24:28]),
                    sum(alloy_df$Inquiries[29:32]),
                    sum(alloy_df$Inquiries[33:36]),
                    sum(alloy_df$Inquiries[37:40]),
                    sum(alloy_df$Inquiries[41:45]),
                    sum(alloy_df$Inquiries[46:49]),
                    sum(alloy_df$Inquiries[50:53]),
                    sum(alloy_df$Inquiries[54:58]),
                    sum(alloy_df$Inquiries[59:62]),
                    sum(alloy_df$Inquiries[63:66]))

inqu_table <- data.table(`5_weeks` = seq(1,15,1), Inqu = `5_weeks_inqu`, Group = c(rep("Initial",3),
                                                                                     rep("Pre-Promo",5),
                                                                                     rep("Promotion",4),
                                                                                     rep("Post-Promo",3)))

ggplot(data = inqu_table, aes(x=inqu_table$`5_weeks`, y=inqu_table$Inqu, fill = Group))  + 
  geom_bar(stat="identity") + 
  scale_x_discrete("Months") + 
  scale_y_continuous("Inquiries") + 
  scale_fill_discrete(breaks=c("Initial","Pre-Promo","Promotion", "Post-Promo")) +
  ggtitle(label = "Inquiries") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(text = element_text(size=15),
        legend.title = element_text(size=10),
        legend.text = element_text(size=10),
        legend.position = "right")


# Unique Visits per Month

`5_weeks_uvist` <- c(sum(alloy_df$`Unique Visits`[2:6]),
                    sum(alloy_df$`Unique Visits`[7:10]),
                    sum(alloy_df$`Unique Visits`[11:15]),
                    sum(alloy_df$`Unique Visits`[16:19]),
                    sum(alloy_df$`Unique Visits`[20:23]),
                    sum(alloy_df$`Unique Visits`[24:28]),
                    sum(alloy_df$`Unique Visits`[29:32]),
                    sum(alloy_df$`Unique Visits`[33:36]),
                    sum(alloy_df$`Unique Visits`[37:40]),
                    sum(alloy_df$`Unique Visits`[41:45]),
                    sum(alloy_df$`Unique Visits`[46:49]),
                    sum(alloy_df$`Unique Visits`[50:53]),
                    sum(alloy_df$`Unique Visits`[54:58]),
                    sum(alloy_df$`Unique Visits`[59:62]),
                    sum(alloy_df$`Unique Visits`[63:66]))


uvisit_table <- data.table(`5_weeks` = seq(1,15,1), UVisit = `5_weeks_uvist`, Group = c(rep("Initial",3),
                                                                                     rep("Pre-Promo",5),
                                                                                     rep("Promotion",4),
                                                                                     rep("Post-Promo",3)))

ggplot(data = uvisit_table, aes(x=uvisit_table$`5_weeks`, y=uvisit_table$UVisit, fill = Group))  + 
  geom_bar(stat="identity") + 
  scale_x_discrete("Months") + 
  scale_y_continuous(" UniqueVisits") + 
  scale_fill_discrete(breaks=c("Initial","Pre-Promo","Promotion", "Post-Promo")) +
  ggtitle(label = "Unique Visits per Period, May 25, 2008 - August 29, 2009") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(text = element_text(size=10),
        legend.title = element_text(size=10),
        legend.text = element_text(size=15),
        legend.position = "right")


# Lbs.Sold by Month

`5_weeks_sold` <- c(sum(alloy_df$`Lbs. Sold`[2:6]),
                     sum(alloy_df$`Lbs. Sold`[7:10]),
                     sum(alloy_df$`Lbs. Sold`[11:15]),
                     sum(alloy_df$`Lbs. Sold`[16:19]),
                     sum(alloy_df$`Lbs. Sold`[20:23]),
                     sum(alloy_df$`Lbs. Sold`[24:28]),
                     sum(alloy_df$`Lbs. Sold`[29:32]),
                     sum(alloy_df$`Lbs. Sold`[33:36]),
                     sum(alloy_df$`Lbs. Sold`[37:40]),
                     sum(alloy_df$`Lbs. Sold`[41:45]),
                     sum(alloy_df$`Lbs. Sold`[46:49]),
                     sum(alloy_df$`Lbs. Sold`[50:53]),
                     sum(alloy_df$`Lbs. Sold`[54:58]),
                     sum(alloy_df$`Lbs. Sold`[59:62]),
                     sum(alloy_df$`Lbs. Sold`[63:66]))


sold_table <- data.table(`5_weeks` = seq(1,15,1), Sold = `5_weeks_sold`, Group = c(rep("Initial",3),
                                                                                        rep("Pre-Promo",5),
                                                                                        rep("Promotion",4),
                                                                                        rep("Post-Promo",3)))

ggplot(data = sold_table, aes(x=sold_table$`5_weeks`, y=sold_table$Sold/1000, fill = Group))  + 
  geom_bar(stat="identity") + 
  scale_x_discrete("Months") + 
  scale_y_continuous(" Lbs. Sold (in tousands)") + 
  scale_fill_discrete(breaks=c("Initial","Pre-Promo","Promotion", "Post-Promo")) +
  ggtitle(label = "Lbs. Sold") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(text = element_text(size=15),
        legend.title = element_text(size=10),
        legend.text = element_text(size=10),
        legend.position = "right")



###################################################################################################################################

# Orders by quarters
# Orders by Qrt
#x=rev_table$`5_weeks`, y=rev_table$Revenue/1000, fill = Group

orders_df <- read_excel("C:/Users/dmows/OneDrive/Dokumenty/Class1 - R/Orders by Qrt.xlsx")
orders_df

bar <- ggplot(orders_df, aes(x=orders_df$Year, y = orders_df$Total/1000, fill = Quarters))

bar + stat_summary(fun.y = mean, geom = "bar", position="dodge") + ggtitle(label = "Lbs Sold (in thousand) by Qrt") + 
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(x = "Year", y = "Lbs Sold (in thousands)", fill = "Qrt") +
  scale_fill_brewer(palette="Blues")

# Daily Visits

visits_df <- read_excel("C:/Users/dmows/OneDrive/Dokumenty/Class1 - R/Daily Visits.xlsx")
visits_df

p1 <- ggplot() + geom_line(aes(y = Visits, x = Day), 
                           data = visits_df, stat="identity")

hist(visits_df$Visits)
?hist


ggplot(data=visits_df, aes(x=Day, y=Visits)) +
  stat_summary(fun.y = mean, geom = "bar", position="dodge")

###########################################################################################################################

# Histogram Profit
mx1 <- mean(alloy_df$Profit)
hist(alloy_df$Profit, # histogram
     col="peachpuff", # column color
     border="black",
     xlab = "Profits",
     main = "Profits Distribution")
abline(v = mx1, col = "black", lwd = 4)

# Histogram Lbs. Sold
mx <- mean(alloy_df$`Lbs. Sold`)


hist(alloy_df$`Lbs. Sold`, # histogram
     col="peachpuff", # column color
     border="black",
     xlab = "Quantity",
     main = "Quantity Distribution") 

abline(v = mx, col = "black", lwd = 4)

# Histogram Revenues

mx3 <- mean(alloy_df$Revenue)


hist(alloy_df$Revenue, # histogram
     col="peachpuff", # column color
     border="black",
     xlab = "Revenues",
     main = "Revenues Distribution") 

abline(v = mx3, col = "black", lwd = 4)

##########################################################################################################################

# Moved Profits and Revenues - 2 weeks

moved_df <- read_excel("C:/Users/dmows/OneDrive/Dokumenty/Class1 - R/Quality Alloys Moved.xlsx")

View(moved_df)

# Correlation matrix

my_num_data2 <- moved_df[, sapply(moved_df, is.numeric)]

res2 <- cor(my_num_data2)
round(res2, 2)

library(reshape2)
melted_cormat2 <- melt(res2)
head(melted_cormat2)

library(gplots)
library(RColorBrewer)

my_palette <- colorRampPalette(c("red", "yellow", "green"))(n = 299)
col_breaks = c(seq(-1,0,length=100), # for red
               seq(0,0.8,length=100),  # for yellow
               seq(0.81,1,length=100)) # for green


cormat2 <- round(cor(my_num_data2),2)
cormat2

# Get lower triangle of the correlation matrix
get_lower_tri<-function(cormat2){
  cormat[upper.tri(cormat2)] <- NA
  return(cormat2)
}
# Get upper triangle of the correlation matrix
get_upper_tri <- function(cormat2){
  cormat[lower.tri(cormat2)]<- NA
  return(cormat2)
}

upper_tri <- get_upper_tri(cormat2)
upper_tri

# Melt the correlation matrix
library(reshape2)
melted_cormat2 <- melt(upper_tri, na.rm = TRUE)
# Heatmap
library(ggplot2)
ggplot(data = melted_cormat2, aes(Var2, Var1, fill = value))+
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Pearson\nCorrelation") +
  theme_minimal()+ 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 12, hjust = 1))+
  coord_fixed()

# Melt the correlation matrix
library(reshape2)
melted_cormat2 <- melt(upper_tri, na.rm = TRUE)
# Heatmap
library(ggplot2)
ggplot(data = melted_cormat2, aes(Var2, Var1, fill = value))+
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Pearson\nCorrelation") +
  theme_minimal()+ 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 12, hjust = 1))+
  coord_fixed()


reorder_cormat2 <- function(cormat2){
  # Use correlation between variables as distance
  dd <- as.dist((1-cormat2)/2)
  hc <- hclust(dd)
  cormat2 <-cormat2[hc$order, hc$order]
}

# Reorder the correlation matrix
cormat2 <- reorder_cormat(cormat2)
upper_tri <- get_upper_tri(cormat2)
# Melt the correlation matrix
melted_cormat2 <- melt(upper_tri, na.rm = TRUE)
# Create a ggheatmap
ggheatmap <- ggplot(melted_cormat2, aes(Var2, Var1, fill = value))+
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Pearson\nCorrelation") +
  theme_minimal()+ # minimal theme
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 12, hjust = 1))+
  coord_fixed()

# Print the heatmap
print(ggheatmap)

ggheatmap + 
  geom_text(aes(Var2, Var1, label = value), color = "black", size = 3) +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.major = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank(),
    axis.ticks = element_blank(),
    legend.justification = c(1, 0),
    legend.position = c(0.6, 0.7),
    legend.direction = "horizontal")+
  guides(fill = guide_colorbar(barwidth = 7, barheight = 1,
                               title.position = "top", title.hjust = 0.5))

cormat
cormat2

####################################################################################################################

# Regression Revenues on Visits before moving the weeksa

install.packages("ggpubr")
library(ggpubr)


ggscatter(alloy_df, x = "Visits", y = "Revenue", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Visits", ylab = "Revenue")


# After moving the weeks

ggscatter(moved_df, x = "Visits", y = "Revenue", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Visits", ylab = "Revenue")

?ggscatter

# Inquiries on Sales

# Regressions

model1 <- lm(Revenue ~ Visits, data = alloy_df)
summary(model1)

model2 <- lm(Revenue ~ Visits, data = moved_df)
summary(model2)


ggscatter(alloy_df, x = "Revenue", y = "Visits", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Revenue", ylab = "Visits")


# After moving the weeks

ggscatter(moved_df, x = "Revenue", y = "Visits", ggbarplot
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Revenue", ylab = "Visits")

##########################################################################################################################

# hist (lbs_sold$`Lbs. Sold`, main="Distribution of Lbs. Sold")
mx4 <- mean(lbs_sold$`Lbs. Sold`)

hist(lbs_sold$`Lbs. Sold`, # histogram
     col="peachpuff", # column color
     border="black",
     xlab = "`Lbs. Sold",
     main = "Orders Quantity Distribution") 

abline(v = mx4, col = "black", lwd = 4)

alloy_df$Rev


ggbarplot(alloy_df, x = alloy_df$`Week (2008-2009)`, y=alloy_df$Revenue)

#########################################################################################################

install.packages("plotly")
library(plotly)

p <- plot_ly(alloy_df, x =alloy_df$`Week (2008-2009)`, y = alloy_df$Revenue, type = 'bar')
p

initial <- alloy_df[1:14,]
pre_prom <-alloy_df[15:35,]
promotion <-alloy_df[36:52,]
post_prom <-alloy_df[53:66,]

p1 <- plot_ly(initial, x =initial$`Week (2008-2009)`, y = initial$Revenue, type = 'bar') %>%
  layout(title = "Revenue - Initial Period",
         xaxis = list(title = ""),
         yaxis = list(title = ""))
p1

p2 <- plot_ly(pre_prom, x =pre_prom$`Week (2008-2009)`, y = pre_prom$Revenue, type = 'bar', color = 'rgba(255, 182, 193, .9)') %>%
  layout(title = "Revenue - Pre-promotion Period",
         xaxis = list(title = ""),
         yaxis = list(title = ""))
p2

p3 <- plot_ly(promotion, x =promotion$`Week (2008-2009)`, y = promotion$Revenue, type = 'bar') %>%
  layout(title = "Revenue - Promotion Period",
         xaxis = list(title = ""),
         yaxis = list(title = ""))
p3

p4 <- plot_ly(post_prom, x =post_prom$`Week (2008-2009)`, y = post_prom$Revenue, type = 'bar', color = 'rgba(255, 182, 193, .9)') %>%
  layout(title = "Revenue - Post-promotion Period",
         xaxis = list(title = ""),
         yaxis = list(title = ""))
p4

#####################################################################################################



daily_df <- read_excel("C:/Users/dmows/OneDrive/Dokumenty/Class1 - R/Daily Visits.xlsx")

View(daily_df)
daily_df$Visits

initial <- daily_df[1:14,]
pre_prom <-daily_df[15:35,]
promotion <-daily_df[36:52,]
post_prom <-daily_df[53:66,]

ggplot(daily_df) +
  geom_bar(aes(x = class), fill = '#003366', color = '#add8e6')

?group_by

bar7<-ggplot(daily_df, aes(`Week Day`, Visits,fill=`Week Day` ))
bar7 + stat_summary(fun.y = sum, geom = "bar", legend.position = "none") 


bar7<-ggplot(daily_df, aes(x=reorder(`Week Day`, -Visits), Visits,fill=`Week Day` ))
bar7 + stat_summary(fun.y = sum, geom = "bar") + guides(fill=FALSE) + labs(x="Week Days", y="Number of Visits") +
  ggtitle(label = "Nb of Visits per Week Day, May 26, 2008 - August 24, 2009") +
  theme(plot.title = element_text(hjust = 0.5)) 

###############################################################################################
# Where to advertise?

mydf <- read_excel("Visits.xlsx")

visits <- mydf$Visits
barplot(visits, main = "Number of Visits per Traffic Sources", xlab = "Traffic Sources", 
        names.arg = c("Referring Sites", "Search Engines", "Direct Traffic", "Others"),
        col = c("red", "blue", "green", "orange"), ylim=c(0, 40000))


mydf2 <- read_excel("Top Ten Referring Sites.xlsx")

others <- sum(tail(mydf2$Visits))
print(others)

visits2 <- c(head(mydf2$Visits, n= 5), others)
?head

par(las=2)
barplot(visits2, main = "Top Referring Sites", xlab = "Visits", horiz = TRUE, 
        names.arg = c("Google Ads", "Googles Indication", "Sedo", "Engineering360", "Search Portal", 
                      "Others"), col = c("red", "blue", "green", "orange", "yellow", "purple"),
        xlim=c(0, 20000),cex.names=0.4, cex.axis=0.8
)

barplot(visits2, main = "Top Referring Sites", xlab = "Visits", horiz = TRUE, 
        names.arg = c("Google Ads", "Googles Indication", "Sedo", "Engineering360", "Search Portal", 
                      "Others"), col = c("red", "blue", "green", "orange", "yellow", "purple"),
        xlim=c(0, 18000)
)
mtext(side=2, line=3, "Y-axis label, bold, bigger", col="orange", font=2, cex=1.2)


mydf3 <- read_excel("Top Ten Browsers.xlsx")
others3 <- sum(tail(mydf3$Visits))
visits3 <- c(head(mydf3$Visits, n= 5), others3)

par(las=2)
barplot(visits3, main = "Top Browsers", xlab = "Visits", horiz = TRUE, 
        names.arg = c("Explorer", "Firefox", "Opera", "Safari", "Chrome", 
                      "Others"), col = c("red", "blue", "green", "orange", "yellow", "purple"),
        cex.names=0.8, cex.axis=0.8, xlim=c(0, 60000))

mydf4 <- read_excel("Top Ten Geographics.xlsx")
visits4 <- mydf4$Visits

par(las=2)
barplot(visits4, main = "Top Ten Geographics", ylab = "Visits", 
        names.arg = c("S. America", "N. America", "C. America",
                      "W. Europe", "E. Asia", "N. Europe", "S. Asia", "S.-E. Asia", "S. Europe",
                      "E. Europe"), col = c("red", "blue", "green", "orange", "yellow", "purple"),
        ylim=c(0, 25000), cex.names=0.8, cex.axis=0.8)


mydf5 <- read_excel("Top Ten Operating Systems.xlsx")
others5 <- sum(tail(mydf5$Visits))
visits5 <- c(head(mydf5$Visits, n= 4), others5)

par(las=2)
barplot(visits5, main = "Top Operating Systems", xlab = "Visits", horiz = TRUE, 
        names.arg = c("Windows", "Macintosh", "Linux", "iPhone", 
                      "Others"), col = c("red", "blue", "green", "orange", "yellow"),
        xlim=c(0,70000), cex.names=0.7, cex.axis=0.8)


mydf6 <- read_excel("Top Ten Search Engines.xlsx")
others6 <- sum(tail(mydf6$Visits))
visits6 <- c(head(mydf6$Visits, n= 5), others6)

par(las=2)
barplot(visits6, main = "Top Search Engines", xlab = "Visits", horiz = TRUE, 
        names.arg = c("Google", "Yahoo", "Search", "MSN", "Aol",
                      "Others"), col = c("red", "blue", "green", "orange", "yellow","purple"),
        xlim=c(0,20000))









