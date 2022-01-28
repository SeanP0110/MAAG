packages = c('tidyverse', 'readxl', 'knitr')

for(p in packages){
  if(!require(p, character.only = T)){
    install.packages(p)
  }
  library(p, character.only = T)
}

orders <- read_xls("data/Superstore-2021.xls", sheet = "Orders")
people <- read_xls("data/Superstore-2021.xls", sheet = "People")
returns <- read_xls("data/Superstore-2021.xls", sheet = "Returns")

join <- left_join(returns, orders, by = c("Order ID" = "Order ID"))

freq_returned <- join %>% 
  count(`Sub-Category`) %>%
  rename(Returns = n)

freq_sort <- freq_returned %>% arrange(desc(Returns))

freq_cum <- freq_sort %>% mutate(CumReturns = cumsum(Returns))

freq_cum

#making the bar chart
bar <- freq_cum %>% ggplot(aes(reorder(`Sub-Category`, -Returns), Returns)) + 
  geom_col(color = "blue", fill = "blue") + labs(x = "Sub-Catgory") +
  scale_y_continuous(limits= c(0,600,100))
bar

#making the line chart
freq_cum_prop <- freq_cum %>% mutate(
  PropCumReturns = CumReturns/sum(Returns)*100)
freq_cum_prop

line <- freq_cum_prop %>% ggplot(aes(
  reorder(`Sub-Category`, PropCumReturns), PropCumReturns)) +
  geom_point(shape = 19, size = 1.5) + 
  geom_path(aes(reorder(`Sub-Category`, PropCumReturns), group = 1))
line

#combining the graphs
pareto <-
  freq_cum_prop %>% ggplot(aes(reorder(`Sub-Category`, -Returns), Returns)) + 
  geom_col(aes(reorder(`Sub-Category`, -Returns), Returns), 
           color = "blue", fill = "blue") +
  geom_point(aes(reorder(`Sub-Category`, PropCumReturns), PropCumReturns), 
             shape = 19, size = 1.5) + 
  geom_path(aes(reorder(`Sub-Category`, PropCumReturns), PropCumReturns, group = 1)) +
  labs(x = "Sub-Catgory") +
  scale_y_continuous("Returns",
                     sec.axis = sec_axis(~.*.01/5, name = "Percentage"))
pareto



   