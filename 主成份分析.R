library(tidyverse)
financial.data <- read_csv("D:\\R resource\\2017_financial index_163 comp.csv")
head(financial.data, 5)
summary(financial.data[,2:ncol(financial.data)])
cor(financial.data[, 2 :ncol(financial.data)])#?????{?? <- ?]???x?}???\Ū?ҥH??HEATMAP
#?p?G?n?s?@???ϥ???tidy?? ?ܼ?1-?ܼ?2-?????Y?? melt????
library(reshape2)
head(melt(cor(financial.data[, 2 :ncol(financial.data)])))
ggplot(melt(cor(financial.data[,2:ncol(financial.data)])),
       aes(Var1,Var2))+
  geom_tile(aes(fill=value),colour="White")+#?????C???ѭȨM?w ?~?ج??զ?
  
  scale_fill_gradient2(low="firebrick4",high = "steelblue",mid="White",midpoint = 0)+
  
  guides(fill= guide_legend(title = "Correlation"))+
  
  theme_bw()+
  
  theme(axis.text.x = element_text(angle=45, hjust = 1,vjust = 1),
        axis.title = element_blank())
#???ƫؼһP?��R
pca.model <- prcomp(financial.data[, 2:ncol(financial.data)], scale = T)#scale=T?????ƼзǤ?
names(pca.model)
summary(pca.model)

#?إ?data.frame
var.exp <- tibble(
  pc = paste0("PC_", formatC(1:16, width=2, flag="0")),
  var = pca.model$sdev^2,
  prop = (pca.model$sdev)^2 / sum((pca.model$sdev)^2),
  cum_prop = cumsum((pca.model$sdev)^2 / sum((pca.model$sdev)^2)))
var.exp

#ø??
library(plotly)
plot_ly(
  x = var.exp$pc,
  y = var.exp$var,
  type = "bar"
) %>%
  layout(
    title = "Variance Explained by Each Principal Component",
    xaxis = list(title = 'Principal Component', tickangle = -60),
    yaxis = list(title = 'Variance'),
    margin = list(r = 30, t = 50, b = 70, l = 50)
  )
plot_ly(
  x = var.exp$pc,
  y = var.exp$cum_prop,
  type = "bar"
) %>%
  layout(
    title = "Cumulative Proportion by Each Principal Component",
    xaxis = list(title = 'Principal Component', tickangle = -60),
    yaxis = list(title = 'Propotion'),
    margin = list(r = 30, t = 50, b = 70, l = 50)
  )#???FPC_06?w?g?F??8?? ?ҥH???e??6???ܼ?
ggplot(melt(pca.model$rotation[,1:6]),
       aes(Var2,Var1))+
  geom_tile(aes(fill=value),colour="White")+
  scale_fill_gradient2(low = "firebrick4",high = "steelblue",mid="White",midpoint = 0)+
  guides(fill=guide_legend(title = "Coefficient"))+
  theme_bw()+
  theme(axis.text.x=element_text(angle=45),
        axis.title = element_blank())#???????t???????��ҥH?ĥΤU?????k
#?D?t?D???����R
library(nsprcomp)
nspca.model <- nscumcomp(
  financial.data[,2:17],
  k=90,
  nneg = T,
  scale. = T
)#k?G?D 0 ?Y?ƭӼơA?q?`?O?u?C?ӥD?????��ݫD 0 ?Y?ƭӼơvx ?ܼƭӼ? nneg???D?t

var.exp <- tibble(
  pc=paste0("PC_",formatC(1:16,width = 2,flag = "0")),
  var=nspca.model$sdev^2,
  prop=(nspca.model$sdev)^2/sum((nspca.model$sdev)^2),
  cum_prop=cumsum((nspca.model$sdev)^2/sum((nspca.model$sdev)^2))
)
head(var.exp,5)
plot_ly(
  x = var.exp$pc,
  y = var.exp$var,
  type = "bar"
) %>%
  layout(
    title = "Variance Explained by Each Principal Component",
    xaxis = list(title = 'Principal Component', tickangle = -60),
    yaxis = list(title = 'Variance'),
    margin = list(r = 30, t = 50, b = 70, l = 50)
  )
plot_ly(
  x = var.exp$pc,
  y = var.exp$cum_prop,
  type = "bar"
) %>%
  layout(
    title = "Cumulative Proportion by Each Principal Component",
    xaxis = list(title = 'Principal Component', tickangle = -60),
    yaxis = list(title = 'Propotion'),
    margin = list(r = 30, t = 50, b = 70, l = 50))#??8?ӥD???��Ӱ???ı??

ggplot(melt(nspca.model$rotation[,1:8]),
       aes(Var2,Var1))+
  geom_tile(aes(fill=value),colour="White")+
  scale_fill_gradient2(low = "firebrick4",high = "steelblue",mid="White")+
  guides(fill=guide_legend(title = "Coefficient"))+
  theme_bw()+
  theme(axis.text.x=element_text(angle=45),
        axis.title = element_blank())
#???q?ӧO?��R
nspca.model$x #?Q?μзǤ??ܼƦ??p?D?????��R???Ѽƨño???D?????��?EX:Y11.....
nspca.score <- data.frame(nspca.model$x)
row.names(nspca.score) <- financial.data$comp_id
plot_ly(
  x = nspca.score[, 1],
  y = financial.data$roe,
  text = financial.data$comp_id,
  type = "scatter",
  mode = "markers"
) %>% layout(
  title = "ROE v.s. PC 1 Score: Scatter Plot",
  xaxis = list(title = 'Principal Component 1'),
  yaxis = list(title = 'Return on Equity'),
  margin = list(r = 30, t = 50, b = 70, l = 50)
)
plot_ly(
  x = nspca.score[, 2],
  y = nspca.score[, 3],
  text = financial.data$comp_id,
  type = "scatter",
  mode = "markers"
) %>% layout(
  title = "PC 2 v.s. PC 3 Score: Scatter Plot",
  xaxis = list(title = 'Principal Component 2'),
  yaxis = list(title = 'Principal Component 3'),
  margin = list(r = 30, t = 50, b = 70, l = 50)
)
