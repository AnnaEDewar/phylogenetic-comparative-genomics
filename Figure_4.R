# Figure 4 code

# Packages ----
library(tidyverse)

## Function for plotting ----
prettyZero <- function(l){
  max.decimals = max(nchar(str_extract(l, "\\.[0-9]+")), na.rm = T)-1
  lnew = formatC(l, replace.zero = T, zero.print = "0",
                 digits = max.decimals, format = "f", preserve.width=T)
  return(lnew)
}


# R2 formula ----
## R2 = t^2 / (t^2 + n-2)
#qt function gives t critical value for given df (n-2) and p-value
r2 <- (qt(p=0.05,df=x-2, lower.tail=FALSE)^2)/(((qt(p=0.05,df=x-2, lower.tail=FALSE))^2)+(x-2))

# Set values ----
x<-5:1000
df<-data.frame(x)


# Produce data for lines
p_0.05 <- function(x) (qt(p=0.05,df=x-2, lower.tail=FALSE)^2)/(((qt(p=0.05,df=x-2, lower.tail=FALSE))^2)+(x-2))
p_0.01 <- function(x) (qt(p=0.01,df=x-2, lower.tail=FALSE)^2)/(((qt(p=0.01,df=x-2, lower.tail=FALSE))^2)+(x-2))
p_0.001 <- function(x) (qt(p=0.001,df=x-2, lower.tail=FALSE)^2)/(((qt(p=0.001,df=x-2, lower.tail=FALSE))^2)+(x-2))
p_0.0001 <- function(x) (qt(p=0.0001,df=x-2, lower.tail=FALSE)^2)/(((qt(p=0.0001,df=x-2, lower.tail=FALSE))^2)+(x-2))
p_0.00001 <- function(x) (qt(p=0.00001,df=x-2, lower.tail=FALSE)^2)/(((qt(p=0.00001,df=x-2, lower.tail=FALSE))^2)+(x-2))


y_p_0.05 <- p_0.05(x)
y_p_0.01 <- p_0.01(x)
y_p_0.001 <- p_0.001(x)
y_p_0.0001 <- p_0.0001(x)
y_p_0.00001 <- p_0.00001(x)


df_x0.05 <- data.frame(x,y_p_0.05)
df_x0.01 <- data.frame(x,y_p_0.01)
df_x0.001 <- data.frame(x,y_p_0.001)
df_x0.0001 <- data.frame(x,y_p_0.0001)
df_x0.00001 <- data.frame(x,y_p_0.00001)


## Bind 4 datasets together with a column indicatign p-value
# Then plot one geom line and have 'aes(group=pvalue)' to get a legend for each line
y <- p_0.05(x)
df_x0.05 <- data.frame(x,y)

y <- p_0.01(x)
df_x0.01 <- data.frame(x,y)

y <- p_0.001(x)
df_x0.001 <- data.frame(x,y)

y <- p_0.0001(x)
df_x0.0001 <- data.frame(x,y)

y <- p_0.00001(x)
df_x0.00001 <- data.frame(x,y)

all_pvalues <- bind_rows(list(p_0.05=df_x0.05, p_0.01=df_x0.01, p_0.001=df_x0.001, p_0.0001=df_x0.0001,p_0.00001=df_x0.00001), .id="id")

# Figure 4 ----
ggplot() +
  annotate("text",y=0.06,x=521, label="<5% variance explained", colour="grey50", size=4) +
  geom_line(data=all_pvalues, aes(x=x, y=y, group=id, colour=id), size=1, alpha=0.9) +
  geom_hline(yintercept=0.05, linetype="dashed",colour="grey50") +
  scale_colour_manual(expression(paste(italic("p"), " value:")), values=c("#E69F00", "#D55E00","#CC79A7", "#0072B2", "#009E73"), labels=c("0.00001", "0.0001", "0.001", "0.01", "0.05")) +
  annotate('rect', xmin=0, xmax=700, ymin=0, ymax=0.05, alpha=0.2, fill='grey50') +
  labs(x="Number of data points", y=expression("Effect size"~"(R"^2~")")) +
  theme_classic() +
  scale_y_continuous(expand=c(0,0),breaks=c(0,0.1,0.2,0.3), labels=prettyZero) +
  scale_x_continuous(expand=c(0,0),breaks=c(0,100,200,300,400,500,600)) +
  coord_cartesian(ylim = c(0,0.25), xlim = c(0,630)) +
  theme(plot.title = element_text(size=13, hjust=0.5), axis.title = element_text(size=15),
        axis.text = element_text(size=14), axis.title.y=element_text(angle=360,vjust=0.5), 
        legend.text = element_text(size=14), legend.title=element_text(size=14))


