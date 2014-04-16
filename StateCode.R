library(ggplot2)
library(plyr)
library(maps)
library(Hmisc)
library(stringi)
data(state)
states <- map_data("state")
colnames(states)[5] <- "State"
states$State <- stri_trans_totitle(states$State)
df <- data.frame(state.x77,
                 State = state.name,
                 Abbrev = state.abb,
                 Region = state.region,
                 Division = state.division
)  

df2 <- merge(states,df,by="State")
df2 <- df2[order(df2$order),]
mid_range <- function(x) mean(range(x,na.rm=TRUE))
centres <- ddply(df2, .(Abbrev),
                 colwise(mid_range,.(lat,long,Population)))


gg <- function(Population) {
  df2$Cols <- df2[,Population]
  p <- ggplot()+                                                          # Changes made here
    geom_polygon(data = df2, aes(long,lat,fill=Population,group=group)) +   # and here.
    geom_text(aes(x=long,y=lat,label=Abbrev), data = centres, size=4)
  p + theme(legend.title = element_text("Population"))
}

gg("Population")