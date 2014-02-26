

d = read.table("fig1A_data.txt", head = T, as.is = T, quote = "", comment.char = "")
d2 = read.table("fig1B_data.txt", head = T, as.is = T, quote = "", comment.char = "")

cont = read.table("merge_v1_v2.clust.edit2", as.is = T, quote = "", comment.char = "")
cont = cont[cont[,4] != "Reference",]
cont = cont[cont[,4] != "Outgroup",]
cont = cont[cont[,4] != "USA",]
d = d[d[,1] %in% cont[,3],]
d2 = d2[d2[,1] %in% cont[,3],]



d = d[order(-d[,2]),]
d2 = d2[order(-d2[,2]),]
d[,4] = NA
d2[,4] = NA
d[,5] = NA
d2[,5] = NA
for (i in 1:nrow(d)){
	p = d[i,1]
	c = cont[cont[,3] == p,]
	d[i,4] = c[1,4]
}

for (i in 1:nrow(d2)){
        p = d2[i,1]
        c = cont[cont[,3] == p,]
        d2[i,4] = c[1,4]
}

d[d[,4] == "Africa",5] = "red"
d2[d2[,4] == "Africa",5] = "red"

d[d[,4] == "Europe",5] = "orange"
d2[d2[,4] == "Europe",5] = "orange"

d[d[,4] == "MiddleEast",5] = "blue"
d2[d2[,4] == "MiddleEast",5] = "blue"

d[d[,4] == "Americas",5] = "yellow"
d2[d2[,4] == "Americas",5] = "yellow"

d[d[,4] == "EastAsia",5] = "green"
d2[d2[,4] == "EastAsia",5] = "green"

d[d[,4] == "CentralAsia",5] = "black"
d2[d2[,4] == "CentralAsia",5] = "black"

d[d[,4] == "Oceania",5] = "turquoise"
d2[d2[,4] == "Oceania",5] = "turquoise"

# let's play with ggplot and plyr
library(ggplot2)
library(plyr)

# get the data ready
fixdf <- function(df) {
  # sensible name
  names(df)[4] <- "Continent"
  # sort the continents by median
  cms <- ddply(df, .(Continent), summarise, med=median(Amp))
  cms <- cms[with(cms, order(med)),]
  df$Continent <- factor(df$Continent,
                         levels=cms$Continent,
                         ordered=TRUE)
  return(df)
}

d <- fixdf(d)
d2 <- fixdf(d2)

# our plotting functions
# we'll go with a pointrange plot faceted by continent
pointrangeplot <- function(df, title) {
  p <- ggplot(df, aes(x=reorder(Population, Amp), y=Amp, ymin=Amp-SE, ymax=Amp+SE, colour=Continent)) +
    geom_pointrange() +
    ylab("ALDER amplitiude") +
    theme_bw() +
    guides(colour=FALSE) +
    facet_grid(.~Continent, scales="free_x", space="free_x", labeller=labelfunc) +
    theme(axis.text.x=element_text(angle=90, hjust=1, vjust=0.5),
          plot.title = element_text(hjust=0, size=35)) +
    xlab("") +
    ggtitle(title)
  print(p)
  return(p)
}


# we need to fix the facet titles - some get cut off
labelfunc <- function(variable, value) {
  v <- sapply(value, function(x) {
    if (x == "Oceania") {
      return("Oce-\nania")
    } else if (x == "MiddleEast") {
      return("Middle\nEast")
    } else if (x == "EastAsia") {
      return("East Asia")
    } else {
      return(levels(x)[x])
    }
  })
  return(v)
}

# arrange the plots one above the other
library(gridExtra)
p1 <- pointrangeplot(d, "A")
p2 <- pointrangeplot(d2, "B")
grid.arrange(p1, p2, ncol=1, nrow=2)
