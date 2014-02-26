

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

pdf("ju_bi.pdf")
par(mfrow = c(2,1), mar = c(2,4,4,2))

plot(d[,2], axes = F, xlab = "", ylab = expression(paste("ALDER amplitude (x", 10^-4, ")")), pch = 20, col = d[,5], ylim = c(0, 4e-4), cex = 0.7)
for (i in 1:nrow(d)){
	lines( c(i, i), c(d[i,2]-d[i,3], d[i,2]+d[i,3]), col = d[i,5])
}

axis(2, at = c(0, 2e-4, 4e-4), lab = c("0", "2", "4"))
mtext("A. Ju|'hoan_North", adj = 0)
arrows(50, 0, 20, 0, length = 0.1)
text(51, 0, lab = "low", adj = 0)
text(19, 0, lab = "high", adj = 1)


legend("topright", legend = c("Africa", "Europe", "Middle East", "Central Asia", "East Asia", "Oceania", "Americas"), col = c("red", "orange", "blue", "black", "green", "turquoise", "yellow"), pch = 20, bty = "n", cex = 0.7)

for (i in 1:3){
	text(1, 3e-4 - i*0.4e-4, lab = paste(i, ".", d[i,1]), col = d[i,5], adj = 0, cex = 0.6)
}


plot(d2[,2], axes = F, xlab = "", ylab =  expression(paste("ALDER amplitude (x", 10^-4, ")")), pch = 20, col = d2[,5], cex = 0.7, ylim = c(0, 0.00013))
for (i in 1:nrow(d2)){
        lines( c(i, i), c(d2[i,2]-d2[i,3], d2[i,2]+d2[i,3]), col = d2[i,5])
}
axis(2, at = c(0, 0.00006, 0.00012), lab = c("0", "0.6", "1.2"))
mtext("B. Biaka", adj = 0)

for (i in 1:3){
        text(1, 0.00008 - i*0.13e-4, lab = paste(i, ".", d2[i,1]), col = d2[i,5], adj = 0, cex = 0.6)
}
arrows(50, 0, 20, 0, length = 0.1)
text(51, 0, lab = "low", adj = 0)
text(19, 0, lab = "high", adj = 1)

dev.off()
