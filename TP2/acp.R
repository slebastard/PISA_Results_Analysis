rm(list = ls())
graphics.off()
Voitures <- read.table("emissions.csv", sep = ";", row.names = 1, header = TRUE)
Voitures <- na.omit(Voitures)

quant = Voitures[,1:9]
qual = Voitures[,10:11]
print(Voitures)

pairs(quant)

acp = princomp(quant, cor = T, scores = T)
summary(acp)

valp = acp $ sdev^2
dev.new()
plot(valp, type = "b")

scores = acp $ scores
print(scores)

dev.new()
plot(scores[, 1], scores[, 2], type = "n")
text(scores[,1], scores[,2], labels = row.names(quant), cex = 0.5)

loadings = acp $ loadings
print(loadings)

corr1 = loadings[, 1]*sqrt(valp[1])
corr2 = loadings[, 2]*sqrt(valp[2])
dev.new()
plot(corr1, corr2, xlim = c(-1, 1), ylim = c(-1, 1), asp = 1, type = "n")
text(corr1, corr2, labels = colnames(quant), cex = 0.5)
symbols(0, 0, circles = 1, inches = F, add = T)

#Retracer les voitures dans le plan u1, u2 en mettant les noms des modèles d'une couleur qui caractérise leur carburant.
dev.new()
plot(scores[, 1], scores[, 2], type = "n")
text(scores[,1], scores[,2], labels = row.names(quant), cex = 0.5, col = ifelse(Voitures[, 11] == "EH", "green", ifelse(Voitures[, 11] == "ES", "black", "blue")))

#Placer le symbole LUXE au barycentre des positions des véhicules de type luxe.
posx = 0
posy = 0
num = 0

for (i in 1:length(Voitures[, 10])) {
	if (Voitures[i, 10] == "LUXE") {
		num = num + 1
		posx = posx + scores[i, 1]
		posy = posy + scores[i, 2]
	}
}

text(posx/num, posy/num, labels = "LUXE", cex = 1)
