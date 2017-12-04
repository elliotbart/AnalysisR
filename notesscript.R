setwd("C:\\Users\\Elliot Okkis\\Documents\\UTC P17\\SY09\\TP01")

notes <- read.csv("sy02-p2016.csv", na.strings="", header=T)
notes$nom <- factor(notes$nom, levels=notes$nom)
notes$correcteur.median <- factor(notes$correcteur.median,
                                    levels=c("Cor1","Cor2","Cor3","Cor4","Cor5","Cor6","Cor7","Cor8"))
notes$correcteur.final <- factor(notes$correcteur.final,
                                   levels=c("Cor1","Cor2","Cor3","Cor4","Cor5","Cor6","Cor7","Cor8"))
notes$niveau <- factor(notes$niveau, ordered=T)
notes$resultat <- factor(notes$resultat, levels=c("F","Fx","E","D","C","B","A"),
                           ordered=T)

notes<-notes[-which(is.na(notes$note.totale)),]

corcontingence=table(notes$correcteur.median,notes$note.median)
corcontingence=corcontingence[-3,]
mergedTable=NULL
for(i in 1:7){mergedTable = rbind(mergedTable, c(sum(corcontingence[i, 1:13]), sum(corcontingence[i, 14:18]), sum(corcontingence[i, 19:24]), sum(corcontingence[i, 25:37])))}

corcontingence1=table(notes$correcteur.final, notes$note.final)
corcontingence1=corcontingence1[-2,]
mergedTable1=NULL
for(i in 1:7){mergedTable1 = rbind(mergedTable1, c(sum(corcontingence1[i, 1:16]), sum(corcontingence1[i, 17:23]), sum(corcontingence1[i, 24:29]), sum(corcontingence1[i, 30:36])))}

#corcontingence=table(notes$correcteur.median,notes$note.median)
#corcontingence=corcontingence[-3,]
#mergedTable=NULL
#for(i in 1:7){mergedTable = rbind(mergedTable, c(sum(corcontingence[i, 1:7]), sum(corcontingence[i, 8:17]), sum(corcontingence[i, 18:27]), sum(corcontingence[i, 19:37])))}

boxplot(notes$note.median ~ notes$correcteur.median)
boxplot(notes$note.final ~ notes$correcteur.final)

for (i in 1:8) {print(mean(notes[notes$correcteur.median==paste("Cor",i, sep = ""),]$note.median))}
vector=NULL
for (i in 1:8) {vector=cbind(vector,mean(notes[notes$correcteur.median==paste("Cor",i, sep = ""),]$note.median))}
vector=NULL
for (i in 1:4) {vector=cbind(vector,mean(mergedTable[,i]))}


for (i in 1:8) {print(mean(notes[notes$correcteur.final==paste("Cor",i, sep = ""),]$note.final))}
vector1=NULL
for (i in 1:8) {vector1=cbind(vector1,mean(notes[notes$correcteur.final==paste("Cor",i, sep = ""),]$note.final))}
vector1=NULL
for (i in 1:4) {vector1=cbind(vector1,mean(mergedTable1[,i]))}


corcontingence2=table(notes$specialite, notes$note.totale)
mergedTable2=NULL
for(i in 1:9){mergedTable2 = rbind(mergedTable2, c(sum(corcontingence2[i, 1:28]), sum(corcontingence2[i, 29:58]), sum(corcontingence2[i, 59:88]), sum(corcontingence2[i, 89:117])))}
vector2=NULL
for (i in 1:4) {vector2=cbind(vector2,mean(mergedTable2[,i]))}

boxplot(notes$note.totale ~ notes$specialite)
boxplot(notes$resultat ~ notes$specialite)

noteswithoutTC=notes[-which(notes$specialite=="TC"),]
boxplot(noteswithoutTC$note.totale ~ noteswithoutTC$niveau)


corcontingence3=table(noteswithoutTC$niveau, noteswithoutTC$note.totale)
mergedTable3=NULL
for(i in 1:6){mergedTable3 = rbind(mergedTable3, c(sum(corcontingence3[i, 1:40]), sum(corcontingence3[i, 41:63]), sum(corcontingence3[i, 64:86]), sum(corcontingence3[i, 87:115])))}
vector3=NULL
for (i in 1:4) {vector3=cbind(vector3,sum(mergedTable3[,i]))}

boxplot(notes$note.totale ~ notes$statut)
boxplot(notes$note.totale ~ notes$dernier.diplome.obtenu)

cor(x=notes$note.final, y=as.numeric(notes$resultat), use = 'complete.obs')
cor(x=notes$note.final, y=notes$note.totale, use = 'complete.obs')

cor(x=notes$note.median, y=as.numeric(notes$resultat), use = 'complete.obs')
cor(x=notes$note.median, y=notes$note.totale, use = 'complete.obs')
cor(x=notes$note.median, y=notes$note.final, use = 'complete.obs')


