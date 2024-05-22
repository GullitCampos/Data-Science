#Miguel Soto Tabajara 12221BCC002
#Gullit Damião Teixeira de Campos 12011BCC034

#Exercício 1

dados<-read.csv(file="SBI.csv",sep=",",header=TRUE)
dados$sbi<-as.factor(dados$sbi)
str(dados)

#a)
infection<-c()
infection[which(dados$sbi=="NotApplicable")]<-"no"
infection[-(which(dados$sbi=="NotApplicable"))]<-"yes"
dados<-data.frame(dados,infection)
dados$infection<-as.factor(dados$infection)
dados$sex<-as.factor(dados$sex)
dados$prevAB<-as.factor(dados$prevAB)

#b)
dados<-dados[,-(c(1,2,8))]
str(dados)

#c)
dados<-dados[sample(nrow(dados)),]
n<-round(0.8*nrow(dados))
treino<-dados[1:n,]
teste<-dados[-(1:n),]

#d)
arvore<-rpart(data=treino,formula=infection~.)
rpart.plot(arvore,extra=101)
previsao<-predict(arvore,newdata = teste,type="class")
mean(previsao==teste$infection)
matriz.arvore<-table(previsao,teste$infection)
matriz.arvore

#e)
floresta<-randomForest(formula=infection~.,data=treino,ntree=200, importance = TRUE)
previsao.floresta<-predict(floresta,newdata=teste,type="class")
plot(floresta) #Não precisa de mais do que 100 árvore para estabilizar o valor da previsao
mean(previsao.floresta==teste$infection)
matriz.floresta<-table(previsao.floresta,teste$infection)
matriz.floresta

#Exercício 2
pinguim<-read.csv(file="pinguim.csv",sep=",",header=TRUE)
str(pinguim)
pinguim$flipper_length_mm<-as.numeric(pinguim$flipper_length_mm)
pinguim$body_mass_g<-as.numeric(pinguim$body_mass_g)
pinguim$species<-as.factor(pinguim$species)
str(pinguim)

#a)
ggplot(data=pinguim,aes(x=flipper_length_mm,y=body_mass_g,col=species))+
    geom_point()

#b)
cor(pinguim$flipper_length_mm,pinguim$body_mass_g) #A correlação entre as variáveis vale 0.8729789

#c)Sim, há uma relação entre as duas variáveis. Quanto mais próximo de 1 for o módulo da correlação, maior a relação linear entre os elementos. Temos que a correlação entre o tamanho da asa e o peso vale 0.82 e é positiva, demonstrando serem variáveis fortemente correlacionadas e há uma reta que pode prever o comportamento entre estes elementos. 

#d)
lm(pinguim$body_mass_g~pinguim$flipper_length_mm)
#body_mass_g = 50.15*flipper_length_mm - 5872.09

#e)O coeficiente angular represente a inclinação da reta a medida que ela cresce ou decresce. Neste caso, a cada um milimetro que a asa cresce, o peso cresce 50.15(coeficiente angular) vezes, menos 5872.09(valor que a reta intercepta o eixo do peso).

#f)
peso_medio = 50.15*204-5872.09
peso_medio #O peso médio será de 4358.51g

summary(pinguim)
#Não seria tão interessante utilizar este modelo para calcular o peso de um pinguim com 168mm de asa, já que o modelo se baseia em dados que variam de 172mm até 231mm de asa. Como um pinguim com este tamanho de asa não se enquadra no modelo, pode resultar em um valor equivocado. Entretanto, aplicando este valor no modelo temos:
peso = 50.15*168-5872.09
peso

#g)
torgersen<-pinguim[which(pinguim$island=="Torgersen"),]
biscoe<-pinguim[which(pinguim$island=="Biscoe"),]
dream<-pinguim[which(pinguim$island=="Dream"),]

#h)
biscoe.femea<-biscoe[which(biscoe$sex=="FEMALE"),]
matriz<-dist(biscoe.femea)
modelo<-hclust(matriz,method="ward.D2")
plot(modelo)
rect.hclust(modelo,k=3)
abline(h=2800,col="blue")

#i)Eu cortaria o aglomerado na altura 2800 pois relaciona os itens sem ter um salto de distância tão grande do próximo aglomerado e aplica a finalidade de tentar separar o aglomerado em espécies. Resultaram 3 aglomerados do corte.

aglomerados<-cutree(modelo,k=3)

mean(biscoe.femea[aglomerados==1,1]=="Adelie")
mean(biscoe.femea[aglomerados==1,1]=="Gentoo")
mean(biscoe.femea[aglomerados==1,1]=="Chainstrap")
#Temos 95.6% de Adelie, 4.4% de Gentoo e 0% de Chainstrap no primeiro aglomerado
mean(biscoe.femea[aglomerados==2,1]=="Adelie")
mean(biscoe.femea[aglomerados==2,1]=="Gentoo")
mean(biscoe.femea[aglomerados==2,1]=="Chainstrap")
#Temos 100% de Gentoo no segundo aglomerado
mean(biscoe.femea[aglomerados==3,1]=="Adelie")
mean(biscoe.femea[aglomerados==3,1]=="Gentoo")
mean(biscoe.femea[aglomerados==3,1]=="Chainstrap")
#Temos 100% de Gentoo no terceiro aglomerado

#Na ilha Biscoe, temos que aproximadamente 67% das pinguins femeas são da especie Gentoo e aproximadamente 33% das pinguins femeas são da espécie Adelie.

#j)
padronizados2<-scale(biscoe.femea[,-c(1,2,7)])
modelo_kmeans2<-kmeans(padronizados2,centers = 2)
modelo_kmeans2

padronizados3<-scale(biscoe.femea[,-c(1,2,7)])
modelo_kmeans3<-kmeans(padronizados3,centers = 3)
modelo_kmeans3

#O modelo kmeans para k=2 tem precisão de 81.2%, enquanto o modelo para k=3 tem 85.1% de precisão.

#k)
pinguim.padronizado<-scale(pinguim[,-c(1,2,7)])
modelo<-kmeans(pinguim.padronizado,centers=3)
modelo
pinguim.padronizado<-data.frame(pinguim.padronizado,cluster = factor(modelo$cluster))
str(pinguim.padronizado)
centro1<-c(modelo$centers[1,3],modelo$centers[1,4])
centro2<-c(modelo$centers[2,3],modelo$centers[2,4])
centro3<-c(modelo$centers[3,3],modelo$centers[3,4])

ggplot(data = pinguim.padronizado,aes(x=flipper_length_mm,y=body_mass_g,col=cluster))+
    geom_point()+
    geom_point(x=centro1[1],y=centro1[2],color="yellow",size=4)+
    geom_point(x=centro2[1],y=centro2[2],color="yellow",size=4)+
    geom_point(x=centro3[1],y=centro3[2],color="yellow",size=4)

#Exercício 3
olive<-read.table(file="olive.txt",sep=",",header=TRUE)
str(olive)

#a)
matriz<-dist(olive)
modelo<-hclust(matriz,method="ward.D2")
plot(modelo)
rect.hclust(modelo,k=5)
abline(h=33,col="blue")

#b)
aglomerados<-cutree(modelo,k=5)

mean(olive[aglomerados==1,1]=="Southern Italy")
mean(olive[aglomerados==1,1]=="Northern Italy")
mean(olive[aglomerados==1,1]=="Sardinia")
summary(olive[aglomerados==1,])
#No primeiro aglomerado temos 100% de azeites do Sul. Abaixo temos alguns valores importantes para os dados que classificam este aglomerado.
#Palmitic: Média 15.25
#Palmitoleic: Média 2.13
#Stearic: Média 2.25
#Oleic: Média 66.39
#Linoleic: Média 12.60
#Linolenic: Média 0.358
#Arachidic: Média 0.63
#Eicosenoic: Média 0.25

mean(olive[aglomerados==2,1]=="Southern Italy")
mean(olive[aglomerados==2,1]=="Northern Italy")
mean(olive[aglomerados==2,1]=="Sardinia")
summary(olive[aglomerados==2,])
#No segundo aglomerado temos aproximadamente 95% de azeites do Sul e 5% de azeites do Norte.Abaixo temos alguns valores importantes para os dados que classificam este aglomerado.
#Palmitic: Média 13.21
#Palmitoleic: Média 1.27
#Stearic: Média 2.53
#Oleic: Média 72.76
#Linoleic: Média 8.432
#Linolenic: Média 0.42
#Arachidic: Média 0.66
#Eicosenoic: Média 0.28

mean(olive[aglomerados==3,1]=="Southern Italy")
mean(olive[aglomerados==3,1]=="Northern Italy")
mean(olive[aglomerados==3,1]=="Sardinia")
summary(olive[aglomerados==3,])
#No terceiro aglomerado temos aproximadamente 22% de azeites do Sul e 78% de azeites do Norte.Abaixo temos alguns valores importantes para os dados que classificam este aglomerado.
#Palmitic: Média 10.78
#Palmitoleic: Média 0.80
#Stearic: Média 2.37
#Oleic: Média 77.98
#Linoleic: Média 6.80
#Linolenic: Média 0.26
#Arachidic: Média 0.44
#Eicosenoic: Média 0.09

mean(olive[aglomerados==4,1]=="Southern Italy")
mean(olive[aglomerados==4,1]=="Northern Italy")
mean(olive[aglomerados==4,1]=="Sardinia")
summary(olive[aglomerados==4,])
#No quarto aglomerado temos aproximadamente 82% de azeites do Sul e 18% de azeites de Sardínia.Abaixo temos alguns valores importantes para os dados que classificam este aglomerado.
#Palmitic: Média 13.39
#Palmitoleic: Média 1.51
#Stearic: Média 2.14
#Oleic: Média 69.91
#Linoleic: Média 11.87
#Linolenic: Média 0.32
#Arachidic: Média 0.61
#Eicosenoic: Média 0.20

mean(olive[aglomerados==5,1]=="Southern Italy")
mean(olive[aglomerados==5,1]=="Northern Italy")
mean(olive[aglomerados==5,1]=="Sardinia")
summary(olive[aglomerados==5,])
#No quinto aglomerado temos aproximadamente 8% dos azeites do Sul, 3% dos azeites do Norte e 89% dos azeites de Sardínia.Abaixo temos alguns valores importantes para os dados que classificam este aglomerado.
#Palmitic: Média 11.08
#Palmitoleic: Média 0.97
#Stearic: Média 2.18
#Oleic: Média 73.58
#Linoleic: Média 11.13
#Linolenic: Média 0.28
#Arachidic: Média 0.70
#Eicosenoic: Média 0.03

#c)
modelo<-kmeans(olive[,-1],centers=5)
modelo

#O modelo tem precisão de 86.7% de acertos.

#Exercício 4
acerto<-c()
n<-100000
for(i in 1:n){
    urna1<-c("preto","preto","preto","preto","preto","preto","branca","branca","branca","vermelha","vermelha","vermelha","vermelha")
    urna2<-c("preto","preto","preto","branca","branca","branca","branca","branca","vermelha","vermelha")
    urna3<-c("preto","preto","preto","preto","branca","branca","vermelha","vermelha")
    urna1<-sample(urna1)
    urna2<-sample(urna2)
    urna3<-sample(urna3)
    dado<-sample(1:6,size=1)
    if(dado==5){
        if(urna1[1]=="vermelha") acerto[i]<-1
        else acerto[i]<-0
    }else if(dado==1||dado==4||dado==6){
        if(urna2[1]=="vermelha") acerto[i]<-1
        else acerto[i]<-0
    }else{
        if(urna3[1]=="vermelha") acerto[i]<-1
        else acerto[i]<-0
    }
}
mean(acerto)
#A probabilidade de uma bola vermelha sair é de aproximadamente 23.5%

#Exercício 5
estatistica<-c()
n<-10000
for(i in 1:n){
    registro<-c()
    cont<-1
    while(length(unique(registro))<30){
        bilhete<-sample(1:30,size=1)
        registro[cont]<-bilhete
        cont<-cont+1
    }
    estatistica[i]<-cont
}
mean(estatistica)
#O valor de E[N] é de aproximadamente 120

#Exercício 6
#a)
n<-10000
estatistica<-c()
for(i in 1:n){
    soma<-0
    cont<-1
    while(soma<12){
        dado<-sample(1:6,size=1)
        if(dado==4) soma<-soma+4
        cont<-cont+1
    }
    estatistica[i]<-cont
}
mean(estatistica)
#A esperança de X é aproximadamente 19

#b)
n<-10000
resultado<-c()
for(i in 1:n){
    soma<-0
    cont<-1
    while(soma<12){
        dado<-sample(1:6,size=1)
        if(dado==4) soma<-soma+dado
        cont<-cont+1
    }
    if(cont<10)resultado[i]<-1
    else resultado[i]<-0
}
mean(resultado)
#A probabilidade de X ser menor que 10 é de aproximadamente 13%



    




