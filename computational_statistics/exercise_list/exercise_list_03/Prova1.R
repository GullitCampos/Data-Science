#Miguel Soto Tabajara 12221BCC002
#Gullit Damião T Campos 12011BCC034

#Exercicio 1
#a)
coordenadas<-c(0,0)
passos<-8
for(j in 1:passos){
    dado<-sample(x=c("R","U","L","D"),size =1)
    if(dado=="R") coordenadas[1]<-coordenadas[1]+1
    if(dado=="U") coordenadas[2]<-coordenadas[2]+1
    if(dado=="L") coordenadas[1]<-coordenadas[1]-1
    if(dado=="D") coordenadas[2]<-coordenadas[2]-1
}
coordenadas
#b)
respostas<-c() #Irá receber 1 se ele parou na posição inicial e 0 caso contrário
for(i in 1:10000){
    coordenadas<-c(0,0)
    passos<-8
    for(j in 1:passos){
        dado<-sample(x=c("R","U","L","D"),size =1)
        if(dado=="R") coordenadas[1]<-coordenadas[1]+1
        if(dado=="U") coordenadas[2]<-coordenadas[2]+1
        if(dado=="L") coordenadas[1]<-coordenadas[1]-1
        if(dado=="D") coordenadas[2]<-coordenadas[2]-1
    }
    if(coordenadas[1]==0&&coordenadas[2]==0) respostas[i] = 1
    else respostas[i] = 0
}
mean(respostas)
#A proporção está descrita na média das respostas da função acima. Isto significa que a chance de o Link retornar ao ínicio depois de 8 rodadas é de 7,5%

#c)
func = function(n){
    if(n%%2!=0) return (c("impossível retornar a origem depois de um número impar de passes"))
    else{
        respostas<-c() 
        for(i in 1:10000){
            coordenadas<-c(0,0)
            for(j in 1:n){
                dado<-sample(x=c("R","U","L","D"),size =1)
                if(dado=="R") coordenadas[1]<-coordenadas[1]+1
                if(dado=="U") coordenadas[2]<-coordenadas[2]+1
                if(dado=="L") coordenadas[1]<-coordenadas[1]-1
                if(dado=="D") coordenadas[2]<-coordenadas[2]-1
            }
            if(coordenadas[1]==0&&coordenadas[2]==0) respostas[i] = 1
            else respostas[i] = 0
        }
        return (mean(respostas))
    }
}
resposta<-func(10)

#Exercicio 2

#a)
primatas<-read.table(file = "primatas.txt",sep=":",header=TRUE)
summary(primatas)
primatas$especie<-as.factor(primatas$especie)
primatas$genero<-as.factor(primatas$genero)
str(primatas)

#b)
barplot(table(primatas$especie))

ggplot(data=primatas,mapping=aes(x=especie,fill = genero))+
    geom_bar()

#c)
posicoes<-which(primatas$especie=="bonobo")
bonobos<-primatas[posicoes,]
ggplot(data=bonobos,aes(x=altura,y = peso,color=genero))+
    geom_point(size = 1.5)

posicoes<-which(primatas$especie=="chimpanze")
chimpanze<-primatas[posicoes,]
ggplot(data=chimpanze,aes(x=altura,y = peso,color=genero))+
    geom_point(size = 1.5)

#d)

femea<-primatas[-(which(primatas$genero=="macho")),]
ggplot(data=femea,aes(x=altura,y = peso,color=especie))+
    geom_point(size = 1.5)

macho<-primatas[-(which(primatas$genero=="femea")),]
ggplot(data=macho,aes(x=altura,y = peso,color=especie))+
    geom_point(size = 1.5)


#e)Bonobos macho possuem em sua maioria menos de 133 de altura e 53 de peso. Enquanto chimpanzes macho possuem mais de 55kg de peso e em sua maioria mais de 133 de altura. Os bonobos femeas possuem em media menos de 37kg e altura em sua maioria maior que 125. Os chimpanzes femea possuem seu peso maior que 37kg e altura menor que 126 em sua maioria. Ambas as especies contém uma quantidade muito proxima de individuos femeas e macho.


#f)
resposta<-c()
for(j in 1:nrow(primatas)){
    if(primatas$genero[j]=="macho"&&primatas$altura[j]<=133&&primatas$peso[j]<52) resposta[j] = "bonobo"
    else if(primatas$genero[j]=="macho"&&primatas$altura[j]>=133&&primatas$peso[j]>52) resposta[j]= "chimpanze"
    else if(primatas$genero[j]=="femea"&&primatas$altura[j]>=125&&primatas$peso[j]<37) resposta[j] = "bonobo"
    else if(primatas$genero[j]=="femea"&&primatas$altura[j]<125&&primatas$peso[j]>37) resposta[j] = "bonobo"
    else resposta[j] = "NA"
}
mean(primatas$especie==resposta)

#O modelo é parcialmente eficiente tendo acuracia de 71%.




