#Miguel Soto Tabajara 12221BCC002
#Gullit Damião Teixeira de Campos 12011BCC034

#Exercicio 1
diabetes<-read.table(file="diabetes.txt",sep=";",header=TRUE)
diabetes
str(diabetes)
diabetes$Diabetic<-as.factor(diabetes$Diabetic)
diabetes<-diabetes[,-1]
summary(diabetes)
set.seed(002034)

n<-round(nrow(diabetes))*0.8
diabetes[sample(nrow(diabetes)),]
treinamento<-diabetes[(1:n),]
teste<-diabetes[-(1:n),]

#a)
ggplot(data=treinamento,aes(x=Age,fill=Diabetic))+
    geom_histogram()+
    theme_minimal()
#Nos dados estudados, a proporção de pessoas com diabetes em idades mais baixas é menor, e a partir dos 35 anos, aproximadamnete, a proporção de pessoas com diabetes por idade aumenta.
ggplot(data=treinamento,aes(x=Pregnancies,fill=Diabetic))+
    geom_histogram()+
    theme_minimal()
#A partir de duas gravidez, a proporção de pessoas com diabetes aumenta a cada gestação aumenta consideravelmente.
ggplot(data=treinamento,aes(x=DiastolicBloodPressure,fill=Diabetic))+
    geom_histogram()+
    theme_minimal()
#O valor de pressão sanguinea tem um aumento consideravel na proporção de indívduos com diabetes quando os valores estão entre 60 e 70
ggplot(data=treinamento,aes(x=DiabetesPedigree,fill=Diabetic))+
    geom_histogram()+
    theme_minimal()
#Entre 1,3 e 1,7, aproximadamente, todos os índividuos apresentam diabetes
ggplot(data=treinamento,aes(x=Age,fill=Diabetic))+
    geom_histogram()+
    theme_minimal()

ggplot(data=treinamento,aes(x=Age,y=Pregnancies,col=Diabetic))+
    geom_point()
#A quantidade de pessoas diabéticas que tiveram mais de duas gravidez que tenham entrem 25 e 65 anos é extremamente alta

#Fatores como gravidez e BMI possuem uma pureza alta, sendo variáveis importantes para a previsão. Outros valores como SerumInsuline,idade e PlasmaGlucose se mostram importantes para fazer um modelo eficiente. Pessoas com mais de duas gravidez possuem maior incidencia de diabetes. Pessoas com menos de 22 BMI possuem menor incidencia de diabetes. A idade, PlasmaGlucose e SerumInsuline refinam em um nível mais baixo quem tem diabetes e quem não tem.



#b)
arvore<-rpart(data=treinamento,formula=Diabetic~.)
rpart.plot(arvore,extra=101)

funcao = function(pregnancie,age,BMI,serum,plasma){
    if(pregnancie<2){
        return(0)
    }
    else{
        if(BMI<22){
            return(0)
        }
        else if(serum<62){
            if(age<36){
                return(0)
            }else{
                return(1)
            }
        }else if(age<36){
            if(plasma<96){
                if(BMI>=33){
                    return(0)
                }else{
                    return(1)
                }
            }else if(age>=24){
                if(age<27){
                    return(0)
                }else{
                    return(1)
                }
            }else{
                return(1)
            }
        }else{
            return(1)
        }
    }
    return(0)
}
respostas<-c()
for(i in 1:nrow(teste)){
    respostas[i]<-funcao(teste[i,1],teste[i,8],teste[i,6],teste[i,5],teste[i,2])
}
mean(respostas==teste$Diabetic)
#A previsão deste modelo é de 90%

#c)
floresta<-randomForest(formula=Diabetic~.,data=treinamento,ntree=200, importance = TRUE)
previsao.floresta<-predict(floresta,newdata=teste,type="class")
plot(floresta) 
mean(previsao.floresta==teste$Diabetic)
#A acurácia do modelo é de 94%

#d) A probabilidade do modelo em árvore diagnosticar corretamenta o paciente é de 90%, enquanto a probabilidade do modelo em floresta aleatória tem uma probabilidade maior de 94%

#e)
#O modelo da floresta aleátoria utiliza os modelos de árvores de decisão, porém de uma maneira democrática e aleatória. Os dados são criteriamente refinados para realizar previsões que não dependam dos outros modelos de árvore construido. Como cada árvore tem sua autenticidade, o modelo une democraticamente as decisões tomadas em cada um e prevalece o com maior frequência. Logo, a floresta aleatória se mostra mais útil que o modelo em árvore, tendo acurácia de 94%, enquanto o modelo em árvore tem acurácia de 90%

#Exercicio 2
sementes<-read.table(file="semente.txt",sep=",",header=TRUE)
?gsub
sementes$V1<-as.numeric(sub("mm2","",sementes$V1))
str(sementes)

matriz<-dist(sementes)
modelo<-hclust(matriz,method="complete")
plot(modelo)
rect.hclust(modelo,k=5)
abline(h=5.5,col="blue")
aglomerados<-cutree(modelo,k=5)
#Altura de corte: 5.5. A altura do corte foi utilizada pois a altura dos clustes abaixo desse nível não dão saltos excessivos, preservando a proximidade entre os dados da raiz.



