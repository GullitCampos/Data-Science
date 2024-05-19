                                    #Lista 2
#Miguel Soto Tabajara 12221BCC002
#Gullit Damião T Campos 12011BCC034

#Exercício 1. Considere o seguinte jogo: Steven e Garnit escolherão, cada um, uma sequência de tamanho 3 em que cada entrada da sequência é cara ou coroa; logo em seguida, uma moeda será lançada três vezes; se aparecer a sequência de um dos jogadores, este jogador vence e o jogo acaba; caso não apareça a sequência de nenhum deles, a moeda é lançada pela quarta vez e os três últimos lançamentos são analisados; se nestes três últimos lançamentos aparecer a sequência de um dos jogadores, este jogador vence e o jogo acaba. Se isto não acontecer, a moeda é lançada pela quinta vez e os três últimos resultados são analisados; se aparecer a sequência de um dos jogadores, este jogador vence e o jogo acaba. Este processo é realizado até que apareça a sequência que um dos dois escolheu; se aparecer primeiro a sequência de Steven, ele ganha; se aparecer primeiro a sequência de Garnit, ela vence. Convencione que cara seja 1 e que coroa seja zero. Supondo que Steven escolheu a sequência (0, 1, 0) e que Garnit escolheu a sequência (0, 0, 1), simule uma partida deste jogo. A simulação deve retornar steven caso Steven tenha vencido ou deve retornar garnit caso contrário. Replique o experimento 10 mil vezes e calcule a média de vitórias de Garnit. Comente o resultado obtido.

garnit<-c(0,0,1)
steven<-c(0,1,0)
vitoriag<-0
vitorias<-0
for(j in 1:10000){
    cont<-1
    dados<-sample(x=0:1,size=3,replace=TRUE)
    if(sum(garnit==dados)==3) vitoriag<-vitoriag+1 else 
        if(sum(steven==dados)==3) vitorias<-vitorias+1 else 
            while (cont!=0) {
                x<-sample(x=0:1,size=1)
                dados<-append(dados,x)
                dados<-dados[-1]
                if(sum(garnit==dados)==3){
                    vitoriag<-vitoriag+1
                    cont<-0
               }
                if(sum(steven==dados)==3){
                    vitorias<-vitorias+1
                    cont<-0
                }
            }
}
mediaGarnit<-vitoriag/10000
mediaGarnit


#Exercício 2. Harold Frederick Shipman (Nottingham, 14 de janeiro de 1946 — Wakefield, 13 de janeiro de 2004), conhecido como “Doutor Morte”, foi um médico e assassino em série britânico condenado pela morte de muitos pacientes entre as décadas de 1970 e 1990. Dr. Shipman é, talvez, o assassino em série mais prolífico da História Moderna. O arquivo dados.txt contém informações sobre o sexo, a idade, o local da morte (casa do paciente; hospital; casa de repouso) e o ano da morte das vítimas de Shipman. Antes de responder as questões abaixo, abra o arquivo dados.txt e compreenda sua estrutura. Importe o arquivo para o R e utilize-o para responder os seguintes itens.
#(a) Escolha um gráfico apropriado para representar as frequências das categorias da variável sexo. Comente os resultados encontrados.
#(b) Apresente o histograma da variável idade em 8 (argumento bins na geometria do histograma) intervalos. Comente os resultados obtidos. Analise este gráfico para cada gênero.
#(c) Apresente o boxplot da variável idade. Comente os resultados obtidos.
#(d) Apresente um gráfico para representar o local da morte. Comente os resultados obtidos.
#(e) Analise graficamente o ano da morte das vítimas de Harold Shipman.
#(f) Com base nas informações obtidas nos itens anteriores, escreva um parágrafo sobre o padrão e o perfil das vítimas de Harold Shipman.

#Exercicio a)
dados<-read.table(file="dados.txt",header=TRUE,sep=";")
dados
summary(dados)
dados$Genero<-as.factor(dados$Genero)
dados$LocalDaMorte<-as.factor(dados$LocalDaMorte)
ggplot(data=dados,mapping=aes(Genero))+
    geom_bar()+
    labs(y="Frequência",x="Gênero")+
#Aproximadamente 17% das vítimas do assasino eram homens, enquanto 83% das suas vitimas eram mulheres.
    
#Exercicio b)
ggplot(data = dados,aes(x=Idade,fill=Genero))+
    geom_histogram(binwidth=8)
#As vitimas do assassino são majoritariamente idodos,na maior parte mulheres, entre 70 e 90 anos de idades. Neste intervalo, a quantidade de assassinatos envolvendo homens aumenta em relação as idades inferiores.

#Exercicio c)
ggplot(data=dados,aes(y=Idade))+
    geom_boxplot()
#Aproximadamente 90% das vitimas do assassino tem mais de 60 anos. Pelo menos 75% das vitimas do assassino tinham mais do que 70 anos. 

#Exercicio d)
ggplot(data=dados,aes(x=LocalDaMorte))+
    geom_bar()+
    labs(x="Local",y="Frequência")
#Aproximadamente 94% das vimitas foram assassinadas em sua propria casa. As outras 6% foram assassinadas em lares de idosos ou hospitais.

#Exercicio e)
ggplot(data=dados,aes(x=AnoDaMorte))+
    geom_bar()+
    labs(x="Ano",y="Frequência")
#O assassinatos começaram a ter frequência entre 1983 e 1991. Entre 1993 e 1998 foram os anos com maiores crescimentos nos números de vitimas.

#Exercicio f) As vitimas de Harold Shipman eram majoritariamente idosas entre 60 e 90 anos de idade. Os assassinatos acontecerem quase em seu total nas próprias casas das vitimas, porém ainda tiveram poucos casos em hospitais e lares de idosos. Shipman começou a fazer vitimas em 1975, tendo picos de frequência entre 1983 e 1991 e atingindo seu ápice de números entre 1993 e 1998.


#Exercício 3. Os arquivos treino_baleias.txt e teste_baleias.txt contém informações sobre as características de algumas espécies de baleias. Os conjuntos de dados possuem, ao todo, 248 observações (198 para treino, 50 para teste). As variáveis incluídas nestes conjuntos de dados são:

#• especie: indica a espécie da baleia e é uma variável categórica;
#• comprimento: indica o comprimento da baleia em metros e é uma variável numérica contínua;
#• peso: indica o peso da baleia em quilos e é uma variável numérica contínua;
#• profundidade_maxima: indica a profundidade máxima mergulhada pela baleia em metros e é uma variável numérica contínua;
#• volume_cranio: indica o volume do crânio da baleia em centímetros cúbicos e é uma variável numérica contínua.

#(a) Crie um conjunto para cada espécie de baleia; cada data frame criado deverá conter apenas baleias de uma espécie.
#(b) Calcule a média, a variância, o desvio padrão e o coeficiente de variação para a variável peso para cada espécie de baleia. Comente os resultados obtidos.
#(c) Apresente o histograma da variável peso para a espécie de baleia azul. Comente os resultados obtidos.
#(d) Apresente numa mesma janela os boxplots para cada espécie para a variável comprimento. Comente os resultados obtidos.
#(e) Apresente um gráfico de dispersão de comprimento versus profundidade_maxima. Cada espécie deve ser registrada por uma cor diferente.
#(f) Com base em todas as informações anteriores, construa um modelo de árvore de decisão a partir de estruturas condicionais e de repetição para prever a espécie de uma baleia com base nas variáveis numéricas do estudo. Justifique as escolhas das variáveis e dos pontos de corte escolhidos. Por fim, utilize o conjunto do arquivo teste_baleias.txt para calcular a taxa de acerto. Comente o resultado obtido.
#(g) Utilize gráficos de dispersão para registrar por linhas horizontais e verticais os pontos de cortes escolhidos em sua árvore de decisão. As espécies de baleias devem ser registradas por diferentes cores.
#(h) Crie um modelo de classificação KNN para classificar as baleias. Utilize 𝐾 = 1 e depois 𝐾 = 3.Compare os resultados dos dois modelos KNN.

treino<-read.table(file="treino_baleias.txt",header=TRUE,sep=",")
teste<-read.table(file="teste_baleias.txt",header=TRUE,sep=",")
treino$especie<-as.factor(treino$especie)
teste$especie<-as.factor(teste$especie)
summary(treino)
summary(teste)

#Exercicio a)
azul<-treino[which(treino$especie=="Baleia Azul"),]
fin<-treino[which(treino$especie=="Baleia Fin"),]
cachalote<-treino[which(treino$especie=="Cachalote"),]
jubarte<-treino[which(treino$especie=="Jubarte"),]

#Exercicio b)
mediaPesoAzul<-sum(azul$peso)/nrow(azul)
mediaPesoFin<-sum(fin$peso)/nrow(fin)
mediaPesoCachalote<-sum(cachalote$peso)/nrow(cachalote)
mediaPesoJubarte<-sum(jubarte$peso)/nrow(jubarte)

varianciaAzul<-sum((azul$peso-mediaPesoAzul)**2)/nrow(azul)
desvioAzul<-sqrt(varianciaAzul)
coefAzul<-(desvioAzul/mediaPesoAzul)*100
#A média de peso dos individuos da espécie azul é de 20284kg, com desvio padrão de +-1450kg. Seu coeficiente de dispersão foi de 7,15%, provando que os dados de peso desta espécie são homogêneos. Esta é a espécie com maior peso entre as estudadas.

varianciaFin<-sum((fin$peso-mediaPesoFin)**2)/nrow(fin)
desvioFin<-sqrt(varianciaFin)
coefFin<-(desvioFin/mediaPesoFin)*100
#A média de peso dos individuos da espécie fin é de 10901kg, com desvio padrão de +-1002. Seu coeficiente de dispersão foi de 9.19%, provando que os dados de peso desta espécie são homogêneos.

varianciaCachalote<-sum((cachalote$peso-mediaPesoCachalote)**2)/nrow(cachalote)
desvioCachalote<-sqrt(varianciaCachalote)
coefCachalote<-(desvioCachalote/mediaPesoCachalote)*100
#A média de peso dos individuos da espécie cachalote é de 5856, com desvio padrão de +-832kg. Seu coeficiente de dispersão foi de 14.2%, provando que os dados de peso desta espécie são homogêneos, porém quase atingindo média dispersão. Esta é a espécie com maior variação de peso entre as estudadas.

varianciaJubarte<-sum((jubarte$peso-mediaPesoJubarte)**2)/nrow(jubarte)
desvioJubarte<-sqrt(varianciaJubarte)
coefJubarte<-(desvioJubarte/mediaPesoJubarte)*100
#A média de peso dos individuos da espécie jubarte é de 4089kg, com desvio padrão de +-492kg. Seu coeficiente de dispersão foi de 12%, provando que os dados de peso desta espécie são homogêneos. Esta é a espécie com menor peso entre as estudadas.

#Exercicio c)
ggplot(data=azul,aes(x=peso))+
    geom_histogram(binwidth=500)
#A maior parte dos individuos desta especie tem seu peso em torno de 20000kg.

#Exercicio d)
ggplot(data=treino,aes(x=especie,y=comprimento))+
    geom_boxplot()
#Baseado nos gráficos, temos que aproximadamente 75% dos individuos da espécie azul tem o comprimento maior do que todos os individuos das outras espécies.Aproximadamente 75% dos individuos da especie Fin tem o comprimento maior que todos os individuos das especies Cachalote e Jubarte. 

#Exercicio e)
ggplot(data=treino,aes(x=comprimento,y=profundidade_maxima,color=especie))+
    geom_point(size=1.5)

#Exercicio f)
final<-c()
for(j in 1:50){
    if(teste$comprimento[j]<23&&teste$profundidade_maxima[j]<175) final[j]<-"Cachalote" else
    if(teste$comprimento[j]<23&&teste$profundidade_maxima[j]>175&&teste$profundidade_maxima[j]<210) final[j]<-"Jubarte" else
    if(teste$comprimento[j]>=22&&teste$comprimento[j]<27&&teste$profundidade_maxima[j]>210&&teste$profundidade_maxima[j]<280) final[j]<-"Baleia Fin" else
    if(teste$comprimento[j]>=24&&teste$profundidade_maxima[j]>=250&&teste$profundidade[j]<=350) final[j]<-"Baleia Azul" else
    final[j]<-"NA"
}
mean(teste$especie==final)
#As variáveis utilizadas foram: comprimento e profundidade máxima. Formando retas que dividem o gráfico comprimento x profundidade_máxima, é possível distinguir as espécies.Utilizando estas informações foi possível criar uma arvóre de decisão que previu corretamente 82% dos dados.

#Exercicio g)
ggplot(data=treino,aes(x=comprimento,y=profundidade_maxima,color=especie))+
    geom_point(size=1.5)+
    geom_hline(yintercept=c(210,175,280))+
    geom_vline(xintercept=c(22,27))

#Exercicio h)
respostas<-c() #Para um vizinho mais próximo
for(j in 1:nrow(teste)){
    distancia<-c()
    for(k in 1:nrow(treino)){
        distancia[k]<-sqrt(sum((teste[j,c(2,3)]-treino[k,c(2,3)])**2))
    }
    respostas[j]<-as.character(treino$especie[order(distancia)[1]]) 
}
KNN1<-mean(respostas==teste$especie)
KNN1 #Precisão de 96%

respostas<-c() #Para os 3 vizinhos mais proximos
for(j in 1:nrow(teste)){
    distancia<-c()
    for(k in 1:nrow(treino)){
        distancia[k]<-sqrt(sum((teste[j,c(2,3)]-treino[k,c(2,3)])**2))
    }
    x<-as.character(treino$especie[order(distancia)[c(1,2,3)]])
    respostas[j]<-x[duplicated(x)]
}
KNN3<-mean(respostas==teste$especie)
KNN3 #Precisão de 96%


#Exercício 4. O conjunto cogumelos.csv contém informações sobre 23 espécies de cogumelos dos gêneros Agaricus e Lepiota, retiradas do Guia de Campo da Sociedade Audubon para Cogumelos da América do Norte (1981). Cada espécie é classificada (class) como comestível (edible = e) ou venenosa (poisonous = p). Detalhes sobre cada uma das variáveis do conjunto estão no Kaggle ou em UC Irvine Machine Learning Repository.
#Embaralhe o conjunto e, em seguida, separe-o em treinamento (80%) e teste (20%). Estude o conjunto de treinamento a partir de uma análise gráfica (nesta parte faça algumas perguntas interessantes e encontre um gráfico que ajudará na sua resposta; exemplos: quantas espécies venenosas há no treinamento? e comestíveis?;a forma, a cor ou o odor pode influenciar na classificação? etc). A partir das conclusões e observações obtidas, crie um modelo de árvore de decisão para classificar um cogumelo como comestível ou venenoso. Avalie a taxa de acerto e comente o resultado obtido.

cogus<-read.csv(file="cogumelos.csv",header=TRUE,sep=",")

amostra<-sample(x=1:8124,size=8124,replace=FALSE)
n<-round(nrow(cogus)*0.8)
treinamento<-cogus[1:n,]
teste<-cogus[(n+1):nrow(cogus),]

treinamento$class<-as.factor(treinamento$class)
summary(treinamento)

p<-which(treinamento[,1]=="p")
odorp<-treinamento[p,]
e<-which(treinamento[,1]=="e")
odore<-treinamento[e,]

treinamento$odor<-as.factor(treinamento$odor)
summary(treinamento)
odore$odor<-as.factor(odore$odor)
summary(odore)
odorp$odor<-as.factor(odorp$odor)
summary(odorp)

#Odor para plantas venenosas: f,p,y,s,c,n
#Odor para plantas não venenosas: n,a,l

resposta<-c()
for(j in 1:nrow(teste)){
    if(teste$odor[j]=="n"||teste$odor[j]=="a"||teste$odor[j]=="l") resposta[j]<-"e" else resposta[j]<-"p"
}
acerto<-mean(resposta==teste$class)
acerto

#As plantas venenosas possuem majoritariamente odor do tipo "f", ainda tendo tipos como "p","y","s","c" e "n" em menores quantidades. As plantas não venenosas possuem odor apenas dos tipos "n","a" e "l". Embora ambas possuam odor tipo "n", as não venenosas tem pouquissimas desse gênero. A arvore de decisão foi baseada no odor, tendo uma precisão de 99% de acertos.




