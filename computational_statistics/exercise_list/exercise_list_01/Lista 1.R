#Miguel Soto Tabajara 12221BCC002
#Gullit Damião T Campos 12011BCC034

#Exercício 1
#a)
v1<-seq(from=10,to=30,by=1)
v1
#b)
v2<-seq(from=30,to=10,by=-1)
v2
#c)
v3<-c(seq(from=10,to=30,by=1),seq(from=29,to=10,by=-1))
v3

#Exercício 2
#a)
v4<-rep(seq(from=2,to=8,by=2),10)
v4
#b)
v5<-rep(seq(from=2,to=8,by=2),11,length.out=41)
v5
#c)
v6<-c(3,7,1)
v7<-rep(v6,3)
v7
v8<-rep(v6,c(4,2,3))
v8

#Exercício 3
#a)
n<-20
soma1<-0
while(n<=30){
  soma1<-soma1+(n*n)+(4*n)
  n<-n+1
}
soma1
#b)
n<-10
soma2<-0
while(n<=20){
  soma2<-soma2+((3**n)/n)+((2**n)/(n**2))
  n<-n+1
}
soma2

#Exercício 4
v9<-sample(x=1:100,size=40,replace=TRUE)
v9
#a)
sum(v9%%2==0)
#b)
sum(v9>70)
#c)
which(v9%%2!=0)

#Exercício 5
cont<-0
cont2<-0
while(cont!=2){
  dado<-sample(x=1:6,size=1)
  if(dado==4) cont<-cont+1
  cont2<-cont2+1
}
cont2

#Exercício 6
vetor<-c()
for(j in 1:10000){
  cont<-0
  cont2<-0
  while(cont!=2){
    dado<-sample(x=1:6,size=1)
    if(dado==4) cont<-cont+1
    cont2<-cont2+1
  }
  vetor[j]<-cont2
}
vetor

#Exercício 7
n<-20
cont<-3
fibo<-c(1,1)
while(length(fibo)!=n){
  fibo[cont]<-fibo[cont-1]+fibo[cont-2]
  cont<-cont+1
}
fibo

#Exercício 8
erro<-0
acerto<-0
for(i in 1:100000){
  nomes<-c('Dwight','Jim','Kevin','Creed','Michael')
  sorteio<-sample(nomes)
  for(j in 1:5){
    if(nomes[j]==sorteio[j]){
        cont<-0
        break
    }else{
      cont<-1
    }
  }
  if(cont==1) acerto<-acerto+1
  if(cont==0) erro<-erro+1
}
erro/100000

#Exercício 9
ganhos<-c()
for(j in 1:100000){
    soma<-sample(x=1:6,size=1,replace=TRUE)+sample(x=1:6,size=1,replace=TRUE)
    aux<-0
    if((soma==7)||(soma==11)){
        ganhos<-c(ganhos,1)
        aux<-1
    }
    if((soma==2)||(soma==3)||(soma==12)){
        ganhos<-c(ganhos,0)
        aux<-1
    }
    while(aux==0){
        soma1<-sample(x=1:6,size=1,replace=TRUE)+sample(x=1:6,size=1,replace=TRUE)
        if(soma1==7){
            ganhos<-c(ganhos,0)
            aux<-1
        }
        if(soma1==soma){
            ganhos<-c(ganhos,1)
            aux<-1
        }
    }
}
proporção<-sum(ganhos)/100000
proporção

#Exercício 10
#a)
destino<-c()
luke = function(l){
    n<-20
    while(l!=n&&l!=0){
        moeda<-sample(0:1,size=1) #utilizando 1 para cara e 0 para coroa
        if(moeda==1) l<-l+1
        else l<-l-1
    }
    if(l==n) return(1)
    if(l==0) return(0)
}
destino<-c(destino,luke(10))
destino

#b)
destino<-c()
luke1 = function(l){
    for(j in 1:10000){
        n<-20
        aux=l
        while(aux!=n&&aux!=0){
            moeda<-sample(0:1,size=1) #utilizando 1 para cara e 0 para coroa
            if(moeda==1) aux<-aux+1
            else aux<-aux-1
        }
        if(aux==n) destino<-c(destino,1)
        if(aux==0) destino<-c(destino,0)
    }
    return(sum(destino)/10000)
}
proporcao<-luke1(10)
proporcao

#c)
estatisticas<-c()
for(j in 1:19){
    estatisticas<-c(estatisticas,luke1(j))
}
plot(1:19,estatisticas)

