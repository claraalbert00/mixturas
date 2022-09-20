 ##Exercici 1 i 2

#DISTRIBUCIO SOTA LA HIPOTESI NULA (NORMAL)
## QUINA NORMAL APROXIMA A LA MIXTURA?

# - simulacio mixtures
d=seq(1.5,3,by=0.25)
mu1=-d/2
mu2=d/2
sg1=sg2=1
p=0.5
n1=100
n2=250
n3=500

set.seed(800)
x=seq(-5,5,by=0.1)


P1=p*dnorm(x,mu1[1],sg1)+(1-p)*dnorm(x,mu2[1],sg2)
P2=p*dnorm(x,mu1[2],sg1)+(1-p)*dnorm(x,mu2[2],sg2)
P3=p*dnorm(x,mu1[3],sg1)+(1-p)*dnorm(x,mu2[3],sg2)
P4=p*dnorm(x,mu1[4],sg1)+(1-p)*dnorm(x,mu2[4],sg2)
P5=p*dnorm(x,mu1[5],sg1)+(1-p)*dnorm(x,mu2[5],sg2)
P6=p*dnorm(x,mu1[6],sg1)+(1-p)*dnorm(x,mu2[6],sg2)
P7=p*dnorm(x,mu1[7],sg1)+(1-p)*dnorm(x,mu2[7],sg2)
d

par(mfrow=c(2,4))
par(mar=c(2,2.5,2,0.5))
plot (x,P1,type="l",main="d=1.5")
plot (x,P2,type="l",main="d=1.75")
plot (x,P3,type="l",main="d=2")
plot (x,P4,type="l",main="d=2.25")
plot (x,P5,type="l",main="d=2.5")
plot (x,P6,type="l",main="d=2.75")
plot (x,P7,type="l",main="d=3")

#Simulador de mixtures normals
sim<-function(p,m1,m2,sg1,sg2){
  ifelse(runif(1)<p,rnorm(1,m1,sg1),rnorm(1,m2,sg2))
}

set.seed(1000)
simix<-function(n,m1,m2,sg1=1,sg2=1,p=0.5){
  mos=numeric(n);
  for(k in 1:n){
  mos[k]<-sim(p,m1,m2,sg1,sg2)
  };mos
}

  ##Exercici 3

#CONSTRUIR TEST DE PEARSON INTERVALS EQUIPROBABLES

# - test de pearson
par(mfrow=c(1,1)) 
set.seed(100)
xs<-rnorm(100,mu2[1],sg1)
pp<-seq(0.1,0.9,0.1)
qq<-qnorm(pp)
qq<-c(qq,c(-15,15));qq
his<-hist(xs,breaks = qq)
obs<-his$counts;
obs
barplot(obs)
xi<-chisq.test(obs); xi$p.value #pvalor=0.87708290
xi
mu1

# Funció test de Pearson

n.rep=10000
d=seq(1.5,3,by=0.25)
mu1=-d/2
mu2=d/2
sg1=sg2=1
p=0.5
n1=100
n2=250
n3=500

pvs=numeric(n.rep)

for(k in 1:n.rep){
  sample=simix(n1,mu1[1],mu2[1])
  his=hist(sample,breaks=qq,plot = F)
  obs=his$counts
  xi=chisq.test(obs)
  taula=jarque.test(sample)
  pvss[k]=taula$p.value
  pvs[k]=xi$p.value
}
n.sig=ifelse (pvs<0.05,1,0)
n.sigg=ifelse (pvss<0.05,1,0)
sum(n.sig)
sum (n.sigg)
pot=sum(n.sigg)/n1
pot

  
# Funció test de Jarque-Bera
  
  install.packages("moments")
  library(moments);
  taula<- jarque.test(sample)
  taula$p.value
 
  n.sigJB<-ifelse(pvs2<0.05,1,0);n.sigJB;
   
  potJB<-sum(n.sigJB)/n.rep;potJB
  

  