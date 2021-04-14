##################################
#ESTADÍSTICA COMPUTACIONAL I     #
#HOJA DE PROBLEMAS 4             #
#Problemas 5,6 y 7               #
##################################

############
#Problema 5
#############
#5.1
####
#la función click muestra en pantalla 
#un punto, que debe ser señalado con el
#ratón; se recoge la localización mostrada
#y la marcada, y el tiempo empleado

click<- function(mano)
{
  x<-runif(1); y<-runif(1)
  plot(x,y,xlim=c(0,1),ylim=c(0,1),
       main=paste("Pulse en el círculo con su mano",mano),
       xlab="",ylab="",axes=FALSE,frame.plot=TRUE)
  clicktime<- system.time(xyclick<-locator(1))
  points(xyclick$x,xyclick$y,col="red")
  list(horapuls=Sys.time(),x=x,y=y,
       xclick=xyclick$x,yclick=xyclick$y,
       tclick=clicktime[3])
}

click("derecha")
click("izquierda")

#Los datos que se van a analizar se han generado
#con las siguientes instrucciones
n<-10
derecha<- data.frame(click("Derecha"))
for (i in 2:n)
  derecha<- rbind(derecha,data.frame(click("Derecha")))


izquierda<- data.frame(click("Izquierda"))
for (i in 2:n)
  izquierda<- rbind(izquierda,data.frame(click("Izquierda")))
#save(derecha,izquierda,file="ClickDerIzq.RData")

load("ClickDerIzq.RData")

##5.2. 
#####
#Distancias de los puntos mostrados a los pulsados
#Ejercicio
###########
distder=
distizq=
###########
#Tiempos con cada mano
tder<- derecha$tclick
tizq<- izquierda$tclick

##5.3.
#####
#Resumen y comparación de tiempos derecha/izquierda
summary(tder)
summary(tizq)
boxplot(tder,tizq,main="Tiempos",
        names=c("Derecha","Izquierda"))
#Comprobar la normalidad 
shapiro.test(tder)
shapiro.test(tizq)
#Tras aceptar la normalidad, veamos las varianzas:
var.test(tder,tizq) #se rechaza
#por defecto, var.equal=FALSE
t.test(tder,tizq,alternative="less")

#Ejercicio:Dibujar las Fn
#######

#######

#otra opción:
ks.test(tder,tizq,"greater")


##5.4.
#####
#Resumen y comparación de distancias derecha/izquierda
#Ejercicio
##################################

##################################


##5.5.
#####

mano<- gl(2,n,labels=c("Derecha","Izquierda"))
x<- c(derecha$x,izquierda$x)
y<- c(derecha$y,izquierda$y)
xclick=c(derecha$xclick,izquierda$xclick)
yclick=c(derecha$yclick,izquierda$yclick)
tiempo<- c(tder,tizq)
distancia<- c(distder,distizq)
datos<- data.frame(mano,x,y,xclick,yclick,tiempo,distancia)
datos
colores<- c("red","blue")
plot(datos$x,datos$y,col=colores[datos$mano],
     main="Localizaciones")
points(datos$xclick,datos$yclick,
       pch=10,col=colores[datos$mano])
legend("bottomleft",col=colores,pch=1,
       legend=c("Derecha","Izquierda"))
###########

#Ejercicio 
#########
#########

##############
##Problema 6. 
#datos sobre el test de aptitud y rendimiento
#############################################
datos<- read.table(file="Aptitudempleo.txt")
colnames(datos)<- c("Grupo","Rendimiento","Test")
datos$Grupo<- factor(datos$Grupo)
levels(datos$Grupo)<- c("A","B")
colores<- c("red","blue")
attach(datos)
plot(Test,Rendimiento,col=colores[datos$Grupo],
     main="Relación entre test de aptitud y rendimiento")
regre<- lm(Rendimiento~Test)
abline(regre,lwd=2)
regreA<- lm(Rendimiento~Test,datos[datos$Grupo=="A",])
abline(regreA,lwd=2,col="red",lty=2)
regreB<- lm(Rendimiento~Test,datos[datos$Grupo=="B",])
abline(regreB,lwd=2,col="blue",lty=2)
legend("topleft",col=c("black","red","blue"),lty=c(1,2,2),legend=c("Común","A","B"))
grid()

GrupoA<- Grupo=="A"
Gruptest<- GrupoA*Test
regremul<- lm(Rendimiento~Test+GrupoA+Gruptest) 
#pvalor=0.055 hay cierta evidencia contra H0: coinciden ambos modelos de reg.lineal
summary(regremul)

#Mínimo test
xinv<- function(y,regre)
{
  coefic<- coef(regre)
  (y-coefic[1])/coefic[2]
}
xinv(5,regre)
xinv(5,regreA)
xinv(5,regreB)
#Añadir dibujo con líneas desde el valor y=5 hasta los x mínimos
#para cada modelo
#######
marcas<- function(x,y,color)
{
  lines(c(0,x,x),c(y,y,0),lty=2,col=color)
}

marcas(xinv(5,regre),5,"black")
marcas(xinv(5,regreA),5,"red")
marcas(xinv(5,regreB),5,"blue")
##########


#########################
##7. Inscripciones curso
###########################
x<-c(10,31,43,16,11,33,45,17,
     13,34,48,19,15,35,51,21)
n<- length(x)
caso<- 1:(n+4)
plot(caso[1:n],x,type="l",xlim=c(1,20),
     xlab="trimestre") #serie estacional
grid()
regre<- lsfit(caso[1:n],x)
abline(regre,lwd=2,lty=2)
#Ejercicio: un modelo de regresión lineal
#con los siguientes predictores;
#caso y 3 variables ficticias 1-0
#para los trimestres 2,3,4
########
trim2<- (caso%%4)==2
trim3<- (caso%%4)==3
trim4<- (caso%%4)==0
datos<- data.frame(x=c(x,rep(NA,4)),
                   caso,trim2,trim3,trim4)
regrestac<- lm(x~caso+trim2+trim3+trim4,datos)
summary(regrestac)
lines(caso,predict(regrestac,datos[,-1]),
      col="red",lty=2)
predict(regrestac,datos[,-1])
###################


