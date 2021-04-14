## Análisis de la normalidad una muestra
ananor<-function(x)
{
        par(mfrow=c(2,2))
        n<-length(x)
        qqnorm(x,main=paste("Gráfico Normal de Prob. n=",n))
        qqline(x)
        boxplot(x,main="Gráfico de caja y bigotes")
        hist(x,col="red",xlim=c(min(x),max(x)),br=30,freq=FALSE)
        curve(dnorm(x,mean(x),sd(x)),min(x),max(x),1000,col="blue",
              add=TRUE,lwd=2)
        plot(ecdf(x), do.points=FALSE, verticals=TRUE)
        curve(pnorm(x,mean(x),sd(x)),min(x),max(x),1000,col="blue",
              add=TRUE,lwd=2)
        par(mfrow=c(1,1))
        shapiro.test(x)
}


ananor_tidy<-function(x)
{
        n<-length(x)
        datos = data.frame(X = x)
        p1 = ggplot(datos, aes(sample = X)) +
                geom_qq() +
                geom_qq_line() +
                labs(
                        title = paste("Gráfico Normal de Prob. n=",n)
                )
        p2 = ggplot(datos, aes(x = X)) +
                geom_boxplot() +
                labs(
                        title = "Gráfico de caja y bigotes"
                )
        p3 = ggplot(datos, aes(x = X)) +
                #geom_histogram(aes(y = ..density..), fill = "red",col="black", bins = 30) +
                geom_density(col="red") +
                xlim(min(datos$X),max(datos$X)) +
                stat_function(aes(x=seq(min(X),max(X),length = length(X))),
                              fun = dnorm, args = list(mean = mean(datos$X),
                                                       sd = sd(datos$X)),color = "blue") +
                labs(
                        title = "Función de densidad"
                )
        p4 = ggplot(datos, aes(x = X)) +
                stat_ecdf(geom = "step") +
                xlim(min(datos$X),max(datos$X)) +
                stat_function(aes(x=seq(min(X),max(X),length = length(X))),
                              fun = pnorm, args = list(mean = mean(datos$X),
                                                       sd = sd(datos$X)),color = "blue") +
                labs(
                        title = "Función de distribución"
                )
        print(shapiro.test(x))
        
        (p1 | p2) / (p3 | p4)
        
}





