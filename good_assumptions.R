library(ggplot2)

sample_size=10#779
dimension=1
set.seed(779)

#generate a random true b value
#b<-matrix(rep(1,each=dimension+1))
b<-matrix(rcauchy(dimension+1))

#figure out why this is rare to not be full rank
x<-matrix(data=rnorm(sample_size*dimension, mean=0, sd=1),ncol = dimension, byrow = FALSE)
#d<-c(rep(1,each=sample_size),rnorm(sample_size*dimension, mean=0, sd=1))
#make the first column 1s
X<-cbind(matrix(rep(1,each=sample_size)), x)

e<-matrix(rnorm(n = sample_size, mean=0, sd=1))
y<-X%*%b+e

datag<-data.frame(y,x)

#the model under ideal conditions
modelg<-lm(y ~ ., data=datag)
summary(modelg)

plot(x,y)
abline(modelg)

#prediction intervals
pre <- predict(modelg, interval = 'confidence')

ggplot(datag, aes(x=x, y=y)) +        # ggplot2 plot with confidence intervals
  geom_point() +
  geom_ribbon( aes(ymin = pre[,2], ymax = pre[,3], color = NULL), alpha = .15) +
  geom_smooth(method='lm', se=FALSE) 


