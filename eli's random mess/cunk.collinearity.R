
#code from a guy on reddit about collinearity mattering when
  #you want to know how adding a var to a regression 
  #model would affect the significance and effect size(?)
  #of other vars in model

  #i dont get it and should re-read the post before it


set.seed(4)
x <- runif(50, 10, 50)
x2 <- runif(50,0,40)
y <- x*2 + 50 + runif(50,-40,50)
y2 <- x2*2 + 150 + runif(50, -50, 40)
df <-data.frame(x = c(x,x2), y = c(y,y2), group=factor(rep(1:2,each = 50)))
plot(df[,c("x","y")], col=df$group)
model1 <- lm(y~x,df)
model2 <- lm(y~x+group,df)
summary(model1)
summary(model1)
lines(df$x, predict(model1), col="plum")
lines(df$x[1:50], predict(model2)[1:50], col= "seagreen")
lines(df$x[51:100], predict(model2[51:100], col="sienna"))


