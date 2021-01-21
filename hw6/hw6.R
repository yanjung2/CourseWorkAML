ori_data <- read.table("housing.data.txt",header=TRUE,sep = "")
ori_data.lm<-lm(y~x1+x2+x3+x4+x5+x6+x7+x8+x9+x10+x11+x12+x13, 
                data=ori_data)

# Linear Regression Plot
layout(matrix(c(1,2,3,4),2,2))
plot(ori_data.lm)
abline(ori_data.lm)

new_data<-ori_data[-c(365,369,372,373),]
new_data.lm<-lm(y~x1+x2+x3+x4+x5+x6+x7+x8+x9+x10+x11+x12+x13, 
                data=new_data)
plot(new_data.lm)
abline(new_data.lm)

new_data<-ori_data[-c(366,370,371),]
new_data.lm<-lm(y~x1+x2+x3+x4+x5+x6+x7+x8+x9+x10+x11+x12+x13, 
                data=new_data)
plot(new_data.lm)
abline(new_data.lm)

par(mfrow=c(1,1))
bc <- boxcox(y ~ x1+x2+x3+x4+x5+x6+x7+x8+x9+x10+x11+x12+x13,
             data=new_data)
lambda <- bc$x[which.max(bc$y)]

final<-lm((new_data$y ^ lambda - 1)/lambda~
            as.matrix(new_data[1:13]))
fitted_val = fitted(final)
fitted_y = (fitted_val*lambda+1)^(1/lambda)
plot(fitted_y,new_data$y)

