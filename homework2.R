#1.a
y = c() 
while(length(y) < 20) {
  a = sample(0:10, 1)
  e = rnorm(1, 0, sqrt(2))
  if(a+e > 0 && a+e <11){
    y = append(y, a+e)
  }
}

y

#1.b
cauchy <- function(theta, x){
  n <- length(x)
  y <- matrix(0,n)
  for(i in c(0:n-1)){
    y[i] <- (theta - x[i]) / (1 + (theta - x[i])^2)
  }
  return(-2 * sum(y))
}

#1.c
theta = 0.3
cauchy(theta, y)

#2.a
install.packages("tidyverse")
library(tidyverse)
year_type <- ifelse (houseprice$Build_year<=1899, "centennial", 
                     ifelse(houseprice$Build_year>=1960, "new", "old"))
print(year_type)
houseprice$year_type <- c(year_type)

#2.b
library(broom)
attach(houseprice)

require(ggplot2)

#觀察房型的箱型圖
ggplot(data = houseprice) + geom_boxplot(aes( x= Type, y= Sale_amount, colour = Type)) + 
  labs( x = 'Type',
        y = 'Sales_amount',
        title = 'Sales Distribution by Type')

#發現一個明顯的離群值，將其刪除
houseprice <- houseprice %>% filter(Sale_amount < 7500000)
attach(houseprice)

ggplot(data = houseprice) + geom_boxplot(aes( x= Type, y= Sale_amount, colour = Type)) + 
  labs( x = 'Type',
        y = 'Sales_amount',
        title = 'Sales Distribution by Type')

#將城市依照西區、中部、東區做分類
Town_type <- ifelse (houseprice$Town %in% c("Tacoma, WA","Corvallis, OR","Eugene, OR","San Luis Obispo, CA",
                                            "Claremont, CA","Berkeley, CA","Logan, UT","Bozeman, MT",
                                            "Flagstaff, AZ","Tempe, AZ") , "west", 
                     ifelse (houseprice$Town %in% c("Boulder, CO","Fort Collins, CO","Fargo, ND",
                                                    "Grand Forks, ND","Manhattan, KS","Lincoln, NE",
                                                    "Lawrence, KS","College Station, TX","Minneapolis, MN",
                                                    "Iowa City, IA","Ames, IA","Columbia, MO","Fayetteville, AR",
                                                    "Madison, WI","Champaign-Urbana, IL","Bloomington, IL"),"middle","east"))

print(Town_type)
houseprice$Town_type <- c(Town_type)

#觀察房子地區的箱型圖
ggplot(data = houseprice) + geom_boxplot(aes( x= Town_type, y= Sale_amount, colour = Town_type)) + 
  labs( x = 'Town_type',
        y = 'Sales_amount',
        title = 'Sales Distribution by Town_type')


#觀察房子年份的箱型圖
ggplot(data = houseprice) + geom_boxplot(aes( x= year_type, y= Sale_amount, colour = year_type)) + 
  labs( x = 'year_type',
        y = 'Sales_amount',
        title = 'Sales Distribution by year_type')

#將Type轉為dummy variables，並試著做迴歸分析
library(dummies)

houseprice$Type = as.factor(as.character(houseprice$Type))
type_df <- data.frame(Type = houseprice$Type)
type_dummies <- dummy.data.frame(type_df)
type_dummies <- type_dummies[,-c(3)]
fit.1 = lm(Sale_amount~.,data = type_dummies)
summary(fit.1)

#將Town轉為dummy variables，並試著做迴歸分析
houseprice$Town_type = as.factor(as.character(houseprice$Town_type))
town_df <- data.frame(Type = houseprice$Town_type)
town_dummies <- dummy.data.frame(town_df)
town_dummies <- town_dummies[,-c(3)]
fit.2 = lm(Sale_amount~.,data = town_dummies)
summary(fit.2)

##將year_type轉為dummy variables，並試著做迴歸分析
houseprice$year_type = as.factor(as.character(houseprice$year_type))
year_df <- data.frame(Type = houseprice$year_type)
year_dummies <- dummy.data.frame(year_df)
year_dummies <- year_dummies[,-c(3)]
fit.3 = lm(Sale_amount~.,data = year_dummies)
summary(fit.3)

#將dummy variables合併到原本的houseprice表中
houseprice_final <- cbind(houseprice, type_dummies, town_dummies, year_dummies)
houseprice_final <- houseprice_final[-c(1, 3, 8:13)]
attach(houseprice_final)

#觀察連續型x變數之間的關係，以及連續型x對y變數的相關性

pairs(houseprice_final[,c(1:5)])

library(corrplot)
cor=cor(houseprice_final[,c(1:5)])
cor

#將Sqft_lot變數刪除
houseprice_final <- houseprice_final[-c(5)]
attach(houseprice_final)

#將前8000筆資料切為訓練集，其餘為測試集
train=houseprice_final[1:8000,]
test=houseprice_final[8001:10658,]

### stepwise
full <- lm(Sale_amount~.,data=train)
glance(full) %>% select(AIC,BIC)
null <-lm(Sale_amount~1,data=train)

#AIC
step(full, direction="backward")
step(null, scope=list(lower=null, upper=full), direction="forward")

#BIC
step(full, direction="backward", criterion = "BIC")

#做迴歸分析
model <- lm(Sale_amount ~ Beds + Baths + Sqft_home  + 
              Typeeast + Typemiddle + Typecentennial + Typenew, data = train)
summary(model)

#加入交互作用項Beds:Sqft_home
model_1 <- lm(Sale_amount ~ Beds + Baths + Sqft_home +Beds:Sqft_home + 
              Typeeast + Typemiddle + Typecentennial + Typenew, data = train)

summary(model_1)
anova(model, model_1)

#加入交互作用項Beds:Baths
model_2 <- lm(Sale_amount ~ Beds + Baths + Sqft_home + Beds:Sqft_home + 
                Beds:Baths + Typeeast + Typemiddle + Typecentennial + Typenew, data = train)
summary(model_2)
anova(model_1, model_2)

#加入交互作用項Baths:Sqft_home
model_3 <- lm(Sale_amount ~ Beds + Baths + Sqft_home + Beds:Sqft_home + 
                Baths:Sqft_home + Typeeast + Typemiddle + Typecentennial + Typenew, data = train)
summary(model_3)
anova(model_1, model_3)

#發現有兩筆離群值，將其刪除
c= cooks.distance(model_3) #>=1, might be outlier, can remove
plot(c)
which(c>1)
houseprice_final <- houseprice_final[-c(1120,1128),]
attach(houseprice_final)

#做AIC、BIC、RMSE測試
train=houseprice_final[1:8000,]
test=houseprice_final[8001:10656,]
AIC(model,k = 2)
AIC(model_3,k = 2)
BIC(model)
BIC(model_3)
predict1 = predict(model,test)
predict2 = predict(model_3,test)
test1 <- cbind(test, predict1)
test2 <- cbind(test, predict2)
RMSE1=sqrt(mean(sum((test$Sale_amount-predict1)^2)))
RMSE2=sqrt(mean(sum((test$Sale_amount-predict2)^2)))
RMSE1
RMSE2






