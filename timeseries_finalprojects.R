library(MASS)
install.packages("dplyr")
library(dplyr)
install.packages("tidyr")
library(tidyr)
install.packages("ggplot2")
library(ggplot2)


install.packages("readxl")
library(readxl)
data = read_excel("Reyeme2.xls") # firstdate랑 closedate 를 날짜로 형식 변경완료(엑셀에서), 255번째 행의 firstdate 제거함(5/92/92로 나와있어서)
summary(data)
head(data)

#null 값 확인 후 제거
colSums(is.na(data))
data = na.omit(data)

#character 변수 factor 변수로 변환
data$Address = as.factor(data$Address)
data$Unit = as.factor(data$Unit)
data$Area = as.factor(data$Area)
data$AreaCode = as.factor(data$AreaCode) #areacode를 팩터형으로 바꾸지 않고 수치형으로 둔 후 회귀분석 돌려야하나?
data$RC = as.factor(data$RC)

summary(data)

#View(data)

class(data)

data = as.data.frame(data)


#회귀분석

#M1 = aov(SalePrice ~ AreaCode+Days+Interior+Bed+Bath+Rooms+Condo+Tax+RC, data = data)
#anova(M1)
#범주형 변수들을 제외하고 분석 돌리는 것이 맞는지?

full = lm(SalePrice ~ Area+FirstDate+CloseDate+Days+Interior+Bed+Bath+Rooms+Condo+Tax+RC, data = data)
null = lm(SalePrice ~ 1, data = data)
anova(full)
anova(null)

#AIC 활용해 최적의 회귀모형 도출
step=stepAIC(full, direction = "both")
step

#혹은 아래 코드(?)
step(null, scope = list(lower = null, upper = full), direction = "forward")
step(full, data = data, direction = "backward")
step(null, scope = list(upper = full), data = data, direction = "both")
#모든 과정에서 Rooms 변수를 제외한 부분모형인 (SalePrice ~ AreaCode+Days+Interior+Bed+Bath+Condo+Tax) 모형이 AIC 값이 가장 작게나옴.

#가중회귀
install.packages("lubridate")
library(lubridate)
data$Year = year(data$CloseDate)
data$Weight = data$Year - 1990

data_wt = lm(SalePrice ~ CloseDate + Days + Interior + Bed + 
               Condo + Tax + RC, data = data, weight = Weight)
round(cbind(data_lm = step$coefficients,
            data_wt = data_wt$coefficients), digits = 3)

#더미변수(Area)
Area_dummies <- model.matrix(~Area-1, data = data)
head(Area_dummies)

#다수의 수준을 갖는 요인변수들(step의 잔차를 이용해 5개의 areacode 그룹 만들었음)
AreaCode_groups <- data %>%
  mutate(resid = residuals(step))%>%
  group_by(AreaCode) %>%
  summarize(med_resid = median(resid),
            cnt = n()) %>%
  arrange(med_resid) %>%
  mutate(cum_cnt = cumsum(cnt),
         AreaCodeGroup = ntile(cum_cnt, 5))
data1 <- data %>%
  left_join(select(AreaCode_groups, AreaCode, AreaCodeGroup), by = 'AreaCode')
#AddressGroup변수 factor 변수로 변환
data1$AreaCodeGroup = as.factor(data1$AreaCodeGroup)

table(AreaCode_groups[c('AreaCodeGroup')])

#다수의 수준을 갖는 요인변수들(step의 잔차를 이용해 5개의 Interior 그룹 만들었음)
Interior_groups <- data %>%
  mutate(resid = residuals(step))%>%
  group_by(Interior) %>%
  summarize(med_resid = median(resid),
            cnt = n()) %>%
  arrange(med_resid) %>%
  mutate(cum_cnt = cumsum(cnt),
         InteriorGroup = ntile(cum_cnt, 5))
data1 <- data %>%
  left_join(select(Interior_groups, Interior, InteriorGroup), by = 'Interior')
#AddressGroup변수 factor 변수로 변환
data1$InteriorGroup = as.factor(data1$InteriorGroup)

table(Interior_groups[c('InteriorGroup')])



#예측변수 간 상관
step$coefficients

update(step, .~.-Bed)

#교란변수
M1=lm(SalePrice ~ CloseDate+Days+Bed+Bath+Rooms+Condo+Tax+RC+AreaCodeGroup, data = data1)
M1
anova(M1)

M2 = lm(SalePrice ~ CloseDate+Days+Bed+Bath+Rooms+Condo+Tax+RC+InteriorGroup, data = data1)
anova(M2)
M2
summary(M2)


#영향점 혹은 극단값 확인 후 제거


#등분산성(로그변환?)

#다중공선성

#잔차분석






#데이터 탐색
par(mfrow=c(2,2))
barplot(table(data$Area), xlab = "Area", ylab = "Frequency", main = "Barplot of data$Area")
hist(data$Price1, xlab = "Price1")
hist(data$Price2, xlab = "Price2")
hist(data$SalePrice, xlab = "SalePrice")
hist(data$Days, xlab = "Days")
hist(data$Interior, xlab = "Interior")
hist(data$Bed, xlab = "Bed")
hist(data$Bath, xlab = "Bath")
hist(data$Rooms, xlab = "Rooms")
hist(data$Condo, xlab = "Condo")
hist(data$Tax, xlab = "Tax")
barplot(table(data$RC), xlab = "RC", ylab = "Frequency", main = "Barplot of data$RC")


a = c(54,59,35,41,46,25,47,60,54,46,49,46,41,34,22)
mean(a)
sd(a)
median(a)
range(a)
summary(a)
boxplot(a)