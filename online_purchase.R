data <- read.csv("Data_TS_Final2.csv", header = T)
colnames(data)
head(data)
str(data)
#data1 <- data %>% filter(CRI_YM == 201905)
head(data)
tail(data)
data1<-data
data1 <- data %>% arrange(DAY, TIME, AGE)
head(data1)
data_f <- data %>% filter(SEX == "F")
data_m <- data %>% filter(SEX == "M")
head(data_f)
head(data_m)
data_m <- rename(data_m, SEX_M = SEX, NUM_M = NUM)
head(data_m)
data2 <- left_join(data_f, data_m, by = c("CRI_YM", "TAG", "BD_HD", "DAY", "TIME", "AGE", "TIME2"))
head(data2)
data2 <- data2 %>% mutate(total = NUM+NUM_M)
head(data2)
data2 <- data2 %>% arrange(TAG, BD_HD, DAY, TIME, AGE, TIME2)
head(data2)
data2[1:15,]
data3 <- data2[,-c(6,10)]
head(data3)
data3 <- rename(data3, NUM_F = NUM)
head(data3)
data_b <- data3 %>% filter(BD_HD == "business_day" | DAY %in% c("F.SAT", "G.SUN"))
head(data_b)
str(data_b)
tail(data_b)
data_h <- data3 %>% filter(BD_HD != "business_day" & DAY != "F.SAT" & DAY != "G.SUN")
head(data_h)
data4 <- left_join(data_b, data_h, by = c("CRI_YM", "TAG", "DAY", "TIME", "AGE","TIME2"))
head(data4)
data4[is.na(data4)] <- 0
head(data4)
data4 <- data4[,-c(3,11)]
head(data4)
data4 <- data4 %>% mutate(NUM_F = NUM_F.x+NUM_F.y, NUM_M = NUM_M.x+NUM_M.y, TOTAL = total.x+total.y)
head(data4)
data4 <- data4[,-c(6,8:12)]
head(data4)
tail(data4)

data4_gro <- data4 %>% filter(TAG == "grocery")
table(data4_gro$TAG)
head(data4_gro)

data5 <- data4_gro %>% group_by(CRI_YM, TIME2) %>% arrange(CRI_YM, TIME2)
head(data5)

write.xlsx(data4, "data4.xlsx")

data4_20 <- data5 %>% filter(AGE == "A.2O") %>% group_by(CRI_YM, TIME2) %>% arrange(CRI_YM, TIME2)
data4_30 <- data5 %>% filter(AGE == "B.3O") %>% group_by(CRI_YM, TIME2) %>% arrange(CRI_YM, TIME2)
data4_40 <- data5 %>% filter(AGE == "C.4O") %>% group_by(CRI_YM, TIME2) %>% arrange(CRI_YM, TIME2)
data4_50 <- data5 %>% filter(AGE == "D.5O") %>% group_by(CRI_YM, TIME2) %>% arrange(CRI_YM, TIME2)
data4_60 <- data5 %>% filter(AGE == "E.60") %>% group_by(CRI_YM, TIME2) %>% arrange(CRI_YM, TIME2)

print(data4_20)
tail(data4_20)

data4_20$TIME3 <- 1:126
data4_30$TIME3 <- 1:126
data4_40$TIME3 <- 1:126
data4_50$TIME3 <- 1:126
data4_60$TIME3 <- 1:126

data6 <- bind_rows(data4_20, data4_30, data4_40, data4_50, data4_60) %>% arrange(TIME3)
head(data6)

write_csv(data6, "TS_data_time3.csv")

library(tidyverse)
g <- ggplot(data = data6) +
  geom_line(aes(x = TIME3, y = TOTAL, col = AGE))+ theme_minimal()
install.packages("plotly")
library(plotly)
ggplotly(g) 



#temp model
purchase_plm <- plm(NUM ~ TIME2, 
                    data = data4,
                    index = c("TAG"), 
                    model = "within")

summary(purchase_plm)
plot(purchase_plm)


pp = ggplot(data = data4) + geom_point(aes(x = TIME2, y = NUM), size = 0.5)

#res = loess(TOTAL~TIME2,span=0.3)
#ppr = pp+geom_line(aes(TIME2,predict(res)),col="red")
#ppr


data6$CRI_YM <- ifelse(data6$CRI_YM == 201905, 2019, 
                       ifelse(data6$CRI_YM == 202005, 2020, 2021))

y2019 <- data6 %>% filter(CRI_YM == 201905)
y2020 <- data6 %>% filter(CRI_YM == 202005)
y2021 <- data6 %>% filter(CRI_YM == 202105)

age30 <- data6 %>% filter(AGE == "B.3O")

age30$CRI_YM <- as.factor(age30$CRI_YM)

age30_mean <- age30 %>% group_by(CRI_YM, DAY, TIME3) %>% summarise(mean = mean(TOTAL))


ggplot(data = age30_mean) +
  geom_point(aes(x = DAY, y = mean, col = CRI_YM))


table(data$TAG)