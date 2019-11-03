library(tidyverse)
library(caret)
library(rpart)
library(naivebayes)
library(genefilter)
library(randomForest)
library(kernlab)
load("rda/dat.rda")

set.seed(2, sample.kind="Rounding")
train_index <- createDataPartition(y = dat$train$y, times = 1, p = 0.0025, list = FALSE)
train_subset <- dat$train[train_index,]
train_subset_mf <- dat$train %>%
  mutate(
    wa1 = as.numeric(wa1=="Presence"),
    wa2 = as.numeric(wa2=="Presence"),
    wa3 = as.numeric(wa3=="Presence"),
    wa4 = as.numeric(wa4=="Presence"),
    st1 = as.numeric(st1=="Presence"),
    st2 = as.numeric(st2=="Presence"),
    st3 = as.numeric(st3=="Presence"),
    st4 = as.numeric(st4=="Presence"),
    st5 = as.numeric(st5=="Presence"),
    st6 = as.numeric(st6=="Presence"),
    st7 = as.numeric(st7=="Presence"),
    st8 = as.numeric(st8=="Presence"),
    st9 = as.numeric(st9=="Presence"),
    st10 = as.numeric(st10=="Presence"),
    st11 = as.numeric(st11=="Presence"),
    st12 = as.numeric(st12=="Presence"),
    st13 = as.numeric(st13=="Presence"),
    st14 = as.numeric(st14=="Presence"),
    st15 = as.numeric(st15=="Presence"),
    st16 = as.numeric(st16=="Presence"),
    st17 = as.numeric(st17=="Presence"),
    st18 = as.numeric(st18=="Presence"),
    st19 = as.numeric(st19=="Presence"),
    st20 = as.numeric(st20=="Presence"),
    st21 = as.numeric(st21=="Presence"),
    st22 = as.numeric(st22=="Presence"),
    st23 = as.numeric(st23=="Presence"),
    st24 = as.numeric(st24=="Presence"),
    st25 = as.numeric(st25=="Presence"),
    st26 = as.numeric(st26=="Presence"),
    st27 = as.numeric(st27=="Presence"),
    st28 = as.numeric(st28=="Presence"),
    st29 = as.numeric(st29=="Presence"),
    st30 = as.numeric(st30=="Presence"),
    st31 = as.numeric(st31=="Presence"),
    st32 = as.numeric(st32=="Presence"),
    st33 = as.numeric(st33=="Presence"),
    st34 = as.numeric(st34=="Presence"),
    st35 = as.numeric(st35=="Presence"),
    st36 = as.numeric(st36=="Presence"),
    st37 = as.numeric(st37=="Presence"),
    st38 = as.numeric(st38=="Presence"),
    st39 = as.numeric(st39=="Presence"),
    st40 = as.numeric(st40=="Presence")
  )

set.seed(3, sample.kind="Rounding")
test_index <- createDataPartition(y = dat$test$y, times = 1, p = 0.0025, list = FALSE)
test_subset <- dat$test[test_index,]
test_subset_mf <- dat$test %>%
  mutate(
    wa1 = as.numeric(wa1=="Presence"),
    wa2 = as.numeric(wa2=="Presence"),
    wa3 = as.numeric(wa3=="Presence"),
    wa4 = as.numeric(wa4=="Presence"),
    st1 = as.numeric(st1=="Presence"),
    st2 = as.numeric(st2=="Presence"),
    st3 = as.numeric(st3=="Presence"),
    st4 = as.numeric(st4=="Presence"),
    st5 = as.numeric(st5=="Presence"),
    st6 = as.numeric(st6=="Presence"),
    st7 = as.numeric(st7=="Presence"),
    st8 = as.numeric(st8=="Presence"),
    st9 = as.numeric(st9=="Presence"),
    st10 = as.numeric(st10=="Presence"),
    st11 = as.numeric(st11=="Presence"),
    st12 = as.numeric(st12=="Presence"),
    st13 = as.numeric(st13=="Presence"),
    st14 = as.numeric(st14=="Presence"),
    st15 = as.numeric(st15=="Presence"),
    st16 = as.numeric(st16=="Presence"),
    st17 = as.numeric(st17=="Presence"),
    st18 = as.numeric(st18=="Presence"),
    st19 = as.numeric(st19=="Presence"),
    st20 = as.numeric(st20=="Presence"),
    st21 = as.numeric(st21=="Presence"),
    st22 = as.numeric(st22=="Presence"),
    st23 = as.numeric(st23=="Presence"),
    st24 = as.numeric(st24=="Presence"),
    st25 = as.numeric(st25=="Presence"),
    st26 = as.numeric(st26=="Presence"),
    st27 = as.numeric(st27=="Presence"),
    st28 = as.numeric(st28=="Presence"),
    st29 = as.numeric(st29=="Presence"),
    st30 = as.numeric(st30=="Presence"),
    st31 = as.numeric(st31=="Presence"),
    st32 = as.numeric(st32=="Presence"),
    st33 = as.numeric(st33=="Presence"),
    st34 = as.numeric(st34=="Presence"),
    st35 = as.numeric(st35=="Presence"),
    st36 = as.numeric(st36=="Presence"),
    st37 = as.numeric(st37=="Presence"),
    st38 = as.numeric(st38=="Presence"),
    st39 = as.numeric(st39=="Presence"),
    st40 = as.numeric(st40=="Presence")
  )

fit <- knn3(y ~ ., data = dat$train, k = 5)
y_hat <- predict(fit, dat$test %>% select(-y), type = "class")
cm <- confusionMatrix(data = y_hat, reference = dat$test$y)
cm$overall["Accuracy"]
# Accuracy 
# 0.9664385

#Confusion Matrix and Statistics

#Reference
#Prediction           5     2     1     7     3     6     4
#               5  2536   183    43     2    17    10     0
#               2   263 82760  1969    29   143   108     1
#               1    27  1778 61374   170     2     7     0
#               7     0    29   161  5952     0     0     0
#               3    18   140     1     0 10338   208   131
#               6     4    99     4     0   183  4849    45
#               4     0     2     0     0    44    29   648

#Overall Statistics

#Accuracy : 0.9664          
#95% CI : (0.9656, 0.9673)
#No Information Rate : 0.4876          
#P-Value [Acc > NIR] : < 2.2e-16       

#Kappa : 0.9461          
#Mcnemar's Test P-Value : NA              
#Statistics by Class:

#                     Class: 5 Class: 2 Class: 1 Class: 7 Class: 3 Class: 6 Class: 4
#Sensitivity           0.89045   0.9738   0.9657  0.96733  0.96374  0.93053 0.785455
#Specificity           0.99851   0.9719   0.9821  0.99887  0.99696  0.99802 0.999568
#Pos Pred Value        0.90863   0.9705   0.9687  0.96907  0.95404  0.93538 0.896266
#Neg Pred Value        0.99818   0.9749   0.9804  0.99880  0.99762  0.99786 0.998980
#Prevalence            0.01634   0.4876   0.3646  0.03530  0.06154  0.02990 0.004733
#Detection Rate        0.01455   0.4748   0.3521  0.03415  0.05931  0.02782 0.003718
#Detection Prevalence  0.01601   0.4892   0.3635  0.03524  0.06217  0.02974 0.004148
#Balanced Accuracy     0.94448   0.9728   0.9739  0.98310  0.98035  0.96428 0.892511

## use pca
x <- train_subset_mf[,1:54] %>% as.matrix()
col_means <- colMeans(x)
d <- dist(x)
image(as.matrix(d), col = rev(RColorBrewer::brewer.pal(9, "RdBu")))

pca <- prcomp(x)
summary(pca)
# Importance of components:
#                             PC1       PC2       PC3       PC4       PC5      PC6      PC7     PC8     PC9  PC10   PC11   PC12   PC13
# Standard deviation     1693.503 1152.1665 276.76365 192.49089 115.17437 45.13932 32.67003 21.1877 5.89320 1.509 0.6233 0.3621 0.2955
# Proportion of Variance    0.663    0.3069   0.01771   0.00857   0.00307  0.00047  0.00025  0.0001 0.00001 0.000 0.0000 0.0000 0.0000
# Cumulative Proportion     0.663    0.9698   0.98754   0.99610   0.99917  0.99964  0.99989  1.0000 1.00000 1.000 1.0000 1.0000 1.0000

data.frame(pca$x[,1:2], covtype=dat$train$y) %>% 
  ggplot(aes(PC1, PC2, fill=covtype)) +
  geom_point(cex=3, pch=21) +
  coord_fixed(ratio = 1)

d_approx <- dist(pca$x[, 1:2])
qplot(d, d_approx) + geom_abline(color="red")

pc <- 1:54
qplot(pc, pca$sdev)

data.frame(PC1 = pca$x[,1], PC2 = pca$x[,2], label=train_subset_mf$y) %>%
  sample_n(2000) %>% 
  ggplot(aes(PC1, PC2, fill=label)) +
  geom_point(cex=3, pch=21)

x_train <- pca$x[,1:2]
y <- train_subset_mf$y
fit <- knn3(x_train, y, k = 5)

x_test <- test_subset_mf[,1:54] %>% as.matrix()
pca_test <- prcomp(x_test)
x_test <- pca_test$x[,1:2]
y_hat <- predict(fit, x_test, type = "class")
cm <- confusionMatrix(data = y_hat, reference = test_subset_mf$y)
# > cm
# Confusion Matrix and Statistics

# Reference
# Prediction   Aspen Cottonwood Douglas Krummholz Lodgepole Panderosa Spruce
# Aspen        296         14      89        33       791       177    499
# Cottonwood    10         52      28        17       207       112    102
# Douglas       83         80     725        92      1637       912   1008
# Krummholz     34          4      65       925      1557        97   1707
# Lodgepole   2718        661    4493      4765     89423      8351  50642
# Panderosa    317        338    1429       258      5121      4659   3025
# Spruce      1289        225    1855      4165     42915      3569  48937

# Overall Statistics

# Accuracy : 0.4992         
# 95% CI : (0.4974, 0.501)
# No Information Rate : 0.4876         
# P-Value [Acc > NIR] : < 2.2e-16      

# Kappa : 0.1599         

# Mcnemar's Test P-Value : < 2.2e-16      

# Statistics by Class:

#                      Class: Aspen Class: Cottonwood Class: Douglas Class: Krummholz Class: Lodgepole Class: Panderosa
# Sensitivity              0.062355          0.037846       0.083487         0.090200           0.6313          0.26061
# Specificity              0.994390          0.998354       0.986474         0.987640           0.5188          0.96153
# Pos Pred Value           0.155872          0.098485       0.159797         0.210754           0.5552          0.30759
# Neg Pred Value           0.984578          0.995441       0.972169         0.967391           0.5966          0.95200
# Prevalence               0.016340          0.004730       0.029892         0.035300           0.4876          0.06154
# Detection Rate           0.001019          0.000179       0.002496         0.003184           0.3078          0.01604
# Detection Prevalence     0.006537          0.001818       0.015617         0.015108           0.5544          0.05214
# Balanced Accuracy        0.528373          0.518100       0.534980         0.538920           0.5750          0.61107

## More knn
# Estimate k using accuracy
set.seed(2, sample.kind="Rounding")
index <- createDataPartition(y = test$class, times = 1, p = 0.02, list = FALSE)
test_subset <- test[index,]
test_fact_subset <- test_fact[index,]

ks <- seq(1, 251, 2)
accuracy <- sapply(ks, function(k) {
   fit <- knn3(y ~ ., data = train_subset, k = k)
   y_hat <- predict(fit, test_subset %>% select(-y), type = "class")
   # accuracy <- mean(y_hat == test_subset$class)
   cm <- confusionMatrix(data = y_hat, reference = test_subset$y)
   debugstr <- paste("k=", k, "accuracy=", cm$overall["Accuracy"], sep = " ", collapse = NULL)
   print(debugstr)
   cm$overall["Accuracy"]
})
# [1] "k= 1 accuracy= 0.964183381088825"
# [1] "k= 3 accuracy= 0.965329512893983"
# [1] "k= 5 accuracy= 0.965902578796562"
# [1] "k= 7 accuracy= 0.963323782234957"
# [1] "k= 9 accuracy= 0.959312320916905"
# [1] "k= 11 accuracy= 0.953868194842407"
# [1] "k= 13 accuracy= 0.946991404011461"
# [1] "k= 15 accuracy= 0.94297994269341"
# [1] "k= 17 accuracy= 0.936676217765043"
# [1] "k= 19 accuracy= 0.93352435530086"
# [1] "k= 21 accuracy= 0.934097421203438"
# [1] "k= 23 accuracy= 0.928080229226361"
# [1] "k= 25 accuracy= 0.923495702005731"
# [1] "k= 27 accuracy= 0.918051575931232"
# [1] "k= 29 accuracy= 0.913180515759312"
# [1] "k= 31 accuracy= 0.913753581661891"
# [1] "k= 33 accuracy= 0.910888252148997"
# [1] "k= 35 accuracy= 0.906590257879656"
# [1] "k= 37 accuracy= 0.905157593123209"
# [1] "k= 39 accuracy= 0.902865329512894"
# [1] "k= 41 accuracy= 0.899426934097421"
# [1] "k= 43 accuracy= 0.897134670487106"
# [1] "k= 45 accuracy= 0.893982808022923"
# [1] "k= 47 accuracy= 0.891404011461318"
# [1] "k= 49 accuracy= 0.887965616045845"
# [1] "k= 51 accuracy= 0.886246418338109"
# [1] "k= 53 accuracy= 0.885386819484241"
# [1] "k= 55 accuracy= 0.883094555873925"
# [1] "k= 57 accuracy= 0.881375358166189"
# [1] "k= 59 accuracy= 0.880515759312321"
# [1] "k= 61 accuracy= 0.87621776504298"
# [1] "k= 63 accuracy= 0.875644699140401"
# [1] "k= 65 accuracy= 0.872779369627507"
# [1] "k= 67 accuracy= 0.871060171919771"
# [1] "k= 69 accuracy= 0.869627507163324"
# [1] "k= 71 accuracy= 0.867908309455587"
# [1] "k= 73 accuracy= 0.864756446991404"
# [1] "k= 75 accuracy= 0.863323782234957"
# [1] "k= 77 accuracy= 0.863896848137536"
# [1] "k= 79 accuracy= 0.863323782234957"
# [1] "k= 81 accuracy= 0.862464183381089"
# [1] "k= 83 accuracy= 0.859598853868195"
# [1] "k= 85 accuracy= 0.859885386819484"
# [1] "k= 87 accuracy= 0.857879656160458"
# [1] "k= 89 accuracy= 0.859312320916905"
# [1] "k= 91 accuracy= 0.856733524355301"
# [1] "k= 93 accuracy= 0.853295128939828"
# [1] "k= 95 accuracy= 0.851862464183381"
# [1] "k= 97 accuracy= 0.851575931232092"
# [1] "k= 99 accuracy= 0.849570200573066"
# [1] "k= 101 accuracy= 0.849283667621777"
# [1] "k= 103 accuracy= 0.848137535816619"
# [1] "k= 105 accuracy= 0.84756446991404"
# [1] "k= 107 accuracy= 0.846991404011461"
# [1] "k= 109 accuracy= 0.847277936962751"
# [1] "k= 111 accuracy= 0.84269340974212"
# [1] "k= 113 accuracy= 0.843266475644699"
# [1] "k= 115 accuracy= 0.841547277936963"
# [1] "k= 117 accuracy= 0.841547277936963"
# [1] "k= 119 accuracy= 0.838395415472779"
# [1] "k= 121 accuracy= 0.838968481375358"
# [1] "k= 123 accuracy= 0.837822349570201"
# [1] "k= 125 accuracy= 0.83810888252149"
# [1] "k= 127 accuracy= 0.837249283667622"
# [1] "k= 129 accuracy= 0.836676217765043"
# [1] "k= 131 accuracy= 0.835530085959885"
# [1] "k= 133 accuracy= 0.834097421203438"
# [1] "k= 135 accuracy= 0.834097421203438"
# [1] "k= 137 accuracy= 0.832951289398281"
# [1] "k= 139 accuracy= 0.832091690544413"
# [1] "k= 141 accuracy= 0.830659025787966"
# [1] "k= 143 accuracy= 0.829512893982808"
# [1] "k= 145 accuracy= 0.830372492836676"
# [1] "k= 147 accuracy= 0.829799426934097"
# [1] "k= 149 accuracy= 0.827793696275072"
# [1] "k= 151 accuracy= 0.827507163323782"
# [1] "k= 153 accuracy= 0.827507163323782"
# [1] "k= 155 accuracy= 0.826074498567335"
# [1] "k= 157 accuracy= 0.82378223495702"
# [1] "k= 159 accuracy= 0.823495702005731"
# [1] "k= 161 accuracy= 0.82378223495702"
# [1] "k= 163 accuracy= 0.822922636103152"
# [1] "k= 165 accuracy= 0.823209169054441"
# [1] "k= 167 accuracy= 0.823495702005731"
# [1] "k= 169 accuracy= 0.822922636103152"
# [1] "k= 171 accuracy= 0.822063037249284"
# [1] "k= 173 accuracy= 0.820630372492837"
# [1] "k= 175 accuracy= 0.820343839541547"
# [1] "k= 177 accuracy= 0.819770773638968"
# [1] "k= 179 accuracy= 0.819484240687679"
# [1] "k= 181 accuracy= 0.81919770773639"
# [1] "k= 183 accuracy= 0.818051575931232"
# [1] "k= 185 accuracy= 0.816905444126074"
# [1] "k= 187 accuracy= 0.816332378223496"
# [1] "k= 189 accuracy= 0.816618911174785"
# [1] "k= 191 accuracy= 0.814899713467049"
# [1] "k= 193 accuracy= 0.812607449856733"
# [1] "k= 195 accuracy= 0.811174785100287"
# [1] "k= 197 accuracy= 0.810028653295129"
# [1] "k= 199 accuracy= 0.810315186246418"
# [1] "k= 201 accuracy= 0.810315186246418"
# [1] "k= 203 accuracy= 0.80974212034384"
# [1] "k= 205 accuracy= 0.810315186246418"
# [1] "k= 207 accuracy= 0.809169054441261"
# [1] "k= 209 accuracy= 0.808595988538682"
# [1] "k= 211 accuracy= 0.808309455587393"
# [1] "k= 213 accuracy= 0.808022922636103"
# [1] "k= 215 accuracy= 0.806876790830946"
# [1] "k= 217 accuracy= 0.806303724928367"
# [1] "k= 219 accuracy= 0.806017191977077"
# [1] "k= 221 accuracy= 0.805157593123209"
# [1] "k= 223 accuracy= 0.805730659025788"
# [1] "k= 225 accuracy= 0.80458452722063"
# [1] "k= 227 accuracy= 0.805444126074499"
# [1] "k= 229 accuracy= 0.80487106017192"
# [1] "k= 231 accuracy= 0.804011461318052"
# [1] "k= 233 accuracy= 0.80487106017192"
# [1] "k= 235 accuracy= 0.803438395415473"
# [1] "k= 237 accuracy= 0.802578796561605"
# [1] "k= 239 accuracy= 0.802578796561605"
# [1] "k= 241 accuracy= 0.801146131805158"
# [1] "k= 243 accuracy= 0.801432664756447"
# [1] "k= 245 accuracy= 0.801146131805158"
# [1] "k= 247 accuracy= 0.800573065902579"
# [1] "k= 249 accuracy= 0.799426934097421"
# [1] "k= 251 accuracy= 0.797994269340974"

k <- ks[which.max(accuracy)]
# > k
# [1] 5

knn_fit <- knn3(class ~ ., data = train, k = k)
y_hat_knn <- predict(knn_fit, test %>% select(-class), type = "class")
cm <- confusionMatrix(data = y_hat_knn, reference = test$class)
cm$overall["Accuracy"]
cm$byClass["Sensitivity"]
cm$byClass["Specificity"]

# Use k-fold cross validation
# https://rafalab.github.io/dsbook/caret.html#caret-cv
control <- trainControl(method = "cv", number = 10, p = .9)
knn_fit <- train(class ~., data = train, method = "knn", tuneGrid = data.frame(k = seq(1, 251, 2)), trControl = control)
y_hat_knn <- predict(knn_fit, test %>% select(-class), type = "class")
cm <- confusionMatrix(data = y_hat_knn, reference = test$class)
cm$overall["Accuracy"]
cm$byClass["Sensitivity"]
cm$byClass["Specificity"]


## ---------------------------------------------------------------------------------------------------------
## kNN with binary data as factors
## ---------------------------------------------------------------------------------------------------------

knn_fit <- knn3(class ~ ., data = train_fact, k = 5)
y_hat_knn <- predict(knn_fit, test_fact %>% select(-class), type = "class")
cm <- confusionMatrix(data = y_hat_knn, reference = test_fact$class)
cm$overall["Accuracy"]
cm$byClass["Sensitivity"]
cm$byClass["Specificity"]

# Estimate k using F1 score or F Measure
ks <- seq(1, 251, 2)
accuracy <- sapply(ks, function(k) {
   fit <- knn3(class ~ ., data = train_fact, k = k)
   y_hat <- predict(fit, test_fact_subset %>% select(-class), type = "class")
   cm <- confusionMatrix(data = y_hat, reference = test_fact_subset$class)
   debugstr <- paste("k=", k, "accuracy=", cm$overall["Accuracy"], sep = " ", collapse = NULL)
   print(debugstr)
   cm$overall["Accuracy"]
})
# [1] "k= 1 accuracy= 0.965902578796562"
# [1] "k= 3 accuracy= 0.965616045845272"
# [1] "k= 5 accuracy= 0.968194842406877"
# [1] "k= 7 accuracy= 0.964469914040115"
# [1] "k= 9 accuracy= 0.958452722063037"
# [1] "k= 11 accuracy= 0.955300859598854"
k <- ks[which.max(accuracy)]
knn_fit <- knn3(class ~ ., data = train_fact, k = 5)
y_hat_knn <- predict(knn_fit, test_fact %>% select(-class), type = "class")
cm <- confusionMatrix(data = y_hat_knn, reference = test_fact$class)
cm$overall["Accuracy"]
cm$byClass["Sensitivity"]
cm$byClass["Specificity"]

# Use k-fold cross validation
# https://rafalab.github.io/dsbook/caret.html#caret-cv
control <- trainControl(method = "cv", number = 10, p = .9)
knn_fit <- train(class ~., data = train_fact, method = "knn", tuneGrid = data.frame(k = seq(1, 251, 2)), trControl = control)
y_hat_knn <- predict(knn_fit, test_fact %>% select(-class), type = "class")
cm <- confusionMatrix(data = y_hat_knn, reference = test_fact$class)
cm$overall["Accuracy"]
cm$byClass["Sensitivity"]
cm$byClass["Specificity"]

## ---------------------------------------------------------------------------------------------------------
## convert train to matrix
## ---------------------------------------------------------------------------------------------------------

m <- train %>% as.matrix()
x <- m[, c(
  "elevation",
  "aspect",
  "slope",
  "x_hydro",
  "y_hydro",
  "x_road",
  "hs_9am",
  "hs_12pm",
  "hs_3pm",
  "x_fire",
  "wa1",
  "wa2",
  "wa3",
  "wa4",
  "st1",
  "st2",
  "st3",
  "st4",
  "st5",
  "st6",
  "st7",
  "st8",
  "st9",
  "st10",
  "st11",
  "st12",
  "st13",
  "st14",
  "st15",
  "st16",
  "st17",
  "st18",
  "st19",
  "st20",
  "st21",
  "st22",
  "st23",
  "st24",
  "st25",
  "st26",
  "st27",
  "st28",
  "st29",
  "st30",
  "st31",
  "st32",
  "st33",
  "st34",
  "st35",
  "st36",
  "st37",
  "st38",
  "st39",
  "st40"
)]
y <- m[, c("class")]


## ---------------------------------------------------------------------------------------------------------
## convert train_fact to matrix
## ---------------------------------------------------------------------------------------------------------

m_fact <- train_fact %>% as.matrix()
x_fact <- m_fact[, c(
  "elevation",
  "aspect",
  "slope",
  "x_hydro",
  "y_hydro",
  "x_road",
  "hs_9am",
  "hs_12pm",
  "hs_3pm",
  "x_fire",
  "wa1",
  "wa2",
  "wa3",
  "wa4",
  "st1",
  "st2",
  "st3",
  "st4",
  "st5",
  "st6",
  "st7",
  "st8",
  "st9",
  "st10",
  "st11",
  "st12",
  "st13",
  "st14",
  "st15",
  "st16",
  "st17",
  "st18",
  "st19",
  "st20",
  "st21",
  "st22",
  "st23",
  "st24",
  "st25",
  "st26",
  "st27",
  "st28",
  "st29",
  "st30",
  "st31",
  "st32",
  "st33",
  "st34",
  "st35",
  "st36",
  "st37",
  "st38",
  "st39",
  "st40"
)]
y_fact <- m_fact[, c("class")]

## ---------------------------------------------------------------------------------------------------------
## Test train columns using t-statistic
## https://courses.edx.org/courses/course-v1:HarvardX+PH125.8x+2T2019/courseware/7c50a2fc6d44433fa902be8ed097f301/2faa56e611464cf4bfd42b13ac7fffa5/?child=first
## ---------------------------------------------------------------------------------------------------------

library(genefilter)
tt <- colttests(x, factor(y))
#> tt
#statistic           dm      p.value
#age     2.28515853   4.09627374 2.282101e-02
#gender  0.84640221   0.03865449 3.978295e-01
#tb      4.95195931   2.85690010 1.081563e-06
#db      5.25656029   1.50089604 2.382842e-07
#tp      3.39513849  86.69844860 7.538466e-04
#alb     3.78731537  72.54501957 1.754193e-04
#agr     4.34306975 103.30856894 1.778914e-05
#sgpt   -0.05101323  -0.00600551 9.593402e-01
#sgot   -2.29591342  -0.19409018 2.219216e-02
#alk    -2.39540212  -0.08313760 1.705654e-02
pvals <- tt$p.value
ind <- which(pvals <= 0.01)
# > tt[ind,]
#     statistic         dm      p.value
# tb   4.951959   2.856900 1.081563e-06
# db   5.256560   1.500896 2.382842e-07
# tp   3.395138  86.698449 7.538466e-04
# alb  3.787315  72.545020 1.754193e-04
# agr  4.343070 103.308569 1.778914e-05


## ---------------------------------------------------------------------------------------------------------
## Test train_fact columns using t-statistic
## https://courses.edx.org/courses/course-v1:HarvardX+PH125.8x+2T2019/courseware/7c50a2fc6d44433fa902be8ed097f301/2faa56e611464cf4bfd42b13ac7fffa5/?child=first
## ---------------------------------------------------------------------------------------------------------

library(genefilter)
tt_fact <- colttests(x_fact, factor(y_fact))
#> tt_fact
#statistic           dm      p.value
#age     2.28515853   4.09627374 2.282101e-02
#gender  0.84640221   0.03865449 3.978295e-01
#tb      4.95195931   2.85690010 1.081563e-06
#db      5.25656029   1.50089604 2.382842e-07
#tp      3.39513849  86.69844860 7.538466e-04
#alb     3.78731537  72.54501957 1.754193e-04
#agr     4.34306975 103.30856894 1.778914e-05
#sgpt   -0.05101323  -0.00600551 9.593402e-01
#sgot   -2.29591342  -0.19409018 2.219216e-02
#alk    -2.39540212  -0.08313760 1.705654e-02
pvals <- tt_fact$p.value
ind <- which(pvals <= 0.01)
# > tt_fact[ind,]
#     statistic         dm      p.value
# tb   4.951959   2.856900 1.081563e-06
# db   5.256560   1.500896 2.382842e-07
# tp   3.395138  86.698449 7.538466e-04
# alb  3.787315  72.545020 1.754193e-04
# agr  4.343070 103.308569 1.778914e-05


## ---------------------------------------------------------------------------------------------------------
## Fit train with lda
## ---------------------------------------------------------------------------------------------------------

fit_lda <- train(class ~ ., data = train, method="lda")
y_hat_lda <- predict(fit_lda, test %>% select(-class))
cm <- confusionMatrix(data = y_hat_lda, reference = test$class)
# > cm
# Confusion Matrix and Statistics

# Reference
# Prediction    5    2    1    7    3    6    4
#          5   13   29    1    0    1    4    0
#          2   40 1301  324    0   14   20    0
#          1    0  311  820   22    0    0    0
#          7    0    6  125  101    0    0    0
#          3    4   19    2    1  104   24    6
#          6    0   23    0    0   80   55    4
#          4    0   11    0    0   16    2    7

# Overall Statistics

# Accuracy : 0.688           
# 95% CI : (0.6723, 0.7033)
# No Information Rate : 0.4871          
# P-Value [Acc > NIR] : < 2.2e-16       

# Kappa : 0.5091          

# Mcnemar's Test P-Value : NA              

# Statistics by Class:

#                      Class: 5 Class: 2 Class: 1 Class: 7 Class: 3 Class: 6 Class: 4
# Sensitivity          0.228070   0.7653   0.6447  0.81452  0.48372  0.52381 0.411765
# Specificity          0.989805   0.7777   0.8499  0.96108  0.98290  0.96839 0.991650
# Pos Pred Value       0.270833   0.7657   0.7112  0.43534  0.65000  0.33951 0.194444
# Neg Pred Value       0.987217   0.7772   0.8066  0.99294  0.96667  0.98498 0.997105
# Prevalence           0.016332   0.4871   0.3645  0.03553  0.06160  0.03009 0.004871
# Detection Rate       0.003725   0.3728   0.2350  0.02894  0.02980  0.01576 0.002006
# Detection Prevalence 0.013754   0.4868   0.3304  0.06648  0.04585  0.04642 0.010315
# Balanced Accuracy    0.608938   0.7715   0.7473  0.88780  0.73331  0.74610 0.701707


## ---------------------------------------------------------------------------------------------------------
## Fit train_fact with lda
## ---------------------------------------------------------------------------------------------------------

fit_lda <- train(class ~ ., data = train_fact, method="lda")
y_hat_lda <- predict(fit_lda, test_fact %>% select(-class))
cm <- confusionMatrix(data = y_hat_lda, reference = test_fact$class)
cm$overall["Accuracy"]
cm$byClass["Sensitivity"]
cm$byClass["Specificity"]

# > cm
# Confusion Matrix and Statistics
#
#             Reference
#            Prediction   Aspen Cottonwood Douglas Krummholz Lodgepole Panderosa Spruce
#              Aspen         13          0       4         0        29         1      1
#              Cottonwood     0          7       2         0        11        16      0
#              Douglas        0          4      55         0        23        80      0
#              Krummholz      0          0       0       101         6         0    125
#              Lodgepole     40          0      20         0      1301        14    324
#              Panderosa      4          6      24         1        19       104      2
#              Spruce         0          0       0        22       311         0    820

# Overall Statistics
                                          
#                Accuracy : 0.688           
#                  95% CI : (0.6723, 0.7033)
#     No Information Rate : 0.4871          
#     P-Value [Acc > NIR] : < 2.2e-16       
                                          
#                   Kappa : 0.5091          
                                          
#  Mcnemar's Test P-Value : NA              

# Statistics by Class:
  
#                            Class: Aspen Class: Cottonwood Class: Douglas Class: Krummholz Class: Lodgepole Class: Panderosa Class: Spruce
# Sensitivity                    0.228070          0.411765        0.52381          0.81452           0.7653          0.48372        0.6447
# Specificity                    0.989805          0.991650        0.96839          0.96108           0.7777          0.98290        0.8499
# Pos Pred Value                 0.270833          0.194444        0.33951          0.43534           0.7657          0.65000        0.7112
# Neg Pred Value                 0.987217          0.997105        0.98498          0.99294           0.7772          0.96667        0.8066
# Prevalence                     0.016332          0.004871        0.03009          0.03553           0.4871          0.06160        0.3645
# Detection Rate                 0.003725          0.002006        0.01576          0.02894           0.3728          0.02980        0.2350
# Detection Prevalence           0.013754          0.010315        0.04642          0.06648           0.4868          0.04585        0.3304
# Balanced Accuracy              0.608938          0.701707        0.74610          0.88780           0.7715          0.73331        0.7473


## ---------------------------------------------------------------------------------------------------------
## Get cond dist with train for naive bayes
## ---------------------------------------------------------------------------------------------------------

fit <- naive_bayes(class ~ ., train)
get_cond_dist(fit)
y_hat <- predict(fit, test %>% select(-class), type = "class")
cm <- confusionMatrix(data = y_hat, reference = test$class)
# > cm
# Confusion Matrix and Statistics

# Reference
# Prediction     5     2     1     7     3     6     4
#          5   558 22242  8790     6    10    59     0
#          2     0     0     0     0     0     0     0
#          1     0   151    17     0     0     0     0
#          7     4 12883 19214  4474     0     0     0
#          3  1850 43081 35134  1673  4211  1818     9
#          6     0    48     5     0     0     5     0
#          4   436  6586   392     0  6506  3329   816

# Overall Statistics

# Accuracy : 0.0578          
# 95% CI : (0.0567, 0.0589)
# No Information Rate : 0.4876          
# P-Value [Acc > NIR] : 1               

# Kappa : 0.0163          

# Mcnemar's Test P-Value : NA              

# Statistics by Class:

#                      Class: 5 Class: 2  Class: 1 Class: 7 Class: 3  Class: 6 Class: 4
# Sensitivity          0.195927   0.0000 2.675e-04  0.72712  0.39256 9.595e-04 0.989091
# Specificity          0.818575   1.0000 9.986e-01  0.80910  0.48915 9.997e-01 0.900572
# Pos Pred Value       0.017622      NaN 1.012e-01  0.12232  0.04797 8.621e-02 0.045170
# Neg Pred Value       0.983946   0.5124 6.351e-01  0.98781  0.92470 9.701e-01 0.999942
# Prevalence           0.016339   0.4876 3.646e-01  0.03530  0.06154 2.990e-02 0.004733
# Detection Rate       0.003201   0.0000 9.753e-05  0.02567  0.02416 2.869e-05 0.004681
# Detection Prevalence 0.181662   0.0000 9.638e-04  0.20983  0.50357 3.327e-04 0.103639
# Balanced Accuracy    0.507251   0.5000 4.995e-01  0.76811  0.44085 5.003e-01 0.944831


## ---------------------------------------------------------------------------------------------------------
## Get cond dist with train_fact for naive bayes
## ---------------------------------------------------------------------------------------------------------

fit <- naive_bayes(class ~ ., train_fact)
get_cond_dist(fit)
#  elevation      aspect       slope     x_hydro     y_hydro      x_road      hs_9am     hs_12pm      hs_3pm      x_fire         wa1 
# "Gaussian"  "Gaussian"  "Gaussian"  "Gaussian"  "Gaussian"  "Gaussian"  "Gaussian"  "Gaussian"  "Gaussian"  "Gaussian" "Bernoulli" 
#        wa2         wa3         wa4         st1         st2         st3         st4         st5         st6         st7         st8 
# "Bernoulli" "Bernoulli" "Bernoulli" "Bernoulli" "Bernoulli" "Bernoulli" "Bernoulli" "Bernoulli" "Bernoulli" "Bernoulli" "Bernoulli" 
#        st9        st10        st11        st12        st13        st14        st15        st16        st17        st18        st19 
# "Bernoulli" "Bernoulli" "Bernoulli" "Bernoulli" "Bernoulli" "Bernoulli" "Bernoulli" "Bernoulli" "Bernoulli" "Bernoulli" "Bernoulli" 
#       st20        st21        st22        st23        st24        st25        st26        st27        st28        st29        st30 
# "Bernoulli" "Bernoulli" "Bernoulli" "Bernoulli" "Bernoulli" "Bernoulli" "Bernoulli" "Bernoulli" "Bernoulli" "Bernoulli" "Bernoulli" 
#       st31        st32        st33        st34        st35        st36        st37        st38        st39        st40 
# "Bernoulli" "Bernoulli" "Bernoulli" "Bernoulli" "Bernoulli" "Bernoulli" "Bernoulli" "Bernoulli" "Bernoulli" "Bernoulli" 
y_hat <- predict(fit, test_fact %>% select(-class), type = "class")
cm <- confusionMatrix(data = y_hat, reference = test_fact$class)
# > cm
# Confusion Matrix and Statistics

# Reference
# Prediction   Aspen Cottonwood Douglas Krummholz Lodgepole Panderosa Spruce
#   Aspen       1034          0     130        10      3786       413    642
#   Cottonwood     0        574     182         0        10       933      0
#   Douglas      115         79    2425         0      2270      1576    215
#   Krummholz      0          0       0      4160      1095         0   4435
#   Lodgepole   1555          0     593        55     56625       696  16038
#   Panderosa     80        172    1881         0      1797      7109     67
#   Spruce        64          0       0      1928     19408         0  42155

# Overall Statistics

# Accuracy : 0.6545          
# 95% CI : (0.6523, 0.6567)
# No Information Rate : 0.4876          
# P-Value [Acc > NIR] : < 2.2e-16       

# Kappa : 0.4668          

# Mcnemar's Test P-Value : NA              

# Statistics by Class:

#                      Class: Aspen Class: Cottonwood Class: Douglas Class: Krummholz Class: Lodgepole Class: Panderosa Class: Spruce
# Sensitivity              0.363062          0.695758        0.46536          0.67609           0.6662          0.66272        0.6633
# Specificity              0.970949          0.993515        0.97484          0.96711           0.7880          0.97557        0.8068
# Pos Pred Value           0.171904          0.337846        0.36302          0.42931           0.7494          0.64010        0.6633
# Neg Pred Value           0.989221          0.998546        0.98338          0.98789           0.7127          0.97783        0.8068
# Prevalence               0.016339          0.004733        0.02990          0.03530           0.4876          0.06154        0.3646
# Detection Rate           0.005932          0.003293        0.01391          0.02387           0.3249          0.04078        0.2418
# Detection Prevalence     0.034508          0.009747        0.03832          0.05559           0.4335          0.06372        0.3646
# Balanced Accuracy        0.667006          0.844636        0.72010          0.82160           0.7271          0.81914        0.7350



## ---------------------------------------------------------------------------------------------------------
## Fit train with naive bayes
## ---------------------------------------------------------------------------------------------------------

params <- expand.grid(kernel <- c("gaussian", "epanechnikov", "rectangular", "triangular", "biweight", "cosine", "optcosine"), bandwidth <- c("nrd0", "nrd", "ucv", "bcv"), stringsAsFactors = FALSE)
nb <- sapply(1:nrow(params), simplify = FALSE, function(i) {
   kernel <- params[i,1]
   bandwidth <- params[i,2]
   adjusts <- seq(0.5, 5, 0.1)
   accuracy <- sapply(adjusts, function(adjust) {
     fit <- naive_bayes(class ~ ., train_fact, usekernel = TRUE, kernel = kernel, bw = bandwidth, adjust = adjust)
     y_hat <- predict(fit, test_fact %>% select(-class), type = "class")
     cm <- confusionMatrix(data = y_hat, reference = test_fact$class)
     # debugstr <- paste("adjust=", adjust, "accuracy=", cm$overall["Accuracy"], sep = " ", collapse = NULL)
     # print(debugstr)
     cm$overall["Accuracy"]
   })
   adjust <- adjusts[which.max(accuracy)]
   fit <- naive_bayes(class ~ ., train_fact, usekernel = TRUE, kernel = kernel, bw = bandwidth, adjust = adjust)
   y_hat <- predict(fit, test_fact %>% select(-class), type = "class")
   cm <- confusionMatrix(data = y_hat, reference = test_fact$class)
   debugstr <- paste("kernel=", kernel, "bandwidth=", bandwidth, "adjust=", adjust, "accuracy=", cm$overall["Accuracy"], sep = " ", collapse = NULL)
   print(debugstr)
   list(kernel = kernel, bandwidth = bandwidth, adjust = adjust, accuracy = cm$overall["Accuracy"])
})
# [1] "kernel= gaussian bandwidth= nrd0 adjust= 0.5 accuracy= 0.687241476245934"
# [1] "kernel= epanechnikov bandwidth= nrd0 adjust= 5 accuracy= 0.682651872845038"
# [1] "kernel= rectangular bandwidth= nrd0 adjust= 4.4 accuracy= 0.682605976811029"
# [1] "kernel= triangular bandwidth= nrd0 adjust= 5 accuracy= 0.682898564027836"
# [1] "kernel= biweight bandwidth= nrd0 adjust= 5 accuracy= 0.683030515125612"
# [1] "kernel= cosine bandwidth= nrd0 adjust= 5 accuracy= 0.683185414240392"
# [1] "kernel= optcosine bandwidth= nrd0 adjust= 5 accuracy= 0.682732190904554"
# [1] "kernel= gaussian bandwidth= nrd adjust= 0.5 accuracy= 0.685864595225665"
# [1] "kernel= epanechnikov bandwidth= nrd adjust= 4.8 accuracy= 0.683214099261648"
# [1] "kernel= rectangular bandwidth= nrd adjust= 4.6 accuracy= 0.683271469304159"
# [1] "kernel= triangular bandwidth= nrd adjust= 4.7 accuracy= 0.683632900571979"
# [1] "kernel= biweight bandwidth= nrd adjust= 4.6 accuracy= 0.683632900571979"
# [1] "kernel= cosine bandwidth= nrd adjust= 4.9 accuracy= 0.683713218631495"
# [1] "kernel= optcosine bandwidth= nrd adjust= 4.4 accuracy= 0.683443579431692"
# [1] "kernel= gaussian bandwidth= ucv adjust= 0.5 accuracy= 0.696764903302793" *BEST PERFORMER
# [1] "kernel= epanechnikov bandwidth= ucv adjust= 0.5 accuracy= 0.686897255990867"
# [1] "kernel= rectangular bandwidth= ucv adjust= 2.4 accuracy= 0.686518613710293"
# [1] "kernel= triangular bandwidth= ucv adjust= 0.5 accuracy= 0.693477599866901"
# [1] "kernel= biweight bandwidth= ucv adjust= 0.5 accuracy= 0.691561440447027"
# [1] "kernel= cosine bandwidth= ucv adjust= 0.5 accuracy= 0.693552180922166"
# [1] "kernel= optcosine bandwidth= ucv adjust= 0.5 accuracy= 0.68829708502814"
# [1] "kernel= gaussian bandwidth= bcv adjust= 5 accuracy= 0.684734405388194"
# [1] "kernel= epanechnikov bandwidth= bcv adjust= 4.9 accuracy= 0.68210112043693"
# [1] "kernel= rectangular bandwidth= bcv adjust= 4.9 accuracy= 0.681458575960805"
# [1] "kernel= triangular bandwidth= bcv adjust= 4.9 accuracy= 0.682990356095854"
# [1] "kernel= biweight bandwidth= bcv adjust= 4.9 accuracy= 0.683259995295657"
# [1] "kernel= cosine bandwidth= bcv adjust= 5 accuracy= 0.683346050359423"
# [1] "kernel= optcosine bandwidth= bcv adjust= 4.9 accuracy= 0.682537132760015"

kernel <- sapply(1:length(nb), function(i) {
   nb[[i]]$kernel
})
bandwidth <- sapply(1:length(nb), function(i) {
   nb[[i]]$bandwidth
})
adjust <- sapply(1:length(nb), function(i) {
   nb[[i]]$adjust
})
accuracy <- sapply(1:length(nb), function(i) {
   nb[[i]]$accuracy
})
kernel <- kernel[which.max(accuracy)]
bandwidth <- bandwidth[which.max(accuracy)]
adjust <- adjust[which.max(accuracy)]

# > kernel <- kernel[which.max(accuracy)]
# > kernel
# [1] "gaussian"
# > bandwidth <- bandwidth[which.max(accuracy)]
# > bandwidth
# [1] "ucv"
# > adjust <- adjust[which.max(accuracy)]
# > adjust
# [1] 0.5


## rpart on train
## https://rafalab.github.io/dsbook/examples-of-algorithms.html#the-curse-of-dimensionality

fit <- rpart(class ~ ., data = train)
plot(fit, margin = 0.1)
text(fit, cex = 0.75)
y_hat <- predict(fit, test %>% select(-class), type = "class")
cm <- confusionMatrix(data = y_hat, reference = test$class)

## rpart on train_fact
## https://rafalab.github.io/dsbook/examples-of-algorithms.html#the-curse-of-dimensionality

fit <- rpart(class ~ ., data = train_fact)
plot(fit, margin = 0.1)
text(fit, cex = 0.75)
y_hat <- predict(fit, test_fact %>% select(-class), type = "class")
cm <- confusionMatrix(data = y_hat, reference = test_fact$class)
# > cm
# Confusion Matrix and Statistics

# Reference
# Prediction   Aspen Cottonwood Douglas Krummholz Lodgepole Panderosa Spruce
# Aspen          0          0       0         0         0         0      0
# Cottonwood     0          0       0         0         0         0      0
# Douglas        0          0       0         0         0         0      0
# Krummholz      0          0       0         0         0         0      0
# Lodgepole   2818          1    1609        24     63661      3389  17497
# Panderosa     30        824    3602         0       976      7338      1
# Spruce         0          0       0      6129     20354         0  46054

# Overall Statistics

# Accuracy : 0.6715          
# 95% CI : (0.6693, 0.6737)
# No Information Rate : 0.4876          
# P-Value [Acc > NIR] : < 2.2e-16       

# Kappa : 0.4478          

# Mcnemar's Test P-Value : NA              

# Statistics by Class:

#                      Class: Aspen Class: Cottonwood Class: Douglas Class: Krummholz Class: Lodgepole Class: Panderosa Class: Spruce
# Sensitivity               0.00000          0.000000         0.0000           0.0000           0.7490          0.68407        0.7247
# Specificity               1.00000          1.000000         1.0000           1.0000           0.7163          0.96679        0.7609
# Pos Pred Value                NaN               NaN            NaN              NaN           0.7153          0.57458        0.6349
# Neg Pred Value            0.98366          0.995267         0.9701           0.9647           0.7500          0.97902        0.8281
# Prevalence                0.01634          0.004733         0.0299           0.0353           0.4876          0.06154        0.3646
# Detection Rate            0.00000          0.000000         0.0000           0.0000           0.3652          0.04210        0.2642
# Detection Prevalence      0.00000          0.000000         0.0000           0.0000           0.5106          0.07327        0.4161
# Balanced Accuracy         0.50000          0.500000         0.5000           0.5000           0.7327          0.82543        0.7428

train_rpart <- train(class ~ ., method = "rpart", tuneGrid = data.frame(cp = seq(0, 0.05, len = 25)), data = train_fact)
ggplot(train_rpart)
plot(train_rpart$finalModel, margin = 0.1)
text(train_rpart$finalModel, cex = 0.75)
train_rpart$bestTune

## randomForest

set.seed(2, sample.kind="Rounding")
index <- createDataPartition(y = train_fact$class, times = 1, p = 0.02, list = FALSE)
train_fact_subset <- train_fact[index,]

fit <- randomForest(class ~ ., data = train_fact_subset)
## Error: vector memory exhausted (limit reached?)
plot(fit)
y_hat <- predict(fit, test_fact %>% select(-class), type = "class")
cm <- confusionMatrix(data = y_hat, reference = test_fact$class)
# > cm
# Confusion Matrix and Statistics

# Reference
# Prediction   Aspen Cottonwood Douglas Krummholz Lodgepole Panderosa Spruce
# Aspen        302          0       0         0        66         0     11
# Cottonwood     0        215       9         0         3         8      0
# Douglas        1          4    1055         0       108       178      2
# Krummholz      0          0       0      3629        53         0    409
# Lodgepole   2378          0    1466        41     72485      1241  14434
# Panderosa     98        606    2676         0       840      9300     43
# Spruce        69          0       5      2483     11436         0  48653

# Overall Statistics

# Accuracy : 0.7782          
# 95% CI : (0.7762, 0.7801)
# No Information Rate : 0.4876          
# P-Value [Acc > NIR] : < 2.2e-16       

# Kappa : 0.6337          

# Mcnemar's Test P-Value : NA              

# Statistics by Class:

#                      Class: Aspen Class: Cottonwood Class: Douglas Class: Krummholz Class: Lodgepole Class: Panderosa Class: Spruce
# Sensitivity              0.106039          0.260606       0.202456          0.58979           0.8529          0.86697        0.7656
# Specificity              0.999551          0.999885       0.998267          0.99725           0.7810          0.97394        0.8737
# Pos Pred Value           0.796834          0.914894       0.782641          0.88707           0.7875          0.68569        0.7766
# Neg Pred Value           0.985362          0.996496       0.975971          0.98517           0.8480          0.99112        0.8666
# Prevalence               0.016339          0.004733       0.029896          0.03530           0.4876          0.06154        0.3646
# Detection Rate           0.001733          0.001233       0.006053          0.02082           0.4158          0.05335        0.2791
# Detection Prevalence     0.002174          0.001348       0.007733          0.02347           0.5281          0.07781        0.3594
# Balanced Accuracy        0.552795          0.630245       0.600362          0.79352           0.8169          0.92046        0.8196


set.seed(2, sample.kind="Rounding")
index <- createDataPartition(y = train_fact$class, times = 1, p = 0.2, list = FALSE)
train_fact_subset <- train_fact[index,]

fit <- randomForest(class ~ ., data = train_fact_subset)
## Error: vector memory exhausted (limit reached?)
plot(fit)
y_hat <- predict(fit, test_fact %>% select(-class), type = "class")
cm <- confusionMatrix(data = y_hat, reference = test_fact$class)
# > cm
# Confusion Matrix and Statistics

# Reference
# Prediction   Aspen Cottonwood Douglas Krummholz Lodgepole Panderosa Spruce
# Aspen          433          0       0         0        20         7      2
# Cottonwood       0        409      34         0         5        36      0
# Douglas          2          7    1596         0        63       132      3
# Krummholz        0          0       0      4291        35         0    348
# Lodgepole     2182          0    1243        35     75199       854  12083
# Panderosa      105        409    2331         0       815      9698     34
# Spruce         126          0       7      1827      8854         0  51082

# Overall Statistics

# Accuracy : 0.8187          
# 95% CI : (0.8169, 0.8205)
# No Information Rate : 0.4876          
# P-Value [Acc > NIR] : < 2.2e-16       

# Kappa : 0.702           

# Mcnemar's Test P-Value : NA              

# Statistics by Class:

#                      Class: Aspen Class: Cottonwood Class: Douglas Class: Krummholz Class: Lodgepole Class: Panderosa Class: Spruce
# Sensitivity              0.152037          0.495758       0.306275          0.69738           0.8848          0.90407        0.8038
# Specificity              0.999831          0.999568       0.998776          0.99772           0.8164          0.97742        0.9024
# Pos Pred Value           0.937229          0.845041       0.885191          0.91806           0.8210          0.72416        0.8253
# Neg Pred Value           0.986108          0.997607       0.979044          0.98902           0.8816          0.99361        0.8891
# Prevalence               0.016339          0.004733       0.029896          0.03530           0.4876          0.06154        0.3646
# Detection Rate           0.002484          0.002346       0.009156          0.02462           0.4314          0.05564        0.2931
# Detection Prevalence     0.002650          0.002777       0.010344          0.02681           0.5255          0.07683        0.3551
# Balanced Accuracy        0.575934          0.747663       0.652526          0.84755           0.8506          0.94075        0.8531


set.seed(2, sample.kind="Rounding")
index <- createDataPartition(y = train_fact$class, times = 1, p = 0.4, list = FALSE)
train_fact_subset <- train_fact[index,]
# > nrow(train_fact_subset)
# [1] 162684

fit <- randomForest(class ~ ., data = train_fact_subset)
## Error: vector memory exhausted (limit reached?)
plot(fit)
y_hat <- predict(fit, test_fact %>% select(-class), type = "class")
cm <- confusionMatrix(data = y_hat, reference = test_fact$class)
# > cm
# Confusion Matrix and Statistics

# Reference
# Prediction   Aspen Cottonwood Douglas Krummholz Lodgepole Panderosa Spruce
#   Aspen        529          0       0         0        31         2      5
#   Cottonwood     0        417      23         0         2        25      0
#   Douglas        3          6    1909         0       119       165      3
#   Krummholz      0          0       0      4490        36         0    342
#   Lodgepole   2076          0    1000        31     75777       773  11358
#   Panderosa    107        402    2269         1       772      9762     39
#   Spruce       133          0      10      1631      8254         0  51805

# Overall Statistics

# Accuracy : 0.8301          
# 95% CI : (0.8283, 0.8318)
# No Information Rate : 0.4876          
# P-Value [Acc > NIR] : < 2.2e-16       

# Kappa : 0.7214          

# Mcnemar's Test P-Value : NA              

# Statistics by Class:

#                      Class: Aspen Class: Cottonwood Class: Douglas Class: Krummholz Class: Lodgepole Class: Panderosa Class: Spruce
# Sensitivity              0.185744          0.505455        0.36634          0.72973           0.8916          0.91004        0.8152
# Specificity              0.999778          0.999712        0.99825          0.99775           0.8294          0.97805        0.9095
# Pos Pred Value           0.932981          0.892934        0.86576          0.92235           0.8326          0.73113        0.8378
# Neg Pred Value           0.986652          0.997653        0.98081          0.99019           0.8894          0.99400        0.8956
# Prevalence               0.016339          0.004733        0.02990          0.03530           0.4876          0.06154        0.3646
# Detection Rate           0.003035          0.002392        0.01095          0.02576           0.4347          0.05600        0.2972
# Detection Prevalence     0.003253          0.002679        0.01265          0.02793           0.5222          0.07660        0.3547
# Balanced Accuracy        0.592761          0.752583        0.68229          0.86374           0.8605          0.94405        0.8623


set.seed(2, sample.kind="Rounding")
index <- createDataPartition(y = train_fact$class, times = 1, p = 0.8, list = FALSE)
train_fact_subset <- train_fact[index,]
# > nrow(train_fact_subset)
# [1] 325366

fit <- randomForest(class ~ ., data = train_fact_subset)
## Error: vector memory exhausted (limit reached?)
plot(fit)
y_hat <- predict(fit, test_fact %>% select(-class), type = "class")
cm <- confusionMatrix(data = y_hat, reference = test_fact$class)
# > cm
# Confusion Matrix and Statistics

# Reference
# Prediction   Aspen Cottonwood Douglas Krummholz Lodgepole Panderosa Spruce
#   Aspen        563          0       0         0        26         3      2
#   Cottonwood     0        460      28         0         5        32      0
#   Douglas        4          1    2013         0        82       144      3
#   Krummholz      0          0       0      4597        32         0    336
#   Lodgepole   2038          0     915        28     76205       752  10860
#   Panderosa    107        364    2245         0       725      9796     32
#   Spruce       136          0      10      1528      7916         0  52319

# Overall Statistics

# Accuracy : 0.8373          
# 95% CI : (0.8356, 0.8391)
# No Information Rate : 0.4876          
# P-Value [Acc > NIR] : < 2.2e-16       

# Kappa : 0.7335          

# Mcnemar's Test P-Value : NA              

# Statistics by Class:

#                      Class: Aspen Class: Cottonwood Class: Douglas Class: Krummholz Class: Lodgepole Class: Panderosa Class: Spruce
# Sensitivity              0.197683          0.557576        0.38630          0.74712           0.8966          0.91321        0.8232
# Specificity              0.999819          0.999625        0.99862          0.99781           0.8366          0.97877        0.9134
# Pos Pred Value           0.947811          0.876190        0.89586          0.92588           0.8393          0.73826        0.8451
# Neg Pred Value           0.986846          0.997900        0.98141          0.99081           0.8948          0.99422        0.9001
# Prevalence               0.016339          0.004733        0.02990          0.03530           0.4876          0.06154        0.3646
# Detection Rate           0.003230          0.002639        0.01155          0.02637           0.4372          0.05620        0.3002
# Detection Prevalence     0.003408          0.003012        0.01289          0.02848           0.5209          0.07612        0.3552
# Balanced Accuracy        0.598751          0.778601        0.69246          0.87246           0.8666          0.94599        0.8683


set.seed(2, sample.kind="Rounding")
index <- createDataPartition(y = train_fact$class, times = 1, p = 0.85, list = FALSE)
train_fact_subset <- train_fact[index,]
# > nrow(train_fact_subset)
# [1] 345702

fit <- randomForest(class ~ ., data = train_fact_subset)
## Error: vector memory exhausted (limit reached?)
plot(fit)
y_hat <- predict(fit, test_fact %>% select(-class), type = "class")
cm <- confusionMatrix(data = y_hat, reference = test_fact$class)
# > cm
# Confusion Matrix and Statistics

#             Reference
# Prediction   Aspen Cottonwood Douglas Krummholz Lodgepole Panderosa Spruce
#   Aspen        604          0       0         0        26         3      2
#   Cottonwood     0        481      27         0         6        35      0
#   Douglas        4          2    2075         0        83       143      3
#   Krummholz      0          0       0      4627        31         0    275
#   Lodgepole   1986          0     969        24     76519       787  10826
#   Panderosa    113        342    2132         2       699      9759     32
#   Spruce       141          0       8      1500      7627         0  52414

# Overall Statistics
                                          
#                Accuracy : 0.8404          
#                  95% CI : (0.8386, 0.8421)
#     No Information Rate : 0.4876          
#     P-Value [Acc > NIR] : < 2.2e-16       
                                          
#                   Kappa : 0.7383          
                                          
#  Mcnemar's Test P-Value : NA              

# Statistics by Class:
  
#                      Class: Aspen Class: Cottonwood Class: Douglas Class: Krummholz Class: Lodgepole Class: Panderosa Class: Spruce
# Sensitivity              0.212079          0.583030        0.39820          0.75199           0.9003          0.90976        0.8247
# Specificity              0.999819          0.999608        0.99861          0.99818           0.8366          0.97970        0.9162
# Pos Pred Value           0.951181          0.876138        0.89827          0.93797           0.8398          0.74616        0.8496
# Neg Pred Value           0.987079          0.998020        0.98177          0.99099           0.8982          0.99400        0.9011
# Prevalence               0.016339          0.004733        0.02990          0.03530           0.4876          0.06154        0.3646
# Detection Rate           0.003465          0.002759        0.01190          0.02655           0.4390          0.05599        0.3007
# Detection Prevalence     0.003643          0.003150        0.01325          0.02830           0.5227          0.07503        0.3539
# Balanced Accuracy        0.605949          0.791319        0.69840          0.87509           0.8685          0.94473        0.8705

## randomForest by Rborist
## https://rafalab.github.io/dsbook/examples-of-algorithms.html#the-curse-of-dimensionality
## https://cran.r-project.org/web/packages/randomForest/randomForest.pdf

set.seed(2, sample.kind="Rounding")
index <- createDataPartition(y = train_fact$class, times = 1, p = 0.02, list = FALSE)
train_fact_subset <- train_fact[index,]
# > nrow(train_fact_subset)
# [1] 8138

fit <- randomForest(train_fact_subset %>% select(-class), train_fact_subset$class, ntree=500, nodesize = 5, importance=TRUE)


# randomForest(x, y=NULL,  xtest=NULL, ytest=NULL, ntree=500,
#              mtry=if (!is.null(y) && !is.factor(y))
#              max(floor(ncol(x)/3), 1) else floor(sqrt(ncol(x))),
#              replace=TRUE, classwt=NULL, cutoff, strata,
#              sampsize = if (replace) nrow(x) else ceiling(.632*nrow(x)),
#              nodesize = if (!is.null(y) && !is.factor(y)) 5 else 1,
#              maxnodes = NULL,
#              importance=FALSE, localImp=FALSE, nPerm=1,
#              proximity, oob.prox=proximity,
#              norm.votes=TRUE, do.trace=FALSE,
#              keep.forest=!is.null(y) && is.null(xtest), corr.bias=FALSE,
#              keep.inbag=FALSE, ...)

y_hat <- predict(fit, test_fact %>% select(-class))
cm <- confusionMatrix(data = y_hat, reference = test_fact$class)
# > cm
# Confusion Matrix and Statistics

# Reference
# Prediction   Aspen Cottonwood Douglas Krummholz Lodgepole Panderosa Spruce
#   Aspen        291          0       0         0        60         0      4
#   Cottonwood     0        230      30         0         7        53      0
#   Douglas        3         19     856         0       129       154      2
#   Krummholz      0          0       0      3348        47         0    390
#   Lodgepole   2305          0    1486        16     71298       960  15033
#   Panderosa    142        576    2835        21      1211      9560     41
#   Spruce       107          0       4      2768     12239         0  48082

# Overall Statistics

# Accuracy : 0.7668          
# 95% CI : (0.7648, 0.7688)
# No Information Rate : 0.4876          
# P-Value [Acc > NIR] : < 2.2e-16       

# Kappa : 0.6158          

# Mcnemar's Test P-Value : NA              

# Statistics by Class:

#                      Class: Aspen Class: Cottonwood Class: Douglas Class: Krummholz Class: Lodgepole Class: Panderosa Class: Spruce
# Sensitivity              0.102177          0.278788       0.164268          0.54412           0.8389          0.89121        0.7566
# Specificity              0.999627          0.999481       0.998184          0.99740           0.7783          0.97050        0.8635
# Pos Pred Value           0.819718          0.718750       0.736028          0.88454           0.7827          0.66453        0.7608
# Neg Pred Value           0.985301          0.996580       0.974848          0.98355           0.8354          0.99270        0.8608
# Prevalence               0.016339          0.004733       0.029896          0.03530           0.4876          0.06154        0.3646
# Detection Rate           0.001669          0.001320       0.004911          0.01921           0.4090          0.05485        0.2758
# Detection Prevalence     0.002037          0.001836       0.006672          0.02171           0.5226          0.08253        0.3626
# Balanced Accuracy        0.550902          0.639135       0.581226          0.77076           0.8086          0.93085        0.8100

fit <- randomForest(train_fact_subset %>% select(-class), train_fact_subset$class, ntree=500, nodesize = 1, importance=TRUE)
y_hat <- predict(fit, test_fact %>% select(-class))
cm <- confusionMatrix(data = y_hat, reference = test_fact$class)

fit <- train(class ~ ., method = "Rborist", tuneGrid = data.frame(predFixed = 2, minNode = c(3, 50)), data = train_fact_subset)
# > fit$bestTune
# predFixed minNode
# 1         2       3
plot(fit)
y_hat <- predict(fit, test_fact %>% select(-class))
cm <- confusionMatrix(data = y_hat, reference = test_fact$class)

## Ensemble
## https://courses.edx.org/courses/course-v1:HarvardX+PH125.8x+2T2019/courseware/a49844e4a3574c239f354654f9679888/638b5f585b604ae187b32d1e089fccb8/?child=first

set.seed(2, sample.kind="Rounding")
index <- createDataPartition(y = train_fact$class, times = 1, p = 0.02, list = FALSE)
train_fact_subset <- train_fact[index,]

## multinom
fit <- train(y ~ ., data = dat$train, method="multinom")
# > fit$bestTune
# decay
# 3   0.1
# > fit$results
# decay  Accuracy     Kappa  AccuracySD     KappaSD
# 1 0e+00 0.7059596 0.5137787 0.002886527 0.005294579
# 2 1e-04 0.7059596 0.5137786 0.002886494 0.005292764
# 3 1e-01 0.7062486 0.5143600 0.002831980 0.005238698
y_hat <- predict(fit, dat$test[-55])
cm <- confusionMatrix(data = y_hat, reference = dat$test$y)
# > cm
# Confusion Matrix and Statistics

# Reference
# Prediction    Aspen Cottonwood Douglas Krummholz Lodgepole Panderosa Spruce
# Aspen           0          0       0         0        16         0      2
# Cottonwood      0        448      89         0         1       556      0
# Douglas         4        226    1563         0       232      1442      1
# Krummholz       0          0       0      5284       305         2   2569
# Lodgepole    4319          2    3277       191    113598      2808  32419
# Panderosa     358        692    3725        47      2699     12945     59
# Spruce         66          6      30      4733     24800       124  70870

# Overall Statistics

# Accuracy : 0.7047         
# 95% CI : (0.703, 0.7063)
# No Information Rate : 0.4876         
# P-Value [Acc > NIR] : < 2.2e-16      

# Kappa : 0.5119         

# Mcnemar's Test P-Value : NA             

# Statistics by Class:

#                      Class: Aspen Class: Cottonwood Class: Douglas Class: Krummholz Class: Lodgepole Class: Panderosa
# Sensitivity             0.000e+00          0.326055        0.17999          0.51526           0.8020          0.72411
# Specificity             9.999e-01          0.997766        0.99324          0.98974           0.7110          0.97220
# Pos Pred Value          0.000e+00          0.409506        0.45069          0.64755           0.7253          0.63069
# Neg Pred Value          9.837e-01          0.996800        0.97519          0.98239           0.7905          0.98173
# Prevalence              1.634e-02          0.004730        0.02989          0.03530           0.4876          0.06154
# Detection Rate          0.000e+00          0.001542        0.00538          0.01819           0.3910          0.04456
# Detection Prevalence    6.196e-05          0.003766        0.01194          0.02809           0.5391          0.07065
# Balanced Accuracy       5.000e-01          0.661911        0.58661          0.75250           0.7565          0.84816
#                      Class: Spruce
# Sensitivity                 0.6691
# Specificity                 0.8388
# Pos Pred Value              0.7043
# Neg Pred Value              0.8154
# Prevalence                  0.3646
# Detection Rate              0.2440
# Detection Prevalence        0.3464
# Balanced Accuracy           0.7539

## gamLoess
fit <- train(y ~ ., data = dat$train, method="gamLoess")
y_hat <- predict(fit, dat$test[-55])
# Warning messages:
# 1: In predict.lm(object, newdata, se.fit, scale = 1, type = if (type ==  :
#   prediction from a rank-deficient fit may be misleading
# 2: In gam.lo(data[["lo(y_hydro, span = 0.5, degree = 1)"]], z, w, span = 0.5,  :
#   eval  -173
# 3: In gam.lo(data[["lo(y_hydro, span = 0.5, degree = 1)"]], z, w, span = 0.5,  :
#  lowerlimit  -169.82
# 4: In gam.lo(data[["lo(y_hydro, span = 0.5, degree = 1)"]], z, w, span = 0.5,  :
#  extrapolation not allowed with blending
cm <- confusionMatrix(data = y_hat, reference = dat$test$y)
# > cm
# Confusion Matrix and Statistics

#             Reference
# Prediction    Aspen Cottonwood Douglas Krummholz Lodgepole Panderosa Spruce
#   Aspen         185          0       0         0        18         0      0
#   Cottonwood   4562       1374    8684     10255    141633     17877 105920
#   Douglas         0          0       0         0         0         0      0
#   Krummholz       0          0       0         0         0         0      0
#   Lodgepole       0          0       0         0         0         0      0
#   Panderosa       0          0       0         0         0         0      0
#   Spruce          0          0       0         0         0         0      0

# Overall Statistics
                                          
#                Accuracy : 0.0054          
#                  95% CI : (0.0051, 0.0056)
#     No Information Rate : 0.4876          
#     P-Value [Acc > NIR] : 1               
                                          
#                   Kappa : 6e-04           
                                          
#  Mcnemar's Test P-Value : NA              

# Statistics by Class:
  
#   Class: Aspen Class: Cottonwood Class: Douglas Class: Krummholz Class: Lodgepole Class: Panderosa
# Sensitivity             0.0389720         1.0000000        0.00000           0.0000           0.0000          0.00000
# Specificity             0.9999370         0.0007021        1.00000           1.0000           1.0000          1.00000
# Pos Pred Value          0.9113300         0.0047330            NaN              NaN              NaN              NaN
# Neg Pred Value          0.9842855         1.0000000        0.97011           0.9647           0.5124          0.93846
# Prevalence              0.0163403         0.0047296        0.02989           0.0353           0.4876          0.06154
# Detection Rate          0.0006368         0.0047296        0.00000           0.0000           0.0000          0.00000
# Detection Prevalence    0.0006988         0.9993012        0.00000           0.0000           0.0000          0.00000
# Balanced Accuracy       0.5194545         0.5003510        0.50000           0.5000           0.5000          0.50000
# Class: Spruce
# Sensitivity                 0.0000
# Specificity                 1.0000
# Pos Pred Value                 NaN
# Neg Pred Value              0.6354
# Prevalence                  0.3646
# Detection Rate              0.0000
# Detection Prevalence        0.0000
# Balanced Accuracy           0.5000

## svmLinear

set.seed(2, sample.kind="Rounding")
train_index <- createDataPartition(y = dat$train$y, times = 1, p = 0.0025, list = FALSE)
train_subset <- dat$train[train_index,]

set.seed(3, sample.kind="Rounding")
test_index <- createDataPartition(y = dat$test$y, times = 1, p = 0.0025, list = FALSE)
test_subset <- dat$test[test_index,]

fit <- train(y ~ ., data = train_subset, method="svmLinear")
y_hat <- predict(fit, test_subset[-55])
head(y_hat)
# [1] Panderosa Panderosa Douglas   Lodgepole Spruce    Spruce   
# Levels: Aspen Cottonwood Douglas Krummholz Lodgepole Panderosa Spruce
cm <- confusionMatrix(data = y_hat, reference = test_subset$y)
# > cm
# Confusion Matrix and Statistics

# Reference
# Prediction   Aspen Cottonwood Douglas Krummholz Lodgepole Panderosa Spruce
# Aspen          2          0       1         0         8         0      6
# Cottonwood     0          1       1         0         1         3      0
# Douglas        0          1       8         0        10         6      1
# Krummholz      0          0       0        15         1         0     12
# Lodgepole      9          0       4         0       205        10     51
# Panderosa      0          2       8         0         3        26      0
# Spruce         1          0       0        11       127         0    195

# Overall Statistics

# Accuracy : 0.62            
# 95% CI : (0.5837, 0.6554)
# No Information Rate : 0.487           
# P-Value [Acc > NIR] : 3.59e-13        

# Kappa : 0.4071          

# Mcnemar's Test P-Value : NA              

# Statistics by Class:

#                      Class: Aspen Class: Cottonwood Class: Douglas Class: Krummholz Class: Lodgepole Class: Panderosa
# Sensitivity              0.166667          0.250000        0.36364          0.57692           0.5775          0.57778
# Specificity              0.979079          0.993103        0.97454          0.98151           0.8021          0.98099
# Pos Pred Value           0.117647          0.166667        0.30769          0.53571           0.7348          0.66667
# Neg Pred Value           0.985955          0.995851        0.98009          0.98431           0.6667          0.97246
# Prevalence               0.016461          0.005487        0.03018          0.03567           0.4870          0.06173
# Detection Rate           0.002743          0.001372        0.01097          0.02058           0.2812          0.03567
# Detection Prevalence     0.023320          0.008230        0.03567          0.03841           0.3827          0.05350
# Balanced Accuracy        0.572873          0.621552        0.66909          0.77922           0.6898          0.77939
#                      Class: Spruce
# Sensitivity                 0.7358
# Specificity                 0.7004
# Pos Pred Value              0.5838
# Neg Pred Value              0.8228
# Prevalence                  0.3635
# Detection Rate              0.2675
# Detection Prevalence        0.4582
# Balanced Accuracy           0.7181

set.seed(2, sample.kind="Rounding")
train_index <- createDataPartition(y = dat$train$y, times = 1, p = 0.005, list = FALSE)
train_subset <- dat$train[train_index,]

set.seed(3, sample.kind="Rounding")
test_index <- createDataPartition(y = dat$test$y, times = 1, p = 0.005, list = FALSE)
test_subset <- dat$test[test_index,]

fit <- train(y ~ ., data = train_subset, method="svmLinear")
save(fit, file = "rda/fit_svm_005.rda")
y_hat <- predict(fit, test_subset[-55])
head(y_hat)
# [1] Lodgepole Spruce    Spruce    Panderosa Spruce    Panderosa
# Levels: Aspen Cottonwood Douglas Krummholz Lodgepole Panderosa Spruce
cm <- confusionMatrix(data = y_hat, reference = test_subset$y)
# > cm
# Confusion Matrix and Statistics

# Reference
# Prediction   Aspen Cottonwood Douglas Krummholz Lodgepole Panderosa Spruce
# Aspen          5          0       0         0         4         1      1
# Cottonwood     0          1       1         0         0         2      0
# Douglas        1          0       9         0        10         9      0
# Krummholz      0          0       0        19         0         0     10
# Lodgepole     15          1      11         0       492         9     92
# Panderosa      1          5      23         0         5        69      0
# Spruce         2          0       0        33       198         0    427

# Overall Statistics

# Accuracy : 0.7019          
# 95% CI : (0.6777, 0.7253)
# No Information Rate : 0.487           
# P-Value [Acc > NIR] : < 2.2e-16       

# Kappa : 0.5206          

# Mcnemar's Test P-Value : NA              

# Statistics by Class:

#                      Class: Aspen Class: Cottonwood Class: Douglas Class: Krummholz Class: Lodgepole Class: Panderosa
# Sensitivity              0.208333         0.1428571       0.204545          0.36538           0.6939          0.76667
# Specificity              0.995810         0.9979296       0.985836          0.99288           0.8286          0.97511
# Pos Pred Value           0.454545         0.2500000       0.310345          0.65517           0.7935          0.66990
# Neg Pred Value           0.986851         0.9958678       0.975473          0.97687           0.7404          0.98448
# Prevalence               0.016484         0.0048077       0.030220          0.03571           0.4870          0.06181
# Detection Rate           0.003434         0.0006868       0.006181          0.01305           0.3379          0.04739
# Detection Prevalence     0.007555         0.0027473       0.019918          0.01992           0.4258          0.07074
# Balanced Accuracy        0.602072         0.5703934       0.595191          0.67913           0.7613          0.87089
#                      Class: Spruce
# Sensitivity                 0.8057
# Specificity                 0.7484
# Pos Pred Value              0.6470
# Neg Pred Value              0.8706
# Prevalence                  0.3640
# Detection Rate              0.2933
# Detection Prevalence        0.4533
# Balanced Accuracy           0.7770


set.seed(2, sample.kind="Rounding")
train_index <- createDataPartition(y = dat$train$y, times = 1, p = 0.01, list = FALSE)
train_subset <- dat$train[train_index,]

set.seed(3, sample.kind="Rounding")
test_index <- createDataPartition(y = dat$test$y, times = 1, p = 0.01, list = FALSE)
test_subset <- dat$test[test_index,]
# > nrow(test_subset)
# [1] 2908

fit <- train(y ~ ., data = train_subset, method="svmLinear")
save(fit, file = "rda/fit_svm_01.rda")
y_hat <- predict(fit, test_subset[-55])
head(y_hat)
# [1] Spruce    Lodgepole Lodgepole Spruce    Lodgepole Spruce   
# Levels: Aspen Cottonwood Douglas Krummholz Lodgepole Panderosa Spruce
cm <- confusionMatrix(data = y_hat, reference = test_subset$y)
# > cm
# Confusion Matrix and Statistics

#             Reference
# Prediction   Aspen Cottonwood Douglas Krummholz Lodgepole Panderosa Spruce
#   Aspen          8          0       0         0        10         1      4
#   Cottonwood     0          6       1         0         0         4      0
#   Douglas        2          0      18         0        12        13      0
#   Krummholz      0          0       0        40         0         0     15
#   Lodgepole     33          0      24         0       824        27    163
#   Panderosa      1          8      44         0        23       134      0
#   Spruce         4          0       0        63       548         0    878

# Overall Statistics
                                          
#                Accuracy : 0.6561          
#                  95% CI : (0.6385, 0.6734)
#     No Information Rate : 0.4873          
#     P-Value [Acc > NIR] : < 2.2e-16       
                                          
#                   Kappa : 0.4521          
                                          
#  Mcnemar's Test P-Value : NA              

# Statistics by Class:
  
#   Class: Aspen Class: Cottonwood Class: Douglas Class: Krummholz Class: Lodgepole Class: Panderosa
# Sensitivity              0.166667          0.428571        0.20690          0.38835           0.5815          0.74860
# Specificity              0.994755          0.998272        0.99043          0.99465           0.8343          0.97215
# Pos Pred Value           0.347826          0.545455        0.40000          0.72727           0.7694          0.63810
# Neg Pred Value           0.986135          0.997239        0.97590          0.97792           0.6772          0.98332
# Prevalence               0.016506          0.004814        0.02992          0.03542           0.4873          0.06155
# Detection Rate           0.002751          0.002063        0.00619          0.01376           0.2834          0.04608
# Detection Prevalence     0.007909          0.003783        0.01547          0.01891           0.3683          0.07221
# Balanced Accuracy        0.580711          0.713422        0.59866          0.69150           0.7079          0.86038
# Class: Spruce
# Sensitivity                 0.8283
# Specificity                 0.6672
# Pos Pred Value              0.5881
# Neg Pred Value              0.8714
# Prevalence                  0.3645
# Detection Rate              0.3019
# Detection Prevalence        0.5134
# Balanced Accuracy           0.7478


set.seed(2, sample.kind="Rounding")
train_index <- createDataPartition(y = dat$train$y, times = 1, p = 0.02, list = FALSE)
train_subset <- dat$train[train_index,]
# > nrow(train_subset)
# [1] 5813

set.seed(3, sample.kind="Rounding")
test_index <- createDataPartition(y = dat$test$y, times = 1, p = 0.02, list = FALSE)
test_subset <- dat$test[test_index,]
# > nrow(test_subset)
# [1] 5813

fit <- train(y ~ ., data = train_subset, method="svmLinear")
save(fit, file = "rda/fit_svm_02.rda")
y_hat <- predict(fit, test_subset[-55])
head(y_hat)
# [1] Spruce    Spruce    Spruce    Spruce    Lodgepole Lodgepole
# Levels: Aspen Cottonwood Douglas Krummholz Lodgepole Panderosa Spruce
cm <- confusionMatrix(data = y_hat, reference = test_subset$y)
# > cm
# Confusion Matrix and Statistics

# Reference
# Prediction   Aspen Cottonwood Douglas Krummholz Lodgepole Panderosa Spruce
# Aspen         23          0       3         0        92         5     18
# Cottonwood     0         14       1         0         0         7      0
# Douglas        2          2      45         0        23        35      1
# Krummholz      0          0       0        54         1         0     17
# Lodgepole     58          0      28         0      1605        26    316
# Panderosa      7         12      97         0        58       285      0
# Spruce         5          0       0       152      1055         0   1767

# Overall Statistics

# Accuracy : 0.6524        
# 95% CI : (0.64, 0.6646)
# No Information Rate : 0.4874        
# P-Value [Acc > NIR] : < 2.2e-16     

# Kappa : 0.4539        

# Mcnemar's Test P-Value : NA            

# Statistics by Class:

#                      Class: Aspen Class: Cottonwood Class: Douglas Class: Krummholz Class: Lodgepole Class: Panderosa
# Sensitivity              0.242105          0.500000        0.25862         0.262136           0.5663          0.79609
# Specificity              0.979367          0.998617        0.98883         0.996790           0.8564          0.96811
# Pos Pred Value           0.163121          0.636364        0.41667         0.750000           0.7895          0.62092
# Neg Pred Value           0.987308          0.997583        0.97739         0.973528           0.6750          0.98637
# Prevalence               0.016340          0.004816        0.02993         0.035432           0.4874          0.06158
# Detection Rate           0.003956          0.002408        0.00774         0.009288           0.2761          0.04902
# Detection Prevalence     0.024252          0.003784        0.01858         0.012384           0.3497          0.07895
# Balanced Accuracy        0.610736          0.749309        0.62373         0.629463           0.7114          0.88210
#                      Class: Spruce
# Sensitivity                 0.8339
# Specificity                 0.6720
# Pos Pred Value              0.5932
# Neg Pred Value              0.8758
# Prevalence                  0.3645
# Detection Rate              0.3039
# Detection Prevalence        0.5124
# Balanced Accuracy           0.7529


set.seed(2, sample.kind="Rounding")
train_index <- createDataPartition(y = dat$train$y, times = 1, p = 0.04, list = FALSE)
train_subset <- dat$train[train_index,]
# > nrow(train_subset)
# [1] 11623

set.seed(3, sample.kind="Rounding")
test_index <- createDataPartition(y = dat$test$y, times = 1, p = 0.04, list = FALSE)
test_subset <- dat$test[test_index,]
# > nrow(test_subset)
# [1] 11624

# 7.52AM start
fit <- train(y ~ ., data = train_subset, method="svmLinear")
save(fit, file = "rda/fit_svm_02.rda")
y_hat <- predict(fit, test_subset[-55])
head(y_hat)
# [1] Spruce    Spruce    Spruce    Spruce    Lodgepole Lodgepole
# Levels: Aspen Cottonwood Douglas Krummholz Lodgepole Panderosa Spruce
cm <- confusionMatrix(data = y_hat, reference = test_subset$y)

# models <- c("glm", "lda", "naive_bayes", "svmLinear", "knn", "gamLoess", "multinom", "qda", "rf", "adaboost")
# models <- c("lda", "naive_bayes", "svmLinear", "knn", "gamLoess", "multinom", "qda", "rf", "adaboost")
# models <- c("naive_bayes", "svmLinear", "knn", "gamLoess", "multinom", "qda", "rf", "adaboost")
library(MASS)
models <- c("lda", "naive_bayes", "svmLinear", "knn", "gamLoess", "multinom", "rf", "adaboost")
fits <- lapply(models, function(model) { 
  print(model)
  if (model == "lda")
    # lda(train_fact[-55], train_fact$class)
    train(y ~ ., data = dat$train, method="lda")
  else if (model == "naive_bayes")
    naive_bayes(y ~ ., dat$train, usekernel = TRUE, kernel = "gaussian", bw = "ucv", adjust = 0.5)
  else if (model == "svmLinear")
    # see ksvm here
    # https://cran.r-project.org/web/packages/kernlab/kernlab.pdf
    ## estimate good sigma values for promotergene
    # data(promotergene)
    # srange <- sigest(Class~.,data = promotergene)
    # srange
  
    train(y ~ ., data = dat$train, method="svmLinear")
  else if (model == "knn")
    knn3(y ~ ., data = dat$train, k = 5)
  else if (model == "gamLoess")
    ## produces rubbish
    train(y ~ ., data = dat$train, method="gamLoess")
  else if (model == "multinom")
    # https://cran.r-project.org/web/packages/nnet/nnet.pdf
    # library(nnet)
    # multinom(formula, data, weights, subset, na.action, contrasts = NULL, Hess = FALSE, summ = 0, censored = FALSE, model = FALSE, ...)
    # multinom(y ~ ., data = dat$train)
    train(y ~ ., data = dat$train, method="multinom")
  else if (model == "rf")
    randomForest(y ~ ., data = dat$train)
  else if (model == "adaboost")
    # https://cran.r-project.org/web/packages/fastAdaboost/fastAdaboost.pdf
    # adaboost(formula, data, nIter, ...)
    # library("fastAdaboost")
    # test_adaboost <- adaboost(Y~X, fakedata, 10)
    # adaboost(y ~ ., data = dat$train, nIter, ...)
    # ONLY FOR BINARY CLASSIFICATION
    print("branch adaboost")
  # train(class ~ ., method = model, data = train_fact_subset)
}) 

models <- c("knn", "rf", "lda", "naive_bayes", "multinom", "svmLinear")
fits <- lapply(models, function(model) { 
    print(model)
    if (model == "lda") {
        fit <- train(y ~ ., data = dat$train, method="lda")
    } else if (model == "naive_bayes") {
        fit <- naive_bayes(y ~ ., dat$train, usekernel = TRUE, kernel = "gaussian", bw = "ucv", adjust = 0.5)
    } else if (model == "svmLinear") {
        fit <- ksvm(y ~ ., data = dat$train)
    } else if (model == "knn") {
        fit <- knn3(y ~ ., data = dat$train, k = 5)
    } else if (model == "multinom") {
        fit <- train(y ~ ., data = dat$train, method="multinom")
    } else if (model == "rf") {
        fit <- randomForest(y ~ ., data = dat$train)
    }
    return(fit)
})
# 1:45am
save(fits, file = "rda/fits.rda")
names(fits) <- models
# > class(fits)
# [1] "list"
pred <- sapply(fits, function(object) {
  predict(object, dat$test[-55], type = "class")
})
y_hat_knn <- predict(fits$knn, dat$test[-55], type = "class")
y_hat_rf <- predict(fits$rf, dat$test[-55])
y_hat_lda <- predict(fits$lda, dat$test[-55])
y_hat_nb <- predict(fits$naive_bayes, dat$test[-55])
y_hat_mn <- predict(fits$multinom, dat$test[-55])
y_hat_svm <- predict(fits$svmLinear, dat$test[-55])

pred <- list(knn = y_hat_knn, rf = y_hat_rf, lda = y_hat_lda, naive_bayes = y_hat_nb, multinom = y_hat_mn, svmLinear = y_hat_svm)
pred_df <- data.frame(knn = y_hat_knn, rf = y_hat_rf, lda = y_hat_lda, naive_bayes = y_hat_nb, multinom = y_hat_mn, svmLinear = y_hat_svm)
# > str(pred)
# List of 6
# $ knn        : Factor w/ 7 levels "Aspen","Cottonwood",..: 1 1 5 1 1 1 5 1 1 1 ...
# $ rf         : Factor w/ 7 levels "Aspen","Cottonwood",..: 5 5 5 5 5 5 5 5 5 5 ...
# ..- attr(*, "names")= chr [1:290508] "1" "2" "3" "4" ...
# $ lda        : Factor w/ 7 levels "Aspen","Cottonwood",..: 5 5 5 5 5 5 5 5 5 5 ...
# $ naive_bayes: Factor w/ 7 levels "Aspen","Cottonwood",..: 5 5 5 5 5 5 5 5 5 5 ...
# $ multinom   : Factor w/ 7 levels "Aspen","Cottonwood",..: 5 5 5 5 5 5 5 5 5 5 ...
# $ svmLinear  : Factor w/ 7 levels "Aspen","Cottonwood",..: 5 5 5 5 5 5 5 5 5 5 ...

# > head(pred$knn)
# [1] Aspen     Aspen     Lodgepole Aspen     Aspen     Aspen    
# Levels: Aspen Cottonwood Douglas Krummholz Lodgepole Panderosa Spruce
# > head(pred$rf)
# 1         2         3         4         5         6 
# Lodgepole Lodgepole Lodgepole Lodgepole Lodgepole Lodgepole 
# Levels: Aspen Cottonwood Douglas Krummholz Lodgepole Panderosa Spruce
# > head(pred$lda)
# [1] Lodgepole Lodgepole Lodgepole Lodgepole Lodgepole Lodgepole
# Levels: Aspen Cottonwood Douglas Krummholz Lodgepole Panderosa Spruce
# > head(pred$naive_bayes)
# [1] Lodgepole Lodgepole Lodgepole Lodgepole Lodgepole Lodgepole
# Levels: Aspen Cottonwood Douglas Krummholz Lodgepole Panderosa Spruce
# > head(pred$multinom)
# [1] Lodgepole Lodgepole Lodgepole Lodgepole Lodgepole Lodgepole
# Levels: Aspen Cottonwood Douglas Krummholz Lodgepole Panderosa Spruce
# > head(pred$svmLinear)
# [1] Lodgepole Lodgepole Lodgepole Lodgepole Lodgepole Lodgepole
# Levels: Aspen Cottonwood Douglas Krummholz Lodgepole Panderosa Spruce

save(pred, file = "rda/pred.rda")
save(pred_df, file = "rda/pred_df.rda")

# pred_matrix <- cbind(data.frame(knn = pred$knn, rf = pred$rf, lda = pred$lda, naive_bayes = pred$naive_bayes, multinom = pred$multinom, svmLinear = pred$svmLinear))
# save(pred_matrix, file = "rda/pred_matrix.rda")

# > colnames(pred_matrix)
# [1] "knn"         "rf"          "lda"         "naive_bayes" "multinom"    "svmLinear"  
# > dim(pred_matrix)
# [1] 290508      6
# > pred_matrix[1:5,]
#         knn        rf       lda naive_bayes  multinom svmLinear
# 1     Aspen Lodgepole Lodgepole   Lodgepole Lodgepole Lodgepole
# 2     Aspen Lodgepole Lodgepole   Lodgepole Lodgepole Lodgepole
# 3 Lodgepole Lodgepole Lodgepole   Lodgepole Lodgepole Lodgepole
# 4     Aspen Lodgepole Lodgepole   Lodgepole Lodgepole Lodgepole
# 5     Aspen Lodgepole Lodgepole   Lodgepole Lodgepole Lodgepole

# acc <- colMeans(pred_matrix == dat$test$y)
acc <- sapply(pred, function(y_hat) {
  mean(y_hat == dat$test$y)
})
save(acc, file = "rda/acc.rda")

mean(acc)

# > acc
#       knn          rf         lda naive_bayes    multinom   svmLinear 
# 0.9577189   0.8372024   0.6784013   0.6960841   0.7046587   0.8141531 
# > class(acc)
# [1] "numeric"
# > colnames(acc)
# NULL
# > acc["knn"]
# knn 
# 0.9577189 
# > acc["rf"]
# rf 
# 0.8372024 
# > acc["lda"]
# lda 
# 0.6784013 
# > acc["naive_bayes"]
# naive_bayes 
# 0.6960841 
# > acc["multinom"]
# multinom 
# 0.7046587 
# > acc["svmLinear"]
# svmLinear 
# 0.8141531 


# y_hat <- predict(fit, test_subset %>% select(-y), type = "class")

# acc_df <- colMeans(pred_df == dat$test$y)
acc_df <- sapply(pred_df, function(y_hat) {
  mean(y_hat == dat$test$y)
})
classes <- c("Spruce", "Lodgepole", "Panderosa", "Cottonwood", "Aspen", "Douglas", "Krummholz")
y_hat_ensemble <- sapply(1:nrow(pred_df), function(i) {
  votes <- pred_df[i,]
  probs <- c(mean(votes == "Spruce"), mean(votes == "Lodgepole"), mean(votes == "Panderosa"), mean(votes == "Cottonwood"), mean(votes == "Aspen"), mean(votes == "Douglas"), mean(votes == "Krummholz"))
  classes[which.max(probs)]
})
# > class(y_hat_ensemble)
# [1] "character"
# > head(y_hat_ensemble)
# [1] "Lodgepole" "Lodgepole" "Lodgepole" "Lodgepole" "Lodgepole" "Lodgepole"

y_hat_ensemble <- factor(y_hat_ensemble, levels = levels(dat$test$y))
acc_ensemble <- mean(y_hat_ensemble == dat$test$y)
acc_ensemble
# > acc_ensemble
# [1] 0.8007938
save(y_hat_ensemble, file = "rda/y_hat_ensemble.rda")
save(acc_ensemble, file = "rda/acc_ensemble.rda")

# How many of the individual methods do better than the ensemble?
ind <- acc_df > acc_ensemble
sum(ind)
# [1] 3
models[ind]
# > models[ind]
# [1] "knn"       "rf"        "svmLinear"

# It is tempting to remove the methods that do not perform well and re-do the ensemble. The problem with this approach is that we 
# are using the test data to make a decision. However, we could use the minimum accuracy estimates obtained from cross validation 
# with the training data for each model. Obtain these estimates and save them in an object. Report the mean of these training set 
# accuracy estimates.

# You can calculate the mean accuracy of the new estimates using the following code:
acc_hat <- sapply(fits, function(fit) {
  # min(fit$results$Accuracy)
  print(fit$results$Accuracy)
})
#       knn          rf         lda naive_bayes    multinom   svmLinear
# fits$lda$results$Accuracy
# > min(fits$multinom$results$Accuracy)
# [1] 0.7052867
mean(acc_hat)

# > pred_df[1,]
# knn        rf       lda naive_bayes  multinom svmLinear
# 1 Aspen Lodgepole Lodgepole   Lodgepole Lodgepole Lodgepole
# > pred_df[1,] == "Aspen"
#    knn    rf   lda naive_bayes multinom svmLinear
# 1 TRUE FALSE FALSE       FALSE    FALSE     FALSE
# > mean(pred_df[1,] == "Aspen")
# [1] 0.1666667



# > acc_df
#       knn          rf         lda naive_bayes    multinom   svmLinear 
# 0.9577189   0.8372024   0.6784013   0.6960841   0.7046587   0.8141531 

# start - 9:51am, end - 1:37pm
# start - 1:41pm, end - 1:37pm
# > dim(pred)
# NULL
# > class(pred)
# [1] "list"
# > str(pred)
# List of 6
# $ knn        : num [1:290508, 1:7] 1 1 0 1 1 1 0.4 0.8 0.6 1 ...
# ..- attr(*, "dimnames")=List of 2
# .. ..$ : NULL
# .. ..$ : chr [1:7] "Aspen" "Cottonwood" "Douglas" "Krummholz" ...
# $ rf         : Factor w/ 7 levels "Aspen","Cottonwood",..: 5 5 5 5 5 5 5 5 5 5 ...
# ..- attr(*, "names")= chr [1:290508] "1" "2" "3" "4" ...
# $ lda        : Factor w/ 7 levels "Aspen","Cottonwood",..: 5 5 5 5 5 5 5 5 5 5 ...
# $ naive_bayes: Factor w/ 7 levels "Aspen","Cottonwood",..: 5 5 5 5 5 5 5 5 5 5 ...
# $ multinom   : Factor w/ 7 levels "Aspen","Cottonwood",..: 5 5 5 5 5 5 5 5 5 5 ...
# $ svmLinear  : Factor w/ 7 levels "Aspen","Cottonwood",..: 5 5 5 5 5 5 5 5 5 5 ...


# https://en.wikipedia.org/wiki/K-nearest_neighbors_algorithm
# Often, the classification accuracy of k-NN can be improved significantly if the distance metric is learned with specialized
# algorithms such as Large Margin Nearest Neighbor or Neighborhood components analysis.
# https://en.wikipedia.org/wiki/Instance-based_learning

## check prevalence
# > train_fact %>% group_by(class) %>% summarize(n = n())
# A tibble: 7 x 2
#   class           n
#   <fct>       <int>
# 1 Aspen        6645
# 2 Cottonwood   1922
# 3 Douglas     12156
# 4 Krummholz   14357
# 5 Lodgepole  198310
# 6 Panderosa   25027
# 7 Spruce     148288

# > test_fact %>% group_by(class) %>% summarize(n = n())
# A tibble: 7 x 2
#   class          n
#   <fct>      <int>
# 1 Aspen       2848
# 2 Cottonwood   825
# 3 Douglas     5211
# 4 Krummholz   6153
# 5 Lodgepole  84991
# 6 Panderosa  10727
# 7 Spruce     63552





