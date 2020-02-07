#remove all the objects stored
rm(list=ls())

#set current working directory
setwd("C:/Users/Hp/Desktop/Project")


#reading given data that is in CSV formate 
credit_card = read.csv("credit_card_data.csv", header = T)

View(credit_card)

#Here i am going to remove one variable from given data set   
credit = credit_card[,-1]

View(credit)

dim(credit)

colnames(credit)

str(credit)

# Identifying Outliers
mystats <- function(x) {
  nmiss<-sum(is.na(x))
  a <- x[!is.na(x)]
  m <- mean(a)
  n <- length(a)
  s <- sd(a)
  min <- min(a)
  p1<-quantile(a,0.01)
  p5<-quantile(a,0.05)
  p10<-quantile(a,0.10)
  q1<-quantile(a,0.25)
  q2<-quantile(a,0.5)
  q3<-quantile(a,0.75)
  p90<-quantile(a,0.90)
  p95<-quantile(a,0.95)
  p99<-quantile(a,0.99)
  max <- max(a)
  UC <- m+2*s
  LC <- m-2*s
  outlier_flag<- max>UC | min<LC
  return(c(n=n, nmiss=nmiss, outlier_flag=outlier_flag, mean=m, stdev=s,min = min, p1=p1,p5=p5,p10=p10,q1=q1,q2=q2,q3=q3,p90=p90,p95=p95,p99=p99,max=max, UC=UC, LC=LC ))
}

#New Variables creation# 

credit$Monthly_Avg_PURCHASES <- credit$PURCHASES/(credit$PURCHASES_FREQUENCY*credit$TENURE)

credit$Monthly_CASH_ADVANCE <- credit$CASH_ADVANCE/(credit$CASH_ADVANCE_FREQUENCY*credit$TENURE)

credit$LIMIT_USAGE <- credit$BALANCE/credit$CREDIT_LIMIT

credit$MIN_PAYMENTS_RATIO <- credit$PAYMENTS/credit$MINIMUM_PAYMENTS

write.csv(credit,"New_variables_creation.csv")


Num_Vars <- c(
  "BALANCE",
  "BALANCE_FREQUENCY",
  "PURCHASES",
  "Monthly_Avg_PURCHASES",
  "ONEOFF_PURCHASES",
  "INSTALLMENTS_PURCHASES",
  "CASH_ADVANCE",
  "Monthly_CASH_ADVANCE",
  "PURCHASES_FREQUENCY",
  "ONEOFF_PURCHASES_FREQUENCY",
  "PURCHASES_INSTALLMENTS_FREQUENCY",
  "CASH_ADVANCE_FREQUENCY",
  "CASH_ADVANCE_TRX",
  "PURCHASES_TRX",
  "CREDIT_LIMIT",
  "LIMIT_USAGE",
  "PAYMENTS",
  "MINIMUM_PAYMENTS",
  "MIN_PAYMENTS_RATIO",
  "PRC_FULL_PAYMENT",
  "TENURE")

Outliers<-t(data.frame(apply(credit[Num_Vars], 2, mystats)))
View(Outliers)

write.csv(Outliers,"Outliers.csv")


# Outlier Treatment
credit$BALANCE[credit$BALANCE>5727.53]<-5727.53

credit$BALANCE_FREQUENCY[credit$BALANCE_FREQUENCY>1.3510787]<-1.3510787

credit$PURCHASES[credit$PURCHASES>5276.46]<-5276.46

credit$Monthly_Avg_PURCHASES[credit$Monthly_Avg_PURCHASES>800.03] <- 800.03

credit$ONEOFF_PURCHASES[credit$ONEOFF_PURCHASES>3912.2173709]<-3912.2173709

credit$INSTALLMENTS_PURCHASES[credit$INSTALLMENTS_PURCHASES>2219.7438751]<-2219.7438751

credit$CASH_ADVANCE[credit$CASH_ADVANCE>5173.1911125]<-5173.1911125

credit$Monthly_CASH_ADVANCE[credit$Monthly_CASH_ADVANCE>2558.53] <- 2558.53

credit$PURCHASES_FREQUENCY[credit$PURCHASES_FREQUENCY>1.2930919]<-1.2930919

credit$ONEOFF_PURCHASES_FREQUENCY[credit$ONEOFF_PURCHASES_FREQUENCY>0.7991299]<-0.7991299

credit$PURCHASES_INSTALLMENTS_FREQUENCY[credit$PURCHASES_INSTALLMENTS_FREQUENCY>1.1593329]<-1.1593329

credit$CASH_ADVANCE_FREQUENCY[credit$CASH_ADVANCE_FREQUENCY>0.535387]<-0.535387

credit$CASH_ADVANCE_TRX[credit$CASH_ADVANCE_TRX>16.8981202]<-16.8981202

credit$PURCHASES_TRX[credit$PURCHASES_TRX>64.4251306]<-64.4251306

credit$CREDIT_LIMIT[credit$CREDIT_LIMIT>11772.09]<-11772.09

credit$LIMIT_USAGE[credit$LIMIT_USAGE>1.1683] <- 1.1683

credit$PAYMENTS[credit$PAYMENTS>7523.26]<-7523.26

credit$MINIMUM_PAYMENTS[credit$MINIMUM_PAYMENTS>5609.1065423]<-5609.1065423

credit$MIN_PAYMENTS_RATIO[credit$MIN_PAYMENTS_RATIO>249.9239] <- 249.9239

credit$PRC_FULL_PAYMENT[credit$PRC_FULL_PAYMENT>0.738713]<-0.738713

credit$TENURE[credit$TENURE>14.19398]<-14.19398

# Missing Value Imputation with mean based on Accuracy  
credit$MINIMUM_PAYMENTS[which(is.na(credit$MINIMUM_PAYMENTS))] <- 721.9256368

credit$CREDIT_LIMIT[which(is.na(credit$CREDIT_LIMIT))] <- 4343.62

credit$Monthly_Avg_PURCHASES[which(is.na(credit$Monthly_Avg_PURCHASES))] <-184.8991609

credit$Monthly_CASH_ADVANCE[which(is.na(credit$Monthly_CASH_ADVANCE))] <- 717.7235629

credit$LIMIT_USAGE[which(is.na(credit$LIMIT_USAGE))] <-0.3889264

credit$MIN_PAYMENTS_RATIO[which(is.na(credit$MIN_PAYMENTS_RATIO))]  <- 9.3500701

# Checking Missing Value
check_Missing_Values<-t(data.frame(apply(credit[Num_Vars], 2, mystats)))

View(check_Missing_Values)

write.csv(credit,"Missing_value_treatment.csv")

# Variable Reduction 

Step_nums <- credit[Num_Vars]

corrm<- cor(Step_nums)    

View(corrm)

write.csv(corrm, "Correlation_matrix.csv")


eigen(corrm)$values

require(dplyr)

eigen_values <- mutate(data.frame(eigen(corrm)$values)
                       ,cum_sum_eigen=cumsum(eigen.corrm..values)
                       , pct_var=eigen.corrm..values/sum(eigen.corrm..values)
                       , cum_pct_var=cum_sum_eigen/sum(eigen.corrm..values))

write.csv(eigen_values, "EigenValues2.csv")

require(psych)
FA<-fa(r=corrm, 7, rotate="varimax", fm="ml")  

#SORTING THE LOADINGS
FA_SORT<-fa.sort(FA)      
FA_SORT$loadings


Loadings<-data.frame(FA_SORT$loadings[1:ncol(Step_nums),])
write.csv(Loadings, "loadings2.csv")


# standardizing the data
segment_prepared <-credit[Num_Vars]

segment_prepared = scale(segment_prepared)

write.csv(segment_prepared, "standardized data.csv")

#building clusters using k-means clustering 
cluster_three <- kmeans(segment_prepared,3)

cluster_four <- kmeans(segment_prepared,4)

cluster_five <- kmeans(segment_prepared,5)

cluster_six <- kmeans(segment_prepared,6)


credit_new<-cbind(credit,km_clust_3=cluster_three$cluster,km_clust_4=cluster_four$cluster,km_clust_5=cluster_five$cluster ,km_clust_6=cluster_six$cluster   )

View(credit_new)

# Profiling

Num_Vars2 <- c(
  "Monthly_Avg_PURCHASES",
  "Monthly_CASH_ADVANCE",
  "CASH_ADVANCE",
  "CASH_ADVANCE_TRX",
  "CASH_ADVANCE_FREQUENCY",
  "ONEOFF_PURCHASES",
  "ONEOFF_PURCHASES_FREQUENCY",
  "PAYMENTS",
  "CREDIT_LIMIT",
  "LIMIT_USAGE",
  "PURCHASES_INSTALLMENTS_FREQUENCY",
  "PURCHASES_FREQUENCY",
  "INSTALLMENTS_PURCHASES",
  "PURCHASES_TRX",
  "MINIMUM_PAYMENTS",
  "MIN_PAYMENTS_RATIO",
  "BALANCE",
  "TENURE"
)

require(tables)

tt <-cbind(tabular(1+factor(km_clust_3)+factor(km_clust_4)+factor(km_clust_5)+
                     factor(km_clust_6)~Heading()*length*All(credit[1]),
                   data=credit_new),tabular(1+factor(km_clust_3)+factor(km_clust_4)+factor(km_clust_5)+
                                              factor(km_clust_6)~Heading()*mean*All(credit[Num_Vars2]),
                                            data=credit_new))

tt2 <- as.data.frame.matrix(tt)

View(tt2)

rownames(tt2)<-c(
  "ALL",
  "KM3_1",
  "KM3_2",
  "KM3_3",
  "KM4_1",
  "KM4_2",
  "KM4_3",
  "KM4_4",
  "KM5_1",
  "KM5_2",
  "KM5_3",
  "KM5_4",
  "KM5_5",
  "KM6_1",
  "KM6_2",
  "KM6_3",
  "KM6_4",
  "KM6_5",
  "KM6_6")


colnames(tt2)<-c(
  "SEGMENT_SIZE",
  "Monthly_Avg_PURCHASES",
  "Monthly_CASH_ADVANCE",
  "CASH_ADVANCE",
  "CASH_ADVANCE_TRX",
  "CASH_ADVANCE_FREQUENCY",
  "ONEOFF_PURCHASES",
  "ONEOFF_PURCHASES_FREQUENCY",
  "PAYMENTS",
  "CREDIT_LIMIT",
  "LIMIT_USAGE",
  "PURCHASES_INSTALLMENTS_FREQUENCY",
  "PURCHASES_FREQUENCY",
  "INSTALLMENTS_PURCHASES",
  "PURCHASES_TRX",
  "MINIMUM_PAYMENTS",
  "MIN_PAYMENTS_RATIO",
  "BALANCE",
  "TENURE"
)

cluster_profiling2 <- t(tt2)

write.csv(cluster_profiling2,'cluster_profiling2.csv')

View(cluster_profiling2)

