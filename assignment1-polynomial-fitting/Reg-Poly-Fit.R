setwd("D:/MTechAssignments/ML/assign1")

#random seed set for reproducing same outputs
set.seed(3)
library("SimDesign", lib.loc="D:/Softwares/R/R-3.5.2/library")

#Function 01 and 02 definitions
fun_01 = function(x) {exp(-5*((x-0.3)^2)) + 0.5*(exp(-100*((x-0.5)^2))) + 0.5*(exp(-100*((x-0.75)^2)))}
fun_02 = function(x) {2-(3*x) + (10*(x^4)) - (5*(x^9)) + (6*(x^14))}

#function 01 plot
png(file="1_Q2_fun_01.png", width = 960, height = 480)
plot(fun_01, -1000, 1000, xlab = "x", ylab= "y=Q2_fun_01(x)", main = "Q2_fun_01 Plot", type = "l", col = "orange", col.main = "darkgray")
dev.off()

#function 02 plot
png(file="2_Q2_fun_02.png", width = 960, height = 480)
plot(fun_02, -1000, 1000, xlab = "x", ylab= "y=Q2_fun_02(x)", main = "Q2_fun_02 Plot", type = "l", col = "purple", col.main = "darkgray")
dev.off()



#100 random input values, x extracted lying between -1000 and 1000 for ten different training sets
x_random_extract_set1<-runif(100,-1000,1000)
x_random_extract_set2<-runif(100,-1000,1000)
x_random_extract_set3<-runif(100,-1000,1000)
x_random_extract_set4<-runif(100,-1000,1000)
x_random_extract_set5<-runif(100,-1000,1000)
x_random_extract_set6<-runif(100,-1000,1000)
x_random_extract_set7<-runif(100,-1000,1000)
x_random_extract_set8<-runif(100,-1000,1000)
x_random_extract_set9<-runif(100,-1000,1000)
x_random_extract_set10<-runif(100,-1000,1000)

#corresponding y values computed for the 100 randomly generated x values for both functions 01 and 02
fun_points_01_y_set1 <- list()
fun_points_01_y_set2 <- list()
fun_points_01_y_set3 <- list()
fun_points_01_y_set4 <- list()
fun_points_01_y_set5 <- list()
fun_points_01_y_set6 <- list()
fun_points_01_y_set7 <- list()
fun_points_01_y_set8 <- list()
fun_points_01_y_set9 <- list()
fun_points_01_y_set10 <- list()

for (i in 1:100){
  y<-fun_01(x_random_extract_set1[[i]])
  fun_points_01_y_set1[[i]]<-y
  y<-fun_01(x_random_extract_set2[[i]])
  fun_points_01_y_set2[[i]]<-y
  y<-fun_01(x_random_extract_set3[[i]])
  fun_points_01_y_set3[[i]]<-y
  y<-fun_01(x_random_extract_set4[[i]])
  fun_points_01_y_set4[[i]]<-y
  y<-fun_01(x_random_extract_set5[[i]])
  fun_points_01_y_set5[[i]]<-y
  y<-fun_01(x_random_extract_set6[[i]])
  fun_points_01_y_set6[[i]]<-y
  y<-fun_01(x_random_extract_set7[[i]])
  fun_points_01_y_set7[[i]]<-y
  y<-fun_01(x_random_extract_set8[[i]])
  fun_points_01_y_set8[[i]]<-y
  y<-fun_01(x_random_extract_set9[[i]])
  fun_points_01_y_set9[[i]]<-y
  y<-fun_01(x_random_extract_set10[[i]])
  fun_points_01_y_set10[[i]]<-y
}

fun_points_02_y_set1 <- list()
fun_points_02_y_set2 <- list()
fun_points_02_y_set3 <- list()
fun_points_02_y_set4 <- list()
fun_points_02_y_set5 <- list()
fun_points_02_y_set6 <- list()
fun_points_02_y_set7 <- list()
fun_points_02_y_set8 <- list()
fun_points_02_y_set9 <- list()
fun_points_02_y_set10 <- list()

for (i in 1:100){
  y<-fun_02(x_random_extract_set1[[i]])
  fun_points_02_y_set1[[i]]<-y
  y<-fun_02(x_random_extract_set2[[i]])
  fun_points_02_y_set2[[i]]<-y
  y<-fun_02(x_random_extract_set3[[i]])
  fun_points_02_y_set3[[i]]<-y
  y<-fun_02(x_random_extract_set4[[i]])
  fun_points_02_y_set4[[i]]<-y
  y<-fun_02(x_random_extract_set5[[i]])
  fun_points_02_y_set5[[i]]<-y
  y<-fun_02(x_random_extract_set6[[i]])
  fun_points_02_y_set6[[i]]<-y
  y<-fun_02(x_random_extract_set7[[i]])
  fun_points_02_y_set7[[i]]<-y
  y<-fun_02(x_random_extract_set8[[i]])
  fun_points_02_y_set8[[i]]<-y
  y<-fun_02(x_random_extract_set9[[i]])
  fun_points_02_y_set9[[i]]<-y
  y<-fun_02(x_random_extract_set10[[i]])
  fun_points_02_y_set10[[i]]<-y
}

#Plot of the 100 random x and y values for both functions 01 and 02
png(file="3_Q2_fun_01_100_random_points_plot.png", width = 960, height = 480)
plot(x_random_extract_set1,fun_points_01_y, xlab="x", ylab="y=Q2_fun_01(x)", main="Q2_fun_01 random 100 points Plot", xlim=c(-1000,1000), pch=16, col="orange", col.main = "darkgray")
dev.off()
png(file="4_Q2_fun_02_100_random_points_plot.png", width = 960, height = 480)
plot(x_random_extract_set1,fun_points_02_y, xlab="x", ylab="y=Q2_fun_02(x)", main="Q2_fun_02 random 100 points Plot", xlim=c(-1000,1000), pch=16, col="purple", col.main = "darkgrey")
dev.off()


#function definition for inducing gaussian noise to our data
Add_Gaussian_Noise = function(func_val_list, mean, std_dev) {
  noisy_val <- list()
  if (is.vector(func_val_list)) {
    length(func_val_list)
    for (i in 1:length(func_val_list)) {
      noise <- rnorm(length(func_val_list), mean, std_dev)
      noisy_val[[i]] <- func_val_list[[i]] + noise[[i]]
    }
  }
  return(noisy_val)
}

#function calls to add normal noise to our data (noise added only to the output values, i.e, y)
#we choose mean=0 and standard deviation as 1 for rnorm call
fun_points_01_y_noisy_set1<-list()
fun_points_02_y_noisy_set1<-list()
fun_points_01_y_noisy_set1<-Add_Gaussian_Noise(fun_points_01_y_set1, 0, 1)
fun_points_02_y_noisy_set1<-Add_Gaussian_Noise(fun_points_02_y_set1, 0, 1)
fun_points_01_y_noisy_set2<-list()
fun_points_02_y_noisy_set2<-list()
fun_points_01_y_noisy_set2<-Add_Gaussian_Noise(fun_points_01_y_set2, 0, 1)
fun_points_02_y_noisy_set2<-Add_Gaussian_Noise(fun_points_02_y_set2, 0, 1)
fun_points_01_y_noisy_set3<-list()
fun_points_02_y_noisy_set3<-list()
fun_points_01_y_noisy_set3<-Add_Gaussian_Noise(fun_points_01_y_set3, 0, 1)
fun_points_02_y_noisy_set3<-Add_Gaussian_Noise(fun_points_02_y_set3, 0, 1)
fun_points_01_y_noisy_set4<-list()
fun_points_02_y_noisy_set4<-list()
fun_points_01_y_noisy_set4<-Add_Gaussian_Noise(fun_points_01_y_set4, 0, 1)
fun_points_02_y_noisy_set4<-Add_Gaussian_Noise(fun_points_02_y_set4, 0, 1)
fun_points_01_y_noisy_set5<-list()
fun_points_02_y_noisy_set5<-list()
fun_points_01_y_noisy_set5<-Add_Gaussian_Noise(fun_points_01_y_set5, 0, 1)
fun_points_02_y_noisy_set5<-Add_Gaussian_Noise(fun_points_02_y_set5, 0, 1)
fun_points_01_y_noisy_set6<-list()
fun_points_02_y_noisy_set6<-list()
fun_points_01_y_noisy_set6<-Add_Gaussian_Noise(fun_points_01_y_set6, 0, 1)
fun_points_02_y_noisy_set6<-Add_Gaussian_Noise(fun_points_02_y_set6, 0, 1)
fun_points_01_y_noisy_set7<-list()
fun_points_02_y_noisy_set7<-list()
fun_points_01_y_noisy_set7<-Add_Gaussian_Noise(fun_points_01_y_set7, 0, 1)
fun_points_02_y_noisy_set7<-Add_Gaussian_Noise(fun_points_02_y_set7, 0, 1)
fun_points_01_y_noisy_set8<-list()
fun_points_02_y_noisy_set8<-list()
fun_points_01_y_noisy_set8<-Add_Gaussian_Noise(fun_points_01_y_set8, 0, 1)
fun_points_02_y_noisy_set8<-Add_Gaussian_Noise(fun_points_02_y_set8, 0, 1)
fun_points_01_y_noisy_set9<-list()
fun_points_02_y_noisy_set9<-list()
fun_points_01_y_noisy_set9<-Add_Gaussian_Noise(fun_points_01_y_set9, 0, 1)
fun_points_02_y_noisy_set9<-Add_Gaussian_Noise(fun_points_02_y_set9, 0, 1)
fun_points_01_y_noisy_set10<-list()
fun_points_02_y_noisy_set10<-list()
fun_points_01_y_noisy_set10<-Add_Gaussian_Noise(fun_points_01_y_set10, 0, 1)
fun_points_02_y_noisy_set10<-Add_Gaussian_Noise(fun_points_02_y_set10, 0, 1)


#plot to visualize the noisy data for both function 01 and 02
png(file="5_Q2_fun_01_100_Noisy_Data.png", width = 960, height = 480)
plot(x_random_extract_set1,fun_points_01_y_noisy_set1, xlab="x", ylab="y=Q2_fun_01(x) + guassian noise", main="Q2_fun_01 Noisy Data Plot", xlim=c(-1000,1000), pch=16, col="orange", col.main = "darkgrey")
dev.off()
png(file="6_Q2_fun_02_100_Noisy_Data.png", width = 960, height = 480)
plot(x_random_extract_set1,fun_points_02_y_noisy_set1, xlab="x axis", ylab="y=Q2_fun_02(x) + gaussian noise", main="Q2_fun_02 Noisy Data Plot", xlim=c(-1000,1000), pch=16, col="purple", col.main = "darkgrey")
dev.off()


#Formulation of the training data into a data frame 
data_frame_1_set1 <- data.frame(x_random_extract_set1, do.call(rbind,fun_points_01_y_noisy_set1))
names(data_frame_1_set1) <- c ("XValue", "Q2_fun_01_noisy_output")
data_frame_1_set1 <- data_frame_1_set1 [, c(1,2)]
data_frame_1_set1

data_frame_1_set2 <- data.frame(x_random_extract_set2, do.call(rbind,fun_points_01_y_noisy_set2))
names(data_frame_1_set2) <- c ("XValue", "Q2_fun_01_noisy_output")
data_frame_1_set2 <- data_frame_1_set2 [, c(1,2)]

data_frame_1_set3 <- data.frame(x_random_extract_set3, do.call(rbind,fun_points_01_y_noisy_set3))
names(data_frame_1_set3) <- c ("XValue", "Q2_fun_01_noisy_output")
data_frame_1_set3 <- data_frame_1_set3 [, c(1,2)]

data_frame_1_set4 <- data.frame(x_random_extract_set4, do.call(rbind,fun_points_01_y_noisy_set4))
names(data_frame_1_set1) <- c ("XValue", "Q2_fun_01_noisy_output")
data_frame_1_set4 <- data_frame_1_set4 [, c(1,2)]

data_frame_1_set5 <- data.frame(x_random_extract_set5, do.call(rbind,fun_points_01_y_noisy_set5))
names(data_frame_1_set5) <- c ("XValue", "Q2_fun_01_noisy_output")
data_frame_1_set5 <- data_frame_1_set5 [, c(1,2)]

data_frame_1_set6 <- data.frame(x_random_extract_set6, do.call(rbind,fun_points_01_y_noisy_set6))
names(data_frame_1_set6) <- c ("XValue", "Q2_fun_01_noisy_output")
data_frame_1_set6 <- data_frame_1_set6 [, c(1,2)]

data_frame_1_set7 <- data.frame(x_random_extract_set7, do.call(rbind,fun_points_01_y_noisy_set7))
names(data_frame_1_set7) <- c ("XValue", "Q2_fun_01_noisy_output")
data_frame_1_set7 <- data_frame_1_set7 [, c(1,2)]

data_frame_1_set8 <- data.frame(x_random_extract_set8, do.call(rbind,fun_points_01_y_noisy_set8))
names(data_frame_1_set8) <- c ("XValue", "Q2_fun_01_noisy_output")
data_frame_1_set8 <- data_frame_1_set8 [, c(1,2)]

data_frame_1_set9 <- data.frame(x_random_extract_set9, do.call(rbind,fun_points_01_y_noisy_set9))
names(data_frame_1_set9) <- c ("XValue", "Q2_fun_01_noisy_output")
data_frame_1_set9 <- data_frame_1_set9 [, c(1,2)]

data_frame_1_set10 <- data.frame(x_random_extract_set10, do.call(rbind,fun_points_01_y_noisy_set10))
names(data_frame_1_set10) <- c ("XValue", "Q2_fun_01_noisy_output")
data_frame_1_set10 <- data_frame_1_set10 [, c(1,2)]



data_frame_2_set1 <- data.frame(x_random_extract_set1, do.call(rbind,fun_points_02_y_noisy_set1))
names(data_frame_2_set1) <- c ("XValue", "Q2_fun_02_noisy_output")
data_frame_2_set1 <- data_frame_2_set1 [, c(1,2)]
data_frame_2_set1

data_frame_2_set2 <- data.frame(x_random_extract_set2, do.call(rbind,fun_points_02_y_noisy_set2))
names(data_frame_2_set2) <- c ("XValue", "Q2_fun_02_noisy_output")
data_frame_2_set2 <- data_frame_2_set2 [, c(1,2)]

data_frame_2_set3 <- data.frame(x_random_extract_set3, do.call(rbind,fun_points_02_y_noisy_set3))
names(data_frame_2_set3) <- c ("XValue", "Q2_fun_02_noisy_output")
data_frame_2_set3 <- data_frame_2_set3 [, c(1,2)]

data_frame_2_set4 <- data.frame(x_random_extract_set4, do.call(rbind,fun_points_02_y_noisy_set4))
names(data_frame_2_set4) <- c ("XValue", "Q2_fun_02_noisy_output")
data_frame_2_set4 <- data_frame_2_set4 [, c(1,2)]

data_frame_2_set5 <- data.frame(x_random_extract_set5, do.call(rbind,fun_points_02_y_noisy_set5))
names(data_frame_2_set5) <- c ("XValue", "Q2_fun_02_noisy_output")
data_frame_2_set5 <- data_frame_2_set5 [, c(1,2)]

data_frame_2_set6 <- data.frame(x_random_extract_set6, do.call(rbind,fun_points_02_y_noisy_set6))
names(data_frame_2_set6) <- c ("XValue", "Q2_fun_02_noisy_output")
data_frame_2_set6 <- data_frame_2_set6 [, c(1,2)]

data_frame_2_set7 <- data.frame(x_random_extract_set7, do.call(rbind,fun_points_02_y_noisy_set7))
names(data_frame_2_set7) <- c ("XValue", "Q2_fun_02_noisy_output")
data_frame_2_set7 <- data_frame_2_set7 [, c(1,2)]

data_frame_2_set8 <- data.frame(x_random_extract_set8, do.call(rbind,fun_points_02_y_noisy_set8))
names(data_frame_2_set8) <- c ("XValue", "Q2_fun_02_noisy_output")
data_frame_2_set8 <- data_frame_2_set8 [, c(1,2)]

data_frame_2_set9 <- data.frame(x_random_extract_set9, do.call(rbind,fun_points_02_y_noisy_set9))
names(data_frame_2_set9) <- c ("XValue", "Q2_fun_02_noisy_output")
data_frame_2_set9 <- data_frame_2_set9 [, c(1,2)]

data_frame_2_set10 <- data.frame(x_random_extract_set10, do.call(rbind,fun_points_02_y_noisy_set10))
names(data_frame_2_set10) <- c ("XValue", "Q2_fun_02_noisy_output")
data_frame_2_set10 <- data_frame_2_set10 [, c(1,2)]

#regression model creation for function Q2_fun_01 to fit polynomials of degree 1, 9 and 18
#coefficients of each regression model are listed
#we are using all the 100 random points to train the model
linear_model_01_d1_set1<-lm(data_frame_1_set1[,2] ~ poly(x_random_extract_set1,1), data = data_frame_1_set1) 
coefficients(linear_model_01_d1_set1)
linear_model_01_d9_set1<-lm(data_frame_1_set1[,2] ~ poly(x_random_extract_set1,9), data = data_frame_1_set1) 
coefficients(linear_model_01_d9_set1)
linear_model_01_d18_set1<-lm(data_frame_1_set1[,2] ~ poly(x_random_extract_set1,18), data = data_frame_1_set1) 
coefficients(linear_model_01_d18_set1)

linear_model_01_d1_set2<-lm(data_frame_1_set2[,2] ~ poly(x_random_extract_set2,1), data = data_frame_1_set2) 
linear_model_01_d9_set2<-lm(data_frame_1_set2[,2] ~ poly(x_random_extract_set2,9), data = data_frame_1_set2) 
linear_model_01_d18_set2<-lm(data_frame_1_set2[,2] ~ poly(x_random_extract_set2,18), data = data_frame_1_set2) 

linear_model_01_d1_set3<-lm(data_frame_1_set3[,2] ~ poly(x_random_extract_set3,1), data = data_frame_1_set3) 
linear_model_01_d9_set3<-lm(data_frame_1_set3[,2] ~ poly(x_random_extract_set3,9), data = data_frame_1_set3) 
linear_model_01_d18_set3<-lm(data_frame_1_set3[,2] ~ poly(x_random_extract_set3,18), data = data_frame_1_set3)

linear_model_01_d1_set4<-lm(data_frame_1_set4[,2] ~ poly(x_random_extract_set4,1), data = data_frame_1_set4) 
linear_model_01_d9_set4<-lm(data_frame_1_set4[,2] ~ poly(x_random_extract_set4,9), data = data_frame_1_set4) 
linear_model_01_d18_set4<-lm(data_frame_1_set4[,2] ~ poly(x_random_extract_set4,18), data = data_frame_1_set4)

linear_model_01_d1_set5<-lm(data_frame_1_set5[,2] ~ poly(x_random_extract_set4,1), data = data_frame_1_set5) 
linear_model_01_d9_set5<-lm(data_frame_1_set5[,2] ~ poly(x_random_extract_set4,9), data = data_frame_1_set5) 
linear_model_01_d18_set5<-lm(data_frame_1_set5[,2] ~ poly(x_random_extract_set4,18), data = data_frame_1_set5)

linear_model_01_d1_set6<-lm(data_frame_1_set6[,2] ~ poly(x_random_extract_set6,1), data = data_frame_1_set6) 
linear_model_01_d9_set6<-lm(data_frame_1_set6[,2] ~ poly(x_random_extract_set6,9), data = data_frame_1_set6) 
linear_model_01_d18_set6<-lm(data_frame_1_set6[,2] ~ poly(x_random_extract_set6,18), data = data_frame_1_set6)

linear_model_01_d1_set7<-lm(data_frame_1_set7[,2] ~ poly(x_random_extract_set7,1), data = data_frame_1_set7)
linear_model_01_d9_set7<-lm(data_frame_1_set7[,2] ~ poly(x_random_extract_set7,9), data = data_frame_1_set7) 
linear_model_01_d18_set7<-lm(data_frame_1_set7[,2] ~ poly(x_random_extract_set7,18), data = data_frame_1_set7)

linear_model_01_d1_set8<-lm(data_frame_1_set8[,2] ~ poly(x_random_extract_set8,1), data = data_frame_1_set8) 
linear_model_01_d9_set8<-lm(data_frame_1_set8[,2] ~ poly(x_random_extract_set8,9), data = data_frame_1_set8) 
linear_model_01_d18_set8<-lm(data_frame_1_set8[,2] ~ poly(x_random_extract_set8,18), data = data_frame_1_set8)

linear_model_01_d1_set9<-lm(data_frame_1_set9[,2] ~ poly(x_random_extract_set9,1), data = data_frame_1_set9) 
linear_model_01_d9_set9<-lm(data_frame_1_set9[,2] ~ poly(x_random_extract_set9,9), data = data_frame_1_set9) 
linear_model_01_d18_set9<-lm(data_frame_1_set9[,2] ~ poly(x_random_extract_set9,18), data = data_frame_1_set9)

linear_model_01_d1_set10<-lm(data_frame_1_set10[,2] ~ poly(x_random_extract_set10,1), data = data_frame_1_set10) 
linear_model_01_d9_set10<-lm(data_frame_1_set10[,2] ~ poly(x_random_extract_set10,9), data = data_frame_1_set10) 
linear_model_01_d18_set10<-lm(data_frame_1_set10[,2] ~ poly(x_random_extract_set10,18), data = data_frame_1_set10)

#regression model creation for function Q2_fun_02 to fit polynomials of degree 1, 9 and 18
#coefficients of each regression model are listed
#we are using all the 100 random points to train the model
linear_model_02_d1_set1<-lm(data_frame_2_set1[,2] ~ poly(x_random_extract_set1,1), data = data_frame_2_set1)
coefficients(linear_model_02_d1_set1)
linear_model_02_d9_set1<-lm(data_frame_2_set1[,2] ~ poly(x_random_extract_set1,9), data = data_frame_2_set1)
coefficients(linear_model_02_d9_set1)
linear_model_02_d18_set1<-lm(data_frame_2_set1[,2] ~ poly(x_random_extract_set1,18), data = data_frame_2_set1)
coefficients(linear_model_02_d18_set1)

linear_model_02_d1_set2<-lm(data_frame_2_set2[,2] ~ poly(x_random_extract_set2,1), data = data_frame_2_set2) 
linear_model_02_d9_set2<-lm(data_frame_2_set2[,2] ~ poly(x_random_extract_set2,9), data = data_frame_2_set2) 
linear_model_02_d18_set2<-lm(data_frame_2_set2[,2] ~ poly(x_random_extract_set2,18), data = data_frame_2_set2) 

linear_model_02_d1_set3<-lm(data_frame_2_set3[,2] ~ poly(x_random_extract_set3,1), data = data_frame_2_set3) 
linear_model_02_d9_set3<-lm(data_frame_2_set3[,2] ~ poly(x_random_extract_set3,9), data = data_frame_2_set3) 
linear_model_02_d18_set3<-lm(data_frame_2_set3[,2] ~ poly(x_random_extract_set3,18), data = data_frame_2_set3)

linear_model_02_d1_set4<-lm(data_frame_2_set4[,2] ~ poly(x_random_extract_set4,1), data = data_frame_2_set4) 
linear_model_02_d9_set4<-lm(data_frame_2_set4[,2] ~ poly(x_random_extract_set4,9), data = data_frame_2_set4) 
linear_model_02_d18_set4<-lm(data_frame_2_set4[,2] ~ poly(x_random_extract_set4,18), data = data_frame_2_set4)

linear_model_02_d1_set5<-lm(data_frame_2_set5[,2] ~ poly(x_random_extract_set4,1), data = data_frame_2_set5) 
linear_model_02_d9_set5<-lm(data_frame_2_set5[,2] ~ poly(x_random_extract_set4,9), data = data_frame_2_set5) 
linear_model_02_d18_set5<-lm(data_frame_2_set5[,2] ~ poly(x_random_extract_set4,18), data = data_frame_2_set5)

linear_model_02_d1_set6<-lm(data_frame_2_set6[,2] ~ poly(x_random_extract_set6,1), data = data_frame_2_set6) 
linear_model_02_d9_set6<-lm(data_frame_2_set6[,2] ~ poly(x_random_extract_set6,9), data = data_frame_2_set6) 
linear_model_02_d18_set6<-lm(data_frame_2_set6[,2] ~ poly(x_random_extract_set6,18), data = data_frame_2_set6)

linear_model_02_d1_set7<-lm(data_frame_2_set7[,2] ~ poly(x_random_extract_set7,1), data = data_frame_2_set7)
linear_model_02_d9_set7<-lm(data_frame_2_set7[,2] ~ poly(x_random_extract_set7,9), data = data_frame_2_set7) 
linear_model_02_d18_set7<-lm(data_frame_2_set7[,2] ~ poly(x_random_extract_set7,18), data = data_frame_2_set7)

linear_model_02_d1_set8<-lm(data_frame_2_set8[,2] ~ poly(x_random_extract_set8,1), data = data_frame_2_set8) 
linear_model_02_d9_set8<-lm(data_frame_2_set8[,2] ~ poly(x_random_extract_set8,9), data = data_frame_2_set8) 
linear_model_02_d18_set8<-lm(data_frame_2_set8[,2] ~ poly(x_random_extract_set8,18), data = data_frame_2_set8)

linear_model_02_d1_set9<-lm(data_frame_2_set9[,2] ~ poly(x_random_extract_set9,1), data = data_frame_2_set9) 
linear_model_02_d9_set9<-lm(data_frame_2_set9[,2] ~ poly(x_random_extract_set9,9), data = data_frame_2_set9) 
linear_model_02_d18_set9<-lm(data_frame_2_set9[,2] ~ poly(x_random_extract_set9,18), data = data_frame_2_set9)

linear_model_02_d1_set10<-lm(data_frame_2_set10[,2] ~ poly(x_random_extract_set10,1), data = data_frame_2_set10) 
linear_model_02_d9_set10<-lm(data_frame_2_set10[,2] ~ poly(x_random_extract_set10,9), data = data_frame_2_set10) 
linear_model_02_d18_set10<-lm(data_frame_2_set10[,2] ~ poly(x_random_extract_set10,18), data = data_frame_2_set10)




#Plots of the regression models based on 300 x values for Q2_fun_01 for all three polynomials
#degree 1
fun_01_d1_a <- with(data_frame_1_set1, seq(min(x_random_extract_set1), max(x_random_extract_set1), length.out=300))
fun_01_d1_b <- predict(linear_model_01_d1_set1, newdata = data.frame(x_random_extract_set1 = fun_01_d1_a))
png(file="7_Q2_fun_01_polyfit_degree1.png", width = 960, height = 480)
plot(fun_01_d1_b ~ fun_01_d1_a, data = data_frame_1_set1,ylim =c(-2,2), xlab = "x", ylab= "polyfit_degree1_for_Q2_fun_01", main = "Q2_fun_01 polyfit degree 1", col.main = "darkgray", col = "orange", pch = 20)
lines(fun_01_d1_a, fun_01_d1_b, col = "orange", lty = 1)
points(x_random_extract_set1,fun_points_01_y_noisy_set1, col = "red", pch = 20)
dev.off()

#degree 9
fun_01_d9_a <- with(data_frame_1_set1, seq(min(x_random_extract_set1), max(x_random_extract_set1), length.out=300))
fun_01_d9_b <- predict(linear_model_01_d9_set1, newdata = data.frame(x_random_extract_set1 = fun_01_d9_a))
png(file="8_Q2_fun_01_polyfit_degree9.png", width = 960, height = 480)
plot(fun_01_d9_b ~ fun_01_d9_a, data = data_frame_1_set1, xlab = "x", ylab= "polyfit_degree9_for_Q2_fun_01", main = "Q2_fun_01 polyfit degree 9", col.main = "darkgray", col = "orange", pch = 20)
lines(fun_01_d9_a, fun_01_d9_b, col = "orange", lty = 1)
points(x_random_extract_set1,fun_points_01_y_noisy_set1, col = "red", pch = 20)
dev.off()

#degree 18
fun_01_d18_a <- with(data_frame_1, seq(min(x_random_extract_set1), max(x_random_extract_set1), length.out=300))
fun_01_d18_b <- predict(linear_model_01_d18_set1, newdata = data.frame(x_random_extract_set1 = fun_01_d18_a))
png(file="9_Q2_fun_01_polyfit_degree18.png", width = 960, height = 480)
plot(fun_01_d18_b ~ fun_01_d18_a, data = data_frame_1_set1, xlab = "x", ylab= "polyfit_degree18_for_Q2_fun_01", main = "Q2_fun_01 polyfit degree 18", col.main = "darkgray", col = "orange", pch = 20)
lines(fun_01_d18_a, fun_01_d18_b, col = "orange", lty = 1)
points(x_random_extract_set1,fun_points_01_y_noisy_set1, col = "red", pch = 20)
dev.off()

#Plots of the regression models based on 300 x values for Q2_fun_02 for all three polynomials
#degree 1
fun_02_d1_c <- with(data_frame_2_set1, seq(min(x_random_extract_set1), max(x_random_extract_set1), length.out=300))
fun_02_d1_d <- predict(linear_model_02_d1_set1, newdata = data.frame(x_random_extract_set1 = fun_02_d1_c))
png(file="10_Q2_fun_02_polyfit_degree1.png", width = 960, height = 480)
plot(fun_02_d1_d ~ fun_02_d1_c, data = data_frame_2_set1, xlab = "x", ylab= "polyfit_degree1_for_Q2_fun_02", main = "Q2_fun_02 polyfit degree 1", col.main = "darkgray", col = "purple", pch = 20)
lines(fun_02_d1_c, fun_02_d1_d, col = "purple", lty = 1)
points(x_random_extract_set1,fun_points_02_y_noisy_set1, col = "red", pch = 20)
dev.off()

#degree 9
fun_02_d9_c <- with(data_frame_2_set1, seq(min(x_random_extract_set1), max(x_random_extract_set1), length.out=300))
fun_02_d9_d <- predict(linear_model_02_d9_set1, newdata = data.frame(x_random_extract_set1 = fun_02_d9_c))
png(file="11_Q2_fun_02_polyfit_degree9.png", width = 960, height = 480)
plot(fun_02_d9_d ~ fun_02_d9_c, data = data_frame_2_set1, xlab = "x", ylab= "polyfit_degree9_for_Q2_fun_02", main = "Q2_fun_02 polyfit degree 9", col.main = "darkgray", col = "purple", pch = 20)
lines(fun_02_d9_c, fun_02_d9_d, col = "purple", lty = 1)
points(x_random_extract_set1,fun_points_02_y_noisy_set1, col = "red", pch =20)
dev.off()

#degree 18
fun_02_d18_c <- with(data_frame_2_set1, seq(min(x_random_extract_set1), max(x_random_extract_set1), length.out=300))
fun_02_d18_d <- predict(linear_model_02_d18_set1, newdata = data.frame(x_random_extract_set1 = fun_02_d18_c))
png(file="12_Q2_fun_02_polyfit_degree18.png", width = 960, height = 480)
plot(fun_02_d18_d ~ fun_02_d18_c, data = data_frame_2_set1, xlab = "x", ylab= "polyfit_degree1_for_Q2_fun_02", main = "Q2_fun_02 polyfit degree 18", col.main = "darkgray", col = "purple", pch = 20)
lines(fun_02_d18_c, fun_02_d18_d, col = "purple", lty = 1)
axis(side=2, at=seq(0, 100,by = 3))
points(x_random_extract_set1,fun_points_02_y_noisy_set1, col = "red", pch =16)
dev.off()



#BIAS compute for Q2_fun_01 polynomials with x_random_extract_set1:
#bias with degree 1 polynomial fit for Q2_fun_01
#we assume our testing set same as our training set
#For all ten models, we compute bias against their own true set.
fun_01_d1_x_random_extract <- with(data_frame_1_set1, x_random_extract_set1) 
error_lm01d1 <- list()
for (i in 1:100){
  fun_01_d1_x_predicted_y <- predict(linear_model_01_d1_set1, newdata = data.frame(x_random_extract_set1 = fun_01_d1_x_random_extract))
  error_lm01d1[[i]] = bias(fun_01_d1_x_predicted_y[[i]],fun_points_01_y_noisy_set1[[i]], abs = TRUE)
  fun_01_d1_x_random_extract <- with(data_frame_1_set2, x_random_extract_set2)
  fun_01_d1_x_predicted_y <- predict(linear_model_01_d1_set2, newdata = data.frame(x_random_extract_set2 = fun_01_d1_x_random_extract))
  error_lm01d1[[i]] = error_lm01d1[[i]] + bias(fun_01_d1_x_predicted_y[[i]],fun_points_01_y_noisy_set2[[i]], abs = TRUE)
  fun_01_d1_x_random_extract <- with(data_frame_1_set3, x_random_extract_set3)
  fun_01_d1_x_predicted_y <- predict(linear_model_01_d1_set3, newdata = data.frame(x_random_extract_set3 = fun_01_d1_x_random_extract))
  error_lm01d1[[i]] = error_lm01d1[[i]] + bias(fun_01_d1_x_predicted_y[[i]],fun_points_01_y_noisy_set3[[i]], abs = TRUE)
  fun_01_d1_x_random_extract <- with(data_frame_1_set4, x_random_extract_set4)
  fun_01_d1_x_predicted_y <- predict(linear_model_01_d1_set4, newdata = data.frame(x_random_extract_set4 = fun_01_d1_x_random_extract))
  error_lm01d1[[i]] = error_lm01d1[[i]] + bias(fun_01_d1_x_predicted_y[[i]],fun_points_01_y_noisy_set4[[i]], abs = TRUE)
  fun_01_d1_x_random_extract <- with(data_frame_1_set5, x_random_extract_set5)
  fun_01_d1_x_predicted_y <- predict(linear_model_01_d1_set5, newdata = data.frame(x_random_extract_set5 = fun_01_d1_x_random_extract))
  error_lm01d1[[i]] = error_lm01d1[[i]] + bias(fun_01_d1_x_predicted_y[[i]],fun_points_01_y_noisy_set5[[i]], abs = TRUE)
  fun_01_d1_x_random_extract <- with(data_frame_1_set6, x_random_extract_set6)
  fun_01_d1_x_predicted_y <- predict(linear_model_01_d1_set6, newdata = data.frame(x_random_extract_set6 = fun_01_d1_x_random_extract))
  error_lm01d1[[i]] = error_lm01d1[[i]] + bias(fun_01_d1_x_predicted_y[[i]],fun_points_01_y_noisy_set6[[i]], abs = TRUE)
  fun_01_d1_x_random_extract <- with(data_frame_1_set7, x_random_extract_set7)  
  fun_01_d1_x_predicted_y <- predict(linear_model_01_d1_set7, newdata = data.frame(x_random_extract_set7 = fun_01_d1_x_random_extract))
  error_lm01d1[[i]] = error_lm01d1[[i]] + bias(fun_01_d1_x_predicted_y[[i]],fun_points_01_y_noisy_set7[[i]], abs = TRUE)
  fun_01_d1_x_random_extract <- with(data_frame_1_set8, x_random_extract_set8)
  fun_01_d1_x_predicted_y <- predict(linear_model_01_d1_set8, newdata = data.frame(x_random_extract_set8 = fun_01_d1_x_random_extract))
  error_lm01d1[[i]] = error_lm01d1[[i]] + bias(fun_01_d1_x_predicted_y[[i]],fun_points_01_y_noisy_set8[[i]], abs = TRUE)
  fun_01_d1_x_random_extract <- with(data_frame_1_set9, x_random_extract_set9)  
  fun_01_d1_x_predicted_y <- predict(linear_model_01_d1_set9, newdata = data.frame(x_random_extract_set9 = fun_01_d1_x_random_extract))
  error_lm01d1[[i]] = error_lm01d1[[i]] + bias(fun_01_d1_x_predicted_y[[i]],fun_points_01_y_noisy_set9[[i]], abs = TRUE)
  fun_01_d1_x_random_extract <- with(data_frame_1_set10, x_random_extract_set10)
  fun_01_d1_x_predicted_y <- predict(linear_model_01_d1_set10, newdata = data.frame(x_random_extract_set10 = fun_01_d1_x_random_extract))
  error_lm01d1[[i]] = error_lm01d1[[i]] + bias(fun_01_d1_x_predicted_y[[i]],fun_points_01_y_noisy_set10[[i]], abs = TRUE)
} #at this point error_lm01d1 contains the sum of biases with the 10 models for degree1 and func_01


#bias with degree 9 polynomial fit for Q2_fun_01
fun_01_d9_x_random_extract <- with(data_frame_1_set1, x_random_extract_set1)
error_lm01d9 <- list()
for (i in 1:100){
  fun_01_d9_x_predicted_y <- predict(linear_model_01_d9_set1, newdata = data.frame(x_random_extract_set1 = fun_01_d9_x_random_extract))
  error_lm01d9[[i]] = bias(fun_01_d9_x_predicted_y[[i]],fun_points_01_y_noisy_set1[[i]], abs = TRUE)
  fun_01_d9_x_random_extract <- with(data_frame_1_set2, x_random_extract_set2)
  fun_01_d9_x_predicted_y <- predict(linear_model_01_d9_set2, newdata = data.frame(x_random_extract_set2 = fun_01_d9_x_random_extract))
  error_lm01d9[[i]] = error_lm01d9[[i]] + bias(fun_01_d9_x_predicted_y[[i]],fun_points_01_y_noisy_set2[[i]], abs = TRUE)
  fun_01_d9_x_random_extract <- with(data_frame_1_set3, x_random_extract_set3)
  fun_01_d9_x_predicted_y <- predict(linear_model_01_d9_set3, newdata = data.frame(x_random_extract_set3 = fun_01_d9_x_random_extract))
  error_lm01d9[[i]] = error_lm01d9[[i]] + bias(fun_01_d9_x_predicted_y[[i]],fun_points_01_y_noisy_set3[[i]], abs = TRUE)
  fun_01_d9_x_random_extract <- with(data_frame_1_set4, x_random_extract_set4)
  fun_01_d9_x_predicted_y <- predict(linear_model_01_d9_set4, newdata = data.frame(x_random_extract_set4 = fun_01_d9_x_random_extract))
  error_lm01d9[[i]] = error_lm01d9[[i]] + bias(fun_01_d9_x_predicted_y[[i]],fun_points_01_y_noisy_set4[[i]], abs = TRUE)
  fun_01_d9_x_random_extract <- with(data_frame_1_set5, x_random_extract_set5)
  fun_01_d9_x_predicted_y <- predict(linear_model_01_d9_set5, newdata = data.frame(x_random_extract_set5 = fun_01_d9_x_random_extract))
  error_lm01d9[[i]] = error_lm01d9[[i]] + bias(fun_01_d9_x_predicted_y[[i]],fun_points_01_y_noisy_set5[[i]], abs = TRUE)
  fun_01_d9_x_random_extract <- with(data_frame_1_set6, x_random_extract_set6)
  fun_01_d9_x_predicted_y <- predict(linear_model_01_d9_set6, newdata = data.frame(x_random_extract_set6 = fun_01_d9_x_random_extract))
  error_lm01d9[[i]] = error_lm01d9[[i]] + bias(fun_01_d9_x_predicted_y[[i]],fun_points_01_y_noisy_set6[[i]], abs = TRUE)
  fun_01_d9_x_random_extract <- with(data_frame_1_set7, x_random_extract_set7)
  fun_01_d9_x_predicted_y <- predict(linear_model_01_d9_set7, newdata = data.frame(x_random_extract_set7 = fun_01_d9_x_random_extract))
  error_lm01d9[[i]] = error_lm01d9[[i]] + bias(fun_01_d9_x_predicted_y[[i]],fun_points_01_y_noisy_set7[[i]], abs = TRUE)
  fun_01_d9_x_random_extract <- with(data_frame_1_set8, x_random_extract_set8)
  fun_01_d9_x_predicted_y <- predict(linear_model_01_d9_set8, newdata = data.frame(x_random_extract_set8 = fun_01_d9_x_random_extract))
  error_lm01d9[[i]] = error_lm01d9[[i]] + bias(fun_01_d9_x_predicted_y[[i]],fun_points_01_y_noisy_set8[[i]], abs = TRUE)
  fun_01_d9_x_random_extract <- with(data_frame_1_set9, x_random_extract_set9)
  fun_01_d9_x_predicted_y <- predict(linear_model_01_d9_set9, newdata = data.frame(x_random_extract_set9 = fun_01_d9_x_random_extract))
  error_lm01d9[[i]] = error_lm01d9[[i]] + bias(fun_01_d9_x_predicted_y[[i]],fun_points_01_y_noisy_set9[[i]], abs = TRUE)
  fun_01_d9_x_random_extract <- with(data_frame_1_set10, x_random_extract_set10)
  fun_01_d9_x_predicted_y <- predict(linear_model_01_d9_set10, newdata = data.frame(x_random_extract_set10 = fun_01_d9_x_random_extract))
  error_lm01d9[[i]] = error_lm01d9[[i]] + bias(fun_01_d9_x_predicted_y[[i]],fun_points_01_y_noisy_set10[[i]], abs = TRUE)
} #at this point error_lm01d9 contains the sum of biases with the 10 models for degree1 and func_01
  
#bias with degree 18 polynomial fit for Q2_fun_01
fun_01_d18_x_random_extract <- with(data_frame_1_set1, x_random_extract_set1)
error_lm01d18 <- list()
for (i in 1:100){
  fun_01_d18_x_predicted_y <- predict(linear_model_01_d18_set1, newdata = data.frame(x_random_extract_set1 = fun_01_d18_x_random_extract))
  error_lm01d18[[i]] = bias(fun_01_d18_x_predicted_y[[i]],fun_points_01_y_noisy_set1[[i]], abs = TRUE)
  fun_01_d18_x_random_extract <- with(data_frame_1_set2, x_random_extract_set2)
  fun_01_d18_x_predicted_y <- predict(linear_model_01_d18_set2, newdata = data.frame(x_random_extract_set2 = fun_01_d18_x_random_extract))
  error_lm01d18[[i]] = error_lm01d18[[i]] + bias(fun_01_d18_x_predicted_y[[i]],fun_points_01_y_noisy_set2[[i]], abs = TRUE)
  fun_01_d18_x_random_extract <- with(data_frame_1_set3, x_random_extract_set3)
  fun_01_d18_x_predicted_y <- predict(linear_model_01_d18_set3, newdata = data.frame(x_random_extract_set3 = fun_01_d18_x_random_extract))
  error_lm01d18[[i]] = error_lm01d18[[i]] + bias(fun_01_d18_x_predicted_y[[i]],fun_points_01_y_noisy_set3[[i]], abs = TRUE)
  fun_01_d18_x_random_extract <- with(data_frame_1_set4, x_random_extract_set4)
  fun_01_d18_x_predicted_y <- predict(linear_model_01_d18_set4, newdata = data.frame(x_random_extract_set4 = fun_01_d18_x_random_extract))
  error_lm01d18[[i]] = error_lm01d18[[i]] + bias(fun_01_d18_x_predicted_y[[i]],fun_points_01_y_noisy_set4[[i]], abs = TRUE)
  fun_01_d18_x_random_extract <- with(data_frame_1_set5, x_random_extract_set5)
  fun_01_d18_x_predicted_y <- predict(linear_model_01_d18_set5, newdata = data.frame(x_random_extract_set5 = fun_01_d18_x_random_extract))
  error_lm01d18[[i]] = error_lm01d18[[i]] + bias(fun_01_d18_x_predicted_y[[i]],fun_points_01_y_noisy_set5[[i]], abs = TRUE)
  fun_01_d18_x_random_extract <- with(data_frame_1_set6, x_random_extract_set6)
  fun_01_d18_x_predicted_y <- predict(linear_model_01_d18_set6, newdata = data.frame(x_random_extract_set6 = fun_01_d18_x_random_extract))
  error_lm01d18[[i]] = error_lm01d18[[i]] + bias(fun_01_d18_x_predicted_y[[i]],fun_points_01_y_noisy_set6[[i]], abs = TRUE)
  fun_01_d18_x_random_extract <- with(data_frame_1_set7, x_random_extract_set7)
  fun_01_d18_x_predicted_y <- predict(linear_model_01_d18_set7, newdata = data.frame(x_random_extract_set7 = fun_01_d18_x_random_extract))
  error_lm01d18[[i]] = error_lm01d18[[i]] + bias(fun_01_d18_x_predicted_y[[i]],fun_points_01_y_noisy_set7[[i]], abs = TRUE)
  fun_01_d18_x_random_extract <- with(data_frame_1_set8, x_random_extract_set8)
  fun_01_d18_x_predicted_y <- predict(linear_model_01_d18_set8, newdata = data.frame(x_random_extract_set8 = fun_01_d18_x_random_extract))
  error_lm01d18[[i]] = error_lm01d18[[i]] + bias(fun_01_d18_x_predicted_y[[i]],fun_points_01_y_noisy_set8[[i]], abs = TRUE)
  fun_01_d18_x_random_extract <- with(data_frame_1_set9, x_random_extract_set9)
  fun_01_d18_x_predicted_y <- predict(linear_model_01_d18_set9, newdata = data.frame(x_random_extract_set9 = fun_01_d18_x_random_extract))
  error_lm01d18[[i]] = error_lm01d18[[i]] + bias(fun_01_d18_x_predicted_y[[i]],fun_points_01_y_noisy_set9[[i]], abs = TRUE)
  fun_01_d18_x_random_extract <- with(data_frame_1_set10, x_random_extract_set10)
  fun_01_d18_x_predicted_y <- predict(linear_model_01_d18_set10, newdata = data.frame(x_random_extract_set10 = fun_01_d18_x_random_extract))
  error_lm01d18[[i]] = error_lm01d18[[i]] + bias(fun_01_d18_x_predicted_y[[i]],fun_points_01_y_noisy_set10[[i]], abs = TRUE)
} #at this point error_lm01d18 contains the sum of biases with the 10 models for degree1 and func_01

bias1_01=(mean(as.numeric(error_lm01d1)))/10
bias2_01=(mean(as.numeric(error_lm01d9)))/10
bias3_01=(mean(as.numeric(error_lm01d18)))/10  #div by 10 is to take the expectation or average of the 10 models

bias_fun_01<-c(bias1_01,bias2_01,bias3_01)
bias_fun_01


#residual variance same as slope variance = (summary(linear_model_01_d18)$coefficients[2,2])**2 for Q2_fun_01
v1_01_set1=(summary(linear_model_01_d1_set1)$sigma)**2
v2_01_set1=(summary(linear_model_01_d9_set1)$sigma)**2
v3_01_set1=(summary(linear_model_01_d18_set1)$sigma)**2

v1_01_set2=(summary(linear_model_01_d1_set2)$sigma)**2
v2_01_set2=(summary(linear_model_01_d9_set2)$sigma)**2
v3_01_set2=(summary(linear_model_01_d18_set2)$sigma)**2

v1_01_set3=(summary(linear_model_01_d1_set3)$sigma)**2
v2_01_set3=(summary(linear_model_01_d9_set3)$sigma)**2
v3_01_set3=(summary(linear_model_01_d18_set3)$sigma)**2

v1_01_set4=(summary(linear_model_01_d1_set4)$sigma)**2
v2_01_set4=(summary(linear_model_01_d9_set4)$sigma)**2
v3_01_set4=(summary(linear_model_01_d18_set4)$sigma)**2


v1_01_set5=(summary(linear_model_01_d1_set5)$sigma)**2
v2_01_set5=(summary(linear_model_01_d9_set5)$sigma)**2
v3_01_set5=(summary(linear_model_01_d18_set5)$sigma)**2



v1_01_set6=(summary(linear_model_01_d1_set6)$sigma)**2
v2_01_set6=(summary(linear_model_01_d9_set6)$sigma)**2
v3_01_set6=(summary(linear_model_01_d18_set6)$sigma)**2


v1_01_set7=(summary(linear_model_01_d1_set7)$sigma)**2
v2_01_set7=(summary(linear_model_01_d9_set7)$sigma)**2
v3_01_set7=(summary(linear_model_01_d18_set7)$sigma)**2


v1_01_set8=(summary(linear_model_01_d1_set8)$sigma)**2
v2_01_set8=(summary(linear_model_01_d9_set8)$sigma)**2
v3_01_set8=(summary(linear_model_01_d18_set8)$sigma)**2


v1_01_set9=(summary(linear_model_01_d1_set9)$sigma)**2
v2_01_set9=(summary(linear_model_01_d9_set9)$sigma)**2
v3_01_set9=(summary(linear_model_01_d18_set9)$sigma)**2


v1_01_set10=(summary(linear_model_01_d1_set10)$sigma)**2
v2_01_set10=(summary(linear_model_01_d9_set10)$sigma)**2
v3_01_set10=(summary(linear_model_01_d18_set10)$sigma)**2

v1_01 = (v1_01_set1+v1_01_set2+v1_01_set3+v1_01_set4+v1_01_set5+v1_01_set6+v1_01_set9+v1_01_set10)/10   #degree 1
v2_01 = (v2_01_set1+v2_01_set2+v2_01_set3+v2_01_set4+v2_01_set5+v2_01_set6+v2_01_set9+v2_01_set10)/10   #degree 9
v3_01 = (v3_01_set1+v3_01_set2+v3_01_set3+v3_01_set4+v3_01_set5+v3_01_set6+v3_01_set9+v3_01_set10)/10   #degree 18


var_fun_01<-c(v1_01,v2_01,v3_01)
degree<-c(1,9,18)

png(file="13_Q2_fun_01_bias_variance.png", width = 960, height = 480)
plot(degree, bias_fun_01, col = "red", type = "l", xlab="degree of polynomial", ylab = "bias/variance", yaxt="n", main = "Bias/Variance vs degree of polynomial for Q2_fun_01", col.main = "darkgray")
points(degree,bias_fun_01, col = "red", pch =16)
par(new=TRUE)
plot(degree, var_fun_01, col = "blue", type = "l", xlab="", ylab = "")
points(degree,var_fun_01, col = "blue", pch =16)
legend("topright", 
       legend = c("Bias", "Variance"), 
       col = c(col = "red", 
               col = "blue"), 
       pch = c(16,16), 
       bty = "n", 
       pt.cex = 2, 
       cex = 1.2, 
       text.col = "black", 
       horiz = F , 
       inset = c(0.1, 0.1))
dev.off()



#BIAS compute for Q2_fun_02 polynomials with x_random_extract_set1:
#bias with degree 1 polynomial fit for Q2_fun_02
#we assume our testing or true set to be: (x_random_extract_set1, fun_points_02_y_noisy_set1)
#For all ten models, we compute bias against this true set.
fun_02_d1_x_random_extract <- with(data_frame_2_set1, x_random_extract_set1) 
error_lm01d1 <- list()
v1_01 = 0
for (i in 1:100){
  fun_02_d1_x_predicted_y <- predict(linear_model_02_d1_set1, newdata = data.frame(x_random_extract_set1 = fun_02_d1_x_random_extract))
  error_lm02d1[[i]] = bias(fun_02_d1_x_predicted_y[[i]],fun_points_01_y_noisy_set1[[i]], abs = TRUE)
  if (i == 100) {
    v1_02 = var(fun_02_d1_x_predicted_y)
  }
  fun_02_d1_x_random_extract <- with(data_frame_2_set2, x_random_extract_set2) 
  fun_02_d1_x_predicted_y <- predict(linear_model_02_d1_set2, newdata = data.frame(x_random_extract_set2 = fun_02_d1_x_random_extract))
  error_lm02d1[[i]] = error_lm02d1[[i]] + bias(fun_02_d1_x_predicted_y[[i]],fun_points_02_y_noisy_set2[[i]], abs = TRUE)
  if (i == 100) {
    v1_02 = v1_02 + var(fun_02_d1_x_predicted_y)
  }
  fun_02_d1_x_random_extract <- with(data_frame_2_set3, x_random_extract_set3) 
  fun_02_d1_x_predicted_y <- predict(linear_model_02_d1_set3, newdata = data.frame(x_random_extract_set3 = fun_02_d1_x_random_extract))
  error_lm02d1[[i]] = error_lm02d1[[i]] + bias(fun_02_d1_x_predicted_y[[i]],fun_points_02_y_noisy_set3[[i]], abs = TRUE)
  if (i == 100) {
    v1_02 = v1_02 + var(fun_02_d1_x_predicted_y)
  }
  fun_02_d1_x_random_extract <- with(data_frame_2_set4, x_random_extract_set4) 
  fun_02_d1_x_predicted_y <- predict(linear_model_02_d1_set4, newdata = data.frame(x_random_extract_set4 = fun_02_d1_x_random_extract))
  error_lm02d1[[i]] = error_lm02d1[[i]] + bias(fun_02_d1_x_predicted_y[[i]],fun_points_02_y_noisy_set4[[i]], abs = TRUE)
  if (i == 100) {
    v1_02 = v1_02 + var(fun_02_d1_x_predicted_y)
  }
  fun_02_d1_x_random_extract <- with(data_frame_2_set5, x_random_extract_set5) 
  fun_02_d1_x_predicted_y <- predict(linear_model_02_d1_set5, newdata = data.frame(x_random_extract_set5 = fun_02_d1_x_random_extract))
  error_lm02d1[[i]] = error_lm02d1[[i]] + bias(fun_02_d1_x_predicted_y[[i]],fun_points_02_y_noisy_set5[[i]], abs = TRUE)
  if (i == 100) {
    v1_02 = v1_02 + var(fun_02_d1_x_predicted_y)
  }
  fun_02_d1_x_random_extract <- with(data_frame_2_set6, x_random_extract_set6) 
  fun_02_d1_x_predicted_y <- predict(linear_model_02_d1_set6, newdata = data.frame(x_random_extract_set6 = fun_02_d1_x_random_extract))
  error_lm02d1[[i]] = error_lm02d1[[i]] + bias(fun_02_d1_x_predicted_y[[i]],fun_points_02_y_noisy_set6[[i]], abs = TRUE)
  if (i == 100) {
    v1_02 = v1_02 + var(fun_02_d1_x_predicted_y)
  }
  fun_02_d1_x_random_extract <- with(data_frame_2_set7, x_random_extract_set7) 
  fun_02_d1_x_predicted_y <- predict(linear_model_02_d1_set7, newdata = data.frame(x_random_extract_set7 = fun_02_d1_x_random_extract))
  error_lm02d1[[i]] = error_lm02d1[[i]] + bias(fun_02_d1_x_predicted_y[[i]],fun_points_02_y_noisy_set7[[i]], abs = TRUE)
  if (i == 100) {
    v1_02 = v1_02 + var(fun_02_d1_x_predicted_y)
  }
  fun_02_d1_x_random_extract <- with(data_frame_2_set8, x_random_extract_set8) 
  fun_02_d1_x_predicted_y <- predict(linear_model_02_d1_set8, newdata = data.frame(x_random_extract_set8 = fun_02_d1_x_random_extract))
  error_lm02d1[[i]] = error_lm02d1[[i]] + bias(fun_02_d1_x_predicted_y[[i]],fun_points_02_y_noisy_set8[[i]], abs = TRUE)
  if (i == 100) {
    v1_02 = v1_02 + var(fun_02_d1_x_predicted_y)
  }
  fun_02_d1_x_random_extract <- with(data_frame_2_set9, x_random_extract_set9) 
  fun_02_d1_x_predicted_y <- predict(linear_model_02_d1_set9, newdata = data.frame(x_random_extract_set9 = fun_02_d1_x_random_extract))
  error_lm02d1[[i]] = error_lm02d1[[i]] + bias(fun_02_d1_x_predicted_y[[i]],fun_points_02_y_noisy_set9[[i]], abs = TRUE)
  if (i == 100) {
    v1_02 = v1_02 + var(fun_02_d1_x_predicted_y)
  }
  fun_02_d1_x_random_extract <- with(data_frame_2_set10, x_random_extract_set10) 
  fun_02_d1_x_predicted_y <- predict(linear_model_02_d1_set10, newdata = data.frame(x_random_extract_set10 = fun_02_d1_x_random_extract))
  error_lm02d1[[i]] = error_lm02d1[[i]] + bias(fun_02_d1_x_predicted_y[[i]],fun_points_02_y_noisy_set10[[i]], abs = TRUE)
  if (i == 100) {
    v1_02 = v1_02 + var(fun_02_d1_x_predicted_y)
  }
} #at this point error_lm01d1 contains the sum of biases with the 10 models for degree1 and func_01


#bias with degree 9 polynomial fit for Q2_fun_01
fun_01_d9_x_random_extract <- with(data_frame_2_set1, x_random_extract_set1)
error_lm01d9 <- list()
v2_02 = 0
for (i in 1:100){
  fun_02_d9_x_predicted_y <- predict(linear_model_02_d9_set1, newdata = data.frame(x_random_extract_set1 = fun_02_d9_x_random_extract))
  error_lm02d9[[i]] = bias(fun_02_d9_x_predicted_y[[i]],fun_points_02_y_noisy_set1[[i]], abs = TRUE)
  if (i == 100) {
    v2_02 = v2_02 + var(fun_02_d9_x_predicted_y)
  }
  fun_01_d9_x_random_extract <- with(data_frame_2_set2, x_random_extract_set2)
  fun_02_d9_x_predicted_y <- predict(linear_model_02_d9_set2, newdata = data.frame(x_random_extract_set2 = fun_02_d9_x_random_extract))
  error_lm02d9[[i]] = error_lm02d9[[i]] + bias(fun_02_d9_x_predicted_y[[i]],fun_points_02_y_noisy_set2[[i]], abs = TRUE)
  if (i == 100) {
    v2_02 = v2_02 + var(fun_02_d9_x_predicted_y)
  }
  fun_01_d9_x_random_extract <- with(data_frame_2_set3, x_random_extract_set3)
  fun_02_d9_x_predicted_y <- predict(linear_model_02_d9_set3, newdata = data.frame(x_random_extract_set3 = fun_02_d9_x_random_extract))
  error_lm02d9[[i]] = error_lm02d9[[i]] + bias(fun_02_d9_x_predicted_y[[i]],fun_points_02_y_noisy_set3[[i]], abs = TRUE)
  if (i == 100) {
    v2_02 = v2_02 + var(fun_02_d9_x_predicted_y)
  }
  fun_01_d9_x_random_extract <- with(data_frame_2_set4, x_random_extract_set4)
  fun_02_d9_x_predicted_y <- predict(linear_model_02_d9_set4, newdata = data.frame(x_random_extract_set4 = fun_02_d9_x_random_extract))
  error_lm02d9[[i]] = error_lm02d9[[i]] + bias(fun_02_d9_x_predicted_y[[i]],fun_points_02_y_noisy_set4[[i]], abs = TRUE)
  if (i == 100) {
    v2_02 = v2_02 + var(fun_02_d9_x_predicted_y)
  }
  fun_01_d9_x_random_extract <- with(data_frame_2_set5, x_random_extract_set5)
  fun_02_d9_x_predicted_y <- predict(linear_model_02_d9_set5, newdata = data.frame(x_random_extract_set5 = fun_02_d9_x_random_extract))
  error_lm02d9[[i]] = error_lm02d9[[i]] + bias(fun_02_d9_x_predicted_y[[i]],fun_points_02_y_noisy_set5[[i]], abs = TRUE)
  if (i == 100) {
    v2_02 = v2_02 + var(fun_02_d9_x_predicted_y)
  }
  fun_01_d9_x_random_extract <- with(data_frame_2_set6, x_random_extract_set6)
  fun_02_d9_x_predicted_y <- predict(linear_model_02_d9_set6, newdata = data.frame(x_random_extract_set6 = fun_02_d9_x_random_extract))
  error_lm02d9[[i]] = error_lm02d9[[i]] + bias(fun_02_d9_x_predicted_y[[i]],fun_points_02_y_noisy_set6[[i]], abs = TRUE)
  if (i == 100) {
    v2_02 = v2_02 + var(fun_02_d9_x_predicted_y)
  }
  fun_01_d9_x_random_extract <- with(data_frame_2_set7, x_random_extract_set7)
  fun_02_d9_x_predicted_y <- predict(linear_model_02_d9_set7, newdata = data.frame(x_random_extract_set7 = fun_02_d9_x_random_extract))
  error_lm02d9[[i]] = error_lm02d9[[i]] + bias(fun_02_d9_x_predicted_y[[i]],fun_points_02_y_noisy_set7[[i]], abs = TRUE)
  if (i == 100) {
    v2_02 = v2_02 + var(fun_02_d9_x_predicted_y)
  }
  fun_01_d9_x_random_extract <- with(data_frame_2_set8, x_random_extract_set8)
  fun_02_d9_x_predicted_y <- predict(linear_model_02_d9_set8, newdata = data.frame(x_random_extract_set8 = fun_02_d9_x_random_extract))
  error_lm02d9[[i]] = error_lm02d9[[i]] + bias(fun_02_d9_x_predicted_y[[i]],fun_points_02_y_noisy_set8[[i]], abs = TRUE)
  if (i == 100) {
    v2_02 = v2_02 + var(fun_02_d9_x_predicted_y)
  }
  fun_01_d9_x_random_extract <- with(data_frame_2_set9, x_random_extract_set9)
  fun_02_d9_x_predicted_y <- predict(linear_model_02_d9_set9, newdata = data.frame(x_random_extract_set9 = fun_02_d9_x_random_extract))
  error_lm02d9[[i]] = error_lm02d9[[i]] + bias(fun_02_d9_x_predicted_y[[i]],fun_points_02_y_noisy_set9[[i]], abs = TRUE)
  if (i == 100) {
    v2_02 = v2_02 + var(fun_02_d9_x_predicted_y)
  }
  fun_01_d9_x_random_extract <- with(data_frame_2_set10, x_random_extract_set10)
  fun_02_d9_x_predicted_y <- predict(linear_model_02_d9_set10, newdata = data.frame(x_random_extract_set10 = fun_02_d9_x_random_extract))
  error_lm02d9[[i]] = error_lm02d9[[i]] + bias(fun_02_d9_x_predicted_y[[i]],fun_points_02_y_noisy_set10[[i]], abs = TRUE)
  if (i == 100) {
    v2_02 = v2_02 + var(fun_02_d9_x_predicted_y)
  }
} #at this point error_lm01d9 contains the sum of biases with the 10 models for degree1 and func_02

#bias with degree 18 polynomial fit for Q2_fun_02
fun_01_d18_x_random_extract <- with(data_frame_2_set1, x_random_extract_set1)
error_lm01d18 <- list()
v3_02
for (i in 1:100){
  fun_02_d18_x_predicted_y <- predict(linear_model_02_d18_set1, newdata = data.frame(x_random_extract_set1 = fun_02_d18_x_random_extract))
  error_lm02d18[[i]] = bias(fun_02_d18_x_predicted_y[[i]],fun_points_02_y_noisy_set1[[i]], abs = TRUE)
  if (i == 100) {
    v3_02 = v3_02 + var(fun_02_d18_x_predicted_y)
  }
  fun_01_d18_x_random_extract <- with(data_frame_2_set2, x_random_extract_set2)
  fun_02_d18_x_predicted_y <- predict(linear_model_02_d18_set2, newdata = data.frame(x_random_extract_set2 = fun_02_d18_x_random_extract))
  error_lm02d18[[i]] = error_lm02d18[[i]] + bias(fun_02_d18_x_predicted_y[[i]],fun_points_02_y_noisy_set2[[i]], abs = TRUE)
  if (i == 100) {
    v3_02 = v3_02 + var(fun_02_d18_x_predicted_y)
  }
  fun_01_d18_x_random_extract <- with(data_frame_2_set3, x_random_extract_set3)
  fun_02_d18_x_predicted_y <- predict(linear_model_02_d18_set3, newdata = data.frame(x_random_extract_set3 = fun_02_d18_x_random_extract))
  error_lm02d18[[i]] = error_lm02d18[[i]] + bias(fun_02_d18_x_predicted_y[[i]],fun_points_02_y_noisy_set3[[i]], abs = TRUE)
  if (i == 100) {
    v3_02 = v3_02 + var(fun_02_d18_x_predicted_y)
  }
  fun_01_d18_x_random_extract <- with(data_frame_2_set4, x_random_extract_set4)
  fun_02_d18_x_predicted_y <- predict(linear_model_02_d18_set4, newdata = data.frame(x_random_extract_set4 = fun_02_d18_x_random_extract))
  error_lm02d18[[i]] = error_lm02d18[[i]] + bias(fun_02_d18_x_predicted_y[[i]],fun_points_02_y_noisy_set4[[i]], abs = TRUE)
  if (i == 100) {
    v3_02 = v3_02 + var(fun_02_d18_x_predicted_y)
  }
  fun_01_d18_x_random_extract <- with(data_frame_2_set5, x_random_extract_set5)
  fun_02_d18_x_predicted_y <- predict(linear_model_02_d18_set5, newdata = data.frame(x_random_extract_set5 = fun_02_d18_x_random_extract))
  error_lm02d18[[i]] = error_lm02d18[[i]] + bias(fun_02_d18_x_predicted_y[[i]],fun_points_02_y_noisy_set5[[i]], abs = TRUE)
  if (i == 100) {
    v3_02 = v3_02 + var(fun_02_d18_x_predicted_y)
  }
  fun_01_d18_x_random_extract <- with(data_frame_2_set6, x_random_extract_set6)
  fun_02_d18_x_predicted_y <- predict(linear_model_02_d18_set6, newdata = data.frame(x_random_extract_set6 = fun_02_d18_x_random_extract))
  error_lm02d18[[i]] = error_lm02d18[[i]] + bias(fun_02_d18_x_predicted_y[[i]],fun_points_02_y_noisy_set6[[i]], abs = TRUE)
  if (i == 100) {
    v3_02 = v3_02 + var(fun_02_d18_x_predicted_y)
  }
  fun_01_d18_x_random_extract <- with(data_frame_2_set7, x_random_extract_set7)
  fun_02_d18_x_predicted_y <- predict(linear_model_02_d18_set7, newdata = data.frame(x_random_extract_set7 = fun_02_d18_x_random_extract))
  error_lm02d18[[i]] = error_lm02d18[[i]] + bias(fun_02_d18_x_predicted_y[[i]],fun_points_02_y_noisy_set7[[i]], abs = TRUE)
  if (i == 100) {
    v3_02 = v3_02 + var(fun_02_d18_x_predicted_y)
  }
  fun_01_d18_x_random_extract <- with(data_frame_2_set8, x_random_extract_set8)
  fun_02_d18_x_predicted_y <- predict(linear_model_02_d18_set8, newdata = data.frame(x_random_extract_set8 = fun_02_d18_x_random_extract))
  error_lm02d18[[i]] = error_lm02d18[[i]] + bias(fun_02_d18_x_predicted_y[[i]],fun_points_02_y_noisy_set8[[i]], abs = TRUE)
  if (i == 100) {
    v3_02 = v3_02 + var(fun_02_d18_x_predicted_y)
  }
  fun_01_d18_x_random_extract <- with(data_frame_2_set9, x_random_extract_set9)
  fun_02_d18_x_predicted_y <- predict(linear_model_02_d18_set9, newdata = data.frame(x_random_extract_set9 = fun_02_d18_x_random_extract))
  error_lm02d18[[i]] = error_lm02d18[[i]] + bias(fun_02_d18_x_predicted_y[[i]],fun_points_02_y_noisy_set9[[i]], abs = TRUE)
  if (i == 100) {
    v3_02 = v3_02 + var(fun_02_d18_x_predicted_y)
  }
  fun_01_d18_x_random_extract <- with(data_frame_2_set10, x_random_extract_set10)
  fun_02_d18_x_predicted_y <- predict(linear_model_02_d18_set10, newdata = data.frame(x_random_extract_set10 = fun_02_d18_x_random_extract))
  error_lm02d18[[i]] = error_lm02d18[[i]] + bias(fun_02_d18_x_predicted_y[[i]],fun_points_02_y_noisy_set10[[i]], abs = TRUE)
  if (i == 100) {
    v3_02 = v3_02 + var(fun_02_d18_x_predicted_y)
  }
} #at this point error_lm01d18 contains the sum of biases with the 10 models for degree1 and func_02

#mean bias for Q2_fun_02
bias1_02=(mean(as.numeric(error_lm02d1)))/10
bias2_02=(mean(as.numeric(error_lm02d9)))/10
bias3_02=(mean(as.numeric(error_lm02d18)))/10  #div by 10 is to take the expectation or average of the 10 models

bias_fun_02<-c(bias1_02,bias2_02,bias3_02)
bias_fun_02

#residual variance same as slope variance = (summary(linear_model_01_d18)$coefficients[2,2])**2 for Q2_fun_01

v1_02 = v1_01/10
v2_02 = v2_02/10
v3_02 = v3_02/10


var_fun_02<-c(v1_02,v2_02,v3_02)
degree<-c(1,9,18)
  

png(file="14_Q2_fun_02_bias_variance.png", width = 960, height = 480)  
plot(degree, bias_fun_02, col = "red", type = "l",xlab="degree of polynomial", ylab = "bias/variance", yaxt="n", main = "Bias/Variance vs degree of polynomial for Q2_fun_02", col.main = "darkgray")
#lines(degree, bias_01, col = "red", lty = 1)
points(degree,bias_fun_02, col = "red", pch =16)
par(new=TRUE)
plot(degree, var_fun_02, col = "blue", type = "l", xlab="", ylab = "")
points(degree,var_fun_02, col = "blue", pch =16)
legend("bottomright", 
       legend = c("Bias", "Variance"), 
       col = c(col = "red", 
               col = "blue"), 
       pch = c(16,16), 
       bty = "n", 
       pt.cex = 2, 
       cex = 1.2, 
       text.col = "black", 
       horiz = F , 
       inset = c(0.1, 0.1))
dev.off()


############# testing on a different set than training, for all ten models, we will test against set1:
#BIAS compute for Q2_fun_01 polynomials with different training and testing set (x_random_extract_set1, fun_points_01_y_noisy_set1):
#bias with degree 1 polynomial fit for Q2_fun_01
#we assume our testing set same as our training set
#For all ten models, we compute bias against TESTING set1.
fun_01_d1_x_random_extract <- with(data_frame_1_set1, x_random_extract_set1)
error_lm01d1 <- list()
for (i in 1:100){
  fun_01_d1_x_predicted_y <- predict(linear_model_01_d1_set1, newdata = data.frame(x_random_extract_set1 = fun_01_d1_x_random_extract))
  error_lm01d1[[i]] = bias(fun_01_d1_x_predicted_y[[i]],fun_points_01_y_noisy_set1[[i]], abs = TRUE)
  fun_01_d1_x_predicted_y <- predict(linear_model_01_d1_set2, newdata = data.frame(x_random_extract_set1 = fun_01_d1_x_random_extract))
  error_lm01d1[[i]] = error_lm01d1[[i]] + bias(fun_01_d1_x_predicted_y[[i]],fun_points_01_y_noisy_set1[[i]], abs = TRUE)
  fun_01_d1_x_predicted_y <- predict(linear_model_01_d1_set3, newdata = data.frame(x_random_extract_set1 = fun_01_d1_x_random_extract))
  error_lm01d1[[i]] = error_lm01d1[[i]] + bias(fun_01_d1_x_predicted_y[[i]],fun_points_01_y_noisy_set1[[i]], abs = TRUE)
  fun_01_d1_x_predicted_y <- predict(linear_model_01_d1_set4, newdata = data.frame(x_random_extract_set1 = fun_01_d1_x_random_extract))
  error_lm01d1[[i]] = error_lm01d1[[i]] + bias(fun_01_d1_x_predicted_y[[i]],fun_points_01_y_noisy_set1[[i]], abs = TRUE)
  fun_01_d1_x_predicted_y <- predict(linear_model_01_d1_set5, newdata = data.frame(x_random_extract_set1 = fun_01_d1_x_random_extract))
  error_lm01d1[[i]] = error_lm01d1[[i]] + bias(fun_01_d1_x_predicted_y[[i]],fun_points_01_y_noisy_set1[[i]], abs = TRUE)
  fun_01_d1_x_predicted_y <- predict(linear_model_01_d1_set6, newdata = data.frame(x_random_extract_set1 = fun_01_d1_x_random_extract))
  error_lm01d1[[i]] = error_lm01d1[[i]] + bias(fun_01_d1_x_predicted_y[[i]],fun_points_01_y_noisy_set1[[i]], abs = TRUE)  
  fun_01_d1_x_predicted_y <- predict(linear_model_01_d1_set7, newdata = data.frame(x_random_extract_set1 = fun_01_d1_x_random_extract))
  error_lm01d1[[i]] = error_lm01d1[[i]] + bias(fun_01_d1_x_predicted_y[[i]],fun_points_01_y_noisy_set1[[i]], abs = TRUE)
  fun_01_d1_x_predicted_y <- predict(linear_model_01_d1_set8, newdata = data.frame(x_random_extract_set1 = fun_01_d1_x_random_extract))
  error_lm01d1[[i]] = error_lm01d1[[i]] + bias(fun_01_d1_x_predicted_y[[i]],fun_points_01_y_noisy_set1[[i]], abs = TRUE)
  fun_01_d1_x_predicted_y <- predict(linear_model_01_d1_set9, newdata = data.frame(x_random_extract_set1 = fun_01_d1_x_random_extract))
  error_lm01d1[[i]] = error_lm01d1[[i]] + bias(fun_01_d1_x_predicted_y[[i]],fun_points_01_y_noisy_set1[[i]], abs = TRUE)
  fun_01_d1_x_predicted_y <- predict(linear_model_01_d1_set10, newdata = data.frame(x_random_extract_set1 = fun_01_d1_x_random_extract))
  error_lm01d1[[i]] = error_lm01d1[[i]] + bias(fun_01_d1_x_predicted_y[[i]],fun_points_01_y_noisy_set1[[i]], abs = TRUE)
} #at this point error_lm01d1 contains the sum of biases with the 10 models for degree1 and func_01


#bias with degree 9 polynomial fit for Q2_fun_01
fun_01_d9_x_random_extract <- with(data_frame_1_set1, x_random_extract_set1)
error_lm01d9 <- list()
for (i in 1:100){
  fun_01_d9_x_predicted_y <- predict(linear_model_01_d9_set1, newdata = data.frame(x_random_extract_set1 = fun_01_d9_x_random_extract))
  error_lm01d9[[i]] = bias(fun_01_d9_x_predicted_y[[i]],fun_points_01_y_noisy_set1[[i]], abs = TRUE)
  fun_01_d9_x_predicted_y <- predict(linear_model_01_d9_set2, newdata = data.frame(x_random_extract_set1 = fun_01_d9_x_random_extract))
  error_lm01d9[[i]] = error_lm01d9[[i]] + bias(fun_01_d9_x_predicted_y[[i]],fun_points_01_y_noisy_set1[[i]], abs = TRUE)
  fun_01_d9_x_predicted_y <- predict(linear_model_01_d9_set3, newdata = data.frame(x_random_extract_set1 = fun_01_d9_x_random_extract))
  error_lm01d9[[i]] = error_lm01d9[[i]] + bias(fun_01_d9_x_predicted_y[[i]],fun_points_01_y_noisy_set1[[i]], abs = TRUE)
  fun_01_d9_x_predicted_y <- predict(linear_model_01_d9_set4, newdata = data.frame(x_random_extract_set1 = fun_01_d9_x_random_extract))
  error_lm01d9[[i]] = error_lm01d9[[i]] + bias(fun_01_d9_x_predicted_y[[i]],fun_points_01_y_noisy_set1[[i]], abs = TRUE)
  fun_01_d9_x_predicted_y <- predict(linear_model_01_d9_set5, newdata = data.frame(x_random_extract_set1 = fun_01_d9_x_random_extract))
  error_lm01d9[[i]] = error_lm01d9[[i]] + bias(fun_01_d9_x_predicted_y[[i]],fun_points_01_y_noisy_set1[[i]], abs = TRUE)
  fun_01_d9_x_predicted_y <- predict(linear_model_01_d9_set6, newdata = data.frame(x_random_extract_set1 = fun_01_d9_x_random_extract))
  error_lm01d9[[i]] = error_lm01d9[[i]] + bias(fun_01_d9_x_predicted_y[[i]],fun_points_01_y_noisy_set1[[i]], abs = TRUE)
  fun_01_d9_x_predicted_y <- predict(linear_model_01_d9_set7, newdata = data.frame(x_random_extract_set1 = fun_01_d9_x_random_extract))
  error_lm01d9[[i]] = error_lm01d9[[i]] + bias(fun_01_d9_x_predicted_y[[i]],fun_points_01_y_noisy_set1[[i]], abs = TRUE)
  fun_01_d9_x_predicted_y <- predict(linear_model_01_d9_set8, newdata = data.frame(x_random_extract_set1 = fun_01_d9_x_random_extract))
  error_lm01d9[[i]] = error_lm01d9[[i]] + bias(fun_01_d9_x_predicted_y[[i]],fun_points_01_y_noisy_set1[[i]], abs = TRUE)
  fun_01_d9_x_predicted_y <- predict(linear_model_01_d9_set9, newdata = data.frame(x_random_extract_set1 = fun_01_d9_x_random_extract))
  error_lm01d9[[i]] = error_lm01d9[[i]] + bias(fun_01_d9_x_predicted_y[[i]],fun_points_01_y_noisy_set1[[i]], abs = TRUE)
  fun_01_d9_x_predicted_y <- predict(linear_model_01_d9_set10, newdata = data.frame(x_random_extract_set1 = fun_01_d9_x_random_extract))
  error_lm01d9[[i]] = error_lm01d9[[i]] + bias(fun_01_d9_x_predicted_y[[i]],fun_points_01_y_noisy_set1[[i]], abs = TRUE)
} #at this point error_lm01d9 contains the sum of biases with the 10 models for degree1 and func_01

#bias with degree 18 polynomial fit for Q2_fun_01
fun_01_d18_x_random_extract <- with(data_frame_1_set1, x_random_extract_set1)
error_lm01d18 <- list()
for (i in 1:100){
  fun_01_d18_x_predicted_y <- predict(linear_model_01_d18_set1, newdata = data.frame(x_random_extract_set1 = fun_01_d18_x_random_extract))
  error_lm01d18[[i]] = bias(fun_01_d18_x_predicted_y[[i]],fun_points_01_y_noisy_set1[[i]], abs = TRUE)
  fun_01_d18_x_predicted_y <- predict(linear_model_01_d18_set2, newdata = data.frame(x_random_extract_set1 = fun_01_d18_x_random_extract))
  error_lm01d18[[i]] = error_lm01d18[[i]] + bias(fun_01_d18_x_predicted_y[[i]],fun_points_01_y_noisy_set1[[i]], abs = TRUE)
  fun_01_d18_x_predicted_y <- predict(linear_model_01_d18_set3, newdata = data.frame(x_random_extract_set1 = fun_01_d18_x_random_extract))
  error_lm01d18[[i]] = error_lm01d18[[i]] + bias(fun_01_d18_x_predicted_y[[i]],fun_points_01_y_noisy_set1[[i]], abs = TRUE)
  fun_01_d18_x_predicted_y <- predict(linear_model_01_d18_set4, newdata = data.frame(x_random_extract_set1 = fun_01_d18_x_random_extract))
  error_lm01d18[[i]] = error_lm01d18[[i]] + bias(fun_01_d18_x_predicted_y[[i]],fun_points_01_y_noisy_set1[[i]], abs = TRUE)
  fun_01_d18_x_predicted_y <- predict(linear_model_01_d18_set5, newdata = data.frame(x_random_extract_set1 = fun_01_d18_x_random_extract))
  error_lm01d18[[i]] = error_lm01d18[[i]] + bias(fun_01_d18_x_predicted_y[[i]],fun_points_01_y_noisy_set1[[i]], abs = TRUE)
  fun_01_d18_x_predicted_y <- predict(linear_model_01_d18_set6, newdata = data.frame(x_random_extract_set1 = fun_01_d18_x_random_extract))
  error_lm01d18[[i]] = error_lm01d18[[i]] + bias(fun_01_d18_x_predicted_y[[i]],fun_points_01_y_noisy_set1[[i]], abs = TRUE)
  fun_01_d18_x_predicted_y <- predict(linear_model_01_d18_set7, newdata = data.frame(x_random_extract_set1 = fun_01_d18_x_random_extract))
  error_lm01d18[[i]] = error_lm01d18[[i]] + bias(fun_01_d18_x_predicted_y[[i]],fun_points_01_y_noisy_set1[[i]], abs = TRUE)
  fun_01_d18_x_predicted_y <- predict(linear_model_01_d18_set8, newdata = data.frame(x_random_extract_set1 = fun_01_d18_x_random_extract))
  error_lm01d18[[i]] = error_lm01d18[[i]] + bias(fun_01_d18_x_predicted_y[[i]],fun_points_01_y_noisy_set1[[i]], abs = TRUE)
  fun_01_d18_x_predicted_y <- predict(linear_model_01_d18_set9, newdata = data.frame(x_random_extract_set1 = fun_01_d18_x_random_extract))
  error_lm01d18[[i]] = error_lm01d18[[i]] + bias(fun_01_d18_x_predicted_y[[i]],fun_points_01_y_noisy_set1[[i]], abs = TRUE)
  fun_01_d18_x_predicted_y <- predict(linear_model_01_d18_set10, newdata = data.frame(x_random_extract_set1 = fun_01_d18_x_random_extract))
  error_lm01d18[[i]] = error_lm01d18[[i]] + bias(fun_01_d18_x_predicted_y[[i]],fun_points_01_y_noisy_set1[[i]], abs = TRUE)
} #at this point error_lm01d18 contains the sum of biases with the 10 models for degree1 and func_01

bias1_01=(mean(as.numeric(error_lm01d1)))/10
bias2_01=(mean(as.numeric(error_lm01d9)))/10
bias3_01=(mean(as.numeric(error_lm01d18)))/10  #div by 10 is to take the expectation or average of the 10 models

bias_fun_01<-c(bias1_01,bias2_01,bias3_01)
bias_fun_01


#residual variance same as slope variance = (summary(linear_model_01_d18)$coefficients[2,2])**2 for Q2_fun_01
v1_01_set1=(summary(linear_model_01_d1_set1)$sigma)**2
v2_01_set1=(summary(linear_model_01_d9_set1)$sigma)**2
v3_01_set1=(summary(linear_model_01_d18_set1)$sigma)**2

v1_01_set2=(summary(linear_model_01_d1_set2)$sigma)**2
v2_01_set2=(summary(linear_model_01_d9_set2)$sigma)**2
v3_01_set2=(summary(linear_model_01_d18_set2)$sigma)**2

v1_01_set3=(summary(linear_model_01_d1_set3)$sigma)**2
v2_01_set3=(summary(linear_model_01_d9_set3)$sigma)**2
v3_01_set3=(summary(linear_model_01_d18_set3)$sigma)**2

v1_01_set4=(summary(linear_model_01_d1_set4)$sigma)**2
v2_01_set4=(summary(linear_model_01_d9_set4)$sigma)**2
v3_01_set4=(summary(linear_model_01_d18_set4)$sigma)**2


v1_01_set5=(summary(linear_model_01_d1_set5)$sigma)**2
v2_01_set5=(summary(linear_model_01_d9_set5)$sigma)**2
v3_01_set5=(summary(linear_model_01_d18_set5)$sigma)**2



v1_01_set6=(summary(linear_model_01_d1_set6)$sigma)**2
v2_01_set6=(summary(linear_model_01_d9_set6)$sigma)**2
v3_01_set6=(summary(linear_model_01_d18_set6)$sigma)**2


v1_01_set7=(summary(linear_model_01_d1_set7)$sigma)**2
v2_01_set7=(summary(linear_model_01_d9_set7)$sigma)**2
v3_01_set7=(summary(linear_model_01_d18_set7)$sigma)**2


v1_01_set8=(summary(linear_model_01_d1_set8)$sigma)**2
v2_01_set8=(summary(linear_model_01_d9_set8)$sigma)**2
v3_01_set8=(summary(linear_model_01_d18_set8)$sigma)**2


v1_01_set9=(summary(linear_model_01_d1_set9)$sigma)**2
v2_01_set9=(summary(linear_model_01_d9_set9)$sigma)**2
v3_01_set9=(summary(linear_model_01_d18_set9)$sigma)**2


v1_01_set10=(summary(linear_model_01_d1_set10)$sigma)**2
v2_01_set10=(summary(linear_model_01_d9_set10)$sigma)**2
v3_01_set10=(summary(linear_model_01_d18_set10)$sigma)**2

v1_01 = (v1_01_set1+v1_01_set2+v1_01_set3+v1_01_set4+v1_01_set5+v1_01_set6+v1_01_set9+v1_01_set10)/10   #degree 1
v2_01 = (v2_01_set1+v2_01_set2+v2_01_set3+v2_01_set4+v2_01_set5+v2_01_set6+v2_01_set9+v2_01_set10)/10   #degree 9
v3_01 = (v3_01_set1+v3_01_set2+v3_01_set3+v3_01_set4+v3_01_set5+v3_01_set6+v3_01_set9+v3_01_set10)/10   #degree 18


var_fun_01<-c(v1_01,v2_01,v3_01)
degree<-c(1,9,18)



png(file="15_Q2_fun_01_bias_variance_with different_training_and_testing_sets.png", width = 960, height = 480)
plot(degree, bias_fun_01, col = "red", type = "l", xlab="degree of polynomial", ylab = "bias/variance", yaxt="n", main = "Bias/Variance vs degree of polynomial for Q2_fun_01 with different test and training sets", col.main = "darkgray")
points(degree,bias_fun_01, col = "red", pch =16)
par(new=TRUE)
plot(degree, var_fun_01, col = "blue", type = "l", xlab="", ylab = "")
points(degree,var_fun_01, col = "blue", pch =16)
legend("topright", 
       legend = c("Bias", "Variance"), 
       col = c(col = "red", 
               col = "blue"), 
       pch = c(16,16), 
       bty = "n", 
       pt.cex = 2, 
       cex = 1.2, 
       text.col = "black", 
       horiz = F , 
       inset = c(0.1, 0.1))
dev.off()