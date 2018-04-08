
# ===============================================
# Load packages and data 

doInstall <- TRUE  
toInstall <- c("XLConnect", "gridExtra", "FactoMineR", "irr", "glmnet",
               "randomForest", "caret", "plyr")
if(doInstall){install.packages(toInstall, repos = "http://cran.r-project.org")}

# =====

if(!file.exists("C:/Desktop/Machine_learning")) dir.create("C:/Desktop/Machine_learning") 
setwd("C:/Desktop/Machine_learning")

library(XLConnect)
Data <- readWorksheet(loadWorkbook(file.choose()),sheet=1 ,header=TRUE)

# ===============================================
# Function for automatic data transformation, cleaning and train-test-validation data split

data_sort <- function(Data){
  
  # identifying dependent 
  x <- readline("Specify column number of dependent variable:  ")
  g <- ifelse(grepl("[[:digit:]]+",x), x, NA)
  try(if(is.na(g)) stop("The analysis is terminated. Please provide column number of dependent variable.", call. = F))
  x <- as.numeric(x)
  dependent <<- as.numeric(x)
  
  # identifying categorical independents 
  n <- readline("Are there any categorical variables in the model? Please, specify Y or N:  ")
  n <- ifelse(grepl("y|n",n, ignore.case = FALSE), as.character(n), NA)
  try(if(is.na(n)) stop("The analysis is terminated. Please provide Y or N.", call. = F)) 
  
  if (tolower(substr(n, 1, 1)) == "y")
    y <- readline("Specify column number/s of categorical independent variable/s:  ")
  
  t <- ifelse(grepl("[[:digit:]]+",y), y, NA)
  try(if(is.na(t)) stop("The analysis is terminated. Please provide column number/s of categorical independent variable/s.", call. = F))
  
  matches <- regmatches(y, gregexpr("[[:digit:]]+", y))
  y <- as.numeric(unlist(matches))
  categorical <<- y
  
  dummy_start<<-ncol(Data)+1
  
  # recoding  categorical independents
  dummy_columns <- rep(list(list()), length(y))
  variables <- matrix(0, nrow=nrow(Data),ncol=length(y))
  
  z=1
  while (z<(length(y)+1))
  {
    i<-y[z]
    variables[,z] <- Data[,i]
    name<-colnames(Data)[i]
    
    variable <- variables[,z]
    df_start <- ncol(Data)
    
    for(level in unique(variable)){
      Data[paste(name, abs(level), sep = "_")] <- ifelse(variable == level, 1, 0)
      df_end<-length(unique(variable))
    }
    
    dummy_columns[[z]]<-c((df_start+1):(df_start+df_end))
    z=z+1
  }
  
  # specifying independents
  
  reference_category <- matrix(nrow=length(y),ncol=1)
  
  i=1
  while(i < (length(y)+1))
  {
    
    e <-as.numeric(dummy_columns[[i]])
    lower <- min(e)
    upper <- max(e)
    
    dummmy_sum <- sapply(Data [,lower:upper], function(w) sum(w))  # selecting reference category
    m <- as.numeric(which.max(dummmy_sum))
    
    reference_category[i,1] <- e[m]
    
    i=i+1
  }
  
  reference_category <<- as.numeric(reference_category)
  
  # creating data
  
  tmp<-na.omit(Data)
  
  # validation data 
  validation<-sample(1:nrow(tmp), round(nrow(tmp)/4))  
  train_test=-validation                        
  validation_data<<-tmp[validation,]
  
  # train-test data 
  train_test_data=tmp[train_test,]
  
  train_test_data <<-train_test_data
  Data <<- Data
}   

data_sort(Data)

# ===============================================
# Function for automatic production of descriptive statistics 

descriptive_statistics = function(Data)
{
  require(gridExtra)
  require(pastecs)
  require(FactoMineR)
  require(factoextra)
  
  wd <- getwd()
  
  Data <- data.frame(Data)
  des.table<-round(stat.desc(Data),2)
  
  tt <- ttheme_default(core=list(fg_params=list(hjust=0, x=0.1)),
                       rowhead=list(fg_params=list(hjust=0, x=0)))
  
  png(filename = paste0(wd, "/descript_1.png"),
      width=14.3, height=4.4, units="in", res=150)
  grid.table(des.table[, 3:10], theme=tt)
  t<- dev.off()
  
  png(filename = paste0(wd, "/descript_2.png"),
      width=13.9, height=4.4, units="in", res=150)
  grid.table(des.table[, 11:18], theme=tt)
  t<- dev.off()
  
  # ------------------------------
  # histogram
  
  png(filename = paste0(wd, "/histogram.png"),
      width=7.7, height=4.2, units="in", res=150)
  par(mfrow=c(1,3))
  hist(Data[,2], main="Histogram of BNUM_OUT", col = "gray")
  hist(Data[,3], main="Histogram of BNUM_IN", col = "gray")
  hist(Data[,4], main="Histogram of CALL_OUT_CNT", col = "gray")
  t<- dev.off()
  
  # ------------------------------
  # correlations
  
  png(filename = paste0(wd, "/correlation.png"),
      width=6.7, height=5.2, units="in", res=150)
  cor_coef <- cor(as.matrix(as.numeric(Data[, dependent])), 
                as.matrix(Data[,-c(1, dependent, dummy_start : length(Data))]),use="pairwise.complete.obs")
  par(mfrow=c(1,1))
  par(las=2) 
  par(mar=c(5,11,4,2))
  barplot(cor_coef[,-7], horiz = T, cex.names = 0.8, main = "Correlation with dependent")
  t <- dev.off()
  
  # ------------------------------
  # boxplots
  
  png(filename = paste0(wd, "/boxplots.png"),
      width=7.7, height=4.2, units="in", res=150)
  par(mfrow=c(1,3))
  par(mar=c(3,3,3,2))
  boxplot(Data[,2], main="Boxplot of BNUM_OUT", col = "gray")
  boxplot(Data[,3], main="Boxplot of BNUM_IN", col = "gray")
  boxplot(Data[,4], main="Boxplot of CALL_OUT_CNT", col = "gray")
  t<- dev.off()
  
  # ------------------------------
  # frequency of outliers
  
  Data_z <- as.data.frame(scale(Data[,-c(dummy_start : length(Data))]))
  
  out_count <- sapply(Data_z, function(y) length(which(y > abs(3))))
  
  png(filename = paste0(wd, "/outliers.png"),
      width=7.2, height=5.2, units="in", res=150)
  par(mfrow=c(1,1))
  par(las=2) 
  par(mar=c(5,11,4,2))
  barplot(out_count, horiz = T, cex.names = 0.8, main = "Frequency of outliers")
  t <- dev.off()
  
  # ------------------------------
  # principal component analysis
  
  pca <- PCA(na.omit(Data[,-c(dependent, categorical,dummy_start : length(Data))]), graph = FALSE)
  
  # The proportion of explained variance
  eigenvalues <- round(pca$eig, digits = 3)[,-3]
  tt <- ttheme_default(core=list(fg_params=list(hjust=0, x=0.1)),
                       rowhead=list(fg_params=list(hjust=0, x=0)))
  png(filename = paste0(wd, "/eigenvalues.png"),
      width=4.2, height=4.4, units="in", res=150)
  grid.table(eigenvalues, theme=tt)
  t<- dev.off()
  
  # Scree plot
  png(filename = paste0(wd, "/scree_plot.png"),
      width=6.7, height=4, units="in", res=150)
  barplot(eigenvalues[, 2], names.arg=1:nrow(eigenvalues), 
          main = "Scree plot",
          xlab = "Principal Components",
          ylab = "Percentage of variance",
          col ="steelblue")
  t<- dev.off()
  
  # The correlation between a variable and a PC
  components <- round(pca$var$coord, digits = 3)
  tt <- ttheme_default(core=list(fg_params=list(hjust=0, x=0.1)),
                       rowhead=list(fg_params=list(hjust=0, x=0)))
  png(filename = paste0(wd, "/components.png"),
      width=6.3, height=4.4, units="in", res=150)
  grid.table(components, theme=tt)
  t<- dev.off()
  
}

descriptive_statistics(Data)

# ===============================================
# Function for automatic selection of a subset of variables 

subset_selection = function(Data)
{
  
  options(warn=-1)
  
  require(irr)
  require(glmnet)
  require (randomForest)
  require(gridExtra)
  
  wd <- getwd()
  
  # ------------------------------
  # data
  
  test<-sample(1:nrow(train_test_data), round(nrow(train_test_data)/3))  
  train=-test                        
  testing_data <<- train_test_data[test,]
  training_data <<- train_test_data[train,]
  
  # ------------------------------
  # logistic regression
  
  mod <- glm(training_data[, dependent]~., data=training_data[, -c(1, dependent, categorical, reference_category)], family=binomial())
  
  coef <- data.frame(coef(mod))
  # ------------------------------
  # logistic regression - error
  
  name <- c("logistic")
  
  probs <- predict(mod, training_data, type="response")
  pred=rep (0, nrow(training_data))
  pred[probs >.5]=1
  
  tmp <-data.frame(training_data[, dependent],pred)
  kappa<-kappa2(tmp[1:2])[[5]]
  conf_matrix <- data.frame(table(training_data[, dependent],pred))
  class_error <- 1-((conf_matrix$Freq[1]+conf_matrix$Freq[4])/sum(conf_matrix$Freq))
  
  ###
  
  probs <- predict(mod, testing_data, type="response")
  pred=rep (0 ,nrow(testing_data))
  pred[probs >.5]=1
  
  tmp <-data.frame(testing_data[, dependent],pred)
  kappa_test<-kappa2(tmp[1:2])[[5]]
  conf_matrix <- data.frame(table(testing_data[, dependent],pred))
  class_error_test <- 1-((conf_matrix$Freq[1]+conf_matrix$Freq[4])/sum(conf_matrix$Freq))
  error<-data.frame(name, kappa, class_error, kappa_test, class_error_test)
  
  # ------------------------------
  # variable subset selection for logistic regression - full 
  p_values <- subset(coef(mod), subset=coef(summary(mod))[,4] < 0.001) # geting coefficients below p=0.001
  p_values <- data.frame(rownames((data.frame(p_values)))) 
  colnames(p_values)<-c("Row.names")
  subset_select_log <- data.frame(p_values, p_values)
  
  # ------------------------------
  # ridge regression
  
  grid=10^seq (10,-2, length=100)
  
  cv.out =cv.glmnet(as.matrix(training_data[, -c(1, dependent, categorical, reference_category)]), as.matrix(training_data[, dependent]), 
                    alpha=0, family ="binomial",lambda=grid, standardize=F)
  
  png(filename = paste0(wd, "/ridge.png"))
  plot(cv.out)
  t<-dev.off()
  
  best_lambda =cv.out$lambda.min
  
  d<-ncol(training_data[, -c(1, dependent, categorical, reference_category)])
  d <- seq(1,(d+1),1)
  
  ridge.coef=predict(cv.out, type ="coefficients", s=best_lambda)[d,]
  coef <- data.frame(coef,ridge.coef)
  # ------------------------------
  # ridge - error
  
  name <- c("ridge")
  pred=predict(cv.out ,s=best_lambda ,newx=as.matrix(training_data[, -c(1, dependent, categorical, reference_category)]),type="class")
  
  tmp <- data.frame(training_data[, dependent],pred)
  kappa <- kappa2(tmp[1:2])[[5]]
  conf_matrix <- data.frame(table(training_data[, dependent],pred))
  class_error <- 1-((conf_matrix$Freq[1]+conf_matrix$Freq[4])/sum(conf_matrix$Freq))
  
  ###
  
  pred=predict(cv.out ,s=best_lambda ,newx=as.matrix(testing_data[, -c(1, dependent, categorical, reference_category)]),type="class")
  
  tmp <-data.frame(testing_data[, dependent],pred)
  kappa_test <- kappa2(tmp[1:2])[[5]]
  conf_matrix <- data.frame(table(testing_data[, dependent],pred))
  class_error_test <- 1-((conf_matrix$Freq[1]+conf_matrix$Freq[4])/sum(conf_matrix$Freq))
  error<-rbind(data.frame(name, kappa, class_error, kappa_test, class_error_test),error)
  
  # ------------------------------
  #  lasso
  
  cv.out = cv.glmnet(as.matrix(training_data[, -c(1, dependent, categorical, reference_category)]), as.matrix(training_data[, dependent]), 
                    alpha=1, family ="binomial",lambda=grid,  standardize=F)
  
  png(filename = paste0(wd, "/lasso.png"))
  plot(cv.out)
  t<-dev.off()
  
  best_lambda =cv.out$lambda.min
  
  d<-ncol(training_data[, -c(1, dependent, categorical, reference_category)])
  d <- seq(1,(d+1),1)
  
  lasso.coef=predict(cv.out, type ="coefficients", s=best_lambda)[d,]
  coef <- data.frame(coef,lasso.coef)
  
  # ------------------------------
  # lasso - error
  
  name <- c("lasso")
  pred=predict(cv.out ,s=best_lambda ,newx=as.matrix(training_data[, -c(1, dependent, categorical, reference_category)]),type="class")
  
  tmp <-data.frame(training_data[, dependent],pred)
  kappa<-kappa2(tmp[1:2])[[5]]
  conf_matrix <- data.frame(table(training_data[, dependent],pred))
  class_error <- 1-((conf_matrix$Freq[1]+conf_matrix$Freq[4])/sum(conf_matrix$Freq))
  
  ###
  
  pred=predict(cv.out ,s=best_lambda ,newx=as.matrix(testing_data[, -c(1, dependent, categorical, reference_category)]),type="class")
  
  tmp <- data.frame(testing_data[, dependent],pred)
  kappa_test <- kappa2(tmp[1:2])[[5]]
  conf_matrix <- data.frame(table(testing_data[, dependent],pred))
  class_error_test <- 1-((conf_matrix$Freq[1]+conf_matrix$Freq[4])/sum(conf_matrix$Freq))
  error<-rbind(data.frame(name, kappa, class_error, kappa_test, class_error_test),error)
  
  # ------------------------------
  # random forest
  
  depen <-as.factor(training_data[, dependent])
  
  independent <- colnames(training_data[, -c(1, dependent, categorical)])
  independent <- c(as.list(independent), sep="+")
  independent <- do.call(paste, independent)
  
  RandomForest <- randomForest (as.formula(paste("depen", "~", independent)), 
                                data=training_data, 
                                importance=TRUE,
                                mtry=4,
                                ntree=1000)
  
  # ------------------------------
  # Random Forest - error
  
  name <- c("random forest")
  pred = predict (RandomForest, newdata=training_data[,-dependent]) 
  
  tmp <- data.frame(training_data[, dependent],pred)
  kappa <- kappa2(tmp[1:2])[[5]]
  conf_matrix <- data.frame(table(training_data[, dependent],pred))
  class_error <- 1-((conf_matrix$Freq[1]+conf_matrix$Freq[4])/sum(conf_matrix$Freq))
  
  ###
  
  pred = predict (RandomForest, newdata=testing_data[,-dependent]) 
  
  tmp <- data.frame(testing_data[, dependent],pred)
  kappa_test <- kappa2(tmp[1:2])[[5]]
  conf_matrix <- data.frame(table(testing_data[, dependent],pred))
  class_error_test <- 1-((conf_matrix$Freq[1]+conf_matrix$Freq[4])/sum(conf_matrix$Freq))
  error <- rbind(data.frame(name, kappa, class_error, kappa_test, class_error_test),error)
  
  # ------------------------------
  # error table
  
  error[2:5]<- round(error[2:5], digits = 3)
  colnames(error) <- c("method", "kappa - train","classification error - train", 
                       "kappa - test","classification error - test")
  error <<- error
  
  # ------------------------------
  # variable subset selection
  
  # ------------------------------
  # variable subset selection for ridge and lasso
  ridge <- subset(coef[-1,], subset=abs(round(ridge.coef, digits=2)) !=0) 
  ridge <- data.frame(rownames(ridge))
  rownames(ridge) <- ridge$rownames.ridge.
  
  lasso <- subset(coef[-1,], subset=abs(round(lasso.coef, digits=3)) !=0) 
  lasso <- data.frame(rownames(lasso))
  rownames(lasso) <- lasso$rownames.lasso.
  
  subset_select_reg <- merge(ridge, lasso, by="row.names",all=T)
  
  # variable subset selection for random forest
  
  png(filename = paste0(wd, "/rf_importance.png"))
  varImpPlot(RandomForest)
  t<-dev.off()
  
  RFimportance<-importance(RandomForest)
  
  Accuracy <- subset(RFimportance, subset=RFimportance[,3] > 20)
  Gini <- subset(RFimportance, subset=RFimportance[,4] > 25)
  Accuracy <- data.frame(rownames(Accuracy))
  rownames(Accuracy) <-Accuracy$rownames.Accuracy.
  Gini <- data.frame(rownames(Gini))
  rownames(Gini) <- Gini$rownames.Gini.
  
  subset_select_RF <- merge(Accuracy, Gini, by="row.names",all=T)
  
  subset_select <- merge(subset_select_reg, subset_select_RF, 
                       by="Row.names", all=T)
  
  
  # variable subset selection for logistic regression
  subset_select<-merge(subset_select, subset_select_log, by="Row.names", all=T)
  subset_select<-subset_select[,-1]
  colnames(subset_select) <- c("ridge", "lasso","random forest-accuracy",
                               "random forest-gini", "logistic")
  subset_select <<- subset_select
  
  # ------------------------------
  # table of coefficients
  
  colnames(coef) <- c("logistic", "ridge", "lasso")
  coef <- round(coef, digits = 3)
  
  # ------------------------------
  # print tables
  tt <- ttheme_default(core=list(fg_params=list(hjust=0, x=0.05)),
                       rowhead=list(fg_params=list(hjust=0, x=0)))
  
  png(filename = paste0(wd, "/coefficients.png"), 
      width=5, height=5.5, units="in", res=300)
  grid.table(coef, theme=tt)
  t <-dev.off()
  
  png(filename = paste0(wd, "/error.png"),
      width=7.7, height=1.5, units="in", res=150)
  grid.table(error, theme=tt, rows = NULL)
  t <-dev.off()
  
  png(filename = paste0(wd, "/subset_select.png"), 
      width=12, height=4.8, units="in", res=300)
  grid.table(subset_select, theme=tt, rows = NULL)
  t <-dev.off()
  
  message('
          Instructions:
          
          Please review selected variables and specify the new formula (variable subset) in form of: 
          variable_subset <- "DEPENDENT ~ INDEPENDENT_1 + INDEPENDENT_2 + INDEPENDENT_3 + ... + INDEPENDENT_p"
          ')
  
}

subset_selection(Data)


# ===============================================
# Function for automated selection of the best model

# define the subset

variable_subset <- "DEPENDENT ~ INDEPENDENT_1 + INDEPENDENT_2 + INDEPENDENT_3 + INDEPENDENT_4"

subset_analysis = function(training_data,testing_data)
{
  
  require(caret)
  require(plyr)
  require(irr)
  require(gridExtra)
  
  training_data[, dependent] <- make.names(factor(training_data[, dependent]))
  
  ### knn
  mod <- train((as.formula(paste(variable_subset))),
               data=training_data, method = "knn")
  
  pred <- predict(mod, training_data)
  pred <- mapvalues(pred, from = c("X0", "X1"), to = c("0", "1"))
  training_data_CHURN <- as.numeric(mapvalues(training_data[, dependent], from = c("X0", "X1"), to = c("0", "1")))
  name <- c("k nearest neighbor")
  tmp <-data.frame(training_data_CHURN,pred)
  kappa<-kappa2(tmp[1:2])[[5]]
  conf_matrix <- data.frame(table(training_data[, dependent],pred))
  class_error <- 1-((conf_matrix$Freq[1]+conf_matrix$Freq[4])/sum(conf_matrix$Freq))
  
  pred <- predict(mod, testing_data)
  pred <- mapvalues(pred, from = c("X0", "X1"), to = c("0", "1"))
  name <- c("knn")
  tmp <-data.frame(testing_data[, dependent],pred)
  kappa_test<-kappa2(tmp[1:2])[[5]]
  conf_matrix <- data.frame(table(testing_data[, dependent],pred))
  class_error_test <- 1-((conf_matrix$Freq[1]+conf_matrix$Freq[4])/sum(conf_matrix$Freq))
  error_subset <-data.frame(name, kappa, class_error,  kappa_test, class_error_test)
  
  ###  C5.0/rule
  mod <- train((as.formula(paste(variable_subset))),
               data=training_data, method = "C5.0")
  
  pred <- predict(mod, training_data)
  pred <- mapvalues(pred, from = c("X0", "X1"), to = c("0", "1"))
  training_data_CHURN <- as.numeric(mapvalues(training_data[, dependent], from = c("X0", "X1"), to = c("0", "1")))
  name <- c("C5.0/rule")  
  tmp <- data.frame(training_data_CHURN,pred)
  kappa<-kappa2(tmp[1:2])[[5]]
  conf_matrix <- data.frame(table(training_data[, dependent],pred))
  class_error <- 1-((conf_matrix$Freq[1]+conf_matrix$Freq[4])/sum(conf_matrix$Freq))
  
  pred <- predict(mod, testing_data)
  pred <- mapvalues(pred, from = c("X0", "X1"), to = c("0", "1"))
  tmp <- data.frame(testing_data[, dependent],pred)
  kappa_test<-kappa2(tmp[1:2])[[5]]
  conf_matrix <- data.frame(table(testing_data[, dependent],pred))
  class_error_test <- 1-((conf_matrix$Freq[1]+conf_matrix$Freq[4])/sum(conf_matrix$Freq))
  error_subset <-rbind(data.frame(name, kappa, class_error,  kappa_test, class_error_test),error_subset)
  
  ###  # nnet
  mod <- train((as.formula(paste(variable_subset))),
               data=training_data,  method="nnet")
  
  pred <- predict(mod, training_data)
  pred <- mapvalues(pred, from = c("X0", "X1"), to = c("0", "1"))
  training_data_CHURN <- as.numeric(mapvalues(training_data[, dependent], from = c("X0", "X1"), to = c("0", "1")))
  name <- c("neural network")
  tmp <-data.frame(training_data_CHURN,pred)
  kappa<-kappa2(tmp[1:2])[[5]]
  conf_matrix <- data.frame(table(training_data[, dependent],pred))
  class_error <- 1-((conf_matrix$Freq[1]+conf_matrix$Freq[4])/sum(conf_matrix$Freq))
  
  pred <- predict(mod, testing_data)
  pred <- mapvalues(pred, from = c("X0", "X1"), to = c("0", "1"))
  tmp <-data.frame(testing_data[, dependent],pred)
  kappa_test <- kappa2(tmp[1:2])[[5]]
  conf_matrix <- data.frame(table(testing_data[, dependent],pred))
  class_error_test <- 1-((conf_matrix$Freq[1]+conf_matrix$Freq[4])/sum(conf_matrix$Freq))
  error_subset <-rbind(data.frame(name, kappa, class_error,  kappa_test, class_error_test),error_subset)
  
  ### svmRadial
  mod <- train((as.formula(paste(variable_subset))), 
               data=training_data,  method="svmRadial")
  
  pred <- predict(mod, training_data)
  pred <- mapvalues(pred, from = c("X0", "X1"), to = c("0", "1"))
  training_data_CHURN <- as.numeric(mapvalues(training_data[, dependent], from = c("X0", "X1"), to = c("0", "1")))
  name <- c("svm radial")
  tmp <- data.frame(training_data_CHURN,pred)
  kappa <- kappa2(tmp[1:2])[[5]]
  conf_matrix <- data.frame(table(training_data[, dependent],pred))
  class_error <- 1-((conf_matrix$Freq[1]+conf_matrix$Freq[4])/sum(conf_matrix$Freq))
  
  pred <- predict(mod, testing_data)
  pred <- mapvalues(pred, from = c("X0", "X1"), to = c("0", "1"))
  tmp <- data.frame(testing_data[, dependent],pred)
  kappa_test <- kappa2(tmp[1:2])[[5]]
  conf_matrix <- data.frame(table(testing_data[, dependent],pred))
  class_error_test <- 1-((conf_matrix$Freq[1]+conf_matrix$Freq[4])/sum(conf_matrix$Freq))
  error_subset <-rbind(data.frame(name, kappa, class_error,  kappa_test, class_error_test),error_subset)
  
  ### logistic
  training_data[, dependent] <- as.numeric(mapvalues(training_data[, dependent], from = c("X0", "X1"), to = c("0", "1")))
  
  mod <- glm((as.formula(paste(variable_subset))),
             data=training_data,family=binomial())
  
  probs <- predict(mod, training_data[, -c(1, dependent, categorical, reference_category)], type="response")
  pred=rep (0 ,nrow(training_data))
  pred[probs >.5]=1
  name <- c("logistic")
  tmp <- data.frame(training_data[, dependent],pred)
  kappa <- kappa2(tmp[1:2])[[5]]
  conf_matrix <- data.frame(table(training_data[, dependent],pred))
  class_error <- 1-((conf_matrix$Freq[1]+conf_matrix$Freq[4])/sum(conf_matrix$Freq))
  
  probs <- predict(mod, testing_data[, -c(1, dependent, categorical, reference_category)], type="response")
  pred=rep (0 ,nrow(testing_data))
  pred[probs >.5]=1
  tmp <-data.frame(testing_data[, dependent],pred)
  kappa_test<-kappa2(tmp[1:2])[[5]]
  conf_matrix <- data.frame(table(testing_data[, dependent],pred))
  class_error_test <- 1-((conf_matrix$Freq[1]+conf_matrix$Freq[4])/sum(conf_matrix$Freq))
  error_subset <-rbind(data.frame(name, kappa, class_error,  kappa_test, class_error_test),error_subset)
  
  # ------------------------------
  # print table
  
  wd <- getwd()
  
  error_subset[2:5]<- round(error_subset[2:5], digits = 3)
  colnames(error_subset) <- c("method", "kappa - train","classification error - train", 
                              "kappa - test","classification error - test")
  error_subset <<- error_subset
  
  tt <- ttheme_default(core=list(fg_params=list(hjust=0, x=0.05)),
                       rowhead=list(fg_params=list(hjust=0, x=0)))
  
  png(filename = paste0(wd, "/error_subset.png"),
      width=7.7, height=2.2, units="in", res=150)
  grid.table(error_subset, theme=tt, rows = NULL)
  t <-dev.off()
  
}

subset_analysis(training_data,testing_data)
