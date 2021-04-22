##### Split data into training and test sets --------------------------------------------------------------

# Counts and means for S&P 500 decreases/increases
SPWeekly %>% group_by(Direction) %>% 
  summarise(Count = n(),
            Change = mean(PctChange))
SPDaily %>% group_by(Direction) %>% 
  summarise(Count = n(),
            Change = mean(PctChange))

# Split S&P 500 data into training and test sets
trainVec <- 1:(25*52)
SPWeekly <- SPWeekly[SPWeekly$Direction != "Zero", ]
SPWeekly$Direction <- factor(SPWeekly$Direction)
SPWeeklyTrain <- SPWeekly[trainVec, ]
SPWeeklyTest <- SPWeekly[-trainVec, ]

# Counts and means for NASDAQ decreases/increases
NDWeekly %>% group_by(Direction) %>% 
  summarise(Count = n(),
            Change = mean(PctChange))
NDDaily %>% group_by(Direction) %>% 
  summarise(Count = n(),
            Change = mean(PctChange))

# Split NASDAQ data into training and test sets
NDWeekly <- NDWeekly[NDWeekly$Direction != "Zero", ]
NDWeekly$Direction <- factor(NDWeekly$Direction)
NDWeeklyTrain <- NDWeekly[trainVec, ]
NDWeeklyTest <- NDWeekly[-trainVec, ]

# Counts and means for Dow Jones decreases/increases
DJWeekly %>% group_by(Direction) %>% 
  summarise(Count = n(),
            Change = mean(PctChange))
DJDaily %>% group_by(Direction) %>% 
  summarise(Count = n(),
            Change = mean(PctChange))

# Split Dow Jones data into training and test sets
trainVec <- 1:(19*52)
DJWeekly <- DJWeekly[DJWeekly$Direction != "Zero", ]
DJWeekly$Direction <- factor(DJWeekly$Direction)
DJWeeklyTrain <- DJWeekly[trainVec, ]
DJWeeklyTest <- DJWeekly[-trainVec, ]





##### Logistic regression ---------------------------------------------------------------------------------

# Note: Our analyses show that VLag1 - VLag5 doesn't make much of a difference
# Thus, we remove it to make the models simpler
select(SPWeeklyTrain, !(VLag1:VLag5)) -> SPWeeklyTrain
select(SPWeeklyTest, !(VLag1:VLag5)) -> SPWeeklyTest

# Function to perform logistic regression and output predictive accuracy
logreg <- function(formula, dataTrain, output){
  
  # Fit logistic regression using training data
  glm.fit <- glm(formula, data = dataTrain, family = "binomial")
  
  # Classify training data; get posterior probabilities
  glm.probs <- predict(glm.fit, dataTrain, type = "response")
  glm.pred <- rep("Down", length(glm.probs))
  glm.pred[glm.probs >= 0.5] <- "Up"
  
  # Confusion matrix of classified observations
  # Must manually add Down row if model never predicts Down
  confMatrix <- table(glm.pred, dataTrain$Direction)
  if(nrow(confMatrix) == 1){
    confMatrix <- rbind(c(0, 0), confMatrix)}
  rownames(confMatrix) <- c("[P]Down", "[P]Up")
  colnames(confMatrix) <- c("[O]Down", "[O]Up")
  
  # Overall predictive accuracy
  accuracy <- sum(diag(confMatrix)/sum(confMatrix))
  
  # Sensitivity and specificity
  specificity <- confMatrix[1, 1]/sum(confMatrix[, 1])
  sensitivity <- confMatrix[2, 2]/sum(confMatrix[, 2])
  
  # Positive and negative predictive value
  npv <- confMatrix[1, 1]/sum(confMatrix[1, ])
  ppv <- confMatrix[2, 2]/sum(confMatrix[2, ])
  
  # Return individual outputs or list of all outputs
  if(output == "all"){
    return(list(confMatrix = confMatrix, accuracy = accuracy, specificity = specificity,
                sensitivity = sensitivity, npv = npv, ppv = ppv))}
  if(output == "confMatrix"){
    return(confMatrix)}
  if(output == "accuracy"){
    return(accuracy)}
  if(output == "specificity"){
    return(specificity)}
  if(output == "sensitivity"){
    return(sensitivity)}
  if(output == "npv"){
    return(npv)}
  if(output == "ppv"){
    return(ppv)}}

# Function to fit logistic regression to all possible combinations of n variables
# Outputs value and combination of variables for desired prediction metric
logreg.comb <- function(dataTrain, output, n){
  combn(names(dataTrain)[-c(2:3)], n) %>% 
    apply(FUN = paste0, MARGIN = 2, collapse = "+") %>% 
    paste0("Direction~", .) %>% 
    sapply(FUN = logreg, dataTrain = dataTrain, output = output) %>% 
    sort() %>%
    return()}

# Create empty list to populate with top models
SPWeeklyLMod <- list()

# Find top 10 models for accuracy
SPWeeklyTop <- unlist(sapply(1:8, FUN = logreg.comb, dataTrain = SPWeeklyTrain, output = "accuracy"))
sort(SPWeeklyTop, decreasing = TRUE)[1:10]
SPWeeklyLMod[[1]] <- names(sort(SPWeeklyTop, decreasing = TRUE)[1])
logreg(SPWeeklyLMod[[1]], dataTrain = SPWeeklyTrain, output = "confMatrix")

# Find top 10 models for ppv
SPWeeklyTop <- unlist(sapply(1:8, FUN = logreg.comb, dataTrain = SPWeeklyTrain, output = "ppv"))
sort(SPWeeklyTop, decreasing = TRUE)[1:10]
SPWeeklyLMod[[2]] <- names(sort(SPWeeklyTop, decreasing = TRUE)[1])
logreg(SPWeeklyLMod[[2]], dataTrain = SPWeeklyTrain, output = "confMatrix")

# Find top 10 models for npv
SPWeeklyTop <- unlist(sapply(1:8, FUN = logreg.comb, dataTrain = SPWeeklyTrain, output = "npv"))
sort(SPWeeklyTop, decreasing = TRUE)[1:10]
logreg(names(sort(SPWeeklyTop, decreasing = TRUE))[1], dataTrain = SPWeeklyTrain, output = "confMatrix")

# Function to remove a term and find new accuracy
logreg.lcv1 <- function(i, dataTrain, output){
  predVec[-i] %>% 
    paste0(., collapse = "+") %>% 
    paste0("Direction~", .) %>% 
    logreg(dataTrain = SPWeeklyTrain, output = output) %>% 
    return()}

# Function to return accuracy and new list of predictors after removing a predictor
logreg.bwd <- function(predVec, output){
  accs <- sapply(1:length(predVec), FUN = logreg.lcv1, dataTrain = SPWeeklyTrain, output = output)
  pv <- predVec[-which.max(accs)]
  return(list(predVec = pv, maxacc = max(accs)))}

# Function to add a term and find new accuracy
logreg.lcv2 <- function(i, dataTrain, output){
  c(predVecFwd, predVecRemain[i]) %>% 
    paste0(., collapse = "+") %>% 
    paste0("Direction~", .) %>% 
    logreg(dataTrain = SPWeeklyTrain, output = output) %>% 
    return()}

# Function to return accuracy and new list of predictors after adding a predictor
logreg.fwd <- function(predVecRemain, predVecFwd, output){
  accs <- sapply(1:length(predVecRemain), FUN = logreg.lcv2, dataTrain = SPWeeklyTrain, output = output)
  pvf <- c(predVecFwd, predVecRemain[which.max(accs)])
  pvr <- predVecRemain[-which.max(accs)]
  return(list(predVecFwd = pvf, predVecRemain = pvr, maxacc = max(accs)))}

# Apply backward step algorithm using accuracy
combn(names(SPWeeklyTrain)[-c(2:3)], 2) %>%
  apply(FUN = paste0, MARGIN = 2, collapse = ":") %>%
  c(names(SPWeeklyTrain)[-c(2:3)]) -> predVec
prevmaxacc <- c(0.5, 0.5)
maxacc <- logreg.bwd(predVec, "accuracy")$maxacc
predVec <- logreg.bwd(predVec, "accuracy")$predVec
prevmaxacc <- c(prevmaxacc, maxacc)
while(length(predVec) > 1){
  maxacc <- logreg.bwd(predVec, "accuracy")$maxacc
  if(maxacc < prevmaxacc[length(prevmaxacc) - 1]){
    break}
  predVec <- logreg.bwd(predVec, "accuracy")$predVec
  prevmaxacc <- c(prevmaxacc, maxacc)}
prevmaxacc[length(prevmaxacc)]
SPWeeklyLMod[[3]] <- paste0("Direction~", paste0(predVec, collapse = "+"))

# Apply forward step algorithm using accuracy
combn(names(SPWeeklyTrain)[-c(2:3)], 2) %>%
  apply(FUN = paste0, MARGIN = 2, collapse = ":") %>%
  c(names(SPWeeklyTrain)[-c(2:3)]) -> predVecRemain
prevmaxacc <- c(0.5, 0.5)
predVecFwd <- c()
maxacc <- logreg.fwd(predVecRemain, predVecFwd, "accuracy")$maxacc
predVecFwd <- logreg.fwd(predVecRemain, predVecFwd, "accuracy")$predVecFwd
predVecRemain <- predVecRemain[predVecRemain != predVecFwd[length(predVecFwd)]]
prevmaxacc <- c(prevmaxacc, maxacc)
while(length(predVecFwd) < 36){
  maxacc <- logreg.fwd(predVecRemain, predVecFwd, "accuracy")$maxacc
  if(maxacc < prevmaxacc[length(prevmaxacc) - 1]){
    break}
  predVecFwd <- logreg.fwd(predVecRemain, predVecFwd, "accuracy")$predVecFwd
  predVecRemain <- predVecRemain[predVecRemain != predVecFwd[length(predVecFwd)]]
  prevmaxacc <- c(prevmaxacc, maxacc)}
prevmaxacc[length(prevmaxacc)]
SPWeeklyLMod[[4]] <- paste0("Direction~", paste0(predVecFwd, collapse = "+"))

# Apply backward step algorithm using PPV
combn(names(SPWeeklyTrain)[-c(2:3)], 2) %>%
  apply(FUN = paste0, MARGIN = 2, collapse = ":") %>%
  c(names(SPWeeklyTrain)[-c(2:3)]) -> predVec
prevmaxacc <- c(0.5, 0.5)
maxacc <- logreg.bwd(predVec, "ppv")$maxacc
predVec <- logreg.bwd(predVec, "ppv")$predVec
prevmaxacc <- c(prevmaxacc, maxacc)
while(length(predVec) > 1){
  maxacc <- logreg.bwd(predVec, "ppv")$maxacc
  if(maxacc < prevmaxacc[length(prevmaxacc) - 1]){
    break}
  predVec <- logreg.bwd(predVec, "ppv")$predVec
  prevmaxacc <- c(prevmaxacc, maxacc)}
prevmaxacc[length(prevmaxacc)]
SPWeeklyLMod[[5]] <- paste0("Direction~", paste0(predVec, collapse = "+"))

# Apply forward step algorithm using PPV
combn(names(SPWeeklyTrain)[-c(2:3)], 2) %>%
  apply(FUN = paste0, MARGIN = 2, collapse = ":") %>%
  c(names(SPWeeklyTrain)[-c(2:3)]) -> predVecRemain
prevmaxacc <- c(0.5, 0.5)
predVecFwd <- c()
maxacc <- logreg.fwd(predVecRemain, predVecFwd, "ppv")$maxacc
predVecFwd <- logreg.fwd(predVecRemain, predVecFwd, "ppv")$predVecFwd
predVecRemain <- predVecRemain[predVecRemain != predVecFwd[length(predVecFwd)]]
prevmaxacc <- c(prevmaxacc, maxacc)
while(length(predVecFwd) < 36){
  maxacc <- logreg.fwd(predVecRemain, predVecFwd, "ppv")$maxacc
  if(maxacc < prevmaxacc[length(prevmaxacc) - 1]){
    break}
  predVecFwd <- logreg.fwd(predVecRemain, predVecFwd, "ppv")$predVecFwd
  predVecRemain <- predVecRemain[predVecRemain != predVecFwd[length(predVecFwd)]]
  prevmaxacc <- c(prevmaxacc, maxacc)}
prevmaxacc[length(prevmaxacc)]
SPWeeklyLMod[[6]] <- paste0("Direction~", paste0(predVecFwd, collapse = "+"))

# Compare model test accuracy
sapply(SPWeeklyLMod, FUN = logreg, dataTrain = SPWeeklyTest, output = "accuracy")

# Compare model test PPV
sapply(SPWeeklyLMod, FUN = logreg, dataTrain = SPWeeklyTest, output = "ppv")

# Function to estimate total return on model vs no model
# Assumes complete liquidation of assets prior to predicted decrease
# Setting lrisk to TRUE liquidates only 35% of assets prior to predicted decrease
returns.logreg <- function(formula, data, final = FALSE, lrisk = FALSE){
  if(formula != "none"){
    glm.fit <- glm(formula = formula, data = data, family = "binomial")
    glm.probs <- predict(glm.fit, data, type = "response")
    glm.pred <- rep("Down", length(glm.probs))
    glm.pred[glm.probs >= 0.5] <- "Up"
    pcts <- data$PctChange
    if(lrisk == FALSE){
      pcts[glm.pred == "Down"] <- 0}
    if(lrisk == TRUE){
      pcts[glm.pred == "Down"] <- pcts[glm.pred == "Down"]*0.65}
    totalReturn <- c()
    for(i in 1:length(pcts)){
      totalReturn <- c(totalReturn, prod((pcts[1:i]/100)+1))}}
  if(formula == "none"){
    totalReturn <- c()
    for(i in 1:nrow(data)){
      totalReturn <- c(totalReturn, prod((data$PctChange[1:i]/100)+1))}}
  ifelse(final == TRUE, return(totalReturn[length(totalReturn)]), return(totalReturn))}

# Compare lifetime returns; use loop since sapply doesn't work for some reason
for(i in 1:6){
  print(returns.logreg(SPWeeklyLMod[[i]], data = SPWeeklyTest, final = TRUE))}

# Remove variables from global environment
remove(trainVec, SPWeeklyTop, maxacc, predVec, predVecFwd, predVecRemain, prevmaxacc)





##### Discriminant analysis -------------------------------------------------------------------------------

# Function to perform discriminant analysis and output predictive accuracy
damod <- function(formula, dataTrain, daType, output){
  
  # Fit linear or quadratic discriminant analysis using training data
  if(daType == "lda"){
    da.fit <- lda(formula, data = dataTrain)}
  if(daType == "qda"){
    da.fit <- qda(formula, data = dataTrain)}
  
  # Classify training data; get posterior probabilities
  da.pred <- predict(da.fit, dataTrain)$class
  
  # Confusion matrix of classified observations
  # Must manually add Down row if model never predicts Down
  confMatrix <- table(da.pred, dataTrain$Direction)
  if(nrow(confMatrix) == 1){
    confMatrix <- rbind(c(0, 0), confMatrix)}
  rownames(confMatrix) <- c("[P]Down", "[P]Up")
  colnames(confMatrix) <- c("[O]Down", "[O]Up")
  
  # Overall predictive accuracy
  accuracy <- sum(diag(confMatrix)/sum(confMatrix))
  
  # Sensitivity and specificity
  specificity <- confMatrix[1, 1]/sum(confMatrix[, 1])
  sensitivity <- confMatrix[2, 2]/sum(confMatrix[, 2])
  
  # Positive and negative predictive value
  npv <- confMatrix[1, 1]/sum(confMatrix[1, ])
  ppv <- confMatrix[2, 2]/sum(confMatrix[2, ])
  
  # Return individual outputs or list of all outputs
  if(output == "all"){
    return(list(confMatrix = confMatrix, accuracy = accuracy, specificity = specificity,
                sensitivity = sensitivity, npv = npv, ppv = ppv))}
  if(output == "confMatrix"){
    return(confMatrix)}
  if(output == "accuracy"){
    return(accuracy)}
  if(output == "specificity"){
    return(specificity)}
  if(output == "sensitivity"){
    return(sensitivity)}
  if(output == "npv"){
    return(npv)}
  if(output == "ppv"){
    return(ppv)}}

# Function to fit logistic regression to all possible combinations of n variables
# Outputs value and combination of variables for desired prediction metric
damod.comb <- function(dataTrain, daType, output, n){
  combn(names(dataTrain)[-c(2:3)], n) %>% 
    apply(FUN = paste0, MARGIN = 2, collapse = "+") %>% 
    paste0("Direction~", .) %>%
    sapply(formula) %>% 
    sapply(FUN = damod, dataTrain = dataTrain, daType = daType, output = output) %>% 
    sort() %>%
    return()}

# Create empty list to populate with top models
SPWeeklyDMod <- list()

# Find top 10 models for accuracy: LDA
SPWeeklyTop <- unlist(sapply(1:8, FUN = damod.comb, dataTrain = SPWeeklyTrain, daType = "lda", output = "accuracy"))
sort(SPWeeklyTop, decreasing = TRUE)[1:10]
SPWeeklyDMod[[1]] <- names(sort(SPWeeklyTop, decreasing = TRUE)[1])
damod(formula(SPWeeklyDMod[[1]]), dataTrain = SPWeeklyTrain, daType = "lda", output = "confMatrix")

# Find top 10 models for accuracy: QDA
SPWeeklyTop <- unlist(sapply(1:8, FUN = damod.comb, dataTrain = SPWeeklyTrain, daType = "qda", output = "accuracy"))
sort(SPWeeklyTop, decreasing = TRUE)[1:10]
SPWeeklyDMod[[2]] <- names(sort(SPWeeklyTop, decreasing = TRUE)[1])
damod(formula(SPWeeklyDMod[[2]]), dataTrain = SPWeeklyTrain, daType = "qda", output = "confMatrix")

# Find top 10 models for ppv: LDA
SPWeeklyTop <- unlist(sapply(1:8, FUN = damod.comb, dataTrain = SPWeeklyTrain, daType = "lda", output = "ppv"))
sort(SPWeeklyTop, decreasing = TRUE)[1:10]
SPWeeklyDMod[[3]] <- names(sort(SPWeeklyTop, decreasing = TRUE)[1])
damod(formula(SPWeeklyDMod[[3]]), dataTrain = SPWeeklyTrain, daType = "lda", output = "confMatrix")

# Find top 10 models for ppv: QDA
SPWeeklyTop <- unlist(sapply(1:8, FUN = damod.comb, dataTrain = SPWeeklyTrain, daType = "qda", output = "ppv"))
sort(SPWeeklyTop, decreasing = TRUE)[1:10]
SPWeeklyDMod[[4]] <- names(sort(SPWeeklyTop, decreasing = TRUE)[1])
damod(formula(SPWeeklyDMod[[4]]), dataTrain = SPWeeklyTrain, daType = "qda", output = "confMatrix")

# Find top 10 models for npv
SPWeeklyTop <- unlist(sapply(1:8, FUN = damod.comb, dataTrain = SPWeeklyTrain, daType = "lda", output = "npv"))
sort(SPWeeklyTop, decreasing = TRUE)[1:10]
logreg(names(sort(SPWeeklyTop, decreasing = TRUE))[1], dataTrain = SPWeeklyTrain, output = "confMatrix")

# Function to remove a term and find new accuracy
damod.lcv1 <- function(i, dataTrain, daType, output){
  predVec[-i] %>% 
    paste0(., collapse = "+") %>% 
    paste0("Direction~", .) %>%
    formula() %>% 
    damod(dataTrain = SPWeeklyTrain, daType = daType, output = output) %>% 
    return()}

# Function to return accuracy and new list of predictors after removing a predictor
damod.bwd <- function(predVec, daType, output){
  accs <- sapply(1:length(predVec), FUN = damod.lcv1, dataTrain = SPWeeklyTrain, daType = daType, output = output)
  pv <- predVec[-which.max(accs)]
  return(list(predVec = pv, maxacc = max(accs)))}

# Function to add a term and find new accuracy
damod.lcv2 <- function(i, dataTrain, daType, output){
  c(predVecFwd, predVecRemain[i]) %>% 
    paste0(., collapse = "+") %>% 
    paste0("Direction~", .) %>%
    formula() %>%
    damod(dataTrain = SPWeeklyTrain, daType = daType, output = output) %>% 
    return()}

# Function to return accuracy and new list of predictors after adding a predictor
damod.fwd <- function(predVecRemain, predVecFwd, daType, output){
  accs <- sapply(1:length(predVecRemain), FUN = damod.lcv2, dataTrain = SPWeeklyTrain,
                 daType = daType, output = output)
  pvf <- c(predVecFwd, predVecRemain[which.max(accs)])
  pvr <- predVecRemain[-which.max(accs)]
  return(list(predVecFwd = pvf, predVecRemain = pvr, maxacc = max(accs)))}

# Apply backward step algorithm using accuracy: LDA
combn(names(SPWeeklyTrain)[-c(2:3)], 2) %>%
  apply(FUN = paste0, MARGIN = 2, collapse = ":") %>%
  c(names(SPWeeklyTrain)[-c(2:3)]) -> predVec
prevmaxacc <- c(0.5, 0.5)
maxacc <- damod.bwd(predVec, "lda", "accuracy")$maxacc
predVec <- damod.bwd(predVec, "lda", "accuracy")$predVec
prevmaxacc <- c(prevmaxacc, maxacc)
while(length(predVec) > 1){
  maxacc <- damod.bwd(predVec, "lda", "accuracy")$maxacc
  if(maxacc < prevmaxacc[length(prevmaxacc) - 1]){
    break}
  predVec <- damod.bwd(predVec, "lda", "accuracy")$predVec
  prevmaxacc <- c(prevmaxacc, maxacc)}
prevmaxacc[length(prevmaxacc)]
SPWeeklyDMod[[5]] <- paste0("Direction~", paste0(predVec, collapse = "+"))

# Apply forward step algorithm using accuracy: LDA
combn(names(SPWeeklyTrain)[-c(2:3)], 2) %>%
  apply(FUN = paste0, MARGIN = 2, collapse = ":") %>%
  c(names(SPWeeklyTrain)[-c(2:3)]) -> predVecRemain
prevmaxacc <- c(0.5, 0.5)
predVecFwd <- c()
maxacc <- damod.fwd(predVecRemain, predVecFwd, "lda", "accuracy")$maxacc
predVecFwd <- damod.fwd(predVecRemain, predVecFwd, "lda", "accuracy")$predVecFwd
predVecRemain <- predVecRemain[predVecRemain != predVecFwd[length(predVecFwd)]]
prevmaxacc <- c(prevmaxacc, maxacc)
while(length(predVecFwd) < 36){
  maxacc <- damod.fwd(predVecRemain, predVecFwd, "lda", "accuracy")$maxacc
  if(maxacc < prevmaxacc[length(prevmaxacc) - 1]){
    break}
  predVecFwd <- damod.fwd(predVecRemain, predVecFwd, "lda", "accuracy")$predVecFwd
  predVecRemain <- predVecRemain[predVecRemain != predVecFwd[length(predVecFwd)]]
  prevmaxacc <- c(prevmaxacc, maxacc)}
prevmaxacc[length(prevmaxacc)]
SPWeeklyDMod[[6]] <- paste0("Direction~", paste0(predVecFwd, collapse = "+"))

# Apply backward step algorithm using accuracy: QDA
combn(names(SPWeeklyTrain)[-c(2:3)], 2) %>%
  apply(FUN = paste0, MARGIN = 2, collapse = ":") %>%
  c(names(SPWeeklyTrain)[-c(2:3)]) -> predVec
prevmaxacc <- c(0.5, 0.5)
maxacc <- damod.bwd(predVec, "qda", "accuracy")$maxacc
predVec <- damod.bwd(predVec, "qda", "accuracy")$predVec
prevmaxacc <- c(prevmaxacc, maxacc)
while(length(predVec) > 1){
  maxacc <- damod.bwd(predVec, "qda", "accuracy")$maxacc
  if(maxacc < prevmaxacc[length(prevmaxacc) - 1]){
    break}
  predVec <- damod.bwd(predVec, "qda", "accuracy")$predVec
  prevmaxacc <- c(prevmaxacc, maxacc)}
prevmaxacc[length(prevmaxacc)]
SPWeeklyDMod[[7]] <- paste0("Direction~", paste0(predVec, collapse = "+"))

# Apply forward step algorithm using accuracy: QDA
combn(names(SPWeeklyTrain)[-c(2:3)], 2) %>%
  apply(FUN = paste0, MARGIN = 2, collapse = ":") %>%
  c(names(SPWeeklyTrain)[-c(2:3)]) -> predVecRemain
prevmaxacc <- c(0.5, 0.5)
predVecFwd <- c()
maxacc <- damod.fwd(predVecRemain, predVecFwd, "qda", "accuracy")$maxacc
predVecFwd <- damod.fwd(predVecRemain, predVecFwd, "qda", "accuracy")$predVecFwd
predVecRemain <- predVecRemain[predVecRemain != predVecFwd[length(predVecFwd)]]
prevmaxacc <- c(prevmaxacc, maxacc)
while(length(predVecFwd) < 36){
  maxacc <- damod.fwd(predVecRemain, predVecFwd, "qda", "accuracy")$maxacc
  if(maxacc < prevmaxacc[length(prevmaxacc) - 1]){
    break}
  predVecFwd <- damod.fwd(predVecRemain, predVecFwd, "qda", "accuracy")$predVecFwd
  predVecRemain <- predVecRemain[predVecRemain != predVecFwd[length(predVecFwd)]]
  prevmaxacc <- c(prevmaxacc, maxacc)}
prevmaxacc[length(prevmaxacc)]
SPWeeklyDMod[[8]] <- paste0("Direction~", paste0(predVecFwd, collapse = "+"))

# Apply backward step algorithm using PPV: LDA
combn(names(SPWeeklyTrain)[-c(2:3)], 2) %>%
  apply(FUN = paste0, MARGIN = 2, collapse = ":") %>%
  c(names(SPWeeklyTrain)[-c(2:3)]) -> predVec
prevmaxacc <- c(0.5, 0.5)
maxacc <- damod.bwd(predVec, "lda", "ppv")$maxacc
predVec <- damod.bwd(predVec, "lda", "ppv")$predVec
prevmaxacc <- c(prevmaxacc, maxacc)
while(length(predVec) > 1){
  maxacc <- damod.bwd(predVec, "lda", "ppv")$maxacc
  if(maxacc < prevmaxacc[length(prevmaxacc) - 1]){
    break}
  predVec <- damod.bwd(predVec, "lda", "ppv")$predVec
  prevmaxacc <- c(prevmaxacc, maxacc)}
prevmaxacc[length(prevmaxacc)]
SPWeeklyDMod[[9]] <- paste0("Direction~", paste0(predVec, collapse = "+"))

# Apply forward step algorithm using PPV: LDA
combn(names(SPWeeklyTrain)[-c(2:3)], 2) %>%
  apply(FUN = paste0, MARGIN = 2, collapse = ":") %>%
  c(names(SPWeeklyTrain)[-c(2:3)]) -> predVecRemain
prevmaxacc <- c(0.5, 0.5)
predVecFwd <- c()
maxacc <- damod.fwd(predVecRemain, predVecFwd, "lda", "ppv")$maxacc
predVecFwd <- damod.fwd(predVecRemain, predVecFwd, "lda", "ppv")$predVecFwd
predVecRemain <- predVecRemain[predVecRemain != predVecFwd[length(predVecFwd)]]
prevmaxacc <- c(prevmaxacc, maxacc)
while(length(predVecFwd) < 36){
  maxacc <- damod.fwd(predVecRemain, predVecFwd, "lda", "ppv")$maxacc
  if(maxacc < prevmaxacc[length(prevmaxacc) - 1]){
    break}
  predVecFwd <- damod.fwd(predVecRemain, predVecFwd, "lda", "ppv")$predVecFwd
  predVecRemain <- predVecRemain[predVecRemain != predVecFwd[length(predVecFwd)]]
  prevmaxacc <- c(prevmaxacc, maxacc)}
prevmaxacc[length(prevmaxacc)]
SPWeeklyDMod[[10]] <- paste0("Direction~", paste0(predVecFwd, collapse = "+"))

# Apply backward step algorithm using PPV: QDA
combn(names(SPWeeklyTrain)[-c(2:3)], 2) %>%
  apply(FUN = paste0, MARGIN = 2, collapse = ":") %>%
  c(names(SPWeeklyTrain)[-c(2:3)]) -> predVec
prevmaxacc <- c(0.5, 0.5)
maxacc <- damod.bwd(predVec, "qda", "ppv")$maxacc
predVec <- damod.bwd(predVec, "qda", "ppv")$predVec
prevmaxacc <- c(prevmaxacc, maxacc)
while(length(predVec) > 1){
  maxacc <- damod.bwd(predVec, "qda", "ppv")$maxacc
  if(maxacc < prevmaxacc[length(prevmaxacc) - 1]){
    break}
  predVec <- damod.bwd(predVec, "qda", "ppv")$predVec
  prevmaxacc <- c(prevmaxacc, maxacc)}
prevmaxacc[length(prevmaxacc)]
SPWeeklyDMod[[11]] <- paste0("Direction~", paste0(predVec, collapse = "+"))

# Apply forward step algorithm using PPV: QDA
combn(names(SPWeeklyTrain)[-c(2:3)], 2) %>%
  apply(FUN = paste0, MARGIN = 2, collapse = ":") %>%
  c(names(SPWeeklyTrain)[-c(2:3)]) -> predVecRemain
prevmaxacc <- c(0.5, 0.5)
predVecFwd <- c()
maxacc <- damod.fwd(predVecRemain, predVecFwd, "qda", "ppv")$maxacc
predVecFwd <- damod.fwd(predVecRemain, predVecFwd, "qda", "ppv")$predVecFwd
predVecRemain <- predVecRemain[predVecRemain != predVecFwd[length(predVecFwd)]]
prevmaxacc <- c(prevmaxacc, maxacc)
while(length(predVecFwd) < 36){
  maxacc <- damod.fwd(predVecRemain, predVecFwd, "qda", "ppv")$maxacc
  if(maxacc < prevmaxacc[length(prevmaxacc) - 1]){
    break}
  predVecFwd <- damod.fwd(predVecRemain, predVecFwd, "qda", "ppv")$predVecFwd
  predVecRemain <- predVecRemain[predVecRemain != predVecFwd[length(predVecFwd)]]
  prevmaxacc <- c(prevmaxacc, maxacc)}
prevmaxacc[length(prevmaxacc)]
SPWeeklyDMod[[12]] <- paste0("Direction~", paste0(predVecFwd, collapse = "+"))

# Remove variables from global environment
remove(SPWeeklyTop, maxacc, predVec, predVecFwd, predVecRemain, prevmaxacc)

# Compare model test accuracy (LDA)
SPWeeklyDMod[c(1, 3, 5, 6, 9, 10)] %>% 
  sapply(formula) %>% 
  sapply(FUN = damod, dataTrain = SPWeeklyTest, daType = "lda", output = "accuracy")

# Compare model test accuracy (QDA)
SPWeeklyDMod[c(2, 4, 7, 8, 11, 12)] %>% 
  sapply(formula) %>% 
  sapply(FUN = damod, dataTrain = SPWeeklyTest, daType = "qda", output = "accuracy")

# Compare model test PPV (LDA)
SPWeeklyDMod[c(1, 3, 5, 6, 9, 10)] %>% 
  sapply(formula) %>% 
  sapply(FUN = damod, dataTrain = SPWeeklyTest, daType = "lda", output = "ppv")

# Compare model test PPV (QDA)
SPWeeklyDMod[c(2, 4, 7, 8, 11, 12)] %>% 
  sapply(formula) %>% 
  sapply(FUN = damod, dataTrain = SPWeeklyTest, daType = "qda", output = "ppv")

# Function to estimate total return on model vs no model
# Assumes complete liquidation of assets prior to predicted decrease
# Setting lrisk to TRUE liquidates only 35% of assets prior to predicted decrease
returns.damod <- function(formula, data, daType, final = FALSE, lrisk = FALSE){
  if(formula != "none"){
    if(daType == "lda"){
      da.fit <- lda(formula = formula(formula), data = data)}
    if(daType == "qda"){
      da.fit <- qda(formula = formula(formula), data = data)}
    da.pred <- predict(da.fit, data)$class
    pcts <- data$PctChange
    if(lrisk == FALSE){
      pcts[da.pred == "Down"] <- 0}
    if(lrisk == TRUE){
      pcts[da.pred == "Down"] <- pcts[da.pred == "Down"]*0.65}
    totalReturn <- c()
    for(i in 1:length(pcts)){
      totalReturn <- c(totalReturn, prod((pcts[1:i]/100)+1))}}
  if(formula == "none"){
    totalReturn <- c()
    for(i in 1:nrow(data)){
      totalReturn <- c(totalReturn, prod((data$PctChange[1:i]/100)+1))}}
  ifelse(final == TRUE, return(totalReturn[length(totalReturn)]), return(totalReturn))}

# Compare lifetime returns (LDA); use loop since sapply doesn't work for some reason
for(i in c(1, 3, 5, 6, 9, 10)){
  print(returns.damod(formula(SPWeeklyDMod[[i]]), data = SPWeeklyTest, daType = "lda", final = TRUE))}

# Compare lifetime returns (QDA); use loop since sapply doesn't work for some reason
for(i in c(2, 4, 7, 8, 11, 12)){
  print(returns.damod(formula(SPWeeklyDMod[[i]]), data = SPWeeklyTest, daType = "qda", final = TRUE))}

