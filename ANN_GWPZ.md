# install.packages("required packages")
library(dplyr)
library(neuralnet)
library(clusterSim)
library(gplots)
library(ROCR)
library(raster)
library(rgdal)
library(caret)


# 1 SPLIT DATASET
set.seed(1235678)
df <-  read.csv("path_to_your_csv_file_", header = T)
df$id <- 1:nrow(df)
data_train <- df %>% dplyr::sample_frac(0.75)
data_test  <- dplyr::anti_join(df, data_train, by = 'id')

write.csv(data_train, paste(outdir, "/", "data_train", ".csv", sep=""), row.names=F)
write.csv(data_test, paste(outdir, "/", "data_test", ".csv", sep=""), row.names=F)


outdir <- "path_to_your_csv_file_"
if (!dir.exists(outdir)) {
  dir.create(outdir, recursive = TRUE)
}

# Write the split datasets
write.csv(data_train, file.path(outdir, "data_train.csv"), row.names = FALSE)
write.csv(data_test, file.path(outdir, "data_test.csv"), row.names = FALSE)


data_train <-  read.csv("path_to_your_csv_datatrain_file_", header = T)
data_train <-(na.omit(data_train))
data_train <-data.frame(data_train) 

# LANDCOVER
labels=c("LANDCOVER1","LANDCOVER2","LANDCOVER3","LANDCOVER5","LANDCOVER7","LANDCOVER8")
LANDCOVER<-cut(data_train$LANDCOVER, c(1, 2, 3, 5, 7, 8, 9 ), right=FALSE, labels=labels)
table(LANDCOVER)
flags = data.frame(Reduce(cbind,lapply(levels(LANDCOVER),function(x){(LANDCOVER == x)*1})))
names(flags) = levels(LANDCOVER)
data_train = cbind(data_train, flags) # combine the landcover with original data

# SOIL
labels=c("SOIL1","SOIL2","SOIL3")
SOIL<-cut(data_train$SOIL, c(1, 2, 3, 4 ), right=FALSE, labels=labels)
table(SOIL)
flags = data.frame(Reduce(cbind,lapply(levels(SOIL),function(x){(SOIL == x)*1})))
names(flags) = levels(SOIL)
data_train = cbind(data_train, flags) # combine the soil with original data

# GEOLOGY
labels=c("GEOLOGY1","GEOLOGY2","GEOLOGY3","GEOLOGY4","GEOLOGY5","GEOLOGY6", "GEOLOGY7","GEOLOGY8")
GEOLOGY<-cut(data_train$GEOLOGY, c (1, 2, 3, 4, 5, 6, 7, 8, 9 ), right=FALSE, labels=labels)
table(GEOLOGY)
flags = data.frame(Reduce(cbind,lapply(levels(GEOLOGY),function(x){(GEOLOGY == x)*1})))
names(flags) = levels(GEOLOGY)
data_train = cbind(data_train, flags)  # combine the geology with original data

# LITHOLOGY
labels=c("LITHOLOGY1","LITHOLOGY2","LITHOLOGY3","LITHOLOGY4","LITHOLOGY5")
LITHOLOGY<-cut(data_train$LITHOLOGY, c (1, 2, 3, 4, 5, 6 ), right=FALSE, labels=labels)
table(LITHOLOGY)
flags = data.frame(Reduce(cbind,lapply(levels(LITHOLOGY),function(x){(LITHOLOGY == x)*1})))
names(flags) = levels(LITHOLOGY)
data_train = cbind(data_train, flags) # combine the lithology with original data

# #Remove original LULC, SOIL, GEOLOGY, LITHOLOGY (and id create in the beginning)
data_train <- data_train %>% dplyr::select(-c(LANDCOVER, SOIL, GEOLOGY, LITHOLOGY, id))

# Scale /normalize the dataset (except the categorical data levels, to prevent getting NaN)!
maxs <- apply(data_train[, 1:6], 2, max)
mins <- apply(data_train[, 1:6], 2, min)
scaled_train <- as.data.frame(scale(data_train[, 1:6], center = mins, scale = maxs - mins))
# nor = data.Normalization(data_trainb,type="n1",normalization="column",na.rm=FALSE)

# Add back
scaled_train <-  data.frame(cbind(scaled_train, data_train[, 7:28]))
head(scaled_train)

# 3 TESTING DATASET
data_test <-  read.csv("path_to_your_csv_testing_file", header = T)
data_test <-(na.omit(data_test))
data_test <-data.frame(data_test)

# LANDCOVER
labels=c("LANDCOVER1","LANDCOVER2","LANDCOVER3","LANDCOVER5","LANDCOVER7","LANDCOVER8")
LANDCOVER<-cut(data_test$LANDCOVER, c(1, 2, 3, 5, 7, 8, 9), right=FALSE, labels=labels)
table(LANDCOVER)
flags = data.frame(Reduce(cbind,lapply(levels(LANDCOVER),function(x){(LANDCOVER == x)*1})))
names(flags) = levels(LANDCOVER)
data_test = cbind(data_test, flags) # combine the landcover with original data

# SOIL
labels=c("SOIL1","SOIL2","SOIL3")
SOIL<-cut(data_test$SOIL, c(1, 2, 3, 4), right=FALSE, labels=labels)
table(SOIL)
flags = data.frame(Reduce(cbind,lapply(levels(SOIL),function(x){(SOIL == x)*1})))
names(flags) = levels(SOIL)
data_test = cbind(data_test, flags) # combine the soil with original data

# GEOLOGY
labels=c("GEOLOGY1","GEOLOGY2","GEOLOGY3","GEOLOGY4","GEOLOGY5","GEOLOGY6","GEOLOGY7","GEOLOGY8")
GEOLOGY<-cut(data_test$GEOLOGY, c (1, 2, 3, 4, 5, 6, 7, 8, 9), right=FALSE, labels=labels)
table(GEOLOGY)
flags = data.frame(Reduce(cbind,lapply(levels(GEOLOGY),function(x){(GEOLOGY == x)*1})))
names(flags) = levels(GEOLOGY)
data_test = cbind(data_test, flags) # combine the geology with original data

# LITHOLOGY
labels=c("LITHOLOGY1","LITHOLOGY2","LITHOLOGY3","LITHOLOGY4","LITHOLOGY5")
LITHOLOGY<-cut(data_test$LITHOLOGY, c (1, 2, 3, 4, 5, 6), right=FALSE, labels=labels)
table(LITHOLOGY)
flags = data.frame(Reduce(cbind,lapply(levels(LITHOLOGY),function(x){(LITHOLOGY == x)*1})))
names(flags) = levels(LITHOLOGY)
data_test = cbind(data_test, flags)  # combine the lithology with original data

#Remove original LULC, SOIL, GEOLOGY, LITHOLOGY (and id created in the beginning)
data_test <- data_test %>% dplyr::select(-c(LANDCOVER, SOIL, GEOLOGY, LITHOLOGY, id))

# Scale /normalize the dataset (except the categorical data levels, to prevent gettig NaN)!
maxs <- apply(data_test[, 1:6], 2, max)
mins <- apply(data_test[, 1:6], 2, min)
scaled_test <- as.data.frame(scale(data_test[, 1:6], center = mins, scale = maxs - mins))
# #nor = data.Normalization(data_test,type="n1",normalization="column",na.rm=FALSE)

# Add back
scaled_test <-  data.frame(cbind(scaled_test, data_test[, 7:28]))
head(scaled_test)

# 4 NN for Training data 
data_test <- na.omit(data_test)
data_train <- na.omit(data_train)
scaled_train <- na.omit(scaled_train)
scaled_test <- na.omit(scaled_test)

set.seed(222)
nn.ce <- neuralnet(Training ~ DRAINAGE+LINEAMENT+SEEPAGE+DEPTH+TWI
                   +LANDCOVER1+LANDCOVER2+LANDCOVER3+LANDCOVER5+LANDCOVER7+LANDCOVER8+SOIL1+SOIL2+SOIL3+GEOLOGY1+GEOLOGY2+GEOLOGY3+GEOLOGY4+GEOLOGY5+GEOLOGY6+GEOLOGY7+GEOLOGY8+LITHOLOGY1+LITHOLOGY2+LITHOLOGY3+LITHOLOGY4+LITHOLOGY5,
                   data= scaled_train, hidden=c(6,2), err.fct="ce", linear.output=FALSE, stepmax=1e+05) 

plot(nn.ce, radius  =  0.15,  arrow.length  =  0.2,  intercept  =  TRUE,
     intercept.factor  =  0.4,  information  =  TRUE,  information.pos  =  0.1,
     col.entry.synapse  =  "black",  col.entry  =  "black",
     col.hidden  =  "black",  col.hidden.synapse  =  "black",
     col.out  =  "black",  col.out.synapse  =  "black",
     col.intercept  =  "blue",  fontsize  =  12,  dimension  =  6,
     show.weights  =  TRUE)

nn.ce$result.matrix # to check the error  # error = 3.153287e+01

# Add ANN results to dataframe of covariate (independents)
out_train=data.frame(cbind(nn.ce$covariate, nn.ce$net.result[[1]]))
colnames(out_train)[28] <- "ANN_W" # Add header to ANN results
head(out_train)

# Combine the covariate data with ANN results and directly add the result column
out_train <- data.frame(nn.ce$covariate)
out_train$ANN_W <- nn.ce$net.result[[1]]  # Add ANN results with "ANN_W" as column name

# Check the first few rows to ensure it looks correct
head(out_train)


# 5 NN prediction using Testing data and ROC AUC 
#Let us first Test the model prediction performance using the training data (Data used for model building)
compute.out_train=compute(nn.ce,scaled_train[,c(-1)])
out_pred_train <- data.frame(cbind(scaled_train[,c(-1)], compute.out_train$net.result))
colnames(out_pred_train)[28] <- "ANN_P" # Add header to ANN results
table(scaled_train$Training,  out_pred_train$ANN_P  >  0.5)

# Let us Now Test the model prediction performance using the Testing data (not used during model building)
compute.output_test=compute(nn.ce,scaled_test[,c(-1)])
out_pred_test <- data.frame(cbind(scaled_test[,c(-1)], compute.output_test$net.result))
colnames(out_pred_test)[28] <- "ANN_P"
table(scaled_test$Training,  out_pred_test$ANN_P  >  0.5)

# calculate performance and prediction
detach("package:neuralnet", unload = TRUE)
ROCRpred_pred <- prediction(out_pred_test$ANN_P, data_test$Training)
ROCRpred_sucess <- prediction(out_pred_train$ANN_P, data_train$Training)
ROCRperf_sucess <- performance(ROCRpred_sucess, 'tpr','fpr')
ROCRperf_pred <- performance(ROCRpred_pred, 'tpr','fpr')

# plot performance rate
auc <- performance(ROCRpred_sucess, measure = "auc")
auc <- auc@y.values[[1]]
auc
par(mfcol = c(1,1))
plot(ROCRperf_sucess, main=paste0("Sucess rate AUC=",round(auc,2)), colorize=TRUE, text.adj=c(-0.5,1.7))

# plot prediction rate
auc <- performance(ROCRpred_pred, measure = "auc")
auc <- auc@y.values[[1]]
auc
par(mfcol = c(1,1))
plot(ROCRperf_pred, main=paste0("Prediction rate AUC=",round(auc,2)), colorize=TRUE, text.adj=c(-0.5,1.7))

#Using back propogation algorithm
set.seed(789612)
#set.seed(23478)
nn.bp <- neuralnet(Training ~ DRAINAGE+LINEAMENT+SEEPAGE+DEPTH+TWI
                   +LANDCOVER1+LANDCOVER2+LANDCOVER3+LANDCOVER5+LANDCOVER7+LANDCOVER8+SOIL1+SOIL2+SOIL3+GEOLOGY1+GEOLOGY2+GEOLOGY3+GEOLOGY4+GEOLOGY5+GEOLOGY6+GEOLOGY7+GEOLOGY8+LITHOLOGY1+LITHOLOGY2+LITHOLOGY3+LITHOLOGY4+LITHOLOGY5,  data=scaled_train, hidden= c(12,8), learningrate = 0.1,algorithm = "rprop+",err.fct="ce", linear.output=FALSE,stepmax=1e+05)
plot(nn.bp)
head(nn.bp$generalized.weights[[1]])
# 
compute.output_train=compute(nn.bp,scaled_train[,c(-1)])
out_pred_train <- data.frame(cbind(scaled_train[,c(-1)], compute.output_train$net.result))
colnames(out_pred_train)[28] <- "ANN_P" # Add header to ANN results
table(scaled_train$Training,  out_pred_train$ANN_P  >  0.5)
compute.output_test=compute(nn.bp,scaled_test[,c(-1)])
out_pred_test <- data.frame(cbind(scaled_test[,c(-1)], compute.output_test$net.result))
colnames(out_pred_test)[28] <- "ANN_P"
table(scaled_test$Training,  out_pred_test$ANN_P  >  0.5)

detach("package:neuralnet", unload = TRUE)
ROCRpred_pred <- prediction(out_pred_test$ANN_P, scaled_test$Training)
ROCRpred_sucess <- prediction(out_pred_train$ANN_P, scaled_train$Training)
ROCRperf_sucess <- performance(ROCRpred_sucess, 'tpr','fpr')
ROCRperf_pred <- performance(ROCRpred_pred, 'tpr','fpr')

# plot performance rate
auc <- performance(ROCRpred_sucess, measure = "auc")
auc <- auc@y.values[[1]]
auc
par(mfcol = c(1,1))
plot(ROCRperf_sucess, main=paste0("Sucess rate AUC=",round(auc,2)), colorize=TRUE, text.adj=c(-0.5,1.7))

# plot prediction rate
auc <- performance(ROCRpred_pred, measure = "auc")
auc <- auc@y.values[[1]]
auc
par(mfcol = c(1,1))
plot(ROCRperf_pred, main=paste0("Prediction rate AUC=",round(auc,2)), colorize=TRUE, text.adj=c(-0.5,1.7))

# 7  Produce prediction map using Raster data ---------------------------
# Load the Raster data
DRAINAGE = raster("path_to_your_csv_file_drainage.tif")
LINEAMENT = raster("path_to_your_csv_file_lineament1.tif")
SEEPAGE = raster("path_to_your_csv_file_Infiltration.tif")
DEPTH = raster("path_to_your_csv_file_Profondeur.tif")
TWI = raster("path_to_your_csv_file_TWI.tif")
LANDCOVER = raster("path_to_your_csv_file_lulc.tif")
SOIL = raster("path_to_your_csv_file_soil.tif")
GEOLOGY = raster("path_to_your_csv_file_geology.tif")
LITHOLOGY = raster("path_to_your_csv_file_lithology.tif")
# check attributes and projection and extent
extent(DRAINAGE)
extent(LINEAMENT)
extent(SEEPAGE)
extent(DEPTH)
extent(TWI)
extent(LANDCOVER)
extent(SOIL)
extent(GEOLOGY)
extent(LITHOLOGY)

SOIL <- projectRaster(SOIL, crs = crs(GEOLOGY))
LITHOLOGY <- projectRaster(LITHOLOGY, crs = crs(GEOLOGY))

# Resample them using the smallest area
DRAINAGE_r = resample(DRAINAGE, GEOLOGY, resample='bilinear') 
LINEAMENT_r = resample(LINEAMENT, GEOLOGY, resample='bilinear') 
SEEPAGE_r = resample(SEEPAGE, GEOLOGY, resample='bilinear') 
DEPTH_r = resample(DEPTH, GEOLOGY, resample='bilinear') 
TWI_r = resample(TWI, GEOLOGY, resample='bilinear') 
LANDCOVER_r = resample(LANDCOVER, GEOLOGY, resample='bilinear') 
SOIL_r = resample(SOIL, GEOLOGY, resample='bilinear') 
GEOLOGY_r = resample(GEOLOGY, GEOLOGY, resample='bilinear') 
LITHOLOGY_r = resample(LITHOLOGY, GEOLOGY, resample='bilinear') 
# 
extent(LITHOLOGY_r) # check the new extent
extent(SOIL_r)

# write to a new geotiff file
writeRaster(DRAINAGE_r,filename="path_to_your_new_csv_file_DRAINAGE.tif", format="GTiff", overwrite=TRUE)
writeRaster(LINEAMENT_r,filename="path_to_your_new_csv_file_LINEAMENT.tif", format="GTiff", overwrite=TRUE)
writeRaster(SEEPAGE_r,filename="path_to_your_new_csv_file_SEEPAGE.tif", format="GTiff", overwrite=TRUE)
writeRaster(DEPTH_r,filename="path_to_your_new_csv_file_DEPTH.tif", format="GTiff", overwrite=TRUE)
writeRaster(TWI_r,filename="path_to_your_new_csv_file_TWI.tif",format="GTiff", overwrite=TRUE)
writeRaster(LANDCOVER_r,filename="path_to_your_new_csv_file_LANDCOVER.tif", format="GTiff", overwrite=TRUE)
writeRaster(SOIL_r,filename="path_to_your_new_csv_file_SOIL.tif", format="GTiff", overwrite=TRUE)
writeRaster(GEOLOGY_r,filename="path_to_your_new_csv_file_GEOLOGY.tif", format="GTiff", overwrite=TRUE)
writeRaster(LITHOLOGY_r,filename="path_to_your_new_csv_file_LITHOLOGY.tif", format="GTiff", overwrite=TRUE)

## stack multiple raster files

Stack_List = list.files(path = "02_Rasters/resampled/",pattern = "tif$", full.names = TRUE)
Rasters = stack(Stack_List)
names(Rasters)

#Convert raster to dataframe with Long-Lat
Rasters.df = as.data.frame(Rasters, xy = TRUE, na.rm = TRUE)
head(Rasters.df, 1)

# remove x, y
Rasters.df_N <- Rasters.df %>% dplyr::select(-c(x, y))

# Dealing with Categorical data (Converting numeric variable into groups in R)
#LANDCOVER
labels=c("LANDCOVER1","LANDCOVER2","LANDCOVER3","LANDCOVER5","LANDCOVER7","LANDCOVER8")
LANDCOVERras<-cut(Rasters.df_N$LANDCOVER, c(1, 2, 3, 5, 7, 8, 9), right=FALSE, labels=labels)
table(LANDCOVERras)
LANDCOVERras <- factor(LANDCOVERras)
flagsras = data.frame(Reduce(cbind,lapply(levels(LANDCOVERras), function(x){(LANDCOVERras == x)*1})))
names(flagsras) = levels(LANDCOVERras)
Rasters.df_N = cbind(Rasters.df_N, flagsras) # combine the LANDCOVER with original data

# SOIL
labels=c("SOIL1","SOIL2", "SOIL3")
SOILras<-cut(Rasters.df_N$SOIL, c(1, 2, 3, 4), right=FALSE, labels=labels)
table(SOILras)
SOILras <- factor(SOILras)
flagsras = data.frame(Reduce(cbind, lapply(levels(SOILras), function(x){(SOILras == x)*1})))
names(flagsras) = levels(SOILras)
Rasters.df_N = cbind(Rasters.df_N, flagsras)

# GEOLOGY
labels=c("GEOLOGY1","GEOLOGY2","GEOLOGY3","GEOLOGY4","GEOLOGY5","GEOLOGY6","GEOLOGY7", "GEOLOGY8")
GEOLOGYras<-cut(Rasters.df_N$GEOLOGY, c (1, 2, 3, 4, 5, 6, 7,8, 9), right=FALSE, labels=labels)
table(GEOLOGYras)
GEOLOGYras <- factor(GEOLOGYras)
flagsras = data.frame(Reduce(cbind,lapply(levels(GEOLOGYras), function(x){(GEOLOGYras == x)*1})))
names(flagsras) = levels(GEOLOGYras)
Rasters.df_N = cbind(Rasters.df_N, flagsras)

# LITHOLOGY
labels=c("LITHOLOGY1","LITHOLOGY2","LITHOLOGY3","LITHOLOGY4","LITHOLOGY5" )
LITHOLOGYras<-cut(Rasters.df_N$LITHOLOGY, c (1, 2, 3, 4, 5, 6), right=FALSE, labels=labels)
table(LITHOLOGYras)
LITHOLOGYras <- factor(LITHOLOGYras)
flagsras = data.frame(Reduce(cbind,lapply(levels(LITHOLOGYras), function(x){(LITHOLOGYras == x)*1})))
names(flagsras) = levels(LITHOLOGYras)
Rasters.df_N = cbind(Rasters.df_N, flagsras)

# Remove original LULC, SOIL, GEOLOGY, LITHOLOGY
Rasters.df_N <- Rasters.df_N %>% dplyr::select(-c(LANDCOVER, SOIL, GEOLOGY, LITHOLOGY))

# Scale vars (except the categorical data levels, to prevent gettig NaN)!
maxss <- apply(Rasters.df_N[,1:5], 2, max)
minss <- apply(Rasters.df_N[,1:5], 2, min)
Rasters.df_N_scaled <- as.data.frame(scale(Rasters.df_N[,1:5], center = minss, scale = maxss - minss))


# Now let us add back the (x,y) and the categorical data levels
Rasters.df_N_scaled <- data.frame(cbind(Rasters.df[, c(10,11)], Rasters.df_N_scaled, Rasters.df_N[,c(6:27)]))

# Extract x and y coordinates from Rasters.df (assuming columns 1 and 2 contain x and y)
xy_coords <- Rasters.df[, c(1, 2)]  # This should only include x and y

# Remove x and y columns from the scaled data to prevent duplication
Rasters.df_N_scaled_clean <- Rasters.df_N_scaled  # Assuming Rasters.df_N_scaled already contains scaled data

# Combine the x, y coordinates with the scaled data and categorical data from Rasters.df_N
Rasters.df_N_scaled_combined <- cbind(xy_coords, Rasters.df_N_scaled_clean, Rasters.df_N[, c(6:27)])

# View the result
head(Rasters.df_N_scaled_combined)

# 6- Run the compute (prediction function) 
#Final check if the nn.ce covariate match with Rasters.df_N_scaled
head(nn.ce$covariate, 1)
head(Rasters.df_N_scaled[,c(-1,-2)],1)

# Run the compute(prediction function) to get the predicted weights of ANN model
compute.SM = compute(nn.ce, Rasters.df_N_scaled[,c(-1,-2)]) # to let the variables matched 
out_SM <- data.frame(cbind(Rasters.df_N_scaled, compute.SM$net.result))   # str(out_SM)
colnames(out_SM)[30] <- "ANN_P"

# Remove other variables and keep x,y and ANN to be ploted
out_SM <- out_SM[,c(1,2,30)]

# Convert Dataframe back to rasters with Long-Lat
SM_from_df <- rasterFromXYZ(out_SM)  #Convert first two columns as lon-lat and third as value

# Plot
plot(SM_from_df)

# Add coord. ref. system by using the original data info (Copy n Paste).
projection(SEEPAGE) # borrow the projection from Raster data
proj4string(SM_from_df)=CRS("+proj=utm +zone=30 +datum=WGS84 +units=m +no_defs") # set it to lat-long


# write to a new geotiff file
writeRaster(SM_from_df,filename="05_Results/Prediction_Map2.tif", format="GTiff", overwrite=TRUE)

# Define the number of folds
k <- 5
folds <- createFolds(df$Training, k = k)

# To store the results of each fold
results <- list()

for (i in seq_along(folds)) {
  # Split the data
  fold_train <- df[-folds[[i]], ]
  fold_test <- df[folds[[i]], ]
  
  # Scale the training data
  maxs <- apply(fold_train[, 1:6], 2, max)
  mins <- apply(fold_train[, 1:6], 2, min)
  scaled_train <- as.data.frame(scale(fold_train[, 1:6], center = mins, scale = maxs - mins))
  scaled_train <- cbind(scaled_train, fold_train[, 7:ncol(fold_train)])
  
  # Scale the testing data
  scaled_test <- as.data.frame(scale(fold_test[, 1:6], center = mins, scale = maxs - mins))
  scaled_test <- cbind(scaled_test, fold_test[, 7:ncol(fold_test)])
  
  # Train the model
  set.seed(123)
  nn <- neuralnet(
    Training ~ DRAINAGE + LINEAMENT + SEEPAGE + DEPTH + TWI +
      LANDCOVER1 + LANDCOVER2 + LANDCOVER3 + LANDCOVER5 + LANDCOVER7 + LANDCOVER8 +
      SOIL1 + SOIL2 + SOIL3 +
      GEOLOGY1 + GEOLOGY2 + GEOLOGY3 + GEOLOGY4 + GEOLOGY5 + GEOLOGY6 + GEOLOGY7 + GEOLOGY8 +
      LITHOLOGY1 + LITHOLOGY2 + LITHOLOGY3 + LITHOLOGY4 + LITHOLOGY5,
    data = scaled_train,
    hidden = c(4, 2),  # Simplified architecture
    err.fct = "ce",
    linear.output = FALSE,
    stepmax = 1e+05
  )
  
  # Test the model
  compute_test <- compute(nn, scaled_test[, -1])
  predictions <- compute_test$net.result
  predicted_class <- ifelse(predictions > 0.5, 1, 0)
  
  # Calculate accuracy
  accuracy <- mean(predicted_class == scaled_test$Training)
  
  # Store the result
  results[[i]] <- list(model = nn, accuracy = accuracy)
}

# Evaluate overall performance
accuracies <- sapply(results, function(res) res$accuracy)
mean_accuracy <- mean(accuracies)
print(paste("Mean Accuracy:", mean_accuracy))

# Plot generalized weights for a specific variable
gwplot(nn, selected.covariate = "DRAINAGE")

# Loop over all variables to calculate their importance
importance <- sapply(names(nn$covariate), function(var) {
  weights <- gwplot(nn, selected.covariate = var, plot = FALSE)
  mean(abs(weights))
})

# Sort and display importance
importance <- sort(importance, decreasing = TRUE)
print(importance)


