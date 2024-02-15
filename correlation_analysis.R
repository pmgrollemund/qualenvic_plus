################################################################################
################ Run a Correlation analysis
######### Paul-Marie Grollemund and Mathieu Lepoivre
######### 2021-11-15 
################################################################################
#### Clean up ----
rm(list=ls())

#### Define the appropriate working directory ----
setwd(".")

#### Option  ----
separator <- "######################################"
section <- "####"
sub_section <- "##"
task <- "-"

#### Create the output dir ----
timestamp <- format(Sys.time(), "%Y-%m-%d_%H-%M-%S")
output_dir <- file.path("results",paste("Correlation_run",timestamp,sep="_"))

if(!dir.exists("results")) 
  dir.create("results")

dir.create(output_dir)


#### Start ----
cat(separator,"\n",section,section," Correlation analysis \n",
    sub_section," ",timestamp,"\n",separator,"\n",sep="")

#### Initialization ----
cat(section,"Initialisation.\n")
cat(task,"Load options.\n")

data_path <- "data"
data_file_name <- "database.xlsx"
dictionary_file_name <- "dictionary.xlsx"

#### Required packages ----
cat(task,"Import packages.\n")
suppressPackageStartupMessages(library(readxl))
suppressPackageStartupMessages(library(GGally))


#### Work with the dictionary ----
cat(section,"Pretreat.\n")
cat(sub_section,"dictionary.\n")

# Import the dictionary ----
cat(task,"Import the dictionary.\n")
dictionary <- read_xlsx(file.path(data_path,dictionary_file_name))


# Keep only required columns from  dictionary ----
cat(task,"Clean up dictionary.\n")
id_name <- dictionary$CAP2ER_name[which(dictionary$Type == "id")][1]
var_dictionary <- c("CAP2ER_name","Correlation","Type","full_name","quick_name")
dictionary <- dictionary[,var_dictionary]
colnames(dictionary)[1] <- c("name")

# dictionary pretreament ----
cat(task,"dictionary pretreament.\n")
dictionary <- dictionary[which(!is.na(dictionary$name)),]

# Remove non-unique id ----
cat(task,"Remove non-unique id.\n")
for(i in 1:nrow(dictionary)){
  non_unique_index <- which(dictionary$quick_name == dictionary$quick_name[i])
  non_unique_index <- non_unique_index[non_unique_index != i]
  if(length(non_unique_index) > 0){
    dictionary <- dictionary[-non_unique_index,]
  }
}

#### Pretreat Data ----
cat(sub_section,"Data.\n")

# Data importation ----
cat(task,"Data importation.\n")
data <- read_xlsx(file.path(data_path,data_file_name),col_names = TRUE,skip=1)
data <- as.data.frame(data)

# Remove non-necessary columns ----
cat(task,"Remove non-necessary columns.\n")
if(sum(!(dictionary$name %in% colnames(data))) > 0){
  removed_var <- data.frame(
    name = colnames(data)[which(!(dictionary$name %in% colnames(data)))],
    reason = "no data"
  )
}else{
  removed_var <- NULL
}

# Id as row names ----
cat(task,"Id as row names : variable",id_name[1],"\n")
rownames(data) <- as.character(data[,id_name[1]])

# Check importation----
cat(task,"Check importation.\n")
import_pb_index <- grep(x = colnames(data),pattern="[.]+[0-9]+")
if(length(import_pb_index) > 0){
  var_pb_importation <- colnames(data)[import_pb_index]
  var_pb_importation <- gsub(x = var_pb_importation,pattern="[.]+[0-9]+",replacement="")
  
  # Save ----
  information_pb_importation <- table(var_pb_importation)
  write.csv(information_pb_importation,file.path(output_dir,"information_pb_importation.csv"),
            row.names = F)
  
  # Remove wrong variables ----
  data <- data[,-import_pb_index]
  var_pb_importation_dictionary <- which(dictionary$name %in% var_pb_importation)
  
  # Update ----
  if(!is.null(removed_var)){
    removed_var <- rbind(removed_var,
                         data.frame(
                           name = var_pb_importation_dictionary,
                           reason = "importation_problem"
                         ))
  }
}

# Select ----
index <- which(dictionary$Correlation == "yes" & dictionary$Type %in% c("numeric","frequency"))
col_to_keep <- which(colnames(data) %in% dictionary$name[index])
if(length(col_to_keep) >0){
  removed_var <- rbind(removed_var,
                       data.frame(
                         name = colnames(data)[-col_to_keep],
                         reason = "removed"
                       ))
  data <- data[,col_to_keep]
}

# Var names ----
cat(task,"Use the quick names.\n")
for(j in 1:ncol(data)){
  colnames(data)[j] <- dictionary$quick_name[dictionary$name == colnames(data)[j]]
}


# Save ----
cat(sub_section,"Save data file that it is given to the correlation analysis.\n")
write.csv(data,file = file.path(output_dir,"data_to_correlation.csv"))

#### Correlation Analysis ----
cat(section,"Correlation Analysis.\n")

# Create output sub folders  ----
cat(task,"Create output sub folders.\n")
img_folder <- file.path(output_dir,"img")
invisible(suppressWarnings(dir.create(img_folder)))
negative_correlation_folder <- file.path(img_folder,"negative_correlation")
invisible(suppressWarnings(dir.create(negative_correlation_folder)))
positive_correlation_folder <- file.path(img_folder,"positive_correlation")
invisible(suppressWarnings(dir.create(positive_correlation_folder)))
null_correlation_folder <- file.path(img_folder,"null_correlation")
invisible(suppressWarnings(dir.create(null_correlation_folder)))

# Compute correlations ----
cat(task,"Compute correlations.\n")
res_cor <- cor(data)

# Save ----
cat(task,"Save.\n")
write.csv(res_cor,file = file.path(output_dir,"correlation.csv"))

# Graphical results ----
cat(task,"Graphical results.\n")
p <- ggpairs(data)
ggsave(plot = p,filename = file.path(img_folder,"correlations_pairs.pdf"),
       height = ncol(data), width =  ncol(data))

# Linear fitting ----
cat(task,"Linear fitting.\n")
n_var <- ncol(data)
for(j in 1:(n_var-1)){
  for(k in (j+1):n_var){
    cor <- cor(data[j],data[k])
    if(cor < -0.3){
      folder <- negative_correlation_folder
    }
    if(cor >= -0.3 & cor <= 0.3){
      folder <- null_correlation_folder
    }
    if(cor > 0.3){
      folder <- positive_correlation_folder
    }
    
    x <- unlist(data[j])
    y <- unlist(data[k])
    
    res_lm <- lm(y~x)
    tmp <- summary(res_lm)
    R2 <- round(tmp$r.squared,4)
    pvalue <- round(tmp$coefficients[2,4],4)
    
    p <- 
      ggplot(data,aes(x=data[,j],y=data[,k])) +
      geom_point() +
      geom_smooth(method = lm, se = FALSE) +
      xlab(colnames(data)[j]) + ylab(colnames(data)[k]) + 
      ggtitle(paste("Correlation :",round(cor,4), "\t p-value :",pvalue," \t R^2 :",R2))
    graph_name <- paste("plot_",colnames(data)[j],"_",colnames(data)[k],".pdf",sep="")
    
    suppressMessages(ggsave(plot = p,filename = file.path(folder,graph_name),
                            height = 4,width = 7))
  }
}

#### Save ----
cat(section,"Save.\n")
save.image(file.path(output_dir,"R_env.RData"))

#### End of the analysis ----
cat(separator,"\n","End of the analysis.","\n",separator,"\n",sep="")

