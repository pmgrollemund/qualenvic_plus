################################################################################
################ Run a ANOVA procedure
######### Paul-Marie Grollemund and Mathieu Lepoivre
######### 2022-02-10
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
output_dir <- file.path("results",paste("ANOVA_run",timestamp,sep="_"))

if(!dir.exists("results")) 
  dir.create("results")

dir.create(output_dir)

#### Start ----
cat(separator,"\n",section,section," ANOVA analysis \n",
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
suppressPackageStartupMessages(library(rstatix))
suppressPackageStartupMessages(library(ggpubr))

#### Work with the dictionary ----
cat(section,"Pretreat.\n")
cat(sub_section,"dictionary.\n")

# Import the dictionary ----
cat(task,"Import the dictionary.\n")
dictionary <- read_xlsx(file.path(data_path,dictionary_file_name))

# Keep only required columns from  dictionary ----
cat(task,"Clean up dictionary.\n")
id_name <- dictionary$CAP2ER_name[which(dictionary$Type == "id")][1]
var_dictionary <- c("CAP2ER_name","Anova","Type","full_name","quick_name")
dictionary <- dictionary[,var_dictionary]
colnames(dictionary)[1:2] <- c("name","role")

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

#### Get shell options ----
cat(task,"Get shell options.\n")
args <- commandArgs(trailingOnly=TRUE)
var_group <- args[1]
# For instance : Rscript ANOVA.R GEN_AB
var_group <- "GEN_AB"

if(is.numeric(data[,var_group]))
  stop("\n \t The given variable must be categorial.\n",sep="")

if( !(var_group %in% colnames(data)) ){
  stop("\n \t Give an argument to the Rscript command, in particular the quick name",
       " of the variable that defines farm groups. This variable must be included",
       " in the database.\n",sep="")
}

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

# Add the farm groups ----
cat(task,"Add the farm groups.\n")
ANOVA_group <- data[[var_group]]

# Select ----
index <- which(dictionary$role == "yes" & dictionary$Type %in% c("numeric","frequency"))
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
p <- ncol(data)

# Add the farm groups ----
cat(task,"Add the farm groups.\n")
data$ANOVA_group <- ANOVA_group
data$ANOVA_group <- as.factor(data$ANOVA_group)
J <- nlevels(data$ANOVA_group)

if( J == nrow(data) ){
  stop("\n \t Please indicate the categorial variable to focus on. To do this,",
       " use the following command : \n \t \t Rscript ANOVA.R var_name",sep="")
}

# Save ----
cat(sub_section,"Save data file that it is given to ANOVA function.\n")
write.csv(data,file = file.path(output_dir,"data_to_ANOVA.csv"))

#### Anova ----
cat(section,"Anova.\n")


# Create output sub folders  ----
cat(task,"Create output sub folders.\n")
img_folder <- file.path(output_dir,"img")
invisible(suppressWarnings(dir.create(img_folder)))

# Fitting anova model ----
cat(task,"Fitting anova model.\n")
n_pairs <- choose(J,2)
tabluar <- matrix("NA",nrow=2+p,ncol=J+3+2*n_pairs)
tabluar[1,1:J] <- levels(data$ANOVA_group)
tabluar[1,J+1] <- "p-value"
tabluar[1,J+2] <- "SEM"
tabluar[1,J+3] <- "Significance"
tabluar[2,] <- "------"

pairs_levels <- t(combn(levels(data$ANOVA_group),2))
nom_pairs <- apply(pairs_levels,1,function(v) paste(v,collapse="/"))
tabluar[1,-(1:(J+3))] <- rep(nom_pairs,each=2)

rownames(tabluar) <- c(var_group,"-------",colnames(data)[1:p])
for(j in 1:p){
  res_anova <- aov(as.matrix(data[,j])~data$ANOVA_group)
  
  group_mean <- res_anova$coefficients
  group_mean[-1] <- group_mean[-1] + group_mean[1]
  
  tabluar[j+2,1:J] <- round(group_mean,4)
  tabluar[j+2,J+1] <- summary(res_anova)[[1]]$`Pr(>F)`[1]
  tabluar[j+2,J+2] <- sqrt(summary(res_anova)[[1]]$`Mean Sq`[2])
  tabluar[j+2,J+3] <- "***"
  if(tabluar[j+2,J+1] > 0.001)
    tabluar[j+2,J+3] <- "**"
  if(tabluar[j+2,J+1] > 0.01)
    tabluar[j+2,J+3] <- "*"
  if(tabluar[j+2,J+1] > 0.05)
    tabluar[j+2,J+3] <- "NS"
  
  res_tukey_hsd <- tukey_hsd(res_anova)
  tmp <- as.vector(t(as.matrix(res_tukey_hsd[,c(8,9)])))
  tabluar[j+2,-(1:(J+3))] <- tmp
} 

tabluar <- cbind(" | ",tabluar)
tabluar[2,1] <- "---"

# Save ----
cat(task,"Save.\n")
write.table(tabluar,file = file.path(output_dir,"table_result.csv"),col.names=FALSE)

#### Get graphical outputs ----
cat(sub_section,"Get graphical outputs.\n")

# Marginal boxplot ----
invisible(set_palette(J,"jco"))
colnames(data)[p+1] <- var_group
for(j in 1:p){
  p <- ggboxplot(data,x=var_group,y= colnames(data)[j],
                 color = var_group, 
                 add = "jitter", shape = var_group) +
    stat_compare_means() + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) 
  suppressWarnings(suppressMessages(
    ggsave(file.path(img_folder,paste("Boxplot_",colnames(data)[j],".pdf",sep="")),
           width = 20, height = 20,units="cm")))
}

# Joint boxplot ----
p <-  ncol(data)-1
df_boxplot <- data.frame(
  var_name = rep(colnames(data)[-(p+1)],each=nrow(data)),
  value = as.vector(unlist(data[,1:p])),
  var_group = rep(data[,p+1],times=p)
)
colnames(df_boxplot)[3]<- var_group 

p <- ggboxplot(
  df_boxplot, x = "var_name", y = "value", 
  color = var_group, 
) + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  xlab("") 
suppressWarnings(suppressMessages(
  ggsave(file.path(img_folder,paste("Boxplot_joint.pdf",sep="")),
         width = 20, height = 20,units="cm")))

#### Save ----
cat(section,"Save.\n")
save.image(file.path(output_dir,"R_env.RData"))

#### End of the analysis ----
cat(separator,"\n","End of the analysis.","\n",separator,"\n",sep="")

