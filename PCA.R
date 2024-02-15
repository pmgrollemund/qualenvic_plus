################################################################################
################ Run a PCA analysis
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
output_dir <- file.path("results",paste("PCA_run",timestamp,sep="_"))

if(!dir.exists("results")) 
  dir.create("results")

dir.create(output_dir)

#### Start ----
cat(separator,"\n",section,section," PCA analysis \n",
    sub_section," ",timestamp,"\n",separator,"\n",sep="")

#### Initialization ----
cat(section,"Initialisation.\n")
cat(task,"Load options.\n")

data_path <- "data"
data_file_name <- "database.xlsx"
dictionary_file_name <- "dictionary.xlsx"
ncp_max <- 50 

quanti_sup_folder <- "quanti_sup"

#### Required packages ----
cat(task,"Import packages.\n")
suppressPackageStartupMessages(library(FactoMineR))
suppressPackageStartupMessages(library(factoextra))
suppressPackageStartupMessages(library(nFactors))
suppressPackageStartupMessages(library(readxl))
suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(missMDA))
suppressPackageStartupMessages(library(ggforce))


#### Required funcctions ----
cat(task,"Import functions.\n")
get_component_nb <- function(eig_value){
  res_nScree <- nScree(eig_value[,1]) 
  
  rankings <- sort(table(unlist(res_nScree$Components)))
  rankings_tmp <- rankings[as.numeric(names(rankings)) > 1]
  
  if(length(rankings_tmp) > 0){
    component_nb <- as.numeric(names(rankings_tmp)[which.max(rankings_tmp)])
  }else{
    component_nb <- 2
  }
  
  res <- list(component_nb=component_nb, rankings=rankings)
  return(res)
}
PCA_component_interpretation <- function(res_PCA,component_nb,dictionary){
  
  # Levels informations
  res_var <- data.frame(
    var = rownames(res_PCA$var$contrib),
    contrib = res_PCA$var$contrib[,component_nb],
    coord = res_PCA$var$coord[,component_nb]
  )
  rownames(res_var) <- NULL
  
  # Select most contributive levels
  contrib_threshold <- max(100 / nrow(res_var),quantile(res_var$contrib,0.9))
  res_var <- res_var[res_var$contrib >= contrib_threshold,]
  
  # Sort them
  res_var <- res_var[rev(order(res_var$contrib)),]
  
  # Set up the output
  res_var$var_name <- unlist(lapply(strsplit(as.character(res_var$var),split="_|[.]"),function(v) v[1]))
  res_var$var_complete_name <- rep(NA,nrow(res_var))
  if(nrow(res_var) > 0){
    for(i in 1:nrow(res_var)){
      res_var$var_complete_name[i] <- 
        dictionary$full_name[dictionary$quick_name == res_var$var_name[i]]
    } 
  }
  
  res_var$contrib <- round(res_var$contrib,6)
  res_var$coord <- round(res_var$coord,6)
  return(res_var)
}
uncommon_level_detection <- function(data,threshold){
  
  lvl_count <- NULL
  for(j in 1:ncol(data)){
    table_tmp <- table(data[,j])
    names(table_tmp) <- paste(colnames(data)[j],"$",names(table_tmp),sep="")
    
    lvl_count <- c(lvl_count,table_tmp)
  }
  
  index <- which(lvl_count < threshold)
  
  if(length(index) > 0){
    uncommon_lvl <- names(lvl_count)[index]
  }else{
    uncommon_lvl <- NULL
  }
  
  
  return(uncommon_lvl)
}
compute_circle <- function(center = c(0,0),radius = 1, npoints = 1e3){
  t <- seq(0,2*pi,length.out = npoints)
  x <- center[1] + radius * cos(t)
  y <- center[2] + radius * sin(t)
  return(data.frame(x = x, y = y))
}

#### Work with the dictionary ----
cat(section,"Pretreat.\n")
cat(sub_section,"dictionary.\n")

# Import the dictionary ----
cat(task,"Import the dictionary.\n")
dictionary <- read_xlsx(file.path(data_path,dictionary_file_name))

# Keep only required columns from  dictionary ----
cat(task,"Clean up dictionary.\n")
id_name <- dictionary$CAP2ER_name[which(dictionary$Type == "id")][1]
var_dictionary <- c("CAP2ER_name","Role","Type","full_name","quick_name")
dictionary <- dictionary[,var_dictionary]
colnames(dictionary)[1:2] <- c("name","role")

# dictionary pretreament ----
cat(task,"dictionary pretreament.\n")
dictionary <- dictionary[which(!is.na(dictionary$name)),]

# Remove NA from quick names ----
cat(task,"Remove NA quick names.\n")
dictionary_NA <- dictionary[is.na(dictionary$quick_name),]
dictionary <- dictionary[!is.na(dictionary$quick_name),]

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
index <- which(dictionary$role %in% c("to_explain","explicative")
               & !(dictionary$Type %in% c("id","date")))
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

# Check data dispersion ----
cat(task,"Check data dispersion.\n")
# Detect identical data
get_variety <- apply(data,2,function(v){
  v_tmp <- unique(v)
  if(sum(is.na(v_tmp))>0){
    length(unique(v))-1
  }else{
    length(unique(v))
  }
})
identical <- which(get_variety < 2)

if(length(identical) > 0){
  identical_data <- data[,identical]
  write.csv(identical_data,file.path(output_dir,"identical_data.csv"))
  
  removed_var <- rbind(removed_var,
                        data.frame(
                          name = colnames(data)[identical],
                          reason = "all values are identical"
                        )
  )
}


aka_id <- which(get_variety == nrow(data))

aka_id <- as.data.frame(cbind(names(aka_id),aka_id))
colnames(aka_id) <- c(colnames(dictionary)[1],"index")
aka_id <- merge(aka_id,dictionary,by=colnames(dictionary)[1])

aka_id_categorial <- which(aka_id$Type == "categorial")
if(length(aka_id_categorial) > 0 ){
  data_aka_id <- data[,aka_id]
  write.csv(data_aka_id,file.path(output_dir,"data_aka_id.csv"))
  
  removed_var <- rbind(removed_var,
                        data.frame(
                          name = colnames(data)[aka_id_categorial],
                          reason = "values are all unique"
                        )
  )
}


# Select don dictionary ----
dictionary <- dictionary[which(dictionary$quick_name %in% colnames(data)),]

# Removed data columns not specified in dictionary ----
cat(task,"Removed data columns not specified in dictionary.\n")
not_in_dictionary <- which(!(colnames(data) %in% dictionary$quick_name))
if(length(not_in_dictionary) > 0){
  removed_var <- rbind(removed_var,
                        data.frame(
                          name = colnames(data)[not_in_dictionary],
                          reason = "not in dictionary"
                        )
  )
}else{
  removed_var <- NULL
}

# Do remove
to_remove <- c(identical,aka_id_categorial,not_in_dictionary)
if(length(to_remove) > 0)
  data <- data[,-to_remove]

# Get nature and role of each column ----
cat(task,"Get nature and role of each column.\n")
names_categorial <- dictionary$quick_name[dictionary$Type == "categorial"]
for(i in 1:length(names_categorial)){
  index <- which(colnames(data) == names_categorial[i])
  if(length(index) > 0)
    data[,index] <- as.factor(data[,index])
}

names_numeric <- unique(
  dictionary$quick_name[dictionary$role == "explicative" &
                                (dictionary$Type == "numeric" | dictionary$Type == "frequency")]
)
for(i in 1:length(names_numeric)){
  index <- which(colnames(data) == names_numeric[i])
  data[,index] <- as.numeric(data[,index])
}

quanti_covariable_name <- unique(
  dictionary$quick_name[dictionary$role == "explicative" &
                                (dictionary$Type == "numeric" | dictionary$Type == "frequency")]
)

#### ACP ----
cat(section,"Analysis.\n")
cat(sub_section,"First steps.\n")

# Get supplementaries ----
cat(task,"Get supplementaries.\n")
ind_sup <- NULL
quanti_sup <- NULL
quali_sup <- NULL

names_numeric <- 
  dictionary$quick_name[dictionary$role == "to_explain" &
                                (dictionary$Type == "numeric" | dictionary$Type == "frequency")]
if(length(names_numeric) > 0) 
  quanti_sup <- which(colnames(data) %in% names_numeric)

names_categorial  <- 
  dictionary$quick_name[dictionary$role == "to_explain" & 
                                dictionary$Type == "categorial"]
if(length(names_categorial) > 0) 
  quali_sup <- which(colnames(data) %in% names_categorial)

# Remove NA ----
cat(task,"Remove ind ith too much NA.\n")
nb_NA_ind <- apply(data,1,function(v) sum(is.na(v)))
ind_sup <- as.numeric(which(nb_NA_ind > ncol(data)/2))
if(length(ind_sup) > 0)
  data <- data[-ind_sup,]

# Save ----
cat(sub_section,"Save data file that it is given to PCA function.\n")
write.csv(data,file = file.path(output_dir,"data_to_PCA.csv"))

# Check categorial data ----
var_categorial <- NULL
for(j in 1:ncol(data)){
  if( class(data[,j]) %in% c("character","factor") && 
      dictionary[dictionary$quick_name == colnames(data)[j],"role"] == "explicative")
    var_categorial <- c(var_categorial,colnames(data)[j])
}
if(length(var_categorial) > 0)
  stop(paste("\n No way to use PCA with non-numerical columns..\n",
             " Please remove the following columns : \n  ",
             paste(var_categorial,collapse=" "),"\n",sep="")
  )

# Impute missing data ----
cat(task,"Impute missing data (can be time consuming).\n")
if(length(c(quanti_sup,quali_sup)) > 0){
  data_tmp <- data[,-c(quanti_sup,quali_sup)]
}else{
  data_tmp <- data
}
res.impute <- imputePCA(data_tmp, 
                        ncp=min(5,ncol(data_tmp)-1))
res.impute <- cbind(
  res.impute,
  data[,c(quanti_sup,quali_sup)]
)
res.impute <- res.impute[,colnames(data)]


# Save ----
cat(sub_section,"Save the imputed data.\n")
write.csv(res.impute,file = file.path(output_dir,"data_to_PCA.csv"))

# PCA ----
cat(sub_section,"Analysis.\n")
cat(task,"PCA.\n")
res_PCA <- PCA(res.impute,ind.sup = NULL,
               quanti.sup = quanti_sup ,
               quali.sup = quali_sup,
               graph = F,ncp = ncp_max)

# Get number of principal component ----
cat(task,"Get number of principal component.\n")
eig_value <- get_eigenvalue(res_PCA)
res_component_nb <- get_component_nb(eig_value)
component_nb <- res_component_nb$component_nb
total_component_nb <- nrow(eig_value)

#### Get numerical outputs ----
cat(sub_section,"Get numerical outputs.\n")

# Create output sub folders  ----
cat(task,"Create output sub folders.\n")
img_folder <- file.path(output_dir,"img")
invisible(suppressWarnings(dir.create(img_folder)))
component_folder <- file.path(output_dir,"component_interpretation")
invisible(suppressWarnings(dir.create(component_folder)))

if(total_component_nb > component_nb){
  additionnal_component_folder <- file.path(output_dir,"additional_components")
  invisible(suppressWarnings(dir.create(additionnal_component_folder)))
  additional_img_folder <- file.path(additionnal_component_folder,"img")
  invisible(suppressWarnings(dir.create(additional_img_folder)))
  additionnal_component_folder <- file.path(additionnal_component_folder,"component_interpretation")
  invisible(suppressWarnings(dir.create(additionnal_component_folder))) 
}

# Eigen values ----
cat(task,"Eigen values.\n")
pdf(file.path(img_folder,"eig_value.pdf"))
fviz_screeplot(res_PCA,ncp=nrow(eig_value))
invisible(dev.off())

# Component interpretation ----
cat(task,"Numerical support for component interpretation.\n")
for(j in 1:component_nb){
  interpretation <- PCA_component_interpretation(res_PCA,j,dictionary)
  write.csv(interpretation,file = file.path(component_folder,
                                            paste("component_",j,".csv",sep="")))
}
if(total_component_nb > component_nb){
  for(j in (component_nb+1):total_component_nb){
    interpretation <- PCA_component_interpretation(res_PCA,j,dictionary)
    write.csv(interpretation,file = file.path(additionnal_component_folder,
                                              paste("component_",j,".csv",sep="")))
  }
}

# Coordinates on components ----
cat(task,"Coordinates on components.\n")
coord_var <- res_PCA$var$coord[,1:component_nb]
coord_ind <- res_PCA$ind$coord[,1:component_nb]
coord <- rbind(coord_var,coord_ind)
colnames(coord) <- paste0("component_",1:component_nb,sep="")

write.csv(coord,file = file.path(component_folder,"component_coordinates.csv"))

#### Get graphical outputs ----
cat(sub_section,"Get graphical outputs.\n")

# Plots about variables ----
cat(sub_section,"Plots about variables.\n")
if(component_nb == 1)
  component_nb <- 2
if(component_nb > ncp_max)
  component_nb <- ncp_max

data_name <- colnames(data)
for(j in 1:length(data_name))
  data_name[j] <- dictionary$name[dictionary$quick_name == data_name[j]]
group <- unique(unlist(lapply(strsplit(data_name,split="_"),function(v) v[1])))
group_var <- data.frame(
  var = colnames(data),
  group = unlist(lapply(strsplit(data_name,split="_"),function(v) v[1]))
)

nmoda <- dim(res_PCA$var$coord)[1]
nvar <- dim(data)[2] 
cols <- c("red",NA)

for(j in 1:(component_nb-1)){
  for(k in (j+1):component_nb){
    p <- suppressMessages(fviz_pca_var(res_PCA,axes = c(j,k), repel=T,
                                       col.quanti.sup="black")) 
    
    suppressWarnings(suppressMessages(ggsave(file.path(img_folder,paste("vars_components_",j,"_",k,".pdf",sep="")),
                                             width = 20, height = 20,units="cm")))
    rm(p)
  }
}

if(total_component_nb > component_nb){
  
  for(j in 1:(total_component_nb-1)){
    for(k in (component_nb+1):total_component_nb){
      p <- suppressMessages(fviz_pca_var(res_PCA,axes = c(j,k), repel=T,
                                         col.quanti.sup = "black"))
      
      suppressWarnings(suppressMessages(ggsave(file.path(additional_img_folder,paste("vars_components_",j,"_",k,".pdf",sep="")),
                                               width = 20, height = 20,units="cm")))
      rm(p)
    }
  }
}


# Plots about individuals ----
cat(sub_section,"Plots about individuals.\n")
if(component_nb == 1)
  component_nb <- 2

for(j in 1:(component_nb-1)){
  for(k in (j+1):component_nb){
    p <- fviz_pca_ind(res_PCA,axes = c(j,k),
                      col.quali.var = NA) 
    suppressWarnings(suppressMessages(ggsave(file.path(img_folder,paste("inds_components_",j,"_",k,".pdf",sep="")),
                                             width = 20,height = 20,units="cm")))
    rm(p)
  }
}

if(total_component_nb > component_nb){
  
  for(j in 1:(total_component_nb-1)){
    for(k in (component_nb+1):total_component_nb){
      p <- fviz_pca_ind(res_PCA,axes = c(j,k),
                        col.quali.var = NA) 
      suppressWarnings(suppressMessages(ggsave(file.path(additional_img_folder,paste("inds_components_",j,"_",k,".pdf",sep="")),
                                               width = 20,height = 20,units="cm")))
      rm(p)
    }
  }
}

# Supplementary elements ----
cat(task,"Supplementary elements.\n")
if(length(quanti_sup) > 0){
  for(j in 1:(component_nb-1)){
    for(k in (j+1):component_nb){
      cols_supp_tmp <- as.numeric(rownames(res_PCA$quanti.var$coord) %in% quanti_covariable_name )
      cols_supp <- rep(NA,length(cols_supp_tmp))
      cols_supp[cols_supp_tmp == 1] <- "black"
      
      circle_data <- compute_circle()
      p <- fviz_pca_var(res_PCA,axes = c(j,k),repel=T,col.quanti.sup = "black",
                        col.var = NA,geom = c("arrow", "text"),
                        choice = "var")  +
        geom_path(data=circle_data,aes(x=x,y=y))
      
      suppressWarnings(suppressMessages(ggsave(file.path(img_folder,paste("vars_components_",j,"_",k,".pdf",sep="")),
                                               width = 20, height = 20,units="cm")))
      rm(p)
    }
  }
  
  
  
  if(total_component_nb > component_nb){
    
    for(j in 1:(total_component_nb-1)){
      for(k in (component_nb+1):total_component_nb){
        cols_supp_tmp <- as.numeric(rownames(res_PCA$quanti.var$coord) %in% quanti_covariable_name )
        cols_supp <- rep(NA,length(cols_supp_tmp))
        cols_supp[cols_supp_tmp == 1] <- "black"
        
        circle_data <- compute_circle()
        p <- fviz_pca_var(res_PCA,axes = c(j,k),repel=T,col.quanti.sup = "black",
                          col.var = NA,geom = c("arrow", "text"),
                          choice = "var")  +
          geom_path(data=circle_data,aes(x=x,y=y))
        
        suppressWarnings(suppressMessages(ggsave(file.path(additional_img_folder,paste("vars_components_",j,"_",k,".pdf",sep="")),
                                                 width = 20, height = 20,units="cm")))
        rm(p)
      }
    }
  }
  
}



#### Save ----
cat(section,"Save.\n")
save.image(file.path(output_dir,"R_env.RData"))

#### End of the analysis ----
cat(separator,"\n","End of the analysis.","\n",separator,"\n",sep="")
