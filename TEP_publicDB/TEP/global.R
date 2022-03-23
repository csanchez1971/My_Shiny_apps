# File containing most of the functions that will be used at the app and for Rmarkdown downloadable file
#
# @param species Translate for mouse and rat the gene names. "human" by default
# @param input_gene Selected gene on initial screen. "Select Gene" by default
# @param geneDB Name of DB once adapted to E_gene(x) mode 
# @param grouped_tissue Input selection to display either individual or grouped tissues
# @param ytext, angle_x, vjust_x, dotsize, showLegend, input_tissue, scale Parameters to display plots differently depending of the desired output (screen, document)
# @param circle_size, axis_size Parameters to display Heatmap depending on desired output
# @param heatmapSum Reactive table with the summary of data necessary to plot the Heatmap
# @param input_tissue Tissue Systems selected to be displayed at SingleDB function
# @param input_ZScore_cutoff ZScore cutoff selected to calculate enriched tissues


# setwd("~/My_Shiny_apps/TEP_PublicDB/TEP/")
load("TEP_Shiny_reduced.RData")
# load("/dlab/NGS/bioinformatics/PCS/DIPGENOMICS/TEP_Shiny_data/version_0.9/TEP_Shiny.RData")

# source("http://ava-web.statwb.eu.novartis.net/cfg/installer.R")     # AVA installer
library(tidyverse)
# library(ava)
library(shiny)
library(DT)
library(data.table)
library(knitr)
library(pander)
library(xlsx)
library(shinyWidgets)
library(shinyjs)
library(shinycssloaders)
library(dplyr)
library(tidyr)
library(shinythemes)


TEP_Version <- "version 0.9"



DBs <- c("GTEx",       #Important to keep this order since we will look for the value according to index of E_gene(x)
         "HPA",
         "BarkBase",
         "NHPRTR_CMC",
         "NHPRTR_CMM",
         "NHPRTR_RMI",
         "BioGPS_Mouse",
         "BioGPS_Mouse2",
         "BioGPS_Human",
         "Ratlas",
         "Minipig_Atlas",
         "Mouse_MTAB6081",
         "Rat_MTAB6081",
         "DIS_Atlas_Monkey",
         "DIS_Atlas_Dog",
         "DIS_Atlas_Rat",
         "DIS_Atlas_Mouse",
         "NHPRTR_all_mac",
         "NHPRTR_JMI",
         "DIS_Atlas_Human")




DB_Used <- function(){
  
  GTEx <- c("GTEx", "v8", "Homo sapiens", "58'156", "52", "17'382", "976'868'400")
  HPA  <- c("HPA", "v 19.3", "Homo sapiens", "22'643", "56", "281", "6'362'683")
  BioGPS <- c("BioGPS", "Aug '18", "Homo sapiens", "14'590", "66", "176", "4'288'768")
  BarkBase <- c("BarkBase", "June '19", "Canis familiaris", "13'631", "27", "131", "1'804'263")
  NHPRTR_CMC <- c("NHPRTR CMC", "Sept '14", "Nonhuman primate", "26'204", "15", "16", "419'264")
  NHPRTR_CMM <- c("NHPRTR CMM", "Sept '14", "Nonhuman primate", "26'204", "14", "15", "393'060")
  NHPRTR_JMI <- c("NHPRTR JMI", "Sept '14", "Nonhuman primate", "26'204", "14", "14", "366'856")
  NHPRTR_RMI <- c("NHPRTR RMI", "Sept '14", "Nonhuman primate", "26'204", "14", "14", "366'856")
  BioGPS_GNF1M <- c("BioGPS (GNF1M)", "Aug '18", "Mus musculus", "10'755", "60", "153", "2'104'362")
  BioGPS_MOE403 <- c("BioGPS (MOE403)", "Feb '19", "Mus musculus", "14'528", "82", "189", "5'519'556")
  Mouse_MTAB6081 <- c("Mouse_MTAB6081", "-", "Mus musculus", "35'053", "13", "39", "1'367'067")
  Rat_MTAB6081 <- c("Rat_MTAB6081", "-", "Rattus norvegicus", "24'054", "13", "39", "938'106")

  DB_Used <-data.frame(rbind(GTEx, HPA, BioGPS, 
                             NHPRTR_CMC, NHPRTR_CMM, NHPRTR_JMI, NHPRTR_RMI, 
                             BarkBase, 
                             Rat_MTAB6081,
                             BioGPS_GNF1M, BioGPS_MOE403, Mouse_MTAB6081), 
                       
                       stringsAsFactors = FALSE)
  
  names(DB_Used) <- c("Database", "Version", "Species", "Genes", "Tissues", "Samples", "Registers")
  
  DB_Used
  
}



#Gene translation to homologs


gene_description_function <- function(species = "human", input_gene = "Select Gene"){   #Need to include default value for species and input_gene 
  
  if (input_gene == "Select Gene")
    return(NULL)
  
  gene_db <- org.Hs.eg.db::org.Hs.eg.db
  
  synonyms <- suppressMessages(AnnotationDbi::select(gene_db, 
                                                   keys = input_gene, columns = c("ALIAS"), 
                                                   keytype = "SYMBOL"))

  gene_description <- unique(c(input_gene, synonyms$ALIAS))   #Just in case that input gene not available in "org.Hs.eg.db" we add the geneID DB value selected  
  
  if (species %in% c("mouse", "rat")){
    gene <- human_all_organism[human_all_organism$HomoloGene.ID %in% unique(human_all_organism$HomoloGene.ID[human_all_organism$Symbol %in% gene_description]),]
    genIDs <- gene$Symbol[gene$Common.Organism.Name %in% species]
    
    genIDs <- if(length(genIDs)!=0){
      genIDs <- unique(c(genIDs, str_to_title(gene_description)))   ##### Adding to the list the same name than Human with only the first letter capitalised (seen than some genes don't appear on translation tables and the gene name is similar to human)
      
              }else{
                
                genIDs <- get(paste0('geneID_', species))$SYMBOL[get(paste0('geneID_', species))$HGNC.symbol %in% gene_description]
                genIDs <- c(str_to_title(gene_description))   
                
              } 
    
    return(genIDs)
    
  } else {
    
    return(gene_description)
    
  }
}






#Function to be used by the different plots to display either individual or grouped tissues 

E_gene <- function(geneDB, grouped_tissue){
  
  remove_column <- ifelse(grouped_tissue == "Tissue", "Master", "level1")  #Select individual tissue or grouped tissue (upper level. e.g. "brain")
  keep_column <- ifelse(grouped_tissue == "Tissue", "level1", "Master")
  geneDB <- geneDB[-which(colnames(geneDB) == remove_column)]
  colnames(geneDB)[which(colnames(geneDB) == keep_column)] <- "groupTissue"
  return(geneDB)      }



scale_Y <- function(input_scalePlot = "Linear"){
  if (input_scalePlot == "Linear"){     #Default scale for y-axis is linear and log10 can be selected
    "identity" 
    
  } else{ "log10" }
}





#BARPLOT AND DOTPLOT

graph <- function(geneDB, ytext = 15, angle_x = 45, vjust_x = 1, dotsize = 2, showLegend = "bottom", input_tissue = "All", scale = "identity") {  #ytext, angle_x vjust_x dotsize variables created to be adjusted on downloadable report
  
  validate(need(nrow(geneDB) > 0, message = 'No data available for this Gene'))

  validate(need(input_tissue, message = 'Please, select a Tissue System or "All"'))
  
  if ("All" %in% input_tissue) {
    geneDB <- geneDB
    
  } else {
    geneDB <- geneDB[geneDB$Tissue_type %in% input_tissue, ]
    
  }
  
  
  selectedData <- geneDB[, c("groupTissue", colnames(geneDB[1]))]                 
  
  
  myColors <- c("#F8766D",
                "#CC3232",
                "#E7861B",
                "#AFA100",
                "#72B000",
                "#00B81F",
                "#00C08D",
                "#00BFC4",
                "#00ABFD",
                "#7997FF",
                "#DC71FA",
                "#F763E0",
                "#FF65AE"
  )
  
  names(myColors) <- levels(Atlas_Dictionary_final$Tissue_type)
  colScale <- scale_colour_manual(name = "Tissue System", values = myColors)
  
  ##Function created to have only integer values on y-axis when values too small
  
  integer_breaks <- function(n = 5, ...) {
    
    fxn <- function(x) {
      breaks <- floor(pretty(x, n, ...))
      names(breaks) <- attr(breaks, "labels")
      breaks
    }
    
    return(fxn)
  }
  
  p1 <- ggplot(geneDB, aes(x = reorder(unlist(selectedData[1]),-unlist(geneDB[1])), fill = Tissue_type)) +
    geom_bar(stat = "count") +
    scale_fill_manual(values = myColors) +
    
    scale_y_continuous(breaks = integer_breaks()) +
    
    theme_bw() +
    facet_grid(~ Tissue_type, scale = "free", space = "free_x") +
    ylab("n") +
    theme(
      legend.position = "none",
      strip.background = element_blank(),
      strip.text.x = element_blank(),
      axis.text.x = element_blank(),
      axis.title.x = element_blank(),
      text = element_text(size = ytext)
    )
  
  
  
  p2 <- ggplot(data = geneDB, aes(x = reorder(unlist(selectedData[1]),-unlist(geneDB[1])), y = unlist(geneDB[1]), colour = Tissue_type)) +
    
    geom_point(size = dotsize, stroke = 0, shape = 16) +
    stat_summary(fun = "median", geom = "crossbar", width = 0.5, size =0.2, colour = "black") +
    
    colScale +
    scale_y_continuous(trans = scale) +
    labs(color = "") +
    theme_bw() +
    facet_grid(~ Tissue_type, scale = "free", space = "free_x") +
    ylab(colnames(geneDB[1])) +
    theme(
      legend.position = showLegend,
      strip.background = element_blank(),
      strip.text.x = element_blank(),
      axis.text.x = element_text(
        angle = angle_x,
        vjust = vjust_x,
        hjust = 1,
        size = (ytext / 1.5)
      ),
      axis.title.x = element_blank(),
      text = element_text(size = ytext)
    )
  
  
  egg::ggarrange(p1, p2, heights = c(1, 4))
  
  
}


#Heatmap

heatmap_plot <- function(circle_size, axis_size=14, heatmapSum){
  
  ggplot(data = heatmapSum, aes(x = Database, y = groupTissue, size = Expression)) +
    geom_point(aes(colour = cut(Enrichment, list(-Inf, 1, 2, 3, 999)))) +
    
    scale_color_manual(
      name = "Enrichment (ZScore)",
      values = list(
        "(-Inf,1]" = "skyblue1",
        "(1,2]" = "gold",
        "(2,3]" = "orangered2",
        "(3,999]" = "red4"
      ),
      
      labels = list("<1", "2", "3", ">3"),
      drop = FALSE) +
    scale_size_manual(
      values = c(0.7*circle_size, 1.5*circle_size, 3*circle_size, 5*circle_size),
      breaks = list("below cutoff", "low", "moderate", "high"),
      drop = FALSE) +
    theme_bw() +
    theme(
      legend.position = "bottom",
      strip.text.x = element_text(angle = 0, size = axis_size),
      strip.text.y = element_text(angle = 0, size = axis_size),
      
      axis.text.x = element_text(
        angle = 45,
        vjust = 1,
        hjust = 1,
        size = axis_size
      ),
      
      axis.text.y = element_text(size = axis_size)
      
    ) +
    
    facet_grid(Tissue_type ~ Species, scale = "free", space = "free") +
    
    labs(x = "Tissue", y = "Database") +
    theme(
      legend.position = "bottom",
      axis.title = element_text(size = (axis_size+2), face = "bold"),
      strip.background = element_rect(fill = "#cce6ff")
    )
  
}



# MEAN_DB function used for summary tables


mean_DB <- function(geneDB) {
  
  if (nrow(geneDB) > 0) {
    mean_DB <- aggregate(geneDB[1],
                         by = list(groupTissue = geneDB$groupTissue, Tissue_type = geneDB$Tissue_type),
                         FUN = mean)
    mean_DB[3] <- round(mean_DB[3], 3)
    
    mean_DB$Enrichment <- round(scale(mean_DB[3]), 4)
    
    x <- mean_DB[3]
    x <- (1 - (x / max(x)))
    res <- sum(x)
    mean_DB$Tau <- round(res / (nrow(x) - 1), 4)
    
    if (colnames(mean_DB[3]) == "Signal") {
      
      mean_DB$Expression <- cut(mean_DB[, 3],
                                breaks = c(0, 25, 100, 1000, Inf),
                                labels = c("below cutoff", "low", "moderate", "high"),
                                ordered_result = TRUE
      )
      
    } else {
      
      mean_DB$Expression <- cut(mean_DB[, 3],
                                breaks = c(0, 0.5, 10, 1000, Inf),
                                labels = c("below cutoff", "low", "moderate", "high"),
                                ordered_result = TRUE
      )
      
    }
    
    
    
    db.index <- deparse(substitute(geneDB))  #obtain number from E_genex1-9
    db.index <- as.numeric(gsub("\\D", "", db.index))
    
    
    Species <- c("Homo sapiens",           #Important to keep this order since we will look for the value according to index of E_gene(x)
                 "Homo sapiens",
                 "Canis familiaris",
                 "Nonhuman primate",
                 "Nonhuman primate",
                 "Nonhuman primate",
                 "Mus musculus",
                 "Mus musculus",
                 "Homo sapiens",
                 "Rattus norvegicus",
                 "Sus scrofa domesticus",
                 "Mus musculus",
                 "Rattus norvegicus",
                 "Nonhuman primate",
                 "Canis familiaris",
                 "Rattus norvegicus",
                 "Mus musculus",
                 "Nonhuman primate",
                 "Nonhuman primate",
                 "Homo sapiens"
    )
    
    mean_DB$Database  <- DBs[db.index]
    mean_DB$Species    <- Species[db.index]
    mean_DB$Species <- factor(mean_DB$Species,
                              levels = c("Homo sapiens",
                                         "Nonhuman primate",
                                         "Canis familiaris",
                                         "Rattus norvegicus",
                                         "Mus musculus"
                              )
    )
    return(mean_DB)
    
  } else {
    mean_DB <- data.frame()
  }
}



# Function to display subset data at DATA tab


singleDB <- function(DB, input_tissue) {
  validate(need(nrow(mean_DB(DB)) > 0, message = 'No data available for this Gene'))
  
  singleDB <- mean_DB(DB)[, 1:4][order(mean_DB(DB)$Enrichment, decreasing = T), ]
  colnames(singleDB)[c(1, 2)] <- c("Tissue", "Tissue type")
  rownames(singleDB) <- NULL
  
  
  if ("All" %in% input_tissue) {
    return(singleDB)
    
  } else {
    singleDB <- singleDB[singleDB$`Tissue type` %in% input_tissue, ]
    return(singleDB)
    
  }
  
}






# Function to create table of enriched tissues including asterisks depending on enrichment level

enrich_tissues <- function(geneDB, input_ZScore_cutoff=3){
  enrich_tissues<- if (input_ZScore_cutoff== 3){
    
    paste0(mean_DB(geneDB)$groupTissue[mean_DB(geneDB)$Enrichment >= 3], "***", collapse = ", ")
    
  } else if ((input_ZScore_cutoff== 2)){
    paste(paste0(mean_DB(geneDB)$groupTissue[mean_DB(geneDB)$Enrichment >= 3], "***", collapse = ", "), 
          paste0(mean_DB(geneDB)$groupTissue[mean_DB(geneDB)$Enrichment >= 2 & mean_DB(geneDB)$Enrichment < 3 ], "**", collapse = ", "), 
          sep = ", ")
    
  } else if ((input_ZScore_cutoff== 1)){
    
    paste(paste0(mean_DB(geneDB)$groupTissue[mean_DB(geneDB)$Enrichment >= 3], "***", collapse = ", "), 
          paste0(mean_DB(geneDB)$groupTissue[mean_DB(geneDB)$Enrichment >= 2 & mean_DB(geneDB)$Enrichment < 3 ], "**", collapse = ", "), 
          paste0(mean_DB(geneDB)$groupTissue[mean_DB(geneDB)$Enrichment >= 1 & mean_DB(geneDB)$Enrichment < 2 ], "*", collapse = ", "), 
          sep = ", ")
  } else {
    paste(paste0(mean_DB(geneDB)$groupTissue[mean_DB(geneDB)$Enrichment >= 3], "***", collapse = ", "), 
          paste0(mean_DB(geneDB)$groupTissue[mean_DB(geneDB)$Enrichment >= 2 & mean_DB(geneDB)$Enrichment < 3 ], "**", collapse = ", "), 
          paste0(mean_DB(geneDB)$groupTissue[mean_DB(geneDB)$Enrichment >= 1 & mean_DB(geneDB)$Enrichment < 2 ], "*", collapse = ", "), 
          paste0(mean_DB(geneDB)$groupTissue[mean_DB(geneDB)$Enrichment >= 0 & mean_DB(geneDB)$Enrichment < 1 ], "", collapse = ", "), 
          sep = ", ")
  }
  
  asterisks_alone <- gsub('(^\\*{1,3})|(, \\*{1,2})', '', enrich_tissues)
  gsub('^, ', '', asterisks_alone)  
}





# Function to create table of enriched tissues by species



































