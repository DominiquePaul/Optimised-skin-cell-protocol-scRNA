library(RColorBrewer)
library(dplyr)
library(openxlsx)
library(stringr)


### Upload a file to iSEE

upload_file_to_iSEE <- function(file_path){
  ssh_key <- "~/.ssh/id_imls_servers"
  file_path <- gsub(" ", "\\\\ ", normalizePath(file_path))
  destination <- "dominique@imlspenticton.uzh.ch:/home/Shared/retger/synovial/data/protocol_paper_BBDP/sce/"
  
  # rename the file to include a date of upload
  file_name_components <- str_split(file_path, "/")[[1]]
  file_name <- file_name_components[length(file_name_components)]
  file_name_with_date <- sub(".rds", paste0("___", Sys.Date(), ".rds"), file_name)
  
  # the string to be executed in the shell
  exec_string <- paste0("scp -i ", ssh_key, " ", file_path,  " ", destination, file_name_with_date)
  
  cat(paste0("Command executed: \n\n", exec_string))
  
  system(exec_string)
}









################################################################################
###### Color palettes ######
################################################################################

########################
### Colours Protocol ###
########################

get_protocol_colours <- function(protocol_names, type="colour"){
  protocol_names <- sort(as.vector(unique(protocol_names)))
  colours_protocol <- c("#1C65AE", "#E62B8A", "#39A642", "#B17BA5", "#DB1E0D")
  #colours_protocol <- c("#0081a7", "#E62B8A", "#A4F42F", "#EF55FD", "#FF7F00")
  names(colours_protocol) <- c("Fresh", "Culture", "Tabib_et_al", "Sole_Boldo_et_al", "He_et_al")
  colours_protocol <- colours_protocol[protocol_names]
  
  # return different object depending on whether we are working with colour values or fill values
  if (type == "raw"){
    return_value <- colours_protocol
  } else if (type == "colour"){
    return_value <- scale_colour_manual(name = "Protocol", values = colours_protocol)
  } else if (type == "fill") {
    return_value <- scale_fill_manual(name = "Protocol", values = colours_protocol)
  }
  # return value
  return_value
}


#######################
### Colours samples ###
#######################

get_sample_colours <- function(sample_names, type="colour"){
  sample_names_ <- sort(as.vector(unique(sample_names)))
  
  unique_sample_names <- c("Fresh_S1", "Fresh_S2",
                           "Tabib_S1","Tabib_S124", "Tabib_S125", "Tabib_S18", "Tabib_S32", "Tabib_S33", "Tabib_S34", "Tabib_S4", "Tabib_S50", "Tabib_S68",
                           "Sole_Boldo_S396", "Sole_Boldo_S397", "Sole_Boldo_S398", "Sole_Boldo_S399", "Sole_Boldo_S400",
                           "He_S62", "He_S64", "He_S67", "He_S68", "He_S70", "He_S71", "He_S75",
                           "Culture_S1", "Culture_S2", "Culture_S4", "Culture_S5"
                           )

  colours_sample <- c("#023e8a", "#90e0ef", # blue
                      "#007f5f", "#2b9348", "#55a630", "#80b918", "#aacc00", "#bfd200", "#d4d700", "#dddf00", "#eeef20", "#ffff3f", # greens
                      "#6843E0", "#845AE8", "#A171F0", "#BD88F7", "#D99FFF", # purples
                      #"#03045e", "#023e8a", "#0077b6", "#00b4d8", "#90e0ef", # blues
                      "#a41623", "#d00000", "#dc2f02", "#e85d04", "#f48c06", "#d81159", "#ef476f", # reds
                      "#a41623", "#DC0202", "#f48c06", "#EF26DE" # reds
                      )
  
  names(colours_sample) <- unique_sample_names
  colours_sample <- colours_sample[sample_names_]
  
  label_names <- stringr::str_wrap(names(colours_sample), width=30)
  
  # return different object depending on whether we are working with colour values or fill values
  if (type == "raw"){
    return_value <- colours_sample
  } else if (type == "colour"){
    return_value <- scale_colour_manual(name = "Sample", values = colours_sample, labels=label_names)
  } else if (type == "fill") {
    return_value <- scale_fill_manual(name = "Sample", values = colours_sample, labels=label_names)
  }
  # return value
  return_value
}


#########################
### Colours Cell_type ###
#########################

get_cell_colours <- function(cell_names, type="colour"){
  
  cell_names <- sort(as.vector(unique(cell_names)))
  
  unique_cell_types <- c("Dendritic cells", "Fibroblasts", "Keratinocytes", "Lymphatic endothelial", "Macrophages", 
                         "Macrophages/DC", "Mast cells", "Melanocytes", "Melanocytes/Schwann cells/Neuronal cells", 
                         "Pericytes/VSMC", "Schwann cells", "Sweat gland cells", "T cells", "Vascular endothelial")
  # colours_cells <-  c("#000000", "#1A65AF", "#F5220F", "#2E8836", "#FDB462", "#FDB462", "#FA8172", 
  #                    "#7BAEDE", "#7BAEDE", "#B17BA5", "#882E72", "#E62B8A", "#FF7F00", "#3BB050") 
  
  #colours_cells <-  c("#000000", "#DB1E0D", "#FA8172", "#1A65AF", "#7BAEDE", "#7BAEDE", "#2E8876",  # normal
  #                 "#2E8836", "#2E8836", "#882E72", "#B17BA5", "#FF7F00", "#FDB462", "#E62B8A") 
  
  colours_cells <-  c("#000000", "#1A65AF", "#7BAEDE", "#008886", "#42C24D", "#42C24D", "#882E72", 
                      "#B17BA5", "#B17BA5", "#FF7F00", "#FDF133", "#E62B8A", "#FF9B8E", "#DB1E0D") 
  
  # colours_cells <-  c("#000000", "#F18175", "#D89B00", "#ACAF00", "#54BE00", "#54BE00", "#3DC786", 
  #                  "#3FC6CC", "#3DB9FA", "#3DB9FA", "#4BC9F0", "#A09AFF", "#E476F8", "#F76DC5") 
  
  names(colours_cells) <- unique(unique_cell_types)
  colours_cells_ <- colours_cells[cell_names]
  
  label_names <- stringr::str_wrap(names(colours_cells), width=30)
  
  # return different object depending on whether we are working with colour values or fill values
  if (type == "raw"){
    return_value <- colours_cells_
  } else if (type == "colour"){
    return_value <- scale_colour_manual(name = "Cell type", values = colours_cells_, labels=label_names)
  } else if (type == "fill") {
    return_value <- scale_fill_manual(name = "Cell type", values = colours_cells_, labels=label_names)
  }
  # return value
  return_value
}


##################################
### Colours for unnamed values ###
##################################

get_unnamed_colours <- function(unique_ids, colour_name, type="colour"){
  
  colour_names <- sort(as.vector(unique(unique_ids)))
  colours <-  c("#1A65AF", "#42C24D", "#882E72", "#FF7F00", "#E62B8A", "#DB1E0D") 
  
  # get only as many colours as we need
  if (length(colours) > length(colours)){
    stop("More unique IDs than colours in colour palette")
  }
  colours_subset <- colours[1:length(colour_names)]
  names(colours_subset) <- unique(colour_names)
  
  label_names <- stringr::str_wrap(names(colours_subset), width=30)
  
  # return different object depending on whether we are working with colour values or fill values
  if (type == "raw"){
    return_value <- colours_subset
  } else if (type == "colour"){
    return_value <- scale_colour_manual(name = colour_name, values = colours_subset, labels=label_names)
  } else if (type == "fill") {
    return_value <- scale_fill_manual(name = colour_name, values = colours_subset, labels=label_names)
  }
  # return value
  return_value
}

get_boolean_colour_scale <- function(type="colour", reverse=FALSE){
  if (reverse==FALSE){
    colour_scale <- c("#EA6458", "#008ACC")
  } else {
    colour_scale <- c("#008ACC", "#EA6458")
    
  }
  names(colour_scale) <- c(TRUE, FALSE)
  
  # return different object depending on whether we are working with colour values or fill values
  if (type == "raw"){
    return_value <- colours_subset
  } else if (type == "colour"){
    return_value <- scale_colour_manual(values = colour_scale)
  } else if (type == "fill") {
    return_value <- scale_fill_manual(values = colour_scale)
  }
  # return value
  return_value
}











################################################################################
###### A function to show plots with all data and plots by a column value ######
################################################################################

plot_stratified_views <- function(sce_object, dimred, stratify_by, colour_by=NULL, shape_by=NULL, text_by=NULL, tabset=NULL, point_size=1){
  # If a markdown heading should be displayed then tabset can be set to e.g. '##'
  if (!is.null(tabset)){cat(paste0("\n\n", tabset, " All \n\n"))}
  
  print(plotReducedDim(sce_object, dimred, colour_by=colour_by, shape_by=shape_by, text_by=text_by) +
          theme_ipsum_rc())
  
  for (sample_name in unique(colData(sce_object)[[stratify_by]])){
    if (!is.null(tabset)){cat(paste0("\n\n", tabset, " ", sample_name, " \n\n"))}
    print(plotReducedDim(sce_object[,colData(sce_object)[[stratify_by]] == sample_name], dimred, colour_by=colour_by, shape_by=shape_by, text_by=text_by, point_size=point_size) +
            theme_ipsum_rc())
  }
}










##################################
### annotate_with_other_labels ###
##################################

# A function to run cell type annotation with a list of marker genes from a csv
annotate_with_other_labels <- function(sce_object, csv_input, group_colname, gene_colname, annotations_name, tabset="###", point_size=0.25,
                                       visualisations=c("barchart", "TSNE", "UMAP", "UMAP_faceted")){
  
  # if a csv file is supplied read it and annotate. 
  # The alternative is just to pass the annotations_name which is then used to extract the annotations from the col data
  if (grepl(".csv", csv_input)){
    reference <- read.csv(csv_input, sep=";")
    
    unique_cell_types <- as.vector(unique(reference[group_colname]))
    rownames(unique_cell_types) <- NULL
    
    cell_type_genes <- vector("list", length(unique_cell_types[,group_colname]))
    names(cell_type_genes) <- unique_cell_types[,group_colname]
    
    for (i in 1:length(unique_cell_types[,group_colname])) {
      cell_type_genes[[i]] <- unique(reference[reference[,group_colname] == unique_cell_types[i, group_colname], gene_colname])
    }
    
    all_sets <- lapply(names(cell_type_genes), function(x) {
      GeneSet(cell_type_genes[[x]], setName=x)        
    })
    all_sets <- GeneSetCollection(all_sets)
    
    # AUCell
    rankings <- AUCell_buildRankings(counts(sce_object), plotStats=FALSE, verbose=FALSE)
    cell_aucs <- AUCell_calcAUC(all_sets, rankings, aucMaxRank=500)
    
    results_auccell <- t(assay(cell_aucs))
    
    labels <- colnames(results_auccell)[max.col(results_auccell)]
    
    #todo
    
    # add labels to sce object
    colData(sce_object)[annotations_name] <- labels
  }
  labels <- colData(sce_object)[[annotations_name]]
  tab <- table(Assigned=labels, Cluster=colLabels(sce_object))
  
  ######################
  
  sumdata <- data.frame(value=apply(tab, 1, sum))
  sumdata$key <- rownames(sumdata)
  
  # plot counts
  if ("barchart" %in% visualisations){
    cat(paste0("\n\n", tabset, " Cell type counts \n\n"))
    print(ggplot(data=sumdata, aes(x=key, y=value, fill=key)) +
            ggtitle(paste0("Annotated cell type counts for ", annotations_name)) +
            geom_bar(colour="black", stat="identity") +
            theme(axis.text.x=element_text(angle=50, hjust=1),))
  }
  
  # plot TSNE
  if ("TSNE" %in% visualisations){
    cat(paste0("\n\n", tabset, " TSNE \n\n"))
    print(plotReducedDim(sce_object, dimred="TSNE", colour_by=annotations_name, text_by=annotations_name, point_size=point_size) + 
            labs(title=paste0("T-SNE coloured by ", annotations_name)) +
            theme_ipsum_rc())
  }
  
  # plot UMAP
  if ("UMAP" %in% visualisations){
    cat(paste0("\n\n", tabset, " UMAP \n\n"))
    print(plotReducedDim(sce_object, dimred="UMAP", colour_by=annotations_name, text_by=annotations_name, point_size=point_size) + 
            labs(title=paste0("UMAP coloured by ", annotations_name)) +
            theme_ipsum_rc())
  }
  
  # Faceted UMAP
  if ("UMAP_faceted" %in% visualisations){
    cat(paste0("\n\n", tabset, " UMAP stratified \n\n"))
    print(plotReducedDim(sce_object, dimred="UMAP", colour_by=annotations_name, point_size=point_size) + 
            labs(title=paste0("UMAP coloured by ", annotations_name)) +
            facet_wrap(~colour_by) + #as.formula(paste0("~ ",annotations_name))
            theme_ipsum_rc())
  }
  
  return(sce_object)
}









#############################
### Plot annotation stats ###
#############################+

plot_label_barplots <- function(sce_object, column_name, level1="Protocol", level2="Sample", tabset="##", min_cell_thresh=0.001, print=TRUE, add_labels=c(TRUE, TRUE)){
  # Plots two bar charts to evaluate the annotations per sample. The first 
  # is for a relative comparison and the second for an absolute comparison of
  # cell counts
  
  # min_cell_thresh = The minimum number of cells a `column_name` label has to have to be featured in the plot
  # tabset = indication of the heading hierarchy for tabs in rmarkdown
  
  
  
  # only keep cell labels representing at least 0.1% of all cells
  sce_column_data <- as.data.frame(colData(sce_object))
  min_cell_number_threshold <- dim(sce_object)[2] * min_cell_thresh
  mask <- table(sce_column_data[,column_name]) > dim(sce_object)[2] * min_cell_thresh
  cell_names_to_keep <- names(mask)[as.list(mask) == TRUE]
  
  if (!is.na(level2)){
    # put data together for plots. We want the level1 (protocols), level2 (samples), 
    # and the column value (cell types)
    # grouped together, with counts of the respective cell types per sample/protocol.
    df <- as.data.frame(colData(sce_object)) %>% 
      filter(!!sym(column_name) %in% cell_names_to_keep) %>%
      group_by(!!sym(level1), !!sym(level2), !!sym(column_name)) %>%
      dplyr::count(!!sym(column_name)) %>%
      dplyr::group_by(!!sym(level1), !!sym(level2)) %>%
      dplyr::rename(Count=n, Label=!!sym(column_name), level1=!!sym(level1), level2=!!sym(level2))
  } else {
    
    # set level2 to same value as level1 to keep code for plotting further below as short as possible
    level2 <- level1
    
    # Same as above but only with one grouping level except values
    df <- as.data.frame(colData(sce_object)) %>% 
      filter(!!sym(column_name) %in% cell_names_to_keep) %>%
      group_by(!!sym(level1), !!sym(column_name)) %>%
      dplyr::count(!!sym(column_name)) %>%
      dplyr::group_by(!!sym(level1)) %>%
      dplyr::rename(Count=n, Label=!!sym(column_name), level1=!!sym(level1))
  }
  
  
  # Plot relative stacked barplot (all bars add up to same height for relative comparison)
  plt1_relative_values <- df %>%
    ggplot(aes(x=level2, y=Count, fill=Label, label=Count)) +
    geom_bar(stat="identity", position="fill") +
    labs(title="Frequency of cell type by sample phase by sample",
         subtitle="Labels represent absolute number of cells per group",
         x="",
         y="Frequency") +
    facet_grid(all_of(column_name)~level1, scales="free", space="free_x") +
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),
          axis.ticks=element_blank(), panel.background = element_blank(),
          axis.text.y=element_blank(),
          axis.ticks.y=element_blank()) +
    guides(color=guide_legend(title="",
                              nrow=2,
                              byrow=TRUE, 
                              override.aes = list(size=4, shape = 15, alpha = 1)))
  
  if (add_labels[1] == TRUE){
    plt1_relative_values +
      geom_text(size=3, position = position_fill(vjust=0.5))
  }
  
  # Plot absolute counts per sample, facetted by cell type
  plt2_absolute_values <- df %>% 
    ggplot(aes(x = level2, y = Count, fill=Label, label=Count)) +
    geom_bar(stat='identity') + #stat = "identity", position="fill"
    geom_text(size=3) +
    labs(title="Absolute counts by cell type",
         subtitle="Labels represent absolute number of cells per group",
         x="",
         y="Cell count") +
    facet_grid(Label~level1, scales="free", space="free_x") +
    theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1),
          axis.ticks=element_blank(), panel.background = element_blank(),
          axis.text.y=element_blank(),
          axis.ticks.y=element_blank()) +
    guides(color=guide_legend(title="",
                              nrow=2,
                              byrow=TRUE, 
                              override.aes = list(size=4, shape = 15, alpha = 1)))
  
  if (add_labels[2] == TRUE){
    plt2_absolute_values +
      geom_text(size=3, position = position_fill(vjust=0.5))
  }
  
  
  if (level1 == level2){
    plt1_relative_values <- plt1_relative_values +
      theme(axis.title.x=element_blank(),
            axis.text.x=element_blank(),
            axis.ticks.x=element_blank())
    
    plt2_absolute_values <- plt2_absolute_values +  
      theme(axis.title.x=element_blank(),
            axis.text.x=element_blank(),
            axis.ticks.x=element_blank())
  }
  
  if (print == TRUE){
    # print the plots
    cat(paste0("\n\n", tabset, " Relative counts by ", level2, "\n\n"))
    print(plt1_relative_values)
    
    cat(paste0("\n\n", tabset, " Absolute counts by ", level2, "\n\n"))
    print(plt2_absolute_values)
  } else {
    list(plt_rel=plt1_relative_values,
         plt_abs=plt2_absolute_values)
  }
  
}










######################################################################################
### A function to run marker gene analysis with plots and writing results to excel ###
######################################################################################
# 
# # prepare a theme for adjusting the pheatmap plot
# reduced_ipsum_rc_theme <- theme_ipsum_rc() +
#   theme(axis.title.x=element_blank(),
#         axis.text.x=element_blank(),
#         axis.ticks.x=element_blank(),
#         axis.title.y=element_blank(),
#         axis.text.y=element_blank(),
#         axis.ticks.y=element_blank(),
#         panel.grid.major = element_blank(), panel.grid.minor = element_blank())
# 
# 
# 
# run_marker_gene_analysis <- function(sce, test_name, type_comparison, tabset="###", worksheet_prefix="", excel_workbook=NA, heatmaplimits=c(-3,3), top=5){
#   
#   prefix <- ifelse(test_name=="t", "logFC", "AUC")
#   
#   # set limits depending on the marker method used (AUC only goes from 0 to 1)
#   heatmap_lower_limit <- ifelse(test_name=="t", heatmaplimits[0], 0)
#   heatmap_upper_limit <- ifelse(test_name=="t", heatmaplimits[1], 1)
#   
#   marker_results_per_cell_type <- list()
#   
#   for (cell_type in unique(sce$manual_labels_coarse)){
#     # create tab for markdown
#     cat(paste0("\n\n", tabset, " ", cell_type, " {.tabset} \n\n"))
#     # create the subset
#     sce_subset <- sce[,sce$manual_labels_coarse == cell_type]
#     sce_subset$subcluster_id_k50 <- factor(sce_subset$subcluster_id_k50)
#     
#     # get the marker genes for each cluster
#     all.markers <- findMarkers(sce_subset, test.type=test_name,
#                                pval.type=type_comparison,
#                                groups=sce_subset$subcluster_id_k50,
#                                direction="up", block=sce_subset$Sample)
#     
#     marker_results_per_cell_type[[cell_type]] <- all.markers
#     
#     
#     # iterate of the markers for each cluster
#     
#     for (marker_num in names(all.markers)){
#       
#       # create tab for markdown
#       cat(paste0("\n\n#", tabset, " Cluster ", marker_num, " \n\n"))
#       
#       # subset marker genes object for a given cluster
#       all_cluster_markers <- all.markers[[marker_num]]
#       
#       # Per default we only look at union of the top 5 markers per pairwise comparison
#       if (type_comparison=="any"){
#         cluster_markers <- all_cluster_markers[all_cluster_markers$Top <= top,]  
#       } else {
#         cluster_markers <- all_cluster_markers[1:25,]
#       }
#       
#       cluster_markers_logFCs <- getMarkerEffects(cluster_markers, prefix = prefix)
#       cluster_markers_logFCs[is.na(cluster_markers_logFCs)] <- 0
#       
#       # Create pheatmap plot per cluster
#       heatmap_plt <- as.ggplot(pheatmap(cluster_markers_logFCs, breaks=seq(heatmap_lower_limit, heatmap_upper_limit, length.out=101), silent=T, cluster_cols=FALSE)) +
#         reduced_ipsum_rc_theme +
#         labs(title=paste0("Cluster ", marker_num), subtitle="Pairwise comparisons") 
#       
#       # Change the heading if there is only one other plot to comapre against
#       if (dim(cluster_markers_logFCs)[2] == 1){
#         heatmap_plt <- heatmap_plt +
#           labs(title=paste0("Cluster ", marker_num),
#                subtitle="Pairwise comparisons - comparison with only remaining subcluster")
#       }
#       
#       # finally print the heatmap
#       print(heatmap_plt)
#                 
#       # write to excel if a workbook was specified. Else skip
#       if (is.na(excel_workbook)==FALSE){
#         cell_type_short <- ifelse(nchar(cell_type) > 10, substr(cell_type, 1, 10), cell_type)
#         sheet_name <- paste0("F_", worksheet_prefix, "_", cell_type_short,"_c", marker_num)
#         addWorksheet(excel_workbook, sheet_name)
#         cluster_markers_excel_out <- cbind("Gene"=rownames(all_cluster_markers), all_cluster_markers)
#         writeData(excel_workbook, sheet_name, cluster_markers_excel_out, rowNames = TRUE)
#       }
#       
#     }
#   }
#   
#   # Either only return the marker results or also the updated excel
#   if (is.na(excel_workbook) == TRUE){
#     marker_results_per_cell_type
#   } else {
#     return(list("marker_results"=marker_results_per_cell_type,
#                 "excel_workbook"=excel_workbook))
#   }
#   
# }
# 
# 
# 
# 
# 
# 
# 
# 
# 
