
library(ggplot2)
library(reshape2)
library(readr)
library(pheatmap)
library(dplyr)
library(grid)
library(heatmaply)

install.packages("heatmaply")

kleborate_metadata <- read_csv("MDR-hvKp/MDR-hvKp/data/kleborate_metadata.csv")
# View(kleborate_metadata)
# colnames(kleborate_metadata)

# Filter dataset by ST
ST15_genotypes  <- filter(kleborate_metadata, kleborate_metadata$ST == "ST15")

# Virulence factors and Resistance gene
vir_res_genData <- ST15_genotypes[, c("Yersiniabactin", "Colibactin", "Aerobactin", "rmpA2", 
                                           "Bla_ESBL_acquired", "Tet_acquired", "Bla_Carb_acquired")]

# Change column names
vir_res_genData <- vir_res_genData %>% 
  rename("Bla ESBL" = "Bla_ESBL_acquired",
         "Tetracycline" = "Tet_acquired",
         "Carbapenem" = "Bla_Carb_acquired"
         )
  
# Replace "-" by NA values
df <- replace(vir_res_genData, vir_res_genData == '-', NA)

# Convert from a dataframe to a matrix
vir_res_genData_matrix <- data.matrix(df, rownames.force = NA)

# Count NA in data sets
sum(is.na(vir_res_genData_matrix))

# Replace all NA values with 0 values
vir_res_genData_matrix <- vir_res_genData_matrix %>% replace(is.na(.), 0)

# Convert to a binary matrix
vir_res_genData_Bmatrix <- as.matrix((vir_res_genData_matrix > 0) + 0)

# Prove that the matrix is binary
is.binary.matrix <- function(x) {
  return(is.matrix(x) && all(x == 1 | x == 0))
}

is.binary.matrix(vir_res_genData_Bmatrix)

# Modify ordering of the clusters using clustering callback option
callback = function(hc, mat){
  sv = svd(t(mat))$v[,1]
  dend = reorder(as.dendrogram(hc), wts = sv)
  as.hclust(dend)
}



## For pheatmap_1.0.8 and later:
draw_colnames_45 <- function (coln, gaps, ...) {
  coord = pheatmap:::find_coordinates(length(coln), gaps)
  x = coord$coord - 0.5 * coord$size
  res = textGrob(coln, x = x, y = unit(1, "npc") - unit(3,"bigpts"), vjust = 0.5, hjust = 1, rot = 45, gp = gpar(...))
  return(res)}

## 'Overwrite' default draw_colnames with your own version 
assignInNamespace(x="draw_colnames", value="draw_colnames_45",
                  ns=asNamespace("pheatmap"))

# Annotation col dataframe
ann_col <- data.frame(Gene = rep(c("VFs", "ARG"), c(4,3)))
row.names(ann_col) <- colnames(vir_res_genData)

# Annotation colors customization
ann_colors = list(
  Gene = c(VFs = "#CC0000FF", ARG = "#0000CCFF")
)



# Create heatmap using pheatmap package ##
pheatmap(vir_res_genData_Bmatrix, display_numbers = FALSE, cluster_cols = FALSE, cluster_rows = FALSE,
         scale = "none", clustering_callback = callback,  border_color = "NA", color = c("#CCCCCCFF", "#666666FF"),
         legend_breaks = c(0, 1),
         legend_labels = c("Absent", "Present"),
         annotation_col = ann_col,
         annotation_colors = ann_colors,
          
         )

heatmaply(vir_res_genData_Bmatrix)


# CLEAN UP #################################################

# Clear environment
rm(list = ls())

# Clear packages
detach("package:datasets", unload = TRUE)

# Clear plots
dev.off()  # But only if there IS a plot

# Clear console
cat("\014")  # ctrl+L

# Clear mind :)
