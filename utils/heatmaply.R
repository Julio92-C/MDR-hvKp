
library(heatmaply)
library(readr)


# Input data sets
kleborate_metadata <- read_csv("MDR-hvKp/MDR-hvKp/data/kleborate_metadata.csv")

# Virulence factors and Resistance gene
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


# Annotation col dataframe
ann_col <- data.frame(Gene = rep(c("VFs", "ARG"), c(4,3)))
row.names(ann_col) <- colnames(vir_res_genData)

# # Annotation colors customization
# ann_colors = list(
#   Gene = c(VFs = "#CC0000FF", ARG = "#0000CCFF")
# )

ann_colors <- c(rep("#CC0000FF", 4), rep("#0000CCFF", 3))


hm <- heatmaply(vir_res_genData_Bmatrix, colors = c("#CCCCCCFF", "#666666FF"), 
                limits = c(0, 1),
                Rowv = FALSE,
                Colv = FALSE,
                show_dendrogram = c(FALSE, FALSE),
                col_side_colors = ann_col,
                col_side_palette = c(VFs = "#CC0000FF", ARG = "#0000CCFF")
                
)

hm

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
