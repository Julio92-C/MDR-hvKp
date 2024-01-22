
library(plotly)
library(readr)

# Input data sets
df <- read_csv("MDR-hvKp/MDR-hvKp/data/kleborate_metadata.csv")

data <- df[, c("ST", "virulence_score","resistance_score", "num_resistance_genes")]

# Assuming df is already defined and contains the specified variables

# Create the 3D scatter plot
scatterPlot3D <- plot_ly(data = mean_scores, 
                         x = ~mean_virulence_score, 
                         y = ~mean_resistance_score, 
                         z = ~num_resistance_genes, 
                         type = "scatter3d", 
                         color = ~.data[[input$mainVar_freq]],
                         colors = palt1,
                         mode = "markers",
                         marker = list(size = 10 
                         )) %>%
  layout(scene = list(xaxis = list(title = "Mean Virulence Score"),
                      yaxis = list(title = "Mean Resistance Score"),
                      zaxis = list(title = "Number of Resistance Genes")))
scatterPlot3D



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