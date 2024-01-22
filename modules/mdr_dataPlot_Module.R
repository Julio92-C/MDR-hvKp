# MDR-hvKp dataPlot Module


# Module UI function
MDRdataPlotUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    fluidRow(
      column(3,
             box(width = 12, 
                 solidHeader = TRUE, 
                 collapsible=TRUE,
                 status = "black",
                 title = "Choose a Genotype:",
                 background = "gray",
                 
                 selectInput(ns("Genotype"),
                             label = "Genotype",
                             choices = c("MDR",
                                         "HvKp",
                                         "MDR-HvKp",
                                         "cKp"
                             ),
                             selected = "MDR",
                             multiple = TRUE
                 ),
                 
                 h4("Filter data-set by:"),
                 
                 sliderInput(ns("sequence_type"),
                             label = "Sequence Type",
                             value= 5, min = 0, max = 50
                             
                             ),
                 sliderInput(ns("virulence_score"),
                             label = "Virulence Score",
                             value = 1, min = 0, max = 5
                             
                 ),
                 sliderInput(ns("resistance_score"),
                             label = "Resistance Score",
                             value = 1, min = 0, max = 5
                             
                 ),
                 sliderInput(ns("num_resistance_classes"),
                             label = "Resistance Classes",
                             value = 2, min = 0, max = 11
                             
                 ),
                 sliderInput(ns("num_resistance_genes"), 
                             label = "Resistance Genes",
                             value = 5, min = 0, max = 31,
                             
                             
                 ),
                 
                 fluidRow(
                   column(6,
                          actionButton(ns("plot_button"), icon = icon("chart-bar"), "Render Plot", 
                                       style = "
                                       width: 100%;
                                       background-color: #428bca;
                                       color: white;
                                       border: none;
                                       border-radius: 5px;
                                       padding: 10px 20px;
                                       font-size: 16px;
                                       font-weight: bold;"
                          )
                   ),
                   column(6,
                          downloadButton(ns("download"), "Report",
                                         style = "
                                         width: 100%;
                                         background-color: #428bca;
                                         color: white;
                                         border: none;
                                         border-radius: 5px;
                                         padding: 10px 20px;
                                         font-size: 16px;
                                         font-weight: bold;"
                          )  
                   )
                 )
                 
                 
                 
                 
             )
      ),
      
      column(9,
             fluidRow(
               
               infoBoxOutput(ns("general_stats"), width = 8),
               
               valueBoxOutput(ns("Yersiniabactin_count"), width = 4)
               
               
             ),
             
             fluidRow(
               valueBoxOutput(ns("Bla_ESBL_acquired"), width = 4),
               
               valueBoxOutput(ns("Tet_acquired"), width = 4),
               
               valueBoxOutput(ns("Bla_Carb_acquired"), width = 4)
               
               
             ),
             
             fluidRow(
               
               tabBox(width = 12,
                      height = 700,
                      
                      tabPanel("Features Frequency",
                               fluidRow(
                                 
                                   plotlyOutput(ns("freq_plot"), height = 500)
                                 
                               ),
                               
                               fluidRow(
                                 column(4,
                                        
                                 ),
                                 column(4,
                                       selectInput(ns("mainVar_freq"),
                                                   label = "Choose a genotype:",
                                                   choices = c("ST", "K_locus", "O_locus"),
                                                   selected = "ST",
                                                   multiple = FALSE,
                                       )
                                 ),
                                 
                                 column(4,
                                   
                                 )
                               )
                               
                               
                      ),
                      
                      tabPanel("Resistance vs Virulence",
                               plotlyOutput(ns("Resistance_vs_Virulence"), height = 700)
                      ),
                      
                      tabPanel("Convergent Isolates",
                               fluidRow(
                                 plotlyOutput(ns("Convergent_Isolates"), height = 500)
                               ),
                               fluidRow(
                                 column(4,
                                        
                                 ),
                                 column(4,
                                        textInput(ns("input_st"),
                                                    label = "Choose a genotype:",
                                                    value = "ST15",
                                                    placeholder = "Select a ST (e.g ST15)"
                                                    
                                        )
                                        
                                 ),
                                 
                                 column(4,
                                        
                                 )
                               )
                      ),
                       
                      tabPanel("ST vs Resistance",
                               plotlyOutput(ns("ST_vs_Resistance"), height = 500)

                      ),
                      
                      tabPanel("Correlation ARG-VFs",
                               plotlyOutput(ns("Correlation_ARG_VFs"), height = 500)
                               
                               ),
                      
                      tabPanel("K/O Diversity",
                               fluidRow(
                                 plotlyOutput(ns("ST_vs_Serotypes"), height = 500)
                               ),
                               
                               fluidRow(
                                 column(4,
                                        
                                 ),
                                 column(4,
                                        selectInput(ns("genotype_diversity"),
                                                    label = "Choose a genotype:",
                                                    choices = c("K_locus", "O_locus"),
                                                    selected = "K_locus",
                                                    multiple = FALSE,
                                        )
                                        
                                 ),
                                 
                                 column(4,
                                        
                                 )
                               )
                               
                      ),
                      
                      tabPanel("O locus Frequency",
                               plotOutput(ns("O_locus_Frequency"), height = 500)
                               
                      ),
                      
                      tabPanel("Trending Threats",
                               fluidRow(plotlyOutput(ns("Trending_Threats"), height = 500)),
                               
                               fluidRow(
                                 column(4,

                                        ),
                                 column(4,
                              selectInput(ns("categories"),
                                          "Select Categories",
                                          choices = c("Yersiniabactin", "Colibactin", "Aerobactin",
                                                      "Bla_ESBL_acquired", "Bla_Carb_acquired", "Tet_acquired", 
                                                      "Flq_mutations"),
                                          selected = c("Yersiniabactin", "Colibactin", "Aerobactin"),
                                          multiple = TRUE)

                                 ),
                                 column(4,

                                        )
                              )
                          )
                     )
                      
               )
               
             )
      )
      
    )
  
  
}

# Module server function
MDRdataPlotServer <- function(id, dataframe){
  stopifnot(is.reactive(dataframe))
  
  moduleServer(id, function(input, output, session){
    
    # Filter data by seq_len or coverage or identity or accuracy or model type
    filter_data <- eventReactive(input$plot_button, {
      validate(
        need(dataframe(), "Please input a data-sets as a csv file"),
        need(input$Genotype, "Please select a Genotype")
      )
      
      # Group the data by ST and count the occurrences
      ST_counts <- dataframe() %>% 
        group_by(ST) %>% 
        summarize(count = n())
      
      
      filter_data <- dataframe() %>%
        filter(ST %in% ST_counts$ST[ST_counts$count > input$sequence_type],
               virulence_score >= input$virulence_score, 
               resistance_score >= input$resistance_score, 
               num_resistance_classes >= input$num_resistance_classes, 
               num_resistance_genes >= input$num_resistance_genes, 
               Genotype %in% input$Genotype)
      
    })
    
    # Download plots to a PDF file
    output$download <- downloadHandler(
      filename = function() {
        paste("report-", Sys.Date(), ".html", sep="")
      },
      
      
      content = function(file) {
        # Code to generate the report goes here
        rmarkdown::render("shinyReport.Rmd", output_file = file)
      }
    )
    
    # Display an infoBox with general stats
    output$general_stats <- renderInfoBox({
      infoBox(
        title = "General stats",
        value = paste("No. of Genomes:", length(unique(filter_data()$strain)), "|",
                      "Species:", length(unique(filter_data()$species)), "|",
                      "wg-MLST:", length(unique(filter_data()$ST)), "|",
                      "K Loci:", length(unique(filter_data()$K_locus)), "|",
                      "O Loci:", length(unique(filter_data()$O_locus))
                      ),
        subtitle = "Dataset derived from the PATRIC database. Whole genome multilocus sequence typing (wgMLST) performed with Kleborate.",
        color = "aqua",
        icon = icon("virus"),
        fill = TRUE
      )
    })
    
    output$Yersiniabactin_count <- renderValueBox({
      count <- table(filter_data()$Yersiniabactin, exclude = "-")
      total_ybt_count <- sum(count)
      # print(count)
      # print(total_count)
      
      valueBox(
        value = paste0(total_ybt_count, "/",
                       round(100 * total_ybt_count/length(unique(filter_data()$strain)), 2), "%"),
        subtitle = "Yersiniabactin - Virulence Factors", icon("dna"),
        color = "purple"
      )
    })
    
    
    
    output$Bla_ESBL_acquired <- renderValueBox({
      count <- table(filter_data()$Bla_ESBL_acquired, exclude = "-")
      total_bla_count <- sum(count)
      
      
      valueBox(
        value = paste0(total_bla_count, "/",
                       round(100 * total_bla_count/length(unique(filter_data()$strain)), 2), "%"),
        subtitle = "ESBL - Resistance Genes", icon("capsules"),
        color = "orange"
      )
    })
    
    output$Bla_Carb_acquired <- renderValueBox({
      count <- table(filter_data()$Bla_Carb_acquired, exclude = "-")
      total_bla_count <- sum(count)
      
      
      valueBox(
        value = paste0(total_bla_count, "/",
                       round(100 * total_bla_count/length(unique(filter_data()$strain)), 2), "%"),
        subtitle = "Carbapenem - Resistance Genes", icon("capsules"),
        color = "orange"
      )
    })
    
    output$Tet_acquired <- renderValueBox({
      count <- table(filter_data()$Tet_acquired, exclude = "-")
      total_tet_count <- sum(count)
      
      
      valueBox(
        value = paste0(total_tet_count, "/",
                       round(100 * total_tet_count/length(unique(filter_data()$strain)), 2), "%"),
        subtitle = "Tetracycline - Resistance Genes", icon("capsules"),
        color = "orange"
      )
    })
    
    # Color palettes
    palt1 <- paletteer_d("palettesForR::Cranes", n = 200)
    palt1 <- as.character(palt1)
    
    
    
    # Render ST type frequency plot based on the filter data sets
    output$freq_plot <- renderPlotly({
      # Plot ST type frequency
      a <- ggplot(filter_data(), aes(x= fct_infreq(.data[[input$mainVar_freq]]),
                                     fill=Genotype))
      a <- a + geom_bar()
      a <- a + scale_fill_manual(values=palt1)
      a <- a + theme_classic()
      # a <- a + labs(title = "ST Type Frequency")
      a <- a + labs(fill = "Legend")
      a <- a + theme(legend.position = "Bottom")
      a <- a + theme(legend.justification = "center")
      a <- a + theme(axis.text = element_text(size = 10.5))
      a <- a + theme(plot.title = element_text(hjust = 0.5))
      a <- a + theme(axis.text.x = element_text(angle = 45))
      a <- a + labs(x="", y="No. Genomes")
      a
      
      # Plot Model type frequency with PLOTLY ####
      ST_frequency_plot <- plotly::ggplotly(a)
      ST_frequency_plot
      
    })
    
    
    # Render Resistance vs Virulence plot based on the filter data sets
    output$Resistance_vs_Virulence <- renderPlotly({
      # Calculate mean resistance score and mean virulence score, and count the number of st
      mean_scores <- filter_data() %>%
        group_by(.data[[input$mainVar_freq]], num_resistance_genes) %>%
        summarise(mean_resistance_score = mean(resistance_score, na.rm = TRUE),
                  mean_virulence_score = mean(virulence_score, na.rm = TRUE),
                Count = n())
      
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
      
    })
    
    
    
    # Filter dataset by ST
    ST15_genotypes  <- reactive({
      validate(
        need(as.character(input$input_st), "Please input a valid ST")
      )
      
      filter_data() %>%
        filter(ST %in% input$input_st)
    })
    
    
    
     # Render Virulence-Reistance heatmap plot based on the filter data sets
     output$Convergent_Isolates <- renderPlotly({
       tryCatch({
         # Virulence factors and Resistance gene
         vir_res_genData <- ST15_genotypes()[, c("Yersiniabactin", "Colibactin", "Aerobactin", "Salmochelin", "rmpA2", "RmpADC",
                                                 "Bla_ESBL_acquired", "Tet_acquired", "Bla_Carb_acquired", 
                                                 "Col_mutations", "Omp_mutations", "Flq_mutations")]
         
         # Change column names
         vir_res_genData <- vir_res_genData %>% 
           rename("Bla ESBL" = "Bla_ESBL_acquired",
                  "Tetracycline" = "Tet_acquired",
                  "Carbapenem" = "Bla_Carb_acquired",
                  "Flq mutations" = "Flq_mutations",
                  "Colistin mutations" = "Col_mutations",
                  "Omp mutations" = "Omp_mutations"
           )
         
         # Replace "-" by NA values
         df <- replace(vir_res_genData, vir_res_genData == '-', NA)
         
         # Convert from a dataframe to a matrix
         vir_res_genData_matrix <- data.matrix(df, rownames.force = NA)
         
         # Count NA in data sets
         # sum(is.na(vir_res_genData_matrix))
         
         # Replace all NA values with 0 values
         vir_res_genData_matrix <- vir_res_genData_matrix %>% replace(is.na(.), 0)
         
         # Convert to a binary matrix
         vir_res_genData_Bmatrix <- as.matrix((vir_res_genData_matrix > 0) + 0)
         
         # Prove that the matrix is binary
         observe({
           is.binary.matrix <- function(x) {
             return(is.matrix(x) && all(x == 1 | x == 0))
           }
           
           mbinary <- is.binary.matrix(vir_res_genData_Bmatrix)
           
           print(paste0("The matrix is binary:", mbinary))
         })
         
         # Annotation col dataframe
         ann_col <- data.frame(Gene = rep(c("VFs", "ARG"), c(6,6)))
         row.names(ann_col) <- colnames(vir_res_genData)
         
         ann_colors <- c(VFs = "#C01C24FF", ARG = "#201450FF")
         
         
         hm <- heatmaply(vir_res_genData_Bmatrix, colors = c("#CCCCCCFF", "#666666FF"), 
                         limits = c(0, 1),
                         Rowv = FALSE,
                         Colv = FALSE,
                         show_dendrogram = c(FALSE, FALSE),
                         col_side_colors = ann_col,
                         col_side_palette = ann_colors,
                         plot_method='plotly',
                         hide_colorbar = TRUE,
                         showticklabels = c(TRUE, FALSE)
                         #colorbar_lab_format = c("Absent", "Present")
                         
         )
         
         # Plot Coverage vs seq_length with PLOTLY ####
         Convergent_Isolates_plotly <- plotly::ggplotly(hm)
         Convergent_Isolates_plotly
         
         
         
       }, error = function(e) {
         # Handle the error
         # You can display an error message or take any other appropriate action
         showNotification("An error occurred while generating the plot. Please input a valid ST", 
                          type = "error")
         
         # Return an empty plot
         return(NULL)
       })
       
    })
     
     # Render Correlation matrix among ARG-VFs
     output$Correlation_ARG_VFs <- renderPlotly({
       # Virulence factors and Resistance genes
       vir_res_genData <- filter_data()[, c("Yersiniabactin", "Colibactin", "Aerobactin", "Salmochelin", "rmpA2", "RmpADC", 
                                 "Bla_ESBL_acquired", "Tet_acquired", "Bla_Carb_acquired", "Col_mutations", "Omp_mutations",
                                 "Flq_mutations")]
       
       # Change column names
       vir_res_genData <- vir_res_genData %>% 
         rename("Bla ESBL" = "Bla_ESBL_acquired",
                "Tetracycline" = "Tet_acquired",
                "Carbapenem" = "Bla_Carb_acquired",
                "Flq mutations" = "Flq_mutations",
                "Colistin mutations" = "Col_mutations",
                "Omp mutations" = "Omp_mutations"
         )
       
       # Replace "-" by NA values
       df <- replace(vir_res_genData, vir_res_genData == '-', NA)
       
       # Convert from a dataframe to a matrix
       vir_res_genData_matrix <- data.matrix(df, rownames.force = NA)
       
      
       # Replace all NA values with 0 values
       vir_res_genData_matrix <- vir_res_genData_matrix %>% replace(is.na(.), 0)
       
       observe({
         # Count NA in data sets
         total_NA <- sum(is.na(vir_res_genData_matrix))
         print(paste("Total NA:", total_NA))
       })
       
       # Calculate correlation between resistance genes and virulence factors
       cor_resistance <- cor(vir_res_genData_matrix, use = "complete.obs")
       
       # Melt the correlation matrix for visualization
       cor_resistance_melted <- melt(cor_resistance)
       
       # Plot the correlation matrix for resistance genes and virulence factors
       cor_matrix <- ggplot(cor_resistance_melted, aes(Var1, Var2, fill = value)) +
         geom_tile() +
         scale_fill_gradient2(low = "#080808FF", high = "#C0B090FF", mid = "white", midpoint = 0, limit = c(-1,1), space = "Lab", name="Pearson\nCorrelation") +
         theme_minimal() +
         theme(axis.text.x = element_text(angle = 45, vjust = 1, size = 12, hjust = 1),
               axis.text.y = element_text(size = 12)) +
         labs(x="ARG-VFs", y="") 
         # coord_fixed()
       
       # Plot Correlation ARG-VFs with PLOTLY ####
       Correlation_ARG_VFs_plot <- plotly::ggplotly(cor_matrix)
       Correlation_ARG_VFs_plot
       
     })
    
      
     # Render ST vs No. Resistance Genes plot based on the filter data sets
     output$ST_vs_Resistance <- renderPlotly({
       # Plot ST vs No. Resistance Genes
       m <- ggplot(filter_data(), aes(x=reorder(ST, -num_resistance_genes, FUN = median, na.rm = TRUE), y=num_resistance_genes,
                                      fill= .data[[input$mainVar_freq]]))
       m <- m + geom_boxplot()
       m <- m + stat_boxplot(geom ='errorbar', width=0.2)
       m <- m + scale_fill_manual(values=palt1)
       m <- m + theme_classic()
       m <- m + labs(fill = "Legend")
       m <- m + theme(axis.text = element_text(size = 10.5))
       m <- m + theme(plot.title = element_text(hjust = 0.5))
       m <- m + theme(axis.text.x = element_text(angle = 45))
       m <- m + labs(x="ST", y="No. Resistance Genes")
       m

       # Plot ST vs No. Resistance Genes with PLOTLY ####
       Model_type_vs_identity_plot <- plotly::ggplotly(m)
       Model_type_vs_identity_plot

     })
     
    
     # Plot ST vs Serotypes
     output$ST_vs_Serotypes <- renderPlotly ({
       s <- (filter_data() %>%
               count(ST, .data[[input$genotype_diversity]]) %>%
               group_by(ST) %>%
               mutate(freq = n / sum(n)) %>%
               ggplot(aes( x = ST, y = freq, fill = .data[[input$genotype_diversity]])) +
               geom_bar(stat = "identity") +
               theme_classic() +
               scale_fill_manual(values= palt1) +
               theme(axis.text.x = element_text(angle = 0, hjust = 1)) +
               theme(axis.text = element_text(size = 10.5)) +
               labs(y = "Percentage", x = " ", fill = "Categories") +
               coord_flip() +  # Flipping coordinates for better readability if there are many sequence types
               guides(fill = guide_legend(reverse = TRUE)) + # Reverse legend for consistency with the plot
               scale_y_continuous(labels = scales::percent) + # Convert y-axis to percentage
               theme(legend.position = "none") +
               theme(legend.title = element_text(colour="blue", size=14,
                                                 face="bold")) +
               theme(legend.text = element_text(colour="blue", size=12, hjust = 0,
                                                lineheight = 2,face="bold")) +
               theme(legend.background = element_rect(fill="lightblue",
                                                      linewidth = 0.5, linetype="solid",
                                                      colour ="darkblue"))
       )
       # Plot Model type vs Categories with PLOTLY ####
       ST_vs_Serotypes_plot <- plotly::ggplotly(s)
       ST_vs_Serotypes_plot
     })

    
    
    # O locus Frequency pie chart
     output$O_locus_Frequency <- renderPlot({

       O_locus_count <- table(filter_data()$O_locus)

       pie_chart <- pie(O_locus_count, labels = paste0(names(O_locus_count), " / ", O_locus_count, " / ", round(100 * O_locus_count/sum(O_locus_count), 2), "%"),
                        main="Lipopolysaccharide synthesis loci (O loci)",
                        col = palt1,
                        cex = 0.8, # Adjust label size
                        radius = 1, # adjust the size
                        xlim = c(-1.5, 1.5) # Adjust x-axis limits to create more space for labels
       )
    })

     #  # Render Trending Threats plot based on the filter data sets
      output$Trending_Threats <- renderPlotly({
        # Check if any categories are selected
        if (length(input$categories) == 0) {
          
          # Return an empty plot if no categories are selected
          return(NULL)
        }
        
        data <- subset(filter_data(), select = c("Yersiniabactin", "Colibactin", "Aerobactin",
                                                      "Bla_ESBL_acquired", "Bla_Carb_acquired", "Tet_acquired", 
                                                      "Flq_mutations","Year"))
       
        # Calculate the frequency count of each categorical variable
        data_summary <- data %>%
          select(all_of(c("Year", input$categories))) %>%
          group_by(Year) %>%
          summarise(across(everything(), ~length(unique(.))))
        
        # Calculate the percentage of each categorical variable
        data_summary <- data_summary %>%
          mutate(across(-Year, ~./sum(.)*100))
        
        # Reshape the data to long format
        data_summary_long <- data_summary %>%
          tidyr::pivot_longer(cols = -Year)
        
        # Check reshaped data set
        # print(head(data_summary_long))
        
        # Generate the line plot
        n <- ggplot(data_summary_long, aes(x = Year, y = value, color = name)) +
              geom_line() +
              geom_point() +
              scale_color_manual(values=palt1) +
              labs(x = "Year", y = "Prevalance", title = "", color = "Legend") +
              theme_classic()

        # Plot Model type vs accuracy with PLOTLY ####
        Trending_Threats_plot <- plotly::ggplotly(n)
        Trending_Threats_plot

      })

    
    # Return the reactive that yields the data frame
    return(filter_data)
  }
  
  )
}