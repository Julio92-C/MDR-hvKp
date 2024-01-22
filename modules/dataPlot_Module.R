
# Module UI function
dataPlotUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    fluidRow(
      column(4,
             box(width = 12, 
                 solidHeader = TRUE, 
                 collapsible=TRUE,
                 status = "black",
                 title = "Choose a Model type:",
                 background = "gray",
                 selectInput(ns("model_type"),
                             label = "Model type",
                             choices = c("protein homolog model",
                                         "rRNA mutation model",
                                         "protein variant model",
                                         "protein wild type model"),
                             selected = "protein homolog model",
                             multiple = TRUE
                 ),
                 h4("Filter data-set by:"),  
                 sliderInput(ns("identity"),
                             label = "Identity",
                             value = 80, min = 0, max = 100
                             
                 ),
                 sliderInput(ns("accuracy"),
                             label = "Accuracy",
                             value = 50, min = 0, max = 100
                             
                 ),
                 sliderInput(ns("coverage"),
                             label = "Coverage",
                             value = 80, min = 0, max = 100
                             
                 ),
                 sliderInput(ns("seq_len"), 
                             label = "Sequence length",
                             value = 1000, min = 100, max = 4000,
                             
                             
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
                          downloadButton(ns("download"), "Download Plot",
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
      
      column(8,
             fluidRow(
               
               valueBoxOutput(ns("Number_species"), width = 4),
               
               
               valueBoxOutput(ns("Number_AMR"), width = 4),  
               
               
               valueBoxOutput(ns("Number_categories"), width = 4)  
               
             ),
             
             fluidRow(
               
               tabBox(width = 12, 
                      
                      tabPanel("Model Types Frequency",
                               plotlyOutput(ns("Model_type_frequency"), height = 500)
                      ),
                      
                      tabPanel("Identity vs Accuracy",
                               plotlyOutput(ns("Identity_vs_Accuracy"), height = 500)
                      ),
                      
                      tabPanel("Coverage vs Seq length",
                               plotlyOutput(ns("Coverage_vs_seq_length"), height = 500)
                      ),
                      
                      tabPanel("Model type vs Identity",
                               plotlyOutput(ns("Model_type_vs_identity"), height = 500)      
                               
                      ),
                      tabPanel("Model type vs Accuracy",
                               plotlyOutput(ns("Model_type_vs_accuracy"), height = 500)
                      ),
                      tabPanel("Model type vs Categories",
                               fluidRow(plotlyOutput(ns("Model_type_vs_categories"), height = 500),)
                               # fluidRow(plotOutput(ns("Categories_Frequency"), height = 500))
                      ),
                      
                      tabPanel("Categories Frequency",
                               plotOutput(ns("Categories_Frequency"), height = 800)

                               )

               )
               
             )
        )
      
    )
  )
  
}

# Module server function
dataPlotServer <- function(id, dataframe){
  stopifnot(is.reactive(dataframe))
  
  moduleServer(id, function(input, output, session){
    
                 # Filter data by seq_len or coverage or identity or accuracy or model type
                 filter_data <- eventReactive(input$plot_button, {
                   validate(
                     need(dataframe(), "Please input a data-sets as a csv file"),
                     need(input$model_type, "Please select a model type")
                   )
                   
                   filter_data <- dataframe() %>%
                     filter(seq_len >= input$seq_len, 
                            coverage >= input$coverage, 
                            identity >= input$identity, 
                            accuracy >= input$accuracy, 
                            model %in% input$model_type)
                   
                 })
                 
                 # Download plots to a PDF file
                 output$download <- downloadHandler(
                   filename = function() {
                     paste("plot-", Sys.Date(), ".pdf", sep="")
                   },
                   content = function(file) {
                     ggsave(file, width = 34, height = 14, units = "cm")
                     
                     dev.off()  
                   }
                 )
                 
                 # Display stats in valueBox
                 output$Number_species <- renderValueBox({
                   valueBox(
                     value = length(unique(filter_data()$taxon)),
                     subtitle = "Number of Species", icon("virus"),
                     color = "aqua"
                     
                   )
                   
                 })
                 
                 output$Number_AMR <- renderValueBox({
                   valueBox(
                     value = length(unique(filter_data()$name)),
                     subtitle = "Number of AMR", icon("dna"),
                     color = "aqua"
                   )
                 })
                 
                 output$Number_categories <- renderValueBox({
                   valueBox(
                     value = length(unique(filter_data()$categories)),
                     subtitle = "Number of Categories", icon("list"),
                     color = "aqua"
                   )
                 })
                 
                 # Render Model type frequency plot based on the filter data sets
                 output$Model_type_frequency <- renderPlotly({
                   # Plot Model type frequency
                   a <- ggplot(filter_data(), aes(x= fct_infreq(model), fill=model))
                   a <- a + geom_bar()
                   a <- a + theme_classic()
                   # m <- m + labs(title = "Model Type Frequency")
                   a <- a + labs(fill = "Model Type")
                   a <- a + theme(legend.justification = "center")
                   a <- a + theme(axis.text = element_text(size = 10.5))
                   a <- a + theme(plot.title = element_text(hjust = 0.5))
                   a <- a + labs(x="Model Type", y="Frequency")
                   a
                   
                   # Plot Model type frequency with PLOTLY ####
                   Model_type_frequency_plot <- plotly::ggplotly(a)
                   Model_type_frequency_plot
                   
                 })
                 
                 # Render Identity vs Accuracy plot based on the filter data sets
                 output$Identity_vs_Accuracy <- renderPlotly({
                   # Plot Identity vs Accuracy
                   p <- ggplot(filter_data(), aes(x=accuracy, y=identity, size=seq_len,
                                                  shape=model, group=name, fill=model))
                   p <- p + geom_point(alpha = 0.8)
                   p <- p + theme_classic()
                   # p <- p + labs(title = "Identity vs. Accuracy")
                   p <- p + scale_size_continuous(name = "Sequence Length")
                   p <- p + labs(x="Accurracy", y="Identity", fill = "Model Type")
                   p <- p + theme(legend.title = element_text(size = 10))
                   p <- p + theme(axis.text = element_text(size = 10.5))
                   #p <- p + theme(legend.position = "none")
                   p

                   # Plot Identity vs Accuracy with PLOTLY ####
                   Identity_vs_Accuracy_plot <- plotly::ggplotly(p)
                   Identity_vs_Accuracy_plot
                 })
                 
                 # Render Coverage vs seq_length plot based on the filter data sets
                 output$Coverage_vs_seq_length <- renderPlotly({
                   # Plot Coverage vs seq_length
                   c <- ggplot(filter_data(), aes(x=seq_len, y=coverage,
                                                  fill=model, group=name ))
                   c <- c + geom_bin_2d()
                   c <- c + theme_classic()
                   # c <- c + labs(title = "Sequence Length vs. Sequence Length")
                   c <- c + theme(plot.title = element_text(hjust = 0.5))
                   c <- c + labs(fill = "Model Type")
                   c <- c + theme(axis.text = element_text(size = 10.5))
                   c <- c + labs(x="Sequence Length (bp)", y="Coverage (%)")
                   c
                   
                   # Plot Coverage vs seq_length with PLOTLY ####
                   Coverage_vs_seq_length_plot <- plotly::ggplotly(c)
                   Coverage_vs_seq_length_plot
                   
                 })
                 
                 # Render Model type vs identity plot based on the filter data sets
                 output$Model_type_vs_identity <- renderPlotly({
                   # Plot Model type vs identity
                   m <- ggplot(filter_data(), aes(x=reorder(model, -identity, FUN = median, na.rm = TRUE), y=identity,
                                                  fill=model))
                   m <- m + geom_boxplot()
                   m <- m + stat_boxplot(geom ='errorbar', width=0.2)
                   m <- m + theme_classic()
                   # m <- m + labs(title = "Model Type vs. Identity")
                   m <- m + labs(fill = "Model Type")
                   m <- m + theme(axis.text = element_text(size = 10.5))
                   m <- m + theme(plot.title = element_text(hjust = 0.5))
                   m <- m + labs(x="Model Type", y="Identity")
                   m
                   
                   # Plot Model type vs identity with PLOTLY ####
                   Model_type_vs_identity_plot <- plotly::ggplotly(m)
                   Model_type_vs_identity_plot
                   
                 })
                 
                 # Render Model type vs accuracy plot based on the filter data sets
                 output$Model_type_vs_accuracy <- renderPlotly({
                   # Plot Model type vs accuracy
                   n <- ggplot(filter_data(), aes(x=reorder(model, -accuracy, FUN = median, na.rm = TRUE), y=accuracy,
                                                  fill=model))
                   n <- n + geom_boxplot()
                   n <- n + stat_boxplot(geom ='errorbar', width=0.2)
                   n <- n + theme_classic()
                   # n <- n + labs(title = "Model Type vs. Accuracy")
                   n <- n + labs(fill = "Model Type")
                   # n <- n + coord_flip()
                   n <- n + theme(axis.text = element_text(size = 10.5))
                   n <- n + theme(plot.title = element_text(hjust = 0.5))
                   n <- n + labs(x="Model Type", y="Accuracy")
                   n
                   
                   # Plot Model type vs accuracy with PLOTLY ####
                   Model_type_vs_accuracy_plot <- plotly::ggplotly(n)
                   Model_type_vs_accuracy_plot
                   
                 })
                 
                 palt_categories <- reactive({
                   categories_count <- table(filter_data()$categories)
                   
                   # Color pallete
                   palt_categories <- paletteer_d("palettesForR::Windows", n = nrow(categories_count))
                   palt_categories
                   
                 })
                 
                 # Plot Model type vs Categories
                 output$Model_type_vs_categories <- renderPlotly ({
                   s <- (filter_data() %>%
                     count(model, categories) %>%
                     group_by(model) %>%
                     mutate(freq = n / sum(n)) %>%
                     ggplot(aes( x = model, y = freq, fill = categories)) +
                     geom_bar(stat = "identity") +
                     theme_classic() +
                     scale_fill_manual(values= palt_categories()) +
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
                   Model_type_vs_categories_plot <- plotly::ggplotly(s)
                   Model_type_vs_categories_plot
                 })
                 
                 output$Categories_Frequency <- renderPlot({
                   # Render a Pie Chart for AMR Categories
                   categories_count <- table(filter_data()$categories)
                   
                   
                   pie_chart <- pie(categories_count, labels = paste0(categories_count, " / ", round(100 * categories_count/sum(categories_count), 2), "%"), 
                                    main="AMR Categories",
                                    col = palt_categories(),
                                    cex = 0.8, # Adjust label size
                                    xlim = c(-1.5, 1.5) # Adjust x-axis limits to create more space for labels
                                    )
                 })
                 
                 # Return the reactive that yields the data frame
                 return(filter_data)
               }
               
             )
}

