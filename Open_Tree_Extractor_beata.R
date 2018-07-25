library("shiny")
library("ape")
library("rotl")

# Define UI

ui <- fluidPage(
  
  titlePanel("Open Tree Extractor"),
  
  fileInput('target_upload', 'Choose csv file to upload',
            accept = c(
              'text/csv',
              'text/comma-separated-values',
              '.csv'
            )),
  tags$hr(),
  checkboxInput("header", "Header", TRUE),
  
  radioButtons("separator","Separator: ",choices = c(",", ";", ":"), selected=",", inline=TRUE),
  
  mainPanel(
    tabsetPanel(
      tabPanel("Table", dataTableOutput("table"), downloadButton("DownloadTable", "Download Table")),
      tabPanel("Tree_Plot", plotOutput("plot"), downloadButton("DownloadTree", "Download Tree"))
    )
  )
)

# Define server logic
server <- shinyServer(function(input, output) {
  output$table <- renderDataTable({
    inFile <- input$target_upload
    if (is.null(inFile))
      return(NULL)
    df <- read.csv(inFile$datapath,
                   header = input$header,
                   sep = input$separator, stringsAsFactors=FALSE, quote="")
    names(df) <- c("Common_Name", "Scientific_Name")
    species <- tnrs_match_names(names = df$Scientific_Name)
    species
  })
  
  #Tree plot
  
  output$plot <-renderPlot({
    inFile <- input$target_upload
    if (is.null(inFile))
      return(NULL)
    df <- read.csv(inFile$datapath,
                   header = input$header,
                   sep = input$separator, stringsAsFactors=FALSE, quote="")
    names(df) <- c("Common_Name", "Scientific_Name")
    species <- tnrs_match_names(names = df$Scientific_Name)
    
    tree <- tol_induced_subtree(ott_ids=species$ott_id, label_format = "name")
    plot.phylo(tree)
    
  })
  
  # Downloadable selected file ----
  output$downloadData <- downloadHandler(
    
    filename = function() {
      paste("Request_subset_OTL_", Sys.Date(), ".tre", sep = "")
    },
    content = function(file) {
      write.tree(tree, file)
    }
  )
  
})
 # Run the application 
shinyApp(ui = ui, server = server)