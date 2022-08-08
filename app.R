library(magick)
library(dplyr)
library(png)
library(glue)
library(pdftools)

library(shinythemes)
library(shiny)

source('functions.R')

options(shiny.maxRequestSize=30*1024^2)

ui <- fluidPage(
    theme = shinythemes::shinytheme(theme='sandstone'),
    titlePanel('Zine Cutter'),
    wellPanel(
        fluidRow(
            column(6,
                   shiny::selectInput('input',choices = c('Reading','Imposed','One-page'),label = 'Input Form')),
            column(6,shiny::selectInput('output',choices = c('Reading','Imposed','One-page'), label = 'Output Form'))
        ),
        fluidRow(
            column(6,
                   sliderInput('in_dpi',min = 50, max = 300,value = 100,label = 'Input DPI',step = 5)),
            column(6,
                   sliderInput('out_dpi',min = 50, max = 300, value = 100,label = 'Output DPI',step = 5))
        ),
        fileInput('input_file',label = 'Input PDF'),
        downloadButton(outputId = 'convert',label = 'Convert')
    )

)

server <- function(input, output) {
    output$convert =
        downloadHandler(filename = 'converted.pdf',
                        content = function(file){
                            withProgress(message = 'Processing Input',value = 0,{
                                assertthat::assert_that(input$input_file$type == 'application/pdf')
                                images = switch (input$input,
                                    Reading = nothing(input$input_file$datapath,density_in = input$in_dpi),
                                    Imposed = imposed_to_reading(input$input_file$datapath, density_in = input$in_dpi),
                                    `One-Page` = one_page_to_reading(input$input_file$datapath, density_in = input$in_dpi)
                                )
                                setProgress(value = 0.5, message = "Creating PDF")
                                switch(input$output,
                                       Reading = nothing(images,file_out = file,density_out = input$out_dpi),
                                       Imposed = reading_to_imposed(images,file_out = file,density_out = input$out_dpi),
                                       `One-Page` = reading_to_one_page(images,file_out = file,density_out = input$out_dpi))

                            })

                            })

}

shinyApp(ui = ui, server = server)
