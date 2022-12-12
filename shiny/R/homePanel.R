homePage <- function(id, metadata=metadata, map_info=map_info){
  tabPanel(
    title = "Home",
    div(class = "inlay", style = "height:80px;width:100%;background-color: white;"),
    # h1("Genomic Breed Composition Shiny ",style="margin-left:8.33%"),
    p(strong('i',style='color:#d62728',.noWS="after"),'dentification of ',
      strong('P',style='color:#1f78b4',.noWS="after"),
      strong('I',style='color:#33a02c',.noWS="after"), 'g',
      strong('G',style='color:#ff7f00',.noWS="after"),'enetic resource',
      strong('s',style='color:#6a3d9a',.noWS="before"),'(',
      strong('i',style='color:#d62728',.noWS="outside"),
      strong('P',style='color:#1f78b4',.noWS="after"),
      strong('I',style='color:#33a02c',.noWS="after"),
      strong('G',style='color:#ff7f00',.noWS="after"),
      strong('s',style='color:#6a3d9a',.noWS="after"),')',
      style='font-size:40px;
      margin-left:8.33%;
      font-family: Sans-serif;'
    ),
    tags$div(tags$h3(
      strong('i',style='color:#d62728',.noWS="outside"),
      strong('P',style='color:#1f78b4',.noWS="after"),
      strong('I',style='color:#33a02c',.noWS="after"),
      strong('G',style='color:#ff7f00',.noWS="after"),
      strong('s',style='color:#6a3d9a',.noWS="after"),
      " v0.1 includes a SNP dataset of a wide range of pig breeds. The dataset are used for pig breed idetification and panel design. See How to for detail functions."),
      style="margin-left:8.33%;margin-bottom:50px;"
    ),
    
    
    # Map and info boxes
    fluidRow(
      column(
        width = 11, offset = 1,
        fluidRow(
          column( width = 8,
                  
                  # Sasmple distribution map ------------------------------------------------------
                  box(title = "Reference Samples Distribution",
                      status = "primary",
                      solidHeader = TRUE,
                      width = NULL,
                      leafletOutput("map", height = 410))
          ),
          
          column( width = 3,
                  tags$head(tags$style(HTML(".small-box {width:110%;
                                            margin-bottom:40px;height:130px;}"))),
                  # valueBox2 ----------------------------------------------------------------
                  fluidRow(
                    valueBox(
                      paste0(nrow(metadata$Sample)," samples"),
                      subtitle = "Genome-wide information were used",
                      icon = icon("chart-pie"),
                      color = "red",
                      width = 12
                    )),
                  
                  # valueBox1 ----------------------------------------------------------------
                  fluidRow(
                    valueBox(paste0(dim(metadata$Breed)[1], " pig breeds"),
                             subtitle="Covering main pig breeds across the world",
                             icon=icon("map-marker"),
                             color='yellow',width=12)
                  ),
                  
                  
                  
                  
                  # valueBox4 ----------------------------------------------------------------
                  fluidRow(
                    valueBox(
                      paste0(length(unique(map_info$location))," sites"),
                      subtitle = paste0("Samples were distributed in ", 
                                        length(unique(map_info$location)) , 
                                        " sites of ",length(unique(map_info$Country)),
                                        " countries"),
                      icon = icon("virus"),
                      color = "olive",
                      width = 12
                    ))
                  ))
      )
    )
    )
}
  
