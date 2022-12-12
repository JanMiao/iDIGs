library(shinydashboard)
library(shiny)
library(rintrojs)
library(shinyBS)
library(plotly)
library(waiter)
library(ggsci)
library(shinycssloaders)
library(ggplot2)
library(ggpubr)
library(shinyjs)
library(stringr)
library(shinyWidgets)
library(tidyverse)
library(shinyalert)
library(data.table)
library(leaflet)
library(leaflet.extras) 
library(billboarder)

#setwd("D:\\Jan\\ShinyApp\\iPIG_V2\\iPIGsV2_test")
setwd("/disk195/zz/shinyApp/iPIGs_en")
#plink<-"D:/plink"
plink<-"/disk191/miaoj/software/plink"

options(shiny.maxRequestSize = 10 * 1024^3)
task<-round(as.numeric(Sys.time()))
data_path <- paste0("temp/",task)
#dir.create(data_path,showWarnings = F, recursive = T)
carto = "http://a.basemaps.cartocdn.com/light_nolabels/{z}/{x}/{y}.png"

vec_lst<-read.table(paste0("data/plsda.txt"),h=T)
metadata<-readRDS("data/metadata.rds")
map_info<-metadata$Breed
sampleInfo <- metadata$Sample
rownames(sampleInfo) = NULL
map_info$Latitude <- jitter(map_info$Latitude, factor = 0.0001)
map_info$Longitiude <- jitter(map_info$Longitiude, factor = 0.0001)
rgn<-c(73.60226, 15.77538, 134.77258, 53.56944) # China view region
carto = "http://a.basemaps.cartocdn.com/light_nolabels/{z}/{x}/{y}.png"
#map_info<-map_info[complete.cases(map_info),]
cols = c('#b2df8a','#1f78b4','#e31a1c','#33a02c','#fb9a99','#a6cee3','#fdbf6f',
         '#ff7f00','#cab2d6','#6a3d9a','#ffff99',
         '#b2df8a','#1f78b4','#e31a1c','#33a02c','#fb9a99','#a6cee3','#fdbf6f',
         '#ff7f00','#cab2d6','#6a3d9a','#ffff99')
beatCol <- colorFactor(palette = cols, map_info$Country)

breeds_name<-c("China_Local","Local", "Commercial", "WildBoar")
# col_vec<-pal_locuszoom()(7)[c(1,3,2,5:7,4)][1:length(breeds_name)]
col_vec<-c("#1f77b4", "#ff7f0e", "#2ca02c", "#d62728")
col_mapping<-map(breeds_name,function(x)col_vec[which(breeds_name==x)])
names(col_mapping)<-breeds_name
info<-metadata$Sample

### ui
homePanel <- homePage("self", metadata=metadata, map_info=map_info)
refPanel <- repositoryPage("self")
procPanel <- BIPage(id="bi", title="BI", jobname="Breed Identification analysis", metadata)
panelPanel<-panelPage("panel", metadata)
procPanel2 <- BIPage(id="gbc", title="GBC", jobname="Genome Breed Compositon analysis", metadata)
document_panel <- documentPage("self")

ui<-tagList(
  navbarPage(
    title=span(
      strong('i',style='color:#d62728',.noWS="outside"),
      strong('P',style='color:#1f78b4',.noWS="after"),
      strong('I',style='color:#33a02c',.noWS="after"),
      strong('G',style='color:#ff7f00',.noWS="after"),
      strong('s',style='color:#cab2d6',.noWS="after"),
      style='font-size:30px'
    ),
    
    windowTitle = 'iPIGs',
    position = 'fixed-top',
    header = tagList(
      useShinydashboard(),
      useShinydashboardPlus(),
      use_waiter(),
      useShinyjs(),
      setBackgroundColor(color = c("white")),
      tags$head(tags$style(
        HTML(
          ".shiny-output-error{
          color: red;
          }
          .btn-default{
          color: #000;
          background-color: #eee;
          border: 1px solid #ccc;
          }
          .btn-default:hover, .btn-default:focus{
          color: #000;
          background-color: #eee;
          border: 1px solid #ccc;
          }
          #submit_bttn{
          color:#fff;
          }
          .form-control[disabled], .form-control[readonly], fieldset[disabled] .form-control {
          background-color: #fff;
          border: 1px solid #ccc;
          }
          .bootstrap-select>.dropdown-toggle{
          background-color: #fff;
          }
          
          .document p{
          text-indent: 2em;
          line-height: 25px;
          }
          
          .document li{
          margin-left: 3em;
          line-height: 25px;
          }
          
          ")
        ),
        tags$link(rel="stylesheet", type="text/css", href="styles.css")
        )
      
      ),
    #inverse = TRUE,
    theme = shinythemes::shinytheme(theme = "flatly"),
    # theme="mytheme.css",
    
    homePanel,
    refPanel,
    procPanel,
    panelPanel,
    #procPanel2,
    document_panel
      ),
  tags$hr(),
  tags$footer(includeHTML("www/footer.html"), class = "footer")
      )



server<-function(input,output,session){
  
  # Home Panel ------------------------------------------------------------------
  output$map<-renderLeaflet({
    map_info %>%
      leaflet() %>% addTiles(carto) %>% clearShapes() %>%
      #addProviderTiles("CartoDB", group = "Carto") %>%
      flyToBounds(rgn[1], rgn[2], rgn[3], rgn[4])%>%
      #addPolygons(data = mapdata, fill = F, color = 'red') %>%
      setView(lng = 112.939003, lat = 28.228001, zoom = 2) %>%
      addCircleMarkers(
        lng=~Longitiude,
        lat=~Latitude,
        stroke = FALSE, 
        fillOpacity = 0.4,
        color=~beatCol(Country),
        radius = ~log(Sample)*3,
        # icon = greenLeafIcon,
        popup=~paste0("Breed: ", Breed_FullName, "<br/>", 
                      "location: ", location, "<br/>",
                      "Sample size: ", format(Sample, big.mark=" ",scientific=FALSE))) %>%
      addResetMapButton() 
    
  })
  
  ## PCA plot ----------------------
  pca_plot<-reactive({
    waiter<-Waiter$new(id='pca_plot',html = spin_fading_circles())
    waiter$show()
    on.exit(waiter$hide())
    
    
    pca_plot<-ggplot(vec_lst,aes(x=comp1,y=comp2,colour=type,shape=type, text=breed))+
      geom_point(size=6,alpha=0.8)  +
      #scale_colour_locuszoom() +
      scale_colour_manual(values = col_vec)+
      scale_shape_manual(values=c(17,15,19,21)) +
      theme_pubr()+
      #theme_bw()+
      theme(legend.title=element_blank(),
            legend.position = c(0.1,0.2),
            legend.spacing.y = unit(2.0, 'cm'),
            text = element_text(size=20),
            axis.title.x = element_text(vjust=0,size=20,color='black',margin=margin(t=10)),
            axis.title.y = element_text(margin=margin(r=10)),
            axis.text = element_text(size=20,color='black'),
            axis.title = element_text(size=20,color='black'),
            legend.text = element_text(size=20,color='black'),
            legend.background = element_rect(fill=alpha('white', 0.0))) +
      xlab('Component 1')+ylab('Component 2')+ guides(colour=guide_legend(
        keywidth=0.1,
        keyheight=0.5,
        default.unit="inch")
      )
    
    fig <- ggplotly(pca_plot) %>%
      layout(
        legend = list(
          orientation = "h",   # show entries horizontally
          xanchor = "center",  # use center of legend as anchor
          x = 0.5,y=-0.18),
        font=list(
          family = "sans serif",
          size = 15
        ))
    
    return(fig)
  })
  output$rep_pca<-renderPlotly(
    pca_plot()
  ) %>%
    bindCache(pca_plot())
  
  
  ## Info table ---------------------
  
  output$info_table <- DT::renderDataTable(
    sampleInfo,
    options = list(pageLength = 15,
                   rownames=F,
                   language = list(search = "Filter with keyword:"),
                   initComplete = JS(
                     "function(settings, json) {",
                     "$(this.api().table().header()).css({'background-color': '#4a5a6a', 'color': '#fff'});",
                     "$(this.api().table().header()).css({'font-size': '95%'});",
                     "$(this.api().table().body()).css({'font-size': '90%'});",
                     "}"
                   ))
  )
  
  output$breed_table <- DT::renderDataTable(
    metadata$Breed,
    options = list(pageLength = 15,
                   #lengthChange = FALSE,
                   language = list(search = "Filter with keyword:"),
                   initComplete = JS(
                     "function(settings, json) {",
                     "$(this.api().table().header()).css({'background-color': '#4a5a6a', 'color': '#fff'});",
                     "$(this.api().table().header()).css({'font-size': '95%'});",
                     "$(this.api().table().body()).css({'font-size': '90%'});",
                     "}"
                   ))
  )
  
  AnalysisServer("bi",jobName="Breed Identification analysis", plink="/disk191/miaoj/software/plink")
  PanelServer("panel")
  AnalysisServer("gbc",jobName="Genome Breed Compositon analysis", plink="/disk191/miaoj/software/plink")
}


#################### modules #############################

### server for panel
PanelServer <- function(id){
  moduleServer(id, function(input, output, session) {
    logDir="/disk195/zz/shinyApp/iPIGs_en/log/"
    fullName2breed = setNames(metadata$Breed$Breed, metadata$Breed$Breed_FullName)
    ### some functions
    submit_confirm <- modalDialog(
      "Are you sure to submit? Click\"Confirm\" to submit and the page will be refreshed",
      title = "Submit?",
      footer = tagList(
        actionButton(NS(id,"cancel"), "Cancel"),
        actionButton(NS(id,"ok"), "Confirm", class = "btn btn-danger")
      )
    )
    
    isValidEmail <- function(x) {
      grepl("\\<[A-Z0-9._%+-]+@[A-Z0-9.-]+\\.[A-Z]{2,}\\>", as.character(x), ignore.case=TRUE)
    }
    
    email_check <- modalDialog(
      "Invalid E-mail address，please check again!",
      title = "Invalid E-mail address",
      footer = tagList(
        actionButton(NS(id,"cancel"), "Cancel", class = "btn btn-warning")
      )
    )
    
    check<-function(content,title){
      return(
        modalDialog(
          content,
          title = title,
          footer = tagList(
            actionButton(NS(id,"cancel"), "Cancel", class = "btn btn-warning")
          )
        )
      )
    }
    ## 提交任务 -----------------------------------------
    observeEvent(input$submit_bttn,{
      if(!isValidEmail(input$email)){
        showModal(email_check)
      }else{
        showModal(submit_confirm)
      }
    })
    
    observeEvent(input$cancel,{
      removeModal()})
    
    observeEvent(input$ok,{
      dir.create(data_path,showWarnings = F, recursive = T)
      res <-list()
      res$method<-"Panel Design for breed identification"
      #res$ref_breed<-input$reference_breed
      res$ref<-input$gbc_select_ref
      res$Anomaly<-input$ifAnomaly
      res$dir<-data_path
      res$email<-input$email
      res$task<-task
      res$breedUsed <- fullName2breed[input$breed_used]
      res$panelNum<-input$panelNum
      save(res,file=paste0(data_path,"/par.RData"))
      showNotification("Job submitted! We will send the results to your email address as soon as 
                       the analysis is done. The page will be reset in 5 seconds.",
                       type="message")
      removeModal()
      
      Sys.sleep(1)
      showNotification("4 seconds", type="error")
      Sys.sleep(1)
      showNotification("3 seconds", type="error")
      Sys.sleep(1)
      showNotification("2 seconds", type="error")
      Sys.sleep(1)
      showNotification("1 second", type="error")
      Sys.sleep(1)
      refresh()
      
      con_out<-file(paste0(data_path,"/job_submitter.pbs"), open = "w")
      con<-file("job_submitter.pbs", open = "r")
      while (length(oneLine <- readLines(con, n = 1, warn = FALSE)) > 0) {
        if(str_detect(oneLine,"#PBS -N")){
          oneLine<-str_replace(oneLine, "job_name", 
                               paste0("GBCShiny",task))
        }
        if(str_detect(oneLine,"Rscript gbc_job_submit.R")){
          oneLine<-str_replace(oneLine, "gbc_job_submit.R", "analysisScript/Panel.R")
          oneLine<-str_replace(oneLine, "path", data_path)
        }
        if(str_detect(oneLine,"#PBS -e")){
          oneLine<-str_replace(oneLine, "error", 
                               paste0(logDir,task,".e"))
        }
        if(str_detect(oneLine,"#PBS -o")){
          oneLine<-str_replace(oneLine, "output", 
                               paste0(logDir,task,".o"))
        }
        writeLines(oneLine,con_out)
      }
      
      close(con)
      close(con_out)
      # system("cd /disk195/zz/shinyApp/PorkTrace")
      system(paste0("qsub ",data_path, "/job_submitter.pbs"))
      #system(paste0("nohup Rscript GS_job_submit.R ",data_path, " > log/gs_job", task, ".log 2>&1 &"))
      task<<-as.numeric(Sys.time())
      data_path <<- paste0("temp/",task)
    })
  })
}

### shiny module for BI and GBC analysis
AnalysisServer <- function(id, jobName, plink){
  moduleServer(id, function(input, output, session) {
    logDir="/disk195/zz/shinyApp/iPIGs_en/log/"
    fullName2breed = setNames(metadata$Breed$Breed, metadata$Breed$Breed_FullName)
    if (jobName == "Breed Identification analysis") {
      jobScript = "BI.R"} else {
        jobScript = "GBC.R"}
    ### some functions
    submit_confirm <- modalDialog(
      "Are you sure to submit? Click\"Confirm\" to submit and the page will be refreshed",
      title = "Submit?",
      footer = tagList(
        actionButton(NS(id,"cancel"), "Cancel"),
        actionButton(NS(id,"ok"), "Confirm", class = "btn btn-danger")
      )
    )
    
    isValidEmail <- function(x) {
      grepl("\\<[A-Z0-9._%+-]+@[A-Z0-9.-]+\\.[A-Z]{2,}\\>", as.character(x), ignore.case=TRUE)
    }
    
    email_check <- modalDialog(
      "Invalid E-mail address，please check again!",
      title = "Invalid E-mail address",
      footer = tagList(
        actionButton(NS(id,"cancel"), "Cancel", class = "btn btn-warning")
      )
    )
    
    check<-function(content,title){
      return(
        modalDialog(
          content,
          title = title,
          footer = tagList(
            actionButton(NS(id,"cancel"), "Cancel", class = "btn btn-warning")
          )
        )
      )
    }
    
    file_check <- check(content="Please be sure that you have uploaded genotype (Plink)!",
                        title = "Missing files")
    
    ## 文件上传 -------------------------
    ### 基因型文件
    file_pass<-list()
    gen_dat<-reactive({
      req(input$gen_upload)
      waiter<-Waiter$new(id='gen_summary',html = spin_fading_circles())
      waiter$show()
      on.exit(waiter$hide())
      ext<-tools::file_ext(input$gen_upload$name)
      path<-unique(dirname(input$gen_upload$datapath))
      
      # Rename files with their ordinary names
      full_name<-paste0(dirname(input$gen_upload$datapath),"/",input$gen_upload$name)
      file.rename(input$gen_upload$datapath,full_name)
      
      # file names with path
      name<-unique(tools::file_path_sans_ext(full_name))
      
      # file names without path
      file_name_cln<-basename(input$gen_upload$name)
      name_cln<-unique(tools::file_path_sans_ext(file_name_cln))
      
      # Supposed to be these files
      supp_files<-unlist(map(name,~paste0(.x,c(".bed",".bim",".fam"))))
      supp_files_cln<-unlist(map(name_cln,~paste0(.x,c(".bed",".bim",".fam"))))
      
      # Check file extension
      file_pass$gen_file_err_ext<<-TRUE
      file_err_ext<-!ext%in%c("bim","bed","fam")
      if(any(file_err_ext)){
        file_pass$gen_file_err_ext<<-FALSE
        validate(paste0("[ERROR:] Invalid file type (",
                        file_name_cln[file_err_ext],
                        "); Please upload .bed, .bim and .fam files!"))
      }
      file_pass$gen_file_err_ext<<-TRUE
      
      # Check if some files are supplied in supposed file list
      file_err<-!full_name%in%supp_files
      file_pass$gen_file_redun<<-TRUE
      if(any(file_err)){
        file_pass$gen_file_redun<<-FALSE
        validate(paste0("[ERROR:] Invalid files (",
                        file_name_cln[file_err],
                        "); Please check!"))
      }
      file_pass$gen_file_redun<<-TRUE
      
      # Check if some files that are out of supposed file list
      file_not_exist<-!supp_files%in%full_name
      file_pass$gen_file_exist<<-TRUE
      if(any(file_not_exist)){
        file_pass$gen_file_exist<<-FALSE
        validate(paste0("[ERROR:] Some files are missing (",
                        supp_files_cln[file_not_exist],
                        ") or you supplied wrong file names, please check!"))
      }
      file_pass$gen_file_exist<<-TRUE
      
      # Check individuals
      fam<-data.table::fread(paste0(path,"/", name_cln[1], ".fam"))
      file_pass$gen_fam_num<<-TRUE
      file_pass$gen_fam_row<<-TRUE
      if(length(name)>1){
        for (i in 2:length(name)){
          fam2<-data.table::fread(paste0(path,"/", name_cln[i], ".fam"))
          if(length(fam2[[1]])!=length(fam[[1]])){
            file_pass$gen_fam_num<<-FALSE
            validate("[ERROR:] The numbers of individuals across all fam files must be the same!")
          }else if(!all(fam2[[1]]==fam[[1]]) | !all(fam2[[2]]==fam[[2]])){
            file_pass$gen_fam_row<<-FALSE
            validate("[ERROR:] The rows of all fam files must be the same!")
          }
        }
      }
      file_pass$gen_fam_num<<-TRUE
      file_pass$gen_fam_row<<-TRUE
      
      # Plink check format
      num_of_variant<-rep(NA,length(name))
      num_of_individual<-rep(NA,length(name))
      dir.create(data_path,showWarnings = F, recursive = T)
      file_pass$gen_plink_format<<-TRUE
      for (i in 1:length(name)){
        system(paste0(plink, " --bfile ",
                      name[i]," --out ",data_path,"/", name_cln[i], " --make-bed"))
        num_of_variant[i] <- nrow(data.table::fread(paste0(path,"/", name_cln[i], ".bim")))
        num_of_individual[i] <- nrow(data.table::fread(paste0(path,"/", name_cln[i], ".fam")))
      }
      
      res<-list()
      res$summary<-data.frame(Files=name_cln,No_ind=num_of_individual,No_var=num_of_variant)
      res$ind<-fam[[1]]
      file_pass$gen_plink_format<<-TRUE
      return(res)
    })
    
    output$gen_summary<-renderUI({
      req(input$gen_upload)
      div(paste0("There are a total of ", sum(gen_dat()$summary$No_var)), "markders and ",
          length(gen_dat()$ind), "individuals")
    })
    
    ## 提交任务 -----------------------------------------
    observeEvent(input$submit_bttn,{
      if(!isValidEmail(input$email)){
        showModal(email_check)
      }else if(is.null(input$gen_upload)){
        showModal(file_check)
      }else if(any(!unlist(file_pass))){
        showModal(check(content="There exist some errors for files!", title="File error"))
      }else{
        showModal(submit_confirm)
      }
    })
    
    observeEvent(input$cancel,{
      removeModal()})
    
    observeEvent(input$ok,{
      res<-list()
      res$method<-jobName
      res$ref<-input$gbc_select_ref
      res$Anomaly<-input$ifAnomaly
      res$dir<-data_path
      res$gen_file<-input$gen_upload$name
      res$email<-input$email
      res$task<-task
      res$breedUsed <- fullName2breed[input$breed_used]
      res$num_of_snps<-sum(gen_dat()$summary$No_var)
      res$size<-length(gen_dat()$ind)
      save(res,file=paste0(data_path,"/par.RData"))
      showNotification("Job submitted! We will send the results to your email address as soon as 
                       the analysis is done. The page will be reset in 5 seconds.",
                       type="message")
      removeModal()
      
      Sys.sleep(1)
      showNotification("4 seconds", type="error")
      Sys.sleep(1)
      showNotification("3 seconds", type="error")
      Sys.sleep(1)
      showNotification("2 seconds", type="error")
      Sys.sleep(1)
      showNotification("1 second", type="error")
      Sys.sleep(1)
      refresh()
      
      con_out<-file(paste0(data_path,"/job_submitter.pbs"), open = "w")
      con<-file("job_submitter.pbs", open = "r")
      while (length(oneLine <- readLines(con, n = 1, warn = FALSE)) > 0) {
        if(str_detect(oneLine,"#PBS -N")){
          oneLine<-str_replace(oneLine, "job_name", 
                               paste0("GBCShiny",task))
        }
        if(str_detect(oneLine,"Rscript gbc_job_submit.R")){
          oneLine<-str_replace(oneLine, "gbc_job_submit.R", paste0("analysisScript/",jobScript))
          oneLine<-str_replace(oneLine, "path", data_path)
        }
        if(str_detect(oneLine,"#PBS -e")){
          oneLine<-str_replace(oneLine, "error", 
                               paste0(logDir,task,".e"))
        }
        if(str_detect(oneLine,"#PBS -o")){
          oneLine<-str_replace(oneLine, "output", 
                               paste0(logDir,task,".o"))
        }
        writeLines(oneLine,con_out)
      }
      
      close(con)
      close(con_out)
      # system("cd /disk195/zz/shinyApp/PorkTrace")
      system(paste0("qsub ",data_path, "/job_submitter.pbs"))
      #system(paste0("nohup Rscript GS_job_submit.R ",data_path, " > log/gs_job", task, ".log 2>&1 &"))
      task<<-as.numeric(Sys.time())
      data_path <<- paste0("temp/",task)
    })
  })
}


#################### modules #############################
shinyApp(ui, server)


