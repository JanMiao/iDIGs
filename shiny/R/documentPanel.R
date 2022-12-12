documentPage <- function(id){
  tabPanel(
    title = "How to",
    
    div(class = "inlay", style = "height:100px;width:100%;background-color: white;"),
    
    fluidRow(
      column(
        width = 6, offset = 3,
        box(
          title = NULL, status = "danger", width = NULL,
          
          tags$div(
            class = "document",
            div(tags$b("Introduction",style="font-size:20px;"),style="margin-bottom:10px;"),
            tags$p("Welcome to identification of PIg Genetic resources (iPIGs) v1.0 !"),
            tags$p("iPIGs is a tool for global pig breed identification. Our database has 3,622 samples and 125 breeds worldwide. 
                   Now, iPIGs has two mian functions: pig breed identification and panel design for specific breeds identification."),
            tags$ol(
              tags$li("Breed Identification"),
              tags$li("Panel Design")),
            div(tags$b("Function",style="font-size:20px;"),style="margin-bottom:10px;"),
            h4("1. Breed Identification"),
            tags$p("Using our database to recognize users' pig breed"),
            tags$ul(
              tags$li("Step1: Choose the version of your reference pig genome (10.2 or 11.1) and upload your PLINK binary file.",
                      tags$span("No missing SNPs are allowed",style="font-weight:700;"),
                      ". You can perform imputation using", 
                      tags$a("PHARP", href="http://alphaindex.zju.edu.cn/PHARP/index.php/"), " or Beagle."),
              tags$li("Step2: Choose reference breeds (",tags$span("Optional",style="font-weight:700;"),
                      "). If not chose, all breeds in our database will be used as reference."),
              tags$li("Step3: Input your E-mail address and submit. The results will send to the E-mail address automatically."),
              tags$li("Results:  A table of two columns. The first column is the input sampleID. The second column is the putative breed label. ")),
            h4("2. Panel Design"),
            tags$p("Designing a small panel to distinguish some specific breeds from each other."),
            tags$ul(
              tags$li("Step1: Choose the version of your reference pig genome (10.2 or 11.1) and the SNP number in the panel. 
                      Note that, the SNP number should be less than 10,000."),
              tags$li("Step2: choose breeds you want to distinguish from. You can input multiple breed IDs or a single breed ID. 
                    If you input multiple breed IDs, markers that can distinguish all input breeds from each other will be selected. 
                    If you input a single breed ID, Markers that can distinguish your input breed from all other breeds in our database will be selected."),
              tags$li("Step3: Input your E-mail address and submit. The results will send to the E-mail address automatically."),
              tags$li("Results: 1) marker ID used in panel and 2) performance estimation of the panel using 5-fold CV. 
                    The preformance can be imporved by increasing the SNP number.")
              ),
            div(tags$b("FAQ",style="font-size:20px;"),style="margin-bottom:10px;"),
            tags$p("Q: What type of data may i uploaded when using iPIGs?"),
            tags$p("A: We recommand commercial SNP Chips and WGS."),
            tags$p("Q: How could i upload big PLINK binary files, such as SNPs from WGS data?"),
            tags$p("A: Big data (file size > 1 GB), such as WGS data, is hardly upload. Thus, you can download our ",tags$a("markerID", href="http://alphaindex.zju.edu.cn/ALPHADB/download.html") 
                 ," and only extract overlapped markers to upload."),
            tags$pre(
              "
# if using LINUX
chmod +x MakeSNPidL
# Normalize your markerID and copy files
MakeSNPidL file.bim file.tmp.bim
cp file.bed file.tmp.bed
cp file.fam file.tmp.fam
# extract overlapped SNPs
plink --bfile file.tmp --extract MarkerID_11.txt --make-bed --out upload",
              lang="shell"),
            div(tags$b("Contact",style="font-size:20px;"),style="margin-bottom:10px;"),
            tags$p("Any questions, bug reports and suggestions can be posted to E-mail:"),
            tags$p(tags$a("zhe_zhang@zju.edu.cn", href="zhe_zhang@zju.edu.cn"), "or",
                   tags$a("miaojian@zju.edu.cn", href="miaojian@zju.edu.cn"))
            )
            )
            )
    )
    )
}
