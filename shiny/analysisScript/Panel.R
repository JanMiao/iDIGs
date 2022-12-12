### R script for breed identification
library(data.table)
library(tidyverse)
library(emayili)
library(GBC)
library(caret)
library(e1071)
# analysis script
source("/disk195/zz/shinyApp/iPIGs_en/analysisScript/BreedAssign_PLSDA.R")

# some default parameters, maybe need change
report_path<-"/disk195/zz/shinyApp/iPIGs_en"
data_dir<-"/disk195/zz/shinyApp/iPIGs_en/data"
plink<-"/disk191/miaoj/software/plink"
plink_dir<-"/disk191/miaoj/software/"
from_email<-"zzsjtu1988@163.com"
rdsData <- "/disk195/zz/shinyApp/iPIGs_en/data/metadata.rds"

# breed label to breed full name
breedDF = readRDS(rdsData)$Breed

# work path
options <- commandArgs(trailingOnly = TRUE)
path = options[1]
setwd(path)

# load input parameters
load("par.RData")
to_email<-res$email
refs<-list(
  Suscrofa11.1="../../data/REF_data11.rds",
  Suscrofa10.2="../../data/REF_data10.rds"
)
task<-res$task
method<- res$method
breedUsed <- res$breedUsed
nSNP <- res$panelNum

# begin analysis
panel = Panel(breed=breedUsed, nSNP=nSNP, REF=refs[[res$ref]])
snp = unlist(strsplit(panel$marker, "_"))
df = data.frame(snp=panel$marker)
df = separate(df, snp, c("snp","no"), sep="_") %>% 
	separate(snp, c("Chr", "Pos", "RefAllele", "AltAllele"), sep=":")
df = df[,-5]
report = df
fwrite(report,"report.txt",row.names=F,quote=F,sep="\t")

# breed information
breedinfo = breedDF[breedDF$Breed %in% breedUsed,]

# html report
genotype_check_pass<-TRUE
# marker used for analysis
CVacc = round(panel$accuracy *100, 2)

data_summary<-data.frame(
  Item=c("Breed you selected", "Panel density", "Prediction accuracy from 5-fold CV"),
  Content=c(paste0(breedUsed, collapse = ","), nSNP, paste0(CVacc,"%"))
)

con<-file(paste0(report_path,"/report_templateP.R"), open = "r")
con_out<-file("report.R", open = "w")

data_summary_entry<-"The basic parameters you selected and the performance of the panel are as follows:\n#+ echo=FALSE\nknitr::kable(data_summary)"
model_res_entry<-"**The report will show results for at most 100 SNPs. Note that you can find all SNPs in supplementary files.**
#'
#' 
#+ echo=FALSE
report %>% 
  do(head(.,100)) %>%
  DT::datatable(rownames=F,selection=\"multiple\", escape=FALSE, 
                options = list(pageLength = 15,sDom  = '<\"top\">flrt<\"bottom\">ip',
                               lengthChange = FALSE
                             #   initComplete = DT::JS(
                             #   \"function(settings, json) {\",
                             #   \"$(this.api().table().header()).css({'background-color': '#4a5a6a', 'color': '#fff'});\",
                             #   \"$(this.api().table().header()).css({'font-size': '95%'});\",
                             #   \"$(this.api().table().body()).css({'font-size': '90%'});\",
                             #   \"}\"
                             # )
                               ))
"

breed_res_entry<-"The information of breeds used in panel design is as folliows:
#'
#' 
#+ echo=FALSE
breedinfo %>%
  DT::datatable(rownames=F,selection=\"multiple\", escape=FALSE, 
                options = list(pageLength = 15,sDom  = '<\"top\">flrt<\"bottom\">ip',
                               lengthChange = FALSE
                             #   initComplete = DT::JS(
                             #   \"function(settings, json) {\",
                             #   \"$(this.api().table().header()).css({'background-color': '#4a5a6a', 'color': '#fff'});\",
                             #   \"$(this.api().table().header()).css({'font-size': '95%'});\",
                             #   \"$(this.api().table().body()).css({'font-size': '90%'});\",
                             #   \"}\"
                             # )
                               ))
"


while (length(oneLine <- readLines(con, n = 1, warn = FALSE)) > 0) {
  
  if(genotype_check_pass){
    oneLine<-str_replace(oneLine, "data_summary_is_here", 
                         data_summary_entry)
  }else{
    oneLine<-str_replace(oneLine, "data_summary_is_here", 
                         "**[ERROR: ] Something is wrong, data summary message is shown. Please check your data first!**")
  }
  
  
  if(genotype_check_pass){
    oneLine<-str_replace(oneLine, "\\*\\*\\[ERROR: \\] Something is wrong, the calculation is not run and no result is shown. Please check your data first!\\*\\*", 
                         model_res_entry)
  }

  if(genotype_check_pass){
    oneLine<-str_replace(oneLine, "breed_information_is_here", 
                         breed_res_entry)
    
  }else{
    oneLine<-str_replace(oneLine, "breed_information_is_here", " ")
  }
  

  writeLines(oneLine,con_out)
}

close(con)
close(con_out)

system(paste0("cp ",report_path,"/Rmd.css ."))
knitr::spin("report.R",knit=F)
rmarkdown::render("report.Rmd")
system("rm report.Rmd")

# 打包附件
res_files<-"report.txt"

system(paste0("zip iPIGs_res.zip ", paste(res_files,collapse = " ")))

# 发送邮件
Success<-0
Success<-as.character(Success)
mail_text<-switch(Success,
                  "0"="Dear user,\n\nYour run of iPIGs has sucessfully finished. Please find data analysis report and results in the attachment. If you have any question, please do not hesitate to contact zhe_zhang@zju.edu.cn.\n\nBest\n\niPIGs Team",
                  "1"=paste0("Dear user,\n\nSome traits (",paste(error_trait,collapse = ", "),") of your run had not sucessfully finished. Please find data analysis report and results in the attachment. If you have any question, please do not hesitate to contact zhe_zhang@zju.edu.cn.\n\nBest\n\nHEGS Team"),
                  "2"="Dear user,\n\nYour run of HEGS has run into some errors. Please find data analysis report in the attachment and check your data and model once more. If you have any question, please do not hesitate to contact zhe_zhang@zju.edu.cn.\n\nBest\n\nHEGS Team")

email <- envelope(
  to = to_email,
  from = from_email,
  subject = paste0("iPIGs report for job No.", task),
  text = mail_text
) %>% 
  attachment(path="report.html") %>%
  attachment(path="iPIGs_res.zip")


smtp <- server(host = "smtp.163.com",
               port = 25,
               username = from_email,
               password = "zhangzhe3056436")

tryCatch(
  {
    smtp(email, verbose = F)
    setwd("..")
    #system(paste0("rm -rf ", task))
  },

  error=function(cond){
    message("[ERROR:] Emailing the results failed!")
    message("Here's the original error message:")
    message(cond)
    return(NA)
  },
  warning=function(cond){
    message("[WARNING:] Emailing the results caused a wanning!\n")
    message("Here's the original warning message:")
    message(cond)
    return(NULL)
  }
)
