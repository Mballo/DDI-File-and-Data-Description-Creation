library(haven)
library(Hmisc)
library(tidyverse)
library(foreign)
library(sdcMicro)
library(car)
library(XML)
library(openxlsx)
library(readxl)

#Read the datasets and process everything before running the code
#All the variables should be labelled and make sure the character variable are well even they are not transformed to factor variables


#Before starting to label and add value labels to a variable, you need to transform the variable in character
data$Gender=as.character(data$Gender)

#To add value labels to a factor variable, you can use the function labelled()
data$Gender=labelled(data$Gender,c(Male="1",Female="2"))

#In case of factor variable use the function as_factor instead of as.factor to transform back to factor
data$Gender=as_factor(data$Gender)

#To label a variable, you can use the function label()
label(data$Gender)="Gender of the respondent"


#Load the XML file
XML_template=xmlTreeParse("Data_Desc_XML.xml")

      ###Changing the codebook and the file description

#First you have to give change the ID of the codebook so it has the same ID than the study description
xmlAttrs(XML_template$doc$children[["codeBook"]])[2]="ID_Study_Description"

#Change the ID of the fileDscr. For a second dataset change F5 to F6, F7 etc. but make sure you change F5 in all the code
xmlAttrs(XML_template$doc$children[["codeBook"]][["fileDscr"]])[1]="F5"

#Change name of the dataset in fileDscr
Name_dataset="Name_dataset"
xmlAttrs(XML_template$doc$children[["codeBook"]][["fileDscr"]])[2]=paste(paste("Data_Desc_XML","Nesstar?Index=0&amp;Name",sep="."),Name_dataset,sep="=")

#Change the fileName tag and should have the same name than 
xmlChildren(XML_template$doc$children[["codeBook"]][["fileDscr"]][["fileTxt"]][["fileName"]])=paste(Name_dataset,"NSDstat",sep=".")

#Change the name of variabe and observations
xmlChildren(XML_template$doc$children[["codeBook"]][["fileDscr"]][["fileTxt"]][["dimensns"]][["caseQnty"]])=NROW(data)

xmlChildren(XML_template$doc$children[["codeBook"]][["fileDscr"]][["fileTxt"]][["dimensns"]][["varQnty"]])=NCOL(data)                        


#Generating the tag for each variable in the data description tag
for(j in 1:NCOL(data)){
  if(class(data[,j])[2]=="numeric"){
    
    location=newXMLNode("location",attrs=c(width=10))
    
    labl=newXMLNode("labl")
    xmlChildren(labl)=label(data[,j])
    
    sumStat1=newXMLNode("sumStat",attrs = c(type="vald"))
    xmlChildren(sumStat1)=sum(table(data[,j]))
    
    sumStat2=newXMLNode("sumStat",attrs = c(type="invd"))
    xmlChildren(sumStat2)=NROW(data)-sum(table(data[,j]))
    
    sumStat3=newXMLNode("sumStat",attrs = c(type="min"))
    xmlChildren(sumStat3)=round(as.numeric(min(data[,j],na.rm=T)),3)
    
    sumStat4=newXMLNode("sumStat",attrs = c(type="max"))
    xmlChildren(sumStat4)=round(as.numeric(max(data[,j],na.rm=T)),3)
    
    sumStat5=newXMLNode("sumStat",attrs = c(type="mean"))
    xmlChildren(sumStat5)=round(mean(as.numeric(data[,j]),na.rm=T),3)
    
    sumStat6=newXMLNode("sumStat",attrs = c(type="stdev"))
    xmlChildren(sumStat6)=round(sd(as.numeric(data[,j]),na.rm=T),3)
    
    varFormat=newXMLNode("varFormat",attrs=c(type="numeric",schema="other"))
    
    if(colnames(data)[j]!="wt"){
      range=newXMLNode("range",attrs=c(min=min(data[,j],na.rm=T),max=max(data[,j],na.rm=T)))
      valrng=newXMLNode("valrng",.children = list(range))
      var=newXMLNode("var",.children = list(location,labl, valrng, sumStat1, sumStat2, sumStat3, sumStat4, sumStat5, sumStat6, varFormat), attrs=c(ID=40+j,name=colnames(data)[j],files="F5",dcml="0",intrvl="contin"))
    }
    else{
      range=newXMLNode("range",attrs=c(UNITS="REAL",min=min(data[,j]),max=max(data[,j])))
      valrng=newXMLNode("valrng",.children = list(range))
      var=newXMLNode("var",.children = list(location,labl, valrng, sumStat1, sumStat2, sumStat3, sumStat4, sumStat5, sumStat6, varFormat), attrs=c(ID=40+j,name=colnames(data)[j],wgt="wgt",files="F5",dcml="0",intrvl="contin"))
    }
  }
  if(class(data[,j])[2]=="factor"){
    location=newXMLNode("location",attrs=c(width=12))
    
    labl1=newXMLNode("labl")
    xmlChildren(labl1)=label(data[,j])
    
    range=newXMLNode("range",attrs=c(min=min(as.numeric(data[,j]),na.rm=T),max=max(as.numeric(data[,j]),na.rm=T)))
    valrng=newXMLNode("valrng",.children = list(range))
    
    sumStat1=newXMLNode("sumStat",attrs = c(type="vald"))
    xmlChildren(sumStat1)=sum(table(data[,j]))
    
    sumStat2=newXMLNode("sumStat",attrs = c(type="invd"))
    xmlChildren(sumStat2)=NROW(data)-sum(table(data[,j]))
    var=newXMLNode("var",.children = list(location,labl1, valrng, sumStat1, sumStat2), attrs=c(ID=40+j,name=colnames(data)[j],files="F5",dcml="0",intrvl="discrete"))
      
      for(k in 1:length(table(data[,j]))){
        catStat=newXMLNode("catStat",attrs=c(type="freq"))
        xmlChildren(catStat)=table(data[,j])[k]
        labl2=newXMLNode("labl")
        xmlChildren(labl2)=levels(data[,j])[k]
        catValu=newXMLNode("catValu")
        xmlChildren(catValu)=k
        catgry=newXMLNode("catgry",.children = list(catValu,labl2,catStat))
        var=addChildren(var,kids=list(catgry))
      }
      varFormat=newXMLNode("varFormat",attrs=c(type="numeric",schema="other"))
      catStat=newXMLNode("catStat",attrs=c(type="freq"))
      xmlChildren(catStat)=NROW(data)-sum(table(data[,j]))
      catValu=newXMLNode("catValu")
      xmlChildren(catValu)="Sysmiss"
      catgry=newXMLNode("catgry",.children = list(catValu,catStat),attrs=c(missing="Y"))
      var=addChildren(var,kids=list(catgry,varFormat))
    }
  if(class(data[,j])[2]=="character"){
    location=newXMLNode("location",attrs=c(width=12))
    
    labl1=newXMLNode("labl")
    xmlChildren(labl1)=label(data[,j])
    
    range=newXMLNode("range",attrs=c(min=min(as.numeric(data[,j]),na.rm=T),max=max(as.numeric(data[,j]),na.rm=T)))
    valrng=newXMLNode("valrng",.children = list(range))
    
    sumStat1=newXMLNode("sumStat",attrs = c(type="vald"))
    xmlChildren(sumStat1)=sum(table(data[,j]))
    
    sumStat2=newXMLNode("sumStat",attrs = c(type="invd"))
    xmlChildren(sumStat2)=NROW(data)-sum(table(data[,j]))
    var=newXMLNode("var",.children = list(location,labl1, valrng, sumStat1, sumStat2), attrs=c(ID=40+j+12,name=colnames(data)[j],files="F5",intrvl="discrete"))
      
      for(k in 1:length(table(data[,j]))){
        catStat=newXMLNode("catStat",attrs=c(type="freq"))
        xmlChildren(catStat)=table(data[,j])[k]
        labl2=newXMLNode("labl")
        xmlChildren(labl2)=as.data.frame(table(data[,j]))[k,1]
        catValu=newXMLNode("catValu")
        xmlChildren(catValu)=as.data.frame(table(data[,j]))[k,1]
        catgry=newXMLNode("catgry",.children = list(catValu,labl2,catStat))
        var=addChildren(var,kids=list(catgry))
      }
      varFormat=newXMLNode("varFormat",attrs=c(type="character",schema="other"))
      catStat=newXMLNode("catStat",attrs=c(type="freq"))
      xmlChildren(catStat)=NROW(data)-sum(table(data[,j]))
      catValu=newXMLNode("catValu")
      xmlChildren(catValu)="Sysmiss"
      catgry=newXMLNode("catgry",.children = list(catValu,catStat),attrs=c(missing="Y"))
      var=addChildren(var,kids=list(catgry,varFormat))
    }
  XML_template$doc$children[["codeBook"]][["dataDscr"]]=append.xmlNode(XML_template$doc$children[["codeBook"]][["dataDscr"]],var)
}
saveXML(XML_template$doc$children$codeBook,file="your path and name of the saved XML.xml", encoding="UTF-8")

