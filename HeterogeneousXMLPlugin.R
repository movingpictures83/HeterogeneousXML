#########################################################################################################################
#########################################################################################################################
###### PROJECT:        DBNs
###### NAME:           CreateStyle.R
###### AUTHOR:         Daniel Ruiz-Perez and Vitalii Stebliankin,
###### AFFILIATION:    Florida International University
###### 
###### DESCRIPTION:    This file creates the style.xml file needed to visualize it in Cytoscape. It takes as input a network
######                , a file with the mean abundance and a base style file to update.
#########################################################################################################################
#########################################################################################################################



library(scales)
library(stringr)
dyn.load(paste("RPluMA", .Platform$dynlib.ext, sep=""))
source("RPluMA.R")

input <- function(inputfile) {
	pfix = prefix()
  parameters <<- read.table(inputfile, as.is=T);
  rownames(parameters) <<- parameters[,1];
   # Need to get the three files
   options("scipen"=100, "digits"=4)

   folder <<- paste(pfix, parameters["folder", 2], sep="/")
   nameOfNetwork <<- paste(pfix, parameters["nameOfNetwork", 2], sep="/")
   abundances <<- unlist(read.csv(paste(pfix, parameters["abundances", 2], sep="/"), sep="\t"))

# READ STYLE BASE
f <<- readChar(paste(pfix, parameters["style", 2], sep="/"),file.info(paste(pfix, parameters["style", 2], sep="/"))$size) 
}

run <- function() {

}

output <- function(outputfile) {

split = strsplit(f, "<dependency name=\"nodeSizeLocked\" value=\"true\"/>", fixed = T)
before = unlist(split)[1] # header of the style file
after = unlist(split)[2] # entries with properties

#READ NETWORK
network = readChar(nameOfNetwork, file.info(nameOfNetwork)$size)
network= unlist(strsplit(x =network, "\r\n"))
network= unlist(strsplit(x =network, "\n"))

#CLASSIFY ATTRIBUTES BASED ON TYPE
namesAll = str_extract(network[grep("<node id=",network)], "\"+\"")#((s|p|c)__)?.+\\_ti(\\+1)?
shopping_list <- c("apples x4", "bag of flour", "bag of sugar", "milk x2")
str_extract(shopping_list, "\\d")
namesAll = gsub("<node id=\"", "*.*", namesAll)
namesAll = gsub("_ti", "", namesAll)
namesAll = gsub("\\+1", "", namesAll)
namesAll = gsub("\\[", "[", namesAll)
namesAll = gsub("\\]", "]", namesAll)
namesAll = unique(namesAll)
prefix = substr(namesAll,1,3)
#namesAll = substr(namesAll,4,1000)



taxa = namesAll[which(prefix %in% "s__")]
ptr = namesAll[which(prefix %in% "p__")]
clinical = namesAll[which(prefix %in% "c__")]
outcomes =  c("CRIB_II_Score") #namesAll[! (prefix %in% "g__"| prefix %in% "s__" | prefix %in% "m__")]
all = c(taxa, ptr, outcomes, clinical)


################## NODE COLOR -> timepoint
aux ="\n            <visualProperty name=\"NODE_FILL_COLOR\" default=\"#ff9232\">\n                <discreteMapping attributeType=\"string\" attributeName=\"name\">"
for (i in 1:length(all)){
  aux = paste(aux,"\n                    <discreteMappingEntry value=\"#4d93a8\" attributeValue=\"", paste(all[i],"_ti",sep=""),"\"/>",sep="")
}
aux = paste(aux,"\n                </discreteMapping>\n            </visualProperty>" ,sep="")
f = paste(before,"<dependency name=\"nodeSizeLocked\" value=\"true\"/>",aux,after,sep= "")




############### SIZE -> incoming edges
maxAcum = 1
for (i in 1:length(all))
  maxAcum = max(maxAcum, length(grep(pattern = paste(".*target=\"",all[i],"_ti+1","\".*",sep=""), x = network, ignore.case = T)))

aux =paste(aux,"\n            <visualProperty name=\"NODE_SIZE\" default=\"40\">\n                <discreteMapping attributeType=\"string\" attributeName=\"name\">",sep="")
for (i in 1:length(all)){
  aux = paste(aux,"\n                    <discreteMappingEntry value=\"",(20*length(grep(pattern = paste(".*target=\"",all[i],"_ti","\".*",sep=""), x = network, ignore.case = T))/maxAcum+40),"\" attributeValue=\"",all[i],"_ti","\"/>",sep="")
  aux = paste(aux,"\n                    <discreteMappingEntry value=\"",(20*length(grep(pattern = paste(".*target=\"",all[i],"_ti\\+1","\".*",sep=""), x = network, ignore.case = T))/maxAcum+40),"\" attributeValue=\"",all[i],"_ti+1","\"/>",sep="")
}
aux = paste(aux,"\n                </discreteMapping>\n            </visualProperty>" ,sep="")
f = paste(before,"<dependency name=\"nodeSizeLocked\" value=\"true\"/>",aux,after,sep= "")



############### TRANSPARENCY -> abundance
abundancesScaled = rescale(as.numeric(abundances),c(90,255))
abundancesScaled = c(rep(200,length(taxa)),abundancesScaled)
aux =paste(aux,"\n            <visualProperty name=\"NODE_TRANSPARENCY\" default=\"255\">\n                <discreteMapping attributeType=\"string\" attributeName=\"name\">",sep="")
for (i in 1:length(names)){
  aux = paste(aux,"\n                    <discreteMappingEntry value=\"",round(abundancesScaled[i],1),"\" attributeValue=\"",all[i],"_ti","\"/>",sep="")
  aux = paste(aux,"\n                    <discreteMappingEntry value=\"",round(abundancesScaled[i],1),"\" attributeValue=\"",all[i],"_ti+1","\"/>",sep="")
}
aux = paste(aux,"\n                </discreteMapping>\n            </visualProperty>" ,sep="")
f = paste(before,"<dependency name=\"nodeSizeLocked\" value=\"true\"/>",aux,after,sep= "")



############### SHAPE -> type of data

aux =paste(aux,"\n            <visualProperty name=\"NODE_SHAPE\" default=\"ELLIPSE\">\n                <discreteMapping attributeType=\"string\" attributeName=\"name\">",sep="")
for (i in 1:length(ptr)){
  aux = paste(aux,"\n                    <discreteMappingEntry value=\"DIAMOND\" attributeValue=\"",ptr[i],"_ti","\"/>",sep="")
  aux = paste(aux,"\n                    <discreteMappingEntry value=\"DIAMOND\" attributeValue=\"",ptr[i],"_ti+1","\"/>",sep="")
}
for (i in 1:length(outcomes)){
  aux = paste(aux,"\n                    <discreteMappingEntry value=\"TRIANGLE\" attributeValue=\"",outcomes[i],"_ti","\"/>",sep="")
  aux = paste(aux,"\n                    <discreteMappingEntry value=\"TRIANGLE\" attributeValue=\"",outcomes[i],"_ti+1","\"/>",sep="")
}
for (i in 1:length(clinical)){
  aux = paste(aux,"\n                    <discreteMappingEntry value=\"RECTANGLE\" attributeValue=\"",clinical[i],"_ti","\"/>",sep="")
  aux = paste(aux,"\n                    <discreteMappingEntry value=\"RECTANGLE\" attributeValue=\"",clinical[i],"_ti+1","\"/>",sep="")
}
aux = paste(aux,"\n                </discreteMapping>\n            </visualProperty>" ,sep="")
f = paste(before,"<dependency name=\"nodeSizeLocked\" value=\"true\"/>",aux,after,sep= "")


split = strsplit(f, "<dependency name=\"arrowColorMatchesEdge\" value=\"false\"/>", fixed = T)
before = unlist(split)[1]
after = unlist(split)[2]



################## EDGE LINE TYPE -> intra-inter
aux = "\n            <visualProperty name=\"EDGE_LINE_TYPE\" default=\"SOLID\">\n                <discreteMapping attributeType=\"string\" attributeName=\"shared name\">"
for (i in 1:length(all)){
   for (j in i:length(all)){
      aux = paste(aux,"\n                    <discreteMappingEntry value=\"EQUAL_DASH\" attributeValue=\"",all[i],"_ti+1 (-) ", all[j],"_ti+1","\"/>",sep="")
      aux = paste(aux,"\n                    <discreteMappingEntry value=\"EQUAL_DASH\" attributeValue=\"",all[i],"_ti (-) ", all[j],"_ti","\"/>",sep="")
  }
}
aux = paste(aux,"\n                </discreteMapping>\n            </visualProperty>" ,sep="")


################## EDGE PAINT
# aux =paste(aux,"\n            <visualProperty name=\"EDGE_UNSELECTED_PAINT\" default=\"#CC0033\">\n                <discreteMapping attributeType=\"string\" attributeName=\"name\">",sep="")
# for (i in 1:length(names)){
#     aux = paste(aux,"\n                    <discreteMappingEntry value=\"#FF9999\" attributeValue=\"",names[i],"_ti (-) ", names[i],"_ti+1","\"/>",sep="")
#   }
# 
# aux = paste(aux,"\n                </discreteMapping>\n            </visualProperty>" ,sep="")



################  EDGE TRANSPARENCY  Make the self loops more transparent
# aux =paste(aux,"\n            <visualProperty name=\"EDGE_TRANSPARENCY\" default=\"255\">\n                <discreteMapping attributeType=\"string\" attributeName=\"name\">",sep="")
# for (i in 1:length(all)){
#   aux = paste(aux,"\n                    <discreteMappingEntry value=\"70\" attributeValue=\"",all[i],"_ti (-) ", all[i],"_ti+1","\"/>",sep="")
# }



# 
# ################## EDGE TRANSPARENCY
weights = str_extract(network[grep("key=\"key_bootScore",network)], "\\-*\\d+\\.+\\d+")
weights = weights[!is.na(weights)]
maxAbsWeight =  max(abs(as.numeric(weights)))
minAbsWeight =  min(abs(as.numeric(weights)))

 
# ######## For edge coefficient
aux =paste(aux,"\n<visualProperty name=\"EDGE_TRANSPARENCY\" default=\"2.0\">\n")
aux =paste(aux,"  <continuousMapping attributeType=\"float\" attributeName=\"bootScore\">\n")
aux =paste(aux,"  <continuousMappingPoint lesserValue=\"50.0\" greaterValue=\"50.0\" equalValue=\"50.0\" attrValue=\"",minAbsWeight,"\"/>\n",sep="")
aux =paste(aux,"  <continuousMappingPoint lesserValue=\"50.0\" greaterValue=\"255.0\" equalValue=\"255.0\" attrValue=\"",maxAbsWeight,"\"/>\n",sep="")
aux =paste(aux,"  </continuousMapping>\n")
aux =paste(aux,"  </visualProperty>\n")




################## EDGE WIDTH
weights = str_extract(network[grep("key=\"key_weight",network)], "\\-*\\d+\\.+\\d+")
weights = weights[!is.na(weights)]

maxAbsWeight =  max(abs(as.numeric(weights)))
medianAbsWeight =  median(abs(as.numeric(weights)))
# maxAbsWeight = 150 #when we hide intra edges we need this



######## For edge coefficient normalized
aux =paste(aux,"\n<visualProperty name=\"EDGE_WIDTH\" default=\"2.0\">\n")
aux =paste(aux,"  <continuousMapping attributeType=\"float\" attributeName=\"weight\">\n")
aux =paste(aux,"  <continuousMappingPoint lesserValue=\"15.0\" greaterValue=\"15.0\" equalValue=\"15.0\" attrValue=\"-",1,"\"/>\n",sep="")
aux =paste(aux,"  <continuousMappingPoint lesserValue=\"1.0\" greaterValue=\"1.0\" equalValue=\"1.0\" attrValue=\"0.0\"/>\n")
aux =paste(aux,"  <continuousMappingPoint lesserValue=\"15.0\" greaterValue=\"15.0\" equalValue=\"15.0\" attrValue=\"",1,"\"/>\n",sep="")
aux =paste(aux,"  </continuousMapping>\n")
aux =paste(aux,"  </visualProperty>\n")



# ######## For edge coefficient
# aux =paste(aux,"\n<visualProperty name=\"EDGE_WIDTH\" default=\"2.0\">\n")
# aux =paste(aux,"  <continuousMapping attributeType=\"float\" attributeName=\"weight\">\n")
# aux =paste(aux,"  <continuousMappingPoint lesserValue=\"10.0\" greaterValue=\"15.0\" equalValue=\"15.0\" attrValue=\"-",maxAbsWeight,"\"/>\n",sep="")
# aux =paste(aux,"  <continuousMappingPoint lesserValue=\"1.0\" greaterValue=\"10.0\" equalValue=\"10.0\" attrValue=\"-",medianAbsWeight,"\"/>\n",sep="")
# aux =paste(aux,"  <continuousMappingPoint lesserValue=\"1.0\" greaterValue=\"1.0\" equalValue=\"1.0\" attrValue=\"0.0\"/>\n")
# aux =paste(aux,"  <continuousMappingPoint lesserValue=\"1.0\" greaterValue=\"10.0\" equalValue=\"10.0\" attrValue=\"",medianAbsWeight,"\"/>\n",sep="")
# aux =paste(aux,"  <continuousMappingPoint lesserValue=\"10.0\" greaterValue=\"15.0\" equalValue=\"15.0\" attrValue=\"",maxAbsWeight,"\"/>\n",sep="")
# aux =paste(aux,"  </continuousMapping>\n")
# aux =paste(aux,"  </visualProperty>\n")
  
########### For edge confidence
# aux =paste(aux,"<visualProperty name=\"EDGE_WIDTH\" default=\"2.0\">\n")
# aux =paste(aux,"  <continuousMapping attributeType=\"float\" attributeName=\"weight\">\n")
# aux =paste(aux,"  <continuousMappingPoint lesserValue=\"16.0\" greaterValue=\"20.0\" equalValue=\"20.0\" attrValue=\"-",maxAbsWeight,"\"/>\n",sep="")
# aux =paste(aux,"  <continuousMappingPoint lesserValue=\"8.0\" greaterValue=\"16.0\" equalValue=\"16.0\" attrValue=\"-",80.0,"\"/>\n",sep="")
# aux =paste(aux,"  <continuousMappingPoint lesserValue=\"8.0\" greaterValue=\"8.0\" equalValue=\"8.0\" attrValue=\"-",20.0,"\"/>\n",sep="")
# aux =paste(aux,"  <continuousMappingPoint lesserValue=\"1.0\" greaterValue=\"1.0\" equalValue=\"1.0\" attrValue=\"0.0\"/>\n")
# aux =paste(aux,"  <continuousMappingPoint lesserValue=\"8.0\" greaterValue=\"8.0\" equalValue=\"8.0\" attrValue=\"",20.0,"\"/>\n",sep="")
# aux =paste(aux,"  <continuousMappingPoint lesserValue=\"8.0\" greaterValue=\"16.0\" equalValue=\"16.0\" attrValue=\"",80.0,"\"/>\n",sep="")
# aux =paste(aux,"  <continuousMappingPoint lesserValue=\"16.0\" greaterValue=\"20.0\" equalValue=\"20.0\" attrValue=\"",maxAbsWeight,"\"/>\n",sep="")
# aux =paste(aux,"  </continuousMapping>\n")
# aux =paste(aux,"  </visualProperty>\n")


fileConn<-file(outputfile)
writeLines(paste(before,"<dependency name=\"arrowColorMatchesEdge\" value=\"false\"/>",aux,after,sep=""), fileConn)
close(fileConn)
}
