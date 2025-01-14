####################################################
# Install packagaes if needed
if (!("tm"          %in% rownames(installed.packages()))) {install.packages("tm")}
if (!("igraph"      %in% rownames(installed.packages()))) {install.packages("igraph")}
if (!("lda"         %in% rownames(installed.packages()))) {install.packages("lda")}
if (!("LDAvis"      %in% rownames(installed.packages()))) {install.packages("LDAvis")}
if (!("servr"       %in% rownames(installed.packages()))) {install.packages("servr")}
if (!("proxy"       %in% rownames(installed.packages()))) {install.packages("proxy")}
if (!("SnowballC"   %in% rownames(installed.packages()))) {install.packages("SnowballC")}
if (!("data.table"  %in% rownames(installed.packages()))) {install.packages("data.table")}
if (!("Rmpfr"       %in% rownames(installed.packages()))) {install.packages("Rmpfr")}
if (!("plyr"        %in% rownames(installed.packages()))) {install.packages("plyr")}
if (!("dplyr"       %in% rownames(installed.packages()))) {install.packages("dplyr")}
if (!("topicmodels" %in% rownames(installed.packages()))) {install.packages("topicmodels")}
if (!("ldatuning"   %in% rownames(installed.packages()))) {install.packages("ldatuning")}
if (!("plotly"      %in% rownames(installed.packages()))) {install.packages("plotly")}
if (!("stringr"     %in% rownames(installed.packages()))) {install.packages("stringr")}
if (!("ngram"       %in% rownames(installed.packages()))) {install.packages("ngram")}
if (!("slam"        %in% rownames(installed.packages()))) {install.packages("slam")}
if (!("Rtsne"       %in% rownames(installed.packages()))) {install.packages("Rtsne")}
if (!("tools"       %in% rownames(installed.packages()))) {install.packages("tools")}
if (!("devtools"    %in% rownames(installed.packages()))) {install.packages("devtools")}
if (!("Opener5"     %in% rownames(installed.packages()))) {
  library(devtools)
  install_github("cristianmejia00/Opener5")
}
if (!("DT"          %in% rownames(installed.packages()))) {install.packages("DT")}
if (!("glue"        %in% rownames(installed.packages()))) {install.packages("glue")}
if (!("svglite"     %in% rownames(installed.packages()))) {install.packages("svglite")}
if (!("uuid"        %in% rownames(installed.packages()))) {install.packages("uuid")}
####################################################
# Load libraries

library(tm)
library(igraph)
library(lda)
library(LDAvis)
library(servr)
library(proxy)
library(SnowballC)
library(data.table)
# sudo apt-get install libmpfr-dev
library(Rmpfr)
library(plyr)
library(dplyr)
# sudo apt-get install libgsl-dev
library(topicmodels)
library(ldatuning)
# apt-get install r-cran-rjava
#library(qdap)
library(plotly)
library(stringr)
library(ngram)
#library(devtools)
#install_github("cristianmejia00/Opener5")
library(Opener5)
#library(rowr) #Not available for R 3.6.3 (?!) Dont know where it goes.
library(slam)
library(Rtsne)
library(tools)
library(DT)
library(glue)
library(uuid)