library(igraph)
library(quanteda)
library(dplyr)
library(plotly)

load("my_tokens")
my_tokens <- tokens_tolower(tokens_remove(my_tokens, pattern = stopwords("english"), padding = FALSE))
my_fcm <- fcm(my_tokens, tri = F, context =  "window")
load("my_freq2")

#1.08 GB object - obviously can't plot the whole dataset (quanteda lists a max of 100 features)
object.size(my_fcm)/1024^2

selectedfeatures <- unique(c(my_freq2$V1[1:200],my_freq2$V2[1:200]))
set.seed(123)
my_plot <- fcm_select(my_fcm, pattern = selectedfeatures) %>%
                  textplot_network(min_freq = .8)
jpeg("my_network.jpg", width = 1920, height = 1200)
my_plot
dev.off()
