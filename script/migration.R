# Libraries
devtools::install_github("mattflor/chorddiag")
devtools::install_github("jokergoo/circlize")
devtools::install_github("thomasp85/patchwork")
devtools::install_github("hrbrmstr/hrbrthemes")
library(tidyverse)
library(viridis)
library(patchwork)
library(hrbrthemes)
library(circlize)
library(chorddiag)  #devtools::install_github("mattflor/chorddiag")

# Load the circlize library
library(circlize)
library(readxl)

#********** First Type of Chord Diagram **********
library(chorddiag)
m <- matrix(c(11975,  5871, 8916, 2868,
              1951, 10048, 2060, 6171,
              8010, 16145, 8090, 8045,
              1013,   990,  940, 6907),
            byrow = TRUE,
            nrow = 4, ncol = 4)
haircolors <- c("black", "blonde", "brown", "red")
dimnames(m) <- list(have = haircolors,
                    prefer = haircolors)

groupColors <- c("#000000", "#FFDD89", "#957244", "#F26223")
chorddiag(m, groupColors = groupColors, groupnamePadding = 20)

data <- matrix(c(1100100, 8066,	514688,	1201354,	31220,	935324,	161991,	58034,	51538,
                 979884,	8290657,	38856,	18243,	1277249,	2392860,	171591,	10022, 645,
                 10787164,	13921,	17071332,	277821,	88269,	3366027,	3687437,	336937,	1164348,
                 24100496,	16421,	519767,	6078028,	17397,	5197336,	72554,	796667,	14956,
                 994521,	376847,	403075,	8247,	6320064,	1848583,	1233217,	11134,	4349,
                 4292283,	88972,	2640879,	1262835,	548388,	12472710,	993982,	1246107,	65028,
                 2434640,	425260,	406548,	92947,	727155,	9310492,	16750293,	2256843,	163515,
                 2760669,	8289,	371399,	67073,	49168,	14375448,	1272241,	21957283,	24380,
                 4894838,	3306, 2787271,	22719,	120877,	3661405, 15843391,	57607,	11790702), byrow=TRUE,
                 nrow = 9, ncol=9)
region <- c('North America', 'West & Central Africa', 'East Asia & Pacific', 
            'Latin America & Caribbean', 'Eastern & Southern Africa', 'Western Europe', 
            'Middle East & North Africa', 'Eastern Europe & Central Asia', 'South Asia')
dimnames(data) <- list(have=region,
                       prefer=region)
groupColors <- c('#003c5d', '#e1d45b', '#f73c1f',
                 '#007b83', '#aaddc9', '#f7d5b8', 
                 '#4b7a7a', '#ac685a', '#92a700')
chorddiag(data, groupColors=groupColors, groupnamePadding = 50)

# ********** Another chord diagram ************
grid.col = c('Northern America'="#005073", 'West & Central Africa'='#ffc425', 'East Asia & Pacific'='#d11141', 
             'Latin America & Caribbean'='#317873', 'Eastern & Southern Africa'='#f37735', 'Western Europe'='#f000ff', 
             'Middle East & North Africa'='#a67c00', 'Eastern Europe & Central Asia'='#ff6f69', 'South Asia'='#00b159')
circos.par(gap.after = c(rep(5, ncol(data)-6), 15, rep(5, ncol(data)-4)))
chordDiagram(data, grid.col = grid.col)
circos.clear()

c(rep(5, nrow(data)-6), 15, rep(5, ncol(data)-5), 15)
?circos.par()