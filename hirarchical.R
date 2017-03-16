#Author: Fitsum Tamene
library(readxl)
library(dendextend)


training_data <- read_excel("~/Course/course_data.xlsx")

training_data=data.frame(training_data)

training_data$condition=as.factor(training_data$condition)

distRetino <- dist(training_data[,-1])

hcRetino <- hclust(distRetino, method = "complete")

treatmentRestino=rev(levels(training_data$condition))

dend <- as.dendrogram(hcRetino)
labels_colors(dend) <-
    rainbow_hcl(3)[sort_levels_values(
        as.numeric(training_data[,2])[order.dendrogram(dend)]
    )]

labels(dend) <- paste(as.character(training_data[,1])[order.dendrogram(dend)],
                      "_",labels(dend), 
                      sep = "")

dend <- hang.dendrogram(dend,hang_height=0.1)

dend <- set(dend, "labels_cex", 0.5)

plot(dend,horiz =  F,  nodePar = list(cex = .007))
    legend("topright", legend = treatmentRestino, fill = rainbow_hcl(3))