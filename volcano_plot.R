#Author: Fitsum Tamene
library(readxl)
library(shiny)
library(plotly)
training_data <- read_excel("~/Course/course_data.xlsx")

training_data=data.frame(training_data)

training_data$condition=as.factor(training_data$condition)
mydf = setNames(data.frame(t(training_data[,-1:-2])),training_data[,2])

mydf=mydf+1

p_value=apply(mydf,1,function(x){
    if (mean(x[names(x)=='non.pdr'])/mean(x[names(x)=='pdr'])==1) 
        return (1) 
    else 
        t.test(x[names(x)=='non.pdr'],x[names(x)=='pdr'])$p.value
}
)
q_value=qvalue(p_value,fdr.level = 0.01)
significant=q_value$significant
foldChange_log2=apply(mydf,1,function(x){
    foldChange=mean(x[names(x)=='non.pdr'])/mean(x[names(x)=='pdr'])
    return(log2(foldChange))
}
)
mydf=cbind(mydf,foldChange_log2,pValue=p_value,qValue=q_value$qvalues,significant=q_value$significant)
mydf$Fold_Change=cut(abs(mydf$foldChange_log2),
                     breaks=c(0,1,1.584963,2,2.321928,Inf),right=F,
                     labels=c("x < 2","2 ≤ x < 3","3 ≤ x < 4","4 ≤ x ≤ 5","x >5"))


mydf$proteins=rownames(mydf)

p=ggplot(mydf, aes(x=foldChange_log2, y=-log10(qValue),
                   color=Fold_Change,text = paste(proteins,'q.value=',round(qValue,digits = 4))))+geom_point(size=1.0) +
    theme(legend.justification=c(1,1),legend.position=c(1,0.8),
          panel.border = element_rect(colour="black", fill=NA, size=1),
          legend.key= element_blank(),
          panel.background=element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank()
    )+
    
    geom_vline(xintercept=0,linetype='dashed',col="greenyellow")+
    
    
    
    # geom_text(data=subset(mydf, significant==T ),
    #           aes(x=foldChange_log2, y=-log10(qValue),
    #               label=rownames(subset(mydf, significant==T ))),
    #           show.legend = FALSE, size=3)+
    
    labs( x = "log2 (Fold Change)",y="-log10 (P.value)")+
    annotate("text", x=6, y=8, label= "non.pdr")+annotate("text", x=-4, y=8, label= "pdr")

ggplotly(p)

