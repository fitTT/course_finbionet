#Author: Fitsum Tamene
library(readxl)
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



protein_acc='O00267'
ms1=as.numeric(mydf[protein_acc,grep('pdr|non.pdr',colnames(mydf))])

protD=data.frame(ms1,condtion=colnames(mydf)[grep('pdr|non.pdr',colnames(mydf))])
bp=ggplot(protD, aes(x=condtion,y=log2(ms1)))+
    geom_boxplot(aes(fill=condtion)) +#stat_boxplot(geom ='errorbar')+
    guides(fill=F)+labs(x='',y='log110(MS1)')+theme_classic()+
    ggtitle(protein_acc)
plot(bp)
