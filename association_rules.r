> associatin <- function(){
        
        #install.packages("arules")
        
        library(Matrix)
        library(arules)
        
        load("./data/titanic.raw.rdata")
        data=titanic.raw
        
        ?aprioru
        rules = apriori(data,
                        parameter = list(minlen=1, supp=0.05, conf=0.7), 
                        appearance = list(rhs = c("Survived=No", "Survived=Yes"), default="lhs"),
                        control = list(verbose=T))
        inspect(rules)
        
        rules.sorted = sort(rules, by="lift")
        inspect(rules.sorted)
        
        # find redundant rules
         subset.matrix <- is.subset(rules.sorted, rules.sorted)
         subset.matrix[lower.tri(subset.matrix, diag=T)] <- NA
         redundant <- colSums(subset.matrix, na.rm=T) >= 1
         which(redundant)
         # remove redundant rules
         rules.pruned <- rules.sorted[!redundant]
         inspect(rules.pruned)
        
        #Vizualizing Association rules
        #packages(arules, scatterplot3d, vcd, seriation, igraph, "grid", "cluster", "TSP", "gclus", "colorspace")
        install.packages("arulesViz")
        library(arulesViz)

        plot(rules.pruned)       
        
        plot(rules.pruned, method="graph", measure="support", shading="confidence", interactive=T, control=list(type="items", measureLabels=T))
        
        plot(rules.pruned, method="paracoord", control=list(reorder=TRUE))
        
}

