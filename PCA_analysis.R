PCA_analysis <- function(df){
    library("FactoMineR")
    
    PCA_analysis <- PCA(df, scale.unit = TRUE, graph = TRUE)

    # res.pca <- prcomp(df, scale = TRUE)
    # fviz_eig(res.pca)
    # 
    # fviz_pca_ind(res.pca,
    #              col.ind = "cos2", # Color by the quality of representation
    #              gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
    #              repel = TRUE     # Avoid text overlapping
    # )
    # myData <- myData[,-c(8:9)]
}