########  start here   #######
#### N things Loop #####

    # Create the Hierarchical Clustering
    clusters <- hclust(as.dist(distMatrix), method = "complete")

    #Values for the loop
    bestModelCut <- NULL
    bestCut <- 0
    bestF1 <- 0
    maxHeight <- max(clusters$height)

    #Drops the temp table if exists
    dbSendQuery(con, "drop table if exists main.temp_author_clusters;")

    #Loop that looks for the best cut of the tree
    for (i in seq(0, ceiling(maxHeight), by=0.05)) {
        if(i>maxHeight)
            break

        cut <- as.data.frame(cutree(clusters, h = i))

        # dbClusters <- cbind(str_split_fixed(row.names(cut), "-", n=2), (cut[,1] + df.max[1,1]))
        dbClusters <- cbind(str_split_fixed(row.names(cut), "-", n=2), cut[,1] )
        dbClusters <- as.data.frame(dbClusters, stringsAsFactors=FALSE)
        colnames(dbClusters) <- c("id", "d", "cluster")
        dbClusters$id <- as.numeric(dbClusters$id)
        dbClusters$d <- as.numeric(dbClusters$d)
        dbClusters$cluster <- as.numeric(dbClusters$cluster)

        # str(dbClusters)
        # head(dbClusters, n=30)

        #Writes into the table
        dbWriteTable(
            con, c("main","temp_author_clusters"), value = dbClusters, append = TRUE, row.names = FALSE
        )

        # bring the real cluster
        query_cluster_test <-
            "select
                c.id,
                c.d,
                ad.author_id as real_cluster,
                c.cluster as computed_cluster
            from
                    main.temp_author_clusters c
                join main.authors_disambiguated ad on c.id = ad.id and c.d = ad.d
        ;"
        clusterTest <- dbGetQuery(con, query_cluster_test)

        currentF1 <- pairwiseMetrics(clusterTest$computed_cluster, clusterTest$real_cluster)[1, 3]

        if(!is.na(currentF1) && currentF1 > bestF1){
            bestF1 <- currentF1
            bestCut <- i
            bestCluster <- clusterTest
            # print(paste(i , " - " , currentF1))
        }

        dbSendQuery(con, "truncate table main.temp_author_clusters;")

    }

    head(bestCluster, n=20)

    #Drops the temp table
    dbSendQuery(con, "drop table if exists main.temp_author_clusters;")


    #Plot the best cluster cut
    plot(clusters, cex=0.5)
    abline(h = bestCut, lty = 2)

    # Final Results
    pairwiseResults <- cbind(BestCut = bestCut, Method = "Pairwise", pairwiseMetrics(bestCluster$computed_cluster, bestCluster$real_cluster))
    b3Results <- cbind(BestCut = bestCut, Method = "B3", b3Metrics(bestCluster$computed_cluster, bestCluster$real_cluster))
    rbind(pairwiseResults, b3Results)

}


# Function that retreives all multiple pairs of elements in the same cluster
getPairsOfClusters <- function(r){
    names(r) <- c(1:length(r))
    pairs <- matrix(NA, 2, 0)
    for(i in unique(r)){
        c <- names(r[r==i])
        if(length(c) > 1) 
            pairs <- cbind(pairs, combn(c, 2))
    }
    paste(pairs[1,], "-", pairs[2,])
}

# Function that calculates the Pairwise metrics for validating a clustering
pairwiseMetrics <- function(r, s){
    pairsR <- getPairsOfClusters(r)
    pairsS <- getPairsOfClusters(s)
    Precision <- length(intersect(pairsR, pairsS)) / length(pairsR)
    Recall <- length(intersect(pairsR, pairsS)) / length(pairsS)
    F1 <- (2 * Precision * Recall) / (Precision + Recall)
    cbind(Precision, Recall, F1)
}

#Function that calculates the B3 precission of a clustering
b3Precision <- function(r, s){
    names(r) <- c(1:length(r))
    names(s) <- c(1:length(s))
    sum <- 0
    for(i in 1:length(r)){
        cR <- names(r[r==r[i]])
        cS <- names(s[s==s[i]])
        sum <- sum + (length(intersect(cR,cS)) / length(cR))
    }
    sum / length(r)
}

#Function that calculates the B3 recall of a clustering
b3Recall <- function(r, s){
    names(r) <- c(1:length(r))
    names(s) <- c(1:length(s))
    sum <- 0
    for(i in 1:length(r)){
        cR <- names(r[r==r[i]])
        cS <- names(s[s==s[i]])
        sum <- sum + (length(intersect(cR,cS))/ length(cS))
    }
    sum / length(r)
}

# Function that calculates the B3 metrics for validating a clustering
b3Metrics <- function(r, s){
    Precision <- b3Precision(r, s)
    Recall <- b3Recall(r, s)
    F1 <- (2 * Precision * Recall) / (Precision + Recall)
    cbind(Precision, Recall, F1)
}