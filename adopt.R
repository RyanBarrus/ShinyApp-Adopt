adopt <- function(graph, # the network to examine
                  sampPop= 'EntireGraph', #the sample population to draw from
                  sampPopType = 'High', #high/low: top 10 % or bottom 10 % of sample population (ignored if sampPop = EntireGraph)
                  viewPlots = FALSE, 
                  initialAdopters=3, #number of inital adopters
                  q=.10, #threshold for adoption
                  nSamples=5, #number of times to run the procedure
                  nRounds=4, #number of rounds in each procedure
                  portionOfGraph = .2) { # the portion of the graph to examine. Nodes chosen at random. Force connectivity to largest component.
  
  #select random nodes based on portionOfGraph
  L <- length(V(graph))
  set.seed(12345)
  nodesToUse <- sample(1:L,L*portionOfGraph)
  graph <- induced.subgraph(graph,nodesToUse)
  
  #use decompose with min.vertices of 1/2 vcount to get connected graph (we are usually safe assuming the largest component is more than half the nodes)
  graph <- decompose.graph(graph,min.vertices = vcount(graph)/2)[[1]]
  
  #get centrality measures
  close <- closeness(graph)
  between <- betweenness(graph)
  degree <- degree(graph)
  authority <- authority.score(graph)$vector
  
  #determine the sample population
  if (sampPop == 'Degree') {
    if (sampPopType=='High') {
      samplePopulation <- V(graph)[degree >= quantile(degree,.9)]
    } else {
      samplePopulation <- V(graph)[degree <= quantile(degree,.1)]
    }
  } else if (sampPop == 'Closeness') {
    if (sampPopType=='High') {
      samplePopulation <- V(graph)[close >= quantile(close,.9)]
    } else {
      samplePopulation <- V(graph)[close <= quantile(close,.1)]
    }
  } else if (sampPop == 'Betweenness') {
    if (sampPopType=='High') {
      samplePopulation <- V(graph)[between >= quantile(between,.9)]
    } else {
      samplePopulation <- V(graph)[between <= quantile(between,.1)]
    }
  } else if (sampPop == 'Authority.score') {
    if (sampPopType=='High') {
      samplePopulation <- V(graph)[authority >= quantile(authority,.9)]
    } else {
      samplePopulation <- V(graph)[authority <= quantile(authority,.1)]
    }
  } else {
    samplePopulation <- V(graph)
  }
  
  results <- vector()
  for (samples in 1:nSamples) {
    #set/reset colors to blue
    V(graph)$color <- 'blue'
    #get sample of random initial adopters from sample population
    L <- length(samplePopulation)
    samp <- sample(1:L,initialAdopters)
    #set color of initial adopters to red
    V(graph)[samplePopulation[samp]]$color <- 'red'
    #iterate nrounds
    
    for (i in 1:nRounds) {
      #get current users
      users <- V(graph)[V(graph)$color == 'red']
      #get neighbors of current users
      neighbors <- unlist(neighborhood(graph,nodes = users))
      #only keep neighbors who have not yet adopted
      neighbors <- neighbors[V(graph)[neighbors]$color=='blue']
      RoundAdopters <- vector()
      #iterate across neighbors of current users
      for (j in neighbors) {
        #get neighbors of this neighbor of current users
        neighborTwo <- unlist(neighborhood(graph,nodes = V(graph)[j]))
        #count the number of neighbors of this neighbor
        QTotal <- length(neighborTwo)
        #threshold to adopt is Qtotal * q
        QThreshold <- QTotal * q
        #get actual number of adopted neighbors of this neighbor
        QActual <- sum(V(graph)$color[neighborTwo] == 'red')
        #if actual is greater than or equal to threshold, add this neighbor to adopt list
        #don't adopt yet, just mark as adopter, all adopt together at end of round.
        if (QActual >= QThreshold) {
          RoundAdopters <- c(RoundAdopters,j)
        }
      }
      #adopt this rounds neighbors
      V(graph)$color[RoundAdopters] <- 'red'
      #calculate percent adopted
      adopted <- sum(V(graph)$color == 'red') / length(V(graph))
      #optionally view the plot
      if(viewPlots) {
        plot.igraph(graph,
                    layout=layout_with_fr,vertex.size=10,
                    main=paste0('Round: ',i,'\nPercent Adopted: ',sprintf("%1.2f%%", 100*adopted)),
                    vertex.label=NA)
      }
    }
    #add percent adopted from final iteration to results
    results <- c(results,adopted)
  }
  
  return (results)
}