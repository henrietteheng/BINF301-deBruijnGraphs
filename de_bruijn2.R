library(tidyverse)

#Find the edges of the de Bruijn graph
de_bruijn_edges <- function(string, k){

  #Empty vectors to fill with edges
  node1 <- character()
  node2 <- character()

  #Extract the edges
  for(i in 1:nchar(string)-k+1){

    #Node 1 of edge, starts at i and ends at i+k-2
    node1[i] <- str_sub(string, i, i+k-2)
    #Node 2 of edge, starts at i+1 and ends at i+k-1
    node2[i] <- str_sub(string, i+1, i+k-1)
  }

  #Just add node1 and node2 into a tibble as individual columns
  edges <- tibble(node1, node2)
}
#Check nodes for balance
check_balance <- function(edge_df){

  #Turn into tidy format for summarising
  df <- edge_df %>%
    select(node1, node2) %>%
    rename(outgoing = node1,
           incoming = node2) %>%
    pivot_longer(.,
                 cols = c(outgoing, incoming),
                 names_to = 'where',
                 values_to = 'nodes')

  #Count the in/outdegree of a node and turn into wide format to compare
  balanced <- df %>%
    group_by(where, nodes) %>%
    summarise(n = n()) %>%
    ungroup() %>%
    pivot_wider(.,
                names_from = where,
                values_from = n) %>%
    #Add 0 for NA
    replace_na(list(outgoing = 0, incoming = 0)) %>%

    #If the number of incoming and outgoing edges are the same, mark as 'balanced', if they differ by one mark as 'semi-balanced', else 'non-balanced'
    mutate(balance = case_when(
      outgoing == incoming ~ 'balanced',
      outgoing + 1 == incoming | outgoing - 1 == incoming ~ 'semi_balanced',
      TRUE ~ 'not_balanced'
    )) %>%

    #Count the types of balance of the nodes
    select(balance) %>%
    group_by(balance) %>%
    summarise(n = n()) %>%
    ungroup() %>%
    pivot_wider(
      names_from = 'balance',
      values_from = n
    )

}
#Write edges and if the graph is eularian
deBruijnGraph <- function(string, k){

  #Get the edges of the graph
  edges <- de_bruijn_edges(string = string, k = k) %>%
    #Format to look like assignment output
    group_by(node1, node2) %>%
    summarise(n = n()) %>%
    mutate(output = paste0(node1, ' -> ', node2, ' : ', n)) %>%
    ungroup()

  #Check balance of the nodes
  balances <- check_balance(edges)

  sapply(edges$output, print)

  #Print
  if('not_balanced' %in% names(balances) | balances$semi_balanced > 2){
    print('Graph is not eularian!')
  }else if(balances$semi_balanced  <= 2){
    print('Graph is eularian!')
  }

  #Return both the edges and a named vectors with the individual nodes and their degrees
  results <- list(
    'edges' = edges,
    'nodes' = tibble(node = c(edges$node1, edges$node2)) %>%
      group_by(node) %>%
      mutate(degrees = n()) %>%
      ungroup() %>% unique() %>%
      deframe()
  )

}
#Find index of edge in graph so it can be removed once traversed
findEdgeLocation <- function(node1, node2, graph){
  #Finds the row index of the first occurring edge containing node1 and node2  in graph

  index <- which(graph$node1 == node1 & graph$node2 == node2)[1]

}
#Find next node in path
findPath <- function(node, graph){
  #Look for input-node in the node1 column of graph and prints the node2 node

  #Find first position of node in graph
  temp <- str_which(graph$node1, node)[1]

  #Get character vector of following node
  as.character(graph[temp, 2])

}


#Create the de bruijn graph
graph <- deBruijnGraph('AAGATTCTCTAC', 4)

#Extract just the edges to work with
edges <- graph$edges %>%
  select(node1, node2)

#This repeats the start node unless start node is first or last node
#Create a path that traverse all edges
walkPath <- function(node, graph){

  #Vector to store path
  path <- character()
  #Add start node to path
  path[1] <- c(node)

  #Edges from graph
  edges <- graph
  #node to be updated by while loop
  node <- node

#While there are any edges that haven't been traversed, look for a way to add them to path
while(nrow(edges) > 0) {

  #If node is in path and is the first node of a non-traversed edge, find next node to come after it by searching through graph
  if((node %in% path) & (node %in% as.character(edges$node1))){

    #Next in path
    next_node <- findPath(node = node, graph = edges)

    #Add next_node to path after last occurrence of node
    #Last occurrence of node in path
    node_placement <- tail(which(path == node), 1)

    #Add to path
    path <- append(path, next_node, node_placement)

    #Find location of edge in graph and remove it
    edges <- edges[-findEdgeLocation(node = node, node2 = next_node, graph = edges),]

    #Rerun with next_node as node
    node <- next_node

  }
  #Else if there are any non traversed edges with a node1 that can be found in path start there (Deals with repeated areas)
  else if(any(edges$node2 %in% path) & !(node %in% as.character(c(edges$node1, edges$node2)))){

    #Find a node
    node <- path[which(path %in% edges$node2)[1]]

  }
  #If stuck and there are still non-traversed nodes, pick a node in graph, add to start of path and continue
  else if (!(node %in% as.character(edges$node1)) & sum(edges$node2 == node) == 1) {
    #Stuck = there are still non-traversed edges, but the current node is not the start of a non-traversed edge
    #and current node is only reachable by one other node

    #Choose first free node and restart path
    node <- as.character(edges[1,1])

    #Add to beginning of path
    path <- c(node, path)

  }


}

path

}


#Right now it duplicates start node unless start node is first or last node
#Use node degree rather than edges to check if nodes can still be part of path
test <- walkPath('AAG', edges)
test


#Rebuild the sequence starting at the first node and then adding the last letter for the other nodes
#Extract last letters
temp <- lapply(test[2:length(test)], str_extract, pattern = '\\w$')
#Build
sequence <- paste(unlist(c(test[1], temp)), collapse = '')

print(sequence)
