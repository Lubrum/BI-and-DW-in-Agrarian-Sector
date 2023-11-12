setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

if(!require(ggrepel)) install.packages("ggrepel")
library(ggrepel)

works_per_author <- read.csv("../spreadsheet/works_per_author.csv", sep = ";", stringsAsFactors = FALSE, encoding = "latin1")
works_author_df <- matrix(0, nrow = nrow(works_per_author), ncol = nrow(works_per_author))
rownames(works_author_df) <- works_per_author[, 1]

authors_per_work <- read.csv("../spreadsheet/authors_per_work.csv", sep = ";", stringsAsFactors = FALSE, encoding = "latin1")

# Initialize counters
cont <- 1

# Loop through each row in works_author_df
for (i in 1:nrow(works_author_df)) {
  for (j in 1:nrow(works_author_df)) {
    # Split authors_per_work cell by ";"
    authors <- unlist(strsplit(authors_per_work[j, 1], ";"))
    
    # Loop through each author and check if it's in works_author_df row names
    for (k in 1:length(authors)) {
      author_name <- authors[k]
      
      # Find the row index where author_name matches row names
      row_index <- which(row.names(works_author_df) == author_name)
      
      if (length(row_index) > 0) {
        # Increment the corresponding cell in works_author_df
        works_author_df[row_index, cont] <- works_author_df[row_index, cont] + 1
      }
    }
    
    cont <- cont + 1
    authors_per_work[j, 1] <- 0
  }
}

# Load data and create a term-document matrix
termDocMatrix <- as.matrix(works_author_df)
termDocMatrix[termDocMatrix >= 1] <- 1

# Compute the term similarity matrix
termSimilarityMatrix <- termDocMatrix %*% t(termDocMatrix)

# Create a graph from the term similarity matrix
if(!require(igraph)) install.packages("igraph")
library(igraph)

graphAdjacency <- graph.adjacency(
  termSimilarityMatrix, 
  weighted = T, 
  mode = "undirected"
)

graphAdjacency <- simplify(graphAdjacency)

# Define a function to normalize node sizes based on their degrees
normalizer <- function(x) (x - min(x)) / (max(x) - min(x)) + 0.25 
V(graphAdjacency)$size <- normalizer(degree(graphAdjacency)) * 10

# Load data related to authors and their countries
authors_country <- read.csv(
  "../spreadsheet/authors_country.csv", 
  sep = ";",
  encoding = "latin1", 
  stringsAsFactors = FALSE
)

authors_country <- authors_country[, -3]

# Define a color palette for countries
if(!require(RColorBrewer)) install.packages("RColorBrewer")
library(RColorBrewer)
pal <- colorRampPalette(brewer.pal(9, "Paired"))(length(unique(authors_country$Num)))

# Layout the graph using a force-directed algorithm
coords <- layout_with_fr(graphAdjacency, niter = 1000)

plot(
  graphAdjacency, 
  vertex.label.color = "black", 
  vertex.label.cex = 0.6, 
  layout = layout.kamada.kawai, 
  vertex.label.font = 2,
  edge.color = "grey",
  vertex.color = pal[as.numeric(authors_country$Num)], 
  vertex.label.dist = 1, 
  vertex.frame.color = "black"
)

legend(
  x = -2.3, 
  y = 1.4, 
  unique(authors_country$País), 
  pch = 21, 
  col = "#777777", 
  pt.bg = pal[unique(authors_country$Num)], 
  pt.cex = 1.7, 
  cex = .8, 
  bty = "n", 
  ncol = 1
)

tkplot(
  graphAdjacency,
  canvas.width = 1400,
  canvas.height = 800,
  vertex.label.color = "black",
  vertex.label.cex = 1.3,
  layout = layout.kamada.kawai,
  vertex.label.font = 2, 
  edge.color = "grey",
  vertex.color = pal[as.numeric(authors_country$Num)],
  vertex.label.dist = 1.5,
  vertex.frame.color = "black"
)

graphAdjacency <- remove.vertex.attribute(graphAdjacency, "size")

if(!require(visNetwork)) install.packages("visNetwork")
library(visNetwork)

graphAdjacency_vis <- toVisNetworkData(graphAdjacency) 

names <- sort(graphAdjacency_vis$nodes$id) 

graphAdjacency_vis$nodes$value <- normalizer(degree(graphAdjacency)) * 10

graphAdjacency_vis$nodes$country <- authors_country$País

graphAdjacency_vis$nodes$group <- authors_country$País

vis_plot <- visNetwork(
  nodes = graphAdjacency_vis$nodes, 
  edges = graphAdjacency_vis$edges,
  main = "Collaboration Network",
  submain = "Authors of BI and agriculture published papers",
) %>%
visIgraphLayout(
  layout = "layout_with_kk", 
  smooth = FALSE,            
  physics = TRUE             
) %>%
visEdges( 
  color = list(highlight = "lightgray")
) %>%
visGroups( 
  groupname = graphAdjacency_vis$nodes$country
) %>%
visOptions( 
  selectedBy = "country",
  highlightNearest = list(
    enabled = TRUE,
    degree = 1,
    hover = TRUE,
    labelOnly = TRUE),
    nodesIdSelection = list(
      enabled = TRUE,
      values = names
    )
) %>%
visLegend(  
  position = "right", 
  stepX = 51, 
  stepY = 51, 
  main = "Country", 
  useGroups = TRUE
) %>%
visPhysics( 
  repulsion = list(springlength = 50), 
  maxVelocity = 0.8,
  solver = "forceAtlas2Based",
  forceAtlas2Based = list(gravitationalConstant = -1000),
  timestep = 0.25
)

vis_plot





citations_x_authors = read.csv(
  "../spreadsheet/citations_x_authors.csv", 
  sep = ";", 
  encoding = "latin1", 
  stringsAsFactors = FALSE
)

if(!require(ggplot2)) install.packages("ggplot2")
library(ggplot2)

ggplot( 
  citations_x_authors[,4:3], 
  aes(
    citations_x_authors[,4], 
    citations_x_authors[,3], 
    label = citations_x_authors[,1]
  )
) + 
labs(
  x = gsub("\\.", " ", colnames(citations_x_authors[4])), 
  y = gsub("\\.", " ", colnames(citations_x_authors[3]))
) +
geom_vline(
  xintercept = sort(citations_x_authors$Number.of.the.Citations.of.Most.Cited.Author.in.References, decreasing = TRUE)[round(nrow(citations_x_authors)*0.2)] - 0.5) + 
  geom_hline(yintercept = sort(citations_x_authors$Citation.Number, decreasing = TRUE)[round(nrow(citations_x_authors)*0.2)] - 0.5) +
  geom_text_repel(max.overlaps = 20) +
  scale_shape_identity() +
  geom_point() 





periodics = read.csv(
  "../spreadsheet/periodics.csv", 
  sep = ";",
  encoding = "latin1", 
  stringsAsFactors = FALSE
)

label <- paste(periodics[,1],"(JCR:", periodics[,4], ")")
label <- gsub(" )", ")", label)

ggplot( 
  periodics, 
  aes(x = jitter(periodics[,3],2), y = jitter(periodics[,2]))) +
  labs(x = gsub("\\.", " ", colnames(periodics[3])), y = gsub("\\.", " ", colnames(periodics[2]))) +
  geom_point(aes(colour = colorRampPalette(brewer.pal(11, "Spectral"))(23), size = periodics[,4])) +
  geom_vline(xintercept = 12) +
  geom_hline(yintercept = 1.5) +
  scale_size_continuous(  
    name = "Journals'/Conferences' of BP and it's JCR",
    breaks = periodics[,4], labels = label
  ) +
  theme(  
    legend.key.height = unit(0.52, "cm"),
    legend.text = element_text(colour = "black", size = 8.5, face = "bold"),
    legend.position = "right", 
    legend.background = element_rect(   
      fill = "lightgray",
      linewidth = 0.5,
      linetype = "solid",
      colour ="black"
    )
  ) +
  guides(
    size = guide_legend(ncol = 1)
  ) + 
  scale_color_discrete(   
    name = "Journals'/Conferences' of BP and it's JCR",
    breaks = colorRampPalette(brewer.pal(11, "Spectral"))(23),
    labels = label
  )