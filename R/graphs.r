if(!require(RColorBrewer)) install.packages("RColorBrewer")
library(RColorBrewer)

if(!require(igraph)) install.packages("igraph")
library(igraph)

if(!require(ggplot2)) install.packages("ggplot2")
library(ggplot2)

if(!require(ggrepel)) install.packages("ggrepel")
library(ggrepel)

if(!require(visNetwork)) install.packages("visNetwork")
library(visNetwork)

works_per_author <- read.csv("spreadsheet/works_per_author.csv", sep = ";", stringsAsFactors = FALSE, encoding = "latin1")
works_author_df <- as.data.frame(matrix(nrow = length(works_per_author[,1]), ncol = length(works_per_author[,1])))

for(i in 1:nrow(works_author_df)){
    for(j in 1:ncol(works_author_df)){
        works_author_df[i,j] <- as.numeric(0)
    }
}

for(i in 1:nrow(works_author_df)){
    row.names(works_author_df)[i] <- works_per_author[i,1]
}

authors_per_work <- read.csv("spreadsheet/authors_per_work.csv", sep = ";", stringsAsFactors = FALSE, encoding = "latin1")
k2 <- 1									
cont <- 1									
for(i in 1:nrow(works_author_df)){  							
    for(j in 1:nrow(works_author_df)){
        if(row.names(works_author_df)[i] %in% unlist(strsplit(authors_per_work[j,1], "[;]"))){ 		
            authors <- 0							
            for(k in 1:length(unlist(strsplit(authors_per_work[j,1], "[;]")))){	
                authors[k2] <- unlist(strsplit(authors_per_work[j,1], "[;]"))[k]		
                k2 <- k2 + 1					
            }
            k2 <- 1							
            for(k in 1:length(authors)){					
                for(l in 1:nrow(works_author_df)){					
                    if(authors[k] %in% row.names(works_author_df)[l]){				
                        works_author_df[l,cont] <- works_author_df[l,cont]+1		
                    }
                }
            }
            cont <- cont + 1							
            authors_per_work[j,1] <- 0							
        }
    }
}

termDocMatrix <- as.matrix(works_author_df)
termDocMatrix[termDocMatrix >= 1] <- 1
termMatrix <- termDocMatrix %*% t(termDocMatrix)

g <- graph.adjacency(termMatrix, weighted = T, mode = "undirected")
g <- simplify(g)

normalizer <- function(x) (x - min(x)) / (max(x) - min(x)) + 0.25 
V(g)$size <- normalizer(degree(g)) * 10

authors_country <- read.csv("spreadsheet/authors_country.csv", sep = ";", encoding = "latin1", stringsAsFactors = FALSE)
authors_country <- authors_country[,-3]

pal <- colorRampPalette(brewer.pal(9, "Paired"))(length(unique(authors_country$Num)))

coords <- layout_with_fr(g, niter = 1000)
plot(g, vertex.label.color = "black", vertex.label.cex = 0.6, layout = layout.kamada.kawai, vertex.label.font = 2, edge.color = "grey", vertex.color = pal[as.numeric(authors_country$Num)], vertex.label.dist = 1, vertex.frame.color = "black")
legend(x = -2.3, y = 1.4, unique(authors_country$País), pch = 21, col = "#777777", pt.bg = pal[unique(authors_country$Num)], pt.cex = 1.7, cex = .8, bty = "n", ncol = 1)

tkplot(g, canvas.width = 1400, canvas.height = 800, vertex.label.color = "black", vertex.label.cex = 1.3, layout = layout.kamada.kawai, vertex.label.font = 2, edge.color = "grey", vertex.color = pal[as.numeric(authors_country$Num)], vertex.label.dist = 1.5, vertex.frame.color = "black")

g <- remove.vertex.attribute(g, "size")
g_vis <- toVisNetworkData(g) 
names <- sort(g_vis$nodes$id) 
g_vis$nodes$value <- (normalizer(degree(g)))*10
g_vis$nodes$country <- authors_country$País
g_vis$nodes$group <- authors_country$País
vis_plot <- visNetwork(
    nodes = g_vis$nodes, 
    edges = g_vis$edges,
    main = "Collaboration Network",
    submain = "Authors of BI and agriculture published papers",
    ) %>%
visIgraphLayout(
    layout = "layout_with_kk", 
    smooth = FALSE,            
    physics = TRUE             
    ) %>%
visEdges(   color = list(highlight = "lightgray")) %>%
visGroups(  groupname = g_vis$nodes$country) %>%
visOptions( selectedBy = "country",
            highlightNearest = list(enabled = TRUE,
                                    degree = 1,
                                    hover = TRUE,
                                    labelOnly = TRUE),
                                    nodesIdSelection = list(enabled = TRUE,
                                                            values = names)) %>%
visLegend(  position = "right", stepX = 51, stepY = 51 , main = "Country", useGroups = TRUE) %>%
visPhysics( repulsion = list(springlength = 50), 
            maxVelocity = 0.8,
            solver = "forceAtlas2Based",
            forceAtlas2Based = list(gravitationalConstant = -1000),
            timestep = 0.25)

vis_plot

citations_x_authors = read.csv("spreadsheet/citations_x_authors.csv", sep = ";", encoding = "latin1", stringsAsFactors=FALSE)

ggplot( citations_x_authors[,4:3], 
        aes(citations_x_authors[,4], citations_x_authors[,3], label = citations_x_authors[,1])) +
        labs(   x = gsub("\\.", " ", colnames(citations_x_authors[4])), 
                y = gsub("\\."," ", colnames(citations_x_authors[3]))) +
        geom_vline(xintercept = sort(citations_x_authors$Number.of.the.Citations.of.Most.Cited.Author.in.References, decreasing = TRUE)[round(nrow(citations_x_authors)*0.2)] - 0.5) + 
        geom_hline(yintercept = sort(citations_x_authors$Citation.Number, decreasing = TRUE)[round(nrow(citations_x_authors)*0.2)] - 0.5) +
        geom_text_repel() +
        scale_shape_identity() +
        geom_point() 

periodics = read.csv("spreadsheet/periodics.csv", sep = ";", encoding = "latin1", stringsAsFactors = FALSE)

label <- paste(periodics[,1],"(JCR:", periodics[,4], ")")
label <- gsub(" )", ")", label)

ggplot( periodics, 
        aes(x = jitter(periodics[,3],2), y = jitter(periodics[,2]))) +
        labs(x = gsub("\\.", " ", colnames(periodics[3])), y = gsub("\\.", " ", colnames(periodics[2]))) +
        geom_point(aes(colour = colorRampPalette(brewer.pal(11, "Spectral"))(23), size = periodics[,4])) +
        geom_vline(xintercept = 12) +
        geom_hline(yintercept = 1.5) +
        scale_size_continuous(  name = "Journals'/Conferences' of BP and it's JCR",
                                breaks = periodics[,4], labels = label) +
        theme(  legend.key.height = unit(0.52, "cm"),
                legend.text = element_text(colour = "black", size = 8.5, face = "bold"),
                legend.position = "right", 
                legend.background = element_rect(   fill = "lightgray",
                                                    size = 0.5,
                                                    linetype = "solid",
                                                    colour ="black")) +
        guides(size = guide_legend(ncol=1)) + 
        scale_color_discrete(   name = "Journals'/Conferences' of BP and it's JCR",
                                breaks = colorRampPalette(brewer.pal(11, "Spectral"))(23),
                                labels = label)