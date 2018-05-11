## ---- echo = FALSE, eval=FALSE, mysize=TRUE, size='\\tiny', cache=TRUE----
pdb0 <- tools::CRAN_package_db()
url <- url("https://bioconductor.org/packages/release/bioc/VIEWS")
dcf <- as.data.frame(read.dcf(url), stringsAsFactors=FALSE)
close(url)
url_ann <- url("https://bioconductor.org/packages/release/data/annotation/VIEWS")
dcf_ann <- as.data.frame(read.dcf(url_ann), stringsAsFactors=FALSE)
close(url_ann)
url_exp <- url("https://bioconductor.org/packages/release/data/experiment/VIEWS")
dcf_exp <- as.data.frame(read.dcf(url_exp), stringsAsFactors=FALSE)
close(url_exp)
o1 <- intersect(names(dcf_exp), names(dcf_ann))
dcf2 <- rbind(dcf_exp[,o1], dcf_ann[,o1])
o2 <- intersect(names(dcf), names(dcf2))
dcf3 <- rbind(dcf2[,o2], dcf[,o2])
o3 <- intersect(names(pdb0), names(dcf3))
pdb <- rbind(pdb0[o3], dcf3[o3])
## #o3 <- intersect(names(pdb0), names(dcf))
## #pdb <- rbind(pdb0[o3], dcf[o3])
## #pdb <- tools::CRAN_package_db()
aut <- pdb$Author

## ---- echo = FALSE, eval=FALSE, mysize=TRUE, size='\\tiny', cache=TRUE, warning=FALSE, results="hide"----
suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(stringr))
suppressPackageStartupMessages(library(igraph))
suppressPackageStartupMessages(library(tidygraph))
suppressPackageStartupMessages(library(ggraph))
suppressPackageStartupMessages(library(magrittr))

## ---- echo = FALSE, eval=FALSE, mysize=TRUE, size='\\tiny', cache=TRUE, results="hide"----
aut <- aut %>%
  str_replace_all("\\(([^)]+)\\)", "") %>%
  str_replace_all("\\[([^]]+)\\]", "") %>%
  str_replace_all("<([^>]+)>", "") %>%
  str_replace_all("\n", " ") %>%
  str_replace_all("[Cc]ontribution.* from|[Cc]ontribution.* by|[Cc]ontributors", " ") %>%
  str_replace_all("\\(|\\)|\\[|\\]", " ") %>%
  iconv(to = "ASCII//TRANSLIT") %>%
  str_replace_all("'$|^'", "") %>%
  gsub("([A-Z])([A-Z]{1,})", "\\1\\L\\2", ., perl = TRUE) %>%
  gsub("\\b([A-Z]{1}) \\b", "\\1\\. ", .) %>%
  map(str_split, ",|;|&| \\. |--|(?<=[a-z])\\.| [Aa]nd | [Ww]ith | [Bb]y ", simplify = TRUE) %>%
  map(str_replace_all, "[[:space:]]+", " ") %>%
  map(str_replace_all, " $|^ | \\.", "") %>%
  map(function(x) x[str_length(x) != 0]) %>%
  set_names(pdb$Package) %>%
  extract(map_lgl(., function(x) length(x) > 1))

## ---- echo = FALSE, eval=FALSE, mysize=TRUE, size='\\tiny', cache=TRUE, results="hide"----
aut_list <- aut %>%
  unlist() %>%
  dplyr::as_data_frame() %>%
  count(value) %>%
  rename(Name = value, Package = n)

## ---- echo = FALSE, eval=FALSE, mysize=TRUE, size='\\tiny', cache=TRUE, results="hide"----
edge_list <- aut %>%
  map(combn, m = 2) %>%
  do.call("cbind", .) %>%
  t() %>%
  dplyr::as_data_frame() %>%
  arrange(V1, V2) %>%
  count(V1, V2)

## ---- echo = FALSE, eval=FALSE, mysize=TRUE, size='\\tiny', cache=TRUE, results="hide"----
g <- edge_list %>%
  select(V1, V2) %>%
  as.matrix() %>%
  graph.edgelist(directed = FALSE) %>%
  as_tbl_graph() %>%
  activate("edges") %>%
  mutate(Weight = edge_list$n) %>%
  activate("nodes") %>%
  rename(Name = name) %>%
  mutate(Component = group_components()) %>%
  filter(Component == names(table(Component))[which.max(table(Component))])

## ---- echo = FALSE, eval=FALSE, mysize=TRUE, size='\\tiny', cache=TRUE, results="hide"----
suppressMessages(g <- g %>%
  left_join(aut_list) %>%
  filter(Package > 4) %>%
  mutate(Component = group_components()) %>%
  filter(Component == names(table(Component))[which.max(table(Component))]))

## ---- echo = FALSE, eval=FALSE, mysize=TRUE, size='\\tiny', cache=TRUE, results="hide"----
g <- mutate(g, Community = group_edge_betweenness(),
            Degree = centrality_degree())

## ---- echo = FALSE, eval=FALSE, mysize=TRUE, size='\\tiny', cache=TRUE, results="hide"----
lapply(1:6, function(i) {filter(g, Community == names(sort(table(Community), decr = TRUE))[i]) %>%
select(Name, Package) %>%
arrange(desc(Package)) %>%
top_n(100, Package) %>% as.data.frame()}) -> comms

## ---- fig6, eval=FALSE, fig.show='hide', fig.height=5, fig.width=12, dev.args=list(family="Fira Sans", bg="transparent")----
library(wordcloud)
opar <- par(mar=c(0,0,0,0)+0.1, mfrow=c(1,2))
for (i in 1:2) wordcloud(comms[[i]]$Name, freq=comms[[i]]$Package, scale=rev(3.75*range(comms[[i]]$Package)/max(comms[[2]]$Package)))
par(opar)

## ---- fig7, eval=FALSE, fig.show='hide', fig.height=5, fig.width=12, dev.args=list(family="Fira Sans", bg="transparent")----
opar <- par(mar=c(0,0,0,0)+0.1, mfrow=c(2,3))
for (i in 1:6) wordcloud(comms[[i]]$Name, freq=comms[[i]]$Package, scale=rev(2.75*range(comms[[i]]$Package)/max(comms[[2]]$Package)))
par(opar)

