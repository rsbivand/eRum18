## ---- echo = FALSE, eval=TRUE, mysize=TRUE, size='\\tiny', cache=TRUE, results="hide"----
system("svn log --xml --verbose -r 6:74688 https://svn.r-project.org/R/trunk > trunk_verbose_log1.xml")
library(XML)
tr <- try(xmlTreeParse("trunk_verbose_log1.xml"))
tr1 <- xmlChildren(xmlRoot(tr))
revs <- sapply(tr1, function(x) unname(xmlAttrs(x)))
msgs <- sapply(tr1, function(x) xmlValue(xmlChildren(x)[["msg"]]))
authors <- unname(sapply(tr1, function(x) xmlValue(xmlChildren(x)[["author"]])))
dates <- strptime(substring(unname(sapply(tr1, function(x) xmlValue(xmlChildren(x)[["date"]]))), 1, 18), format="%Y-%m-%dT%H:%M:%S", tz="UTC")
years <- format(dates, "%Y")

## ---- echo = FALSE, eval=TRUE, mysize=TRUE, size='\\tiny', cache=TRUE, results="hide"----
n1 <- sub("thomas", "tlumley", authors)
n2 <- sub("martyn", "plummer", n1)
n3 <- sub("^r$", "rgentlem", n2)
n4 <- sub("paul", "murrell", n3)
n5 <- sub("root", "system", n4)
n6 <- sub("apache", "system", n5)
authors <- factor(n6)
ad_tab <- table(authors, years)
rs <- rowSums(ad_tab)

## ---- fig1, fig.show='hide', fig.height=6, fig.width=12, dev.args=list(family="Fira Sans", bg="transparent")----
pal <- scan("colormap_hex.txt", "character", quiet=TRUE)
set.seed(1)
pal_s <- sample(sample(pal))
plot(1998:2017, colSums(ad_tab)[2:21], type="b", xlab="", ylab="SVN commits", ylim=c(0, 4000))
grid()
abline(v=c(2000.1639344, 2004.7568306, 2013.2547945), col=pal_s[1:3], lwd=2)
legend("topright", legend=c("1.0.0 2000-02-29", "2.0.0 2004-10-04", "3.0.0 2013-04-03"), col=pal_s[1:3], bty="n", cex=0.8, lty=1, lwd=2)

## ---- fig2, fig.show='hide', fig.height=6, fig.width=12, dev.args=list(family="Fira Sans", bg="transparent")----
barplot(ad_tab[order(rs)[5:27],], col=pal_s)
legend("topright", legend=rev(rownames(ad_tab)[order(rs)[5:27]]), ncol=5, fill=rev(pal_s), cex=0.8, bty="n")

## ---- echo = FALSE, eval=TRUE, mysize=TRUE, size='\\tiny', cache=TRUE----
tr1[[1]]

## ---- echo = FALSE, eval=TRUE, mysize=TRUE, size='\\tiny', cache=TRUE----
tr1[[length(tr1)]]

## ---- echo = FALSE, eval=TRUE, mysize=TRUE, size='\\tiny', cache=TRUE, results="hide"----
res <- vector(mode="list", length=length(tr1))
for (i in seq_along(tr1)) res[[i]] <- {x=xmlChildren(xmlChildren(tr1[[i]])[["paths"]]); list(paths=unname(sapply(x, xmlValue)), actions=unname(sapply(x, function(y) xmlAttrs(y)["action"])))}
names(res) <- revs
rl <- sapply(res, function(x) length(x$actions))

## ---- echo = FALSE, eval=TRUE, mysize=TRUE, size='\\tiny', cache=TRUE----
cat(paste(years[order(rl, decreasing=TRUE)][1:10], revs[order(rl, decreasing=TRUE)][1:10], sort(unname(rl), decreasing=TRUE)[1:10], unname(unlist(sub("\\n", ", ", msgs[order(rl, decreasing=TRUE)])))[1:10]), sep="\n")

## ---- echo = FALSE, eval=TRUE, mysize=TRUE, size='\\tiny', cache=TRUE----
res50 <- res[rl < 50]
rl_50 <- rl[rl < 50]
f_50 <- unlist(sapply(res50, "[", 1))
a_50 <- unlist(sapply(res50, "[", 2))
a_50_actions <- unname(a_50)
f_50_files <- unname(f_50)
f_50_filesa <- substring(f_50_files, 2, nchar(f_50_files))
f_50_filesb <- strsplit(f_50_filesa, "/")
f_50_filesc <- t(sapply(f_50_filesb, function(x) {out <- character(9); out[1:length(x)] <- x; out}))
files_df <- data.frame(f_50_filesc)

## ---- echo = FALSE, eval=TRUE, mysize=TRUE, size='\\tiny', cache=TRUE----
tx <- table(files_df$X2)
sort(tx[tx>350])

## ---- echo = FALSE, eval=TRUE, mysize=TRUE, size='\\tiny', cache=TRUE----
tx <- table(files_df[files_df$X2=="src",]$X3)
sort(tx[tx>50])

## ---- echo = FALSE, eval=TRUE, mysize=TRUE, size='\\tiny', cache=TRUE----
tx <- table(files_df[files_df$X2=="src" & files_df$X3=="library",]$X4)
sort(tx[tx>60])

## ---- echo = FALSE, eval=TRUE, mysize=TRUE, size='\\tiny', cache=TRUE----
tx <- table(files_df[files_df$X2=="src" & files_df$X3=="library" & files_df$X4=="base",]$X5)
sort(tx[tx>4])

## ---- echo = FALSE, eval=TRUE, mysize=TRUE, size='\\tiny', cache=TRUE----
a_50_revs <- rep(names(rl_50), times=rl_50)
o <- match(a_50_revs, revs)
a_50_years <- years[o]
t_acts <- table(a_50_years, factor(a_50_actions))

## ---- fig3, fig.show='hide', fig.height=5, fig.width=12, dev.args=list(family="Fira Sans", bg="transparent")----
barplot(t(t_acts), col=rev(pal_s)[1:4])
legend("topright", legend=c("A The item was added", "D The item was deleted", "M The item was changed", "R The item was replaced"), ncol=2, fill=rev(pal_s)[1:4], cex=0.8, bty="n")

## ---- echo = FALSE, eval=TRUE, mysize=TRUE, size='\\tiny', cache=TRUE, warning=FALSE, results="hide"----
suppressPackageStartupMessages(library(BiocInstaller))
bioc <- available.packages(repo = biocinstallRepos()[1])
bioc_ann <- available.packages(repo = biocinstallRepos()[2])
bioc_exp <- available.packages(repo = biocinstallRepos()[3])
cran <- available.packages()
pdb <- rbind(cran, bioc, bioc_ann, bioc_exp)

## ---- echo = FALSE, eval=TRUE, mysize=TRUE, size='\\tiny', cache=TRUE, warning=FALSE, results="hide"----
suppressPackageStartupMessages(library(miniCRAN))
suppressPackageStartupMessages(library(igraph))
suppressPackageStartupMessages(library(magrittr))
pg <- makeDepGraph(pdb[, "Package"], availPkgs = pdb, suggests=FALSE, enhances=TRUE, includeBasePkgs = FALSE)

## ---- echo = FALSE, eval=TRUE, mysize=TRUE, size='\\tiny', cache=TRUE, results="hide"----
pr <- pg %>%
page.rank(directed = FALSE) %>%
use_series("vector") %>%
sort(decreasing = TRUE) %>%
as.matrix %>%
set_colnames("page.rank")

## ---- echo = FALSE, eval=TRUE, mysize=TRUE, size='\\tiny', cache=TRUE----
print(pr[1:30,], digits=4)

## ---- echo = FALSE, eval=TRUE, mysize=TRUE, size='\\tiny', cache=TRUE, results="hide"----
cutoff <- quantile(pr[, "page.rank"], probs = 0.2)
popular <- pr[pr[, "page.rank"] >= cutoff, ]
toKeep <- names(popular)
vids <- V(pg)[toKeep]
gs <- induced.subgraph(pg, vids = toKeep)
cl <- walktrap.community(gs, steps = 3)

## ---- echo = FALSE, eval=TRUE, mysize=TRUE, size='\\tiny', cache=TRUE, results="hide"----
topClusters <- table(cl$membership) %>%
sort(decreasing = TRUE) %>%
head(25)
cluster <- function(i, clusters, pagerank, n=10){
group <- clusters$names[clusters$membership == i]
pagerank[group, ] %>% sort(decreasing = TRUE) %>% head(n)
}
z <- lapply(names(topClusters)[1:15], cluster, clusters=cl, pagerank=pr, n=20)

## ---- fig4, fig.show='hide', fig.height=5, fig.width=12, dev.args=list(family="Fira Sans", bg="transparent"), warning=FALSE----
library(RColorBrewer)
library(wordcloud)
opar <- par(mar=c(0,0,0,0)+0.1, mfrow=c(1,2))
for (i in 1:2) wordcloud(names(z[[i]]), freq=unname(z[[i]]), scale=rev(5*range(unname(z[[i]]))/max(unname(z[[5]]))))
par(opar)

## ---- fig5, fig.show='hide', fig.height=5, fig.width=12, dev.args=list(family="Fira Sans", bg="transparent"), warning=FALSE----
opar <- par(mar=c(0,0,0,0)+0.1, mfrow=c(2,3))
for (i in 1:6) wordcloud(names(z[[i]]), freq=unname(z[[i]]), scale=rev(3*range(unname(z[[i]]))/max(unname(z[[5]]))))
par(opar)

## ---- echo = FALSE, eval=FALSE, mysize=TRUE, size='\\tiny', cache=TRUE----
## pdb0 <- tools::CRAN_package_db()
## url <- url("https://bioconductor.org/packages/release/bioc/VIEWS")
## dcf <- as.data.frame(read.dcf(url), stringsAsFactors=FALSE)
## close(url)
## url_ann <- url("https://bioconductor.org/packages/release/data/annotation/VIEWS")
## dcf_ann <- as.data.frame(read.dcf(url_ann), stringsAsFactors=FALSE)
## close(url_ann)
## url_exp <- url("https://bioconductor.org/packages/release/data/experiment/VIEWS")
## dcf_exp <- as.data.frame(read.dcf(url_exp), stringsAsFactors=FALSE)
## close(url_exp)
## o1 <- intersect(names(dcf_exp), names(dcf_ann))
## dcf2 <- rbind(dcf_exp[,o1], dcf_ann[,o1])
## o2 <- intersect(names(dcf), names(dcf2))
## dcf3 <- rbind(dcf2[,o2], dcf[,o2])
## o3 <- intersect(names(pdb0), names(dcf3))
## pdb <- rbind(pdb0[o3], dcf3[o3])
## #o3 <- intersect(names(pdb0), names(dcf))
## #pdb <- rbind(pdb0[o3], dcf[o3])
## #pdb <- tools::CRAN_package_db()
## aut <- pdb$Author

## ---- echo = FALSE, eval=FALSE, mysize=TRUE, size='\\tiny', cache=TRUE, warning=FALSE, results="hide"----
## suppressPackageStartupMessages(library(tidyverse))
## suppressPackageStartupMessages(library(stringr))
## suppressPackageStartupMessages(library(igraph))
## suppressPackageStartupMessages(library(tidygraph))
## suppressPackageStartupMessages(library(ggraph))
## suppressPackageStartupMessages(library(magrittr))

## ---- echo = FALSE, eval=FALSE, mysize=TRUE, size='\\tiny', cache=TRUE, results="hide"----
## aut <- aut %>%
##   str_replace_all("\\(([^)]+)\\)", "") %>%
##   str_replace_all("\\[([^]]+)\\]", "") %>%
##   str_replace_all("<([^>]+)>", "") %>%
##   str_replace_all("\n", " ") %>%
##   str_replace_all("[Cc]ontribution.* from|[Cc]ontribution.* by|[Cc]ontributors", " ") %>%
##   str_replace_all("\\(|\\)|\\[|\\]", " ") %>%
##   iconv(to = "ASCII//TRANSLIT") %>%
##   str_replace_all("'$|^'", "") %>%
##   gsub("([A-Z])([A-Z]{1,})", "\\1\\L\\2", ., perl = TRUE) %>%
##   gsub("\\b([A-Z]{1}) \\b", "\\1\\. ", .) %>%
##   map(str_split, ",|;|&| \\. |--|(?<=[a-z])\\.| [Aa]nd | [Ww]ith | [Bb]y ", simplify = TRUE) %>%
##   map(str_replace_all, "[[:space:]]+", " ") %>%
##   map(str_replace_all, " $|^ | \\.", "") %>%
##   map(function(x) x[str_length(x) != 0]) %>%
##   set_names(pdb$Package) %>%
##   extract(map_lgl(., function(x) length(x) > 1))

## ---- echo = FALSE, eval=FALSE, mysize=TRUE, size='\\tiny', cache=TRUE, results="hide"----
## aut_list <- aut %>%
##   unlist() %>%
##   dplyr::as_data_frame() %>%
##   count(value) %>%
##   rename(Name = value, Package = n)

## ---- echo = FALSE, eval=FALSE, mysize=TRUE, size='\\tiny', cache=TRUE, results="hide"----
## edge_list <- aut %>%
##   map(combn, m = 2) %>%
##   do.call("cbind", .) %>%
##   t() %>%
##   dplyr::as_data_frame() %>%
##   arrange(V1, V2) %>%
##   count(V1, V2)

## ---- echo = FALSE, eval=FALSE, mysize=TRUE, size='\\tiny', cache=TRUE, results="hide"----
## g <- edge_list %>%
##   select(V1, V2) %>%
##   as.matrix() %>%
##   graph.edgelist(directed = FALSE) %>%
##   as_tbl_graph() %>%
##   activate("edges") %>%
##   mutate(Weight = edge_list$n) %>%
##   activate("nodes") %>%
##   rename(Name = name) %>%
##   mutate(Component = group_components()) %>%
##   filter(Component == names(table(Component))[which.max(table(Component))])

## ---- echo = FALSE, eval=FALSE, mysize=TRUE, size='\\tiny', cache=TRUE, results="hide"----
## suppressMessages(g <- g %>%
##   left_join(aut_list) %>%
##   filter(Package > 4) %>%
##   mutate(Component = group_components()) %>%
##   filter(Component == names(table(Component))[which.max(table(Component))]))

## ---- echo = FALSE, eval=FALSE, mysize=TRUE, size='\\tiny', cache=TRUE, results="hide"----
## g <- mutate(g, Community = group_edge_betweenness(),
##             Degree = centrality_degree())

## ---- echo = FALSE, eval=FALSE, mysize=TRUE, size='\\tiny', cache=TRUE, results="hide"----
## lapply(1:6, function(i) {filter(g, Community == names(sort(table(Community), decr = TRUE))[i]) %>%
## select(Name, Package) %>%
## arrange(desc(Package)) %>%
## top_n(100, Package) %>% as.data.frame()}) -> comms

## ---- fig6, eval=FALSE, fig.show='hide', fig.height=5, fig.width=12, dev.args=list(family="Fira Sans", bg="transparent")----
## opar <- par(mar=c(0,0,0,0)+0.1, mfrow=c(1,2))
## for (i in 1:2) wordcloud(comms[[i]]$Name, freq=comms[[i]]$Package, scale=rev(3.75*range(comms[[i]]$Package)/max(comms[[2]]$Package)))
## par(opar)

## ---- fig7, eval=FALSE, fig.show='hide', fig.height=5, fig.width=12, dev.args=list(family="Fira Sans", bg="transparent")----
## opar <- par(mar=c(0,0,0,0)+0.1, mfrow=c(2,3))
## for (i in 1:6) wordcloud(comms[[i]]$Name, freq=comms[[i]]$Package, scale=rev(2.75*range(comms[[i]]$Package)/max(comms[[2]]$Package)))
## par(opar)

