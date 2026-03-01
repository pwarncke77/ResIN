pkgname <- "ResIN"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
options(pager = "console")
base::assign(".ExTimings", "ResIN-Ex.timings", pos = 'CheckExEnv')
base::cat("name\tuser\tsystem\telapsed\n", file=base::get(".ExTimings", pos = 'CheckExEnv'))
base::assign(".format_ptime",
function(x) {
  if(!is.na(x[4L])) x[1L] <- x[1L] + x[4L]
  if(!is.na(x[5L])) x[2L] <- x[2L] + x[5L]
  options(OutDec = '.')
  format(x[1L:3L], digits = 7L)
},
pos = 'CheckExEnv')

### * </HEADER>
library('ResIN')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
base::assign(".old_wd", base::getwd(), pos = 'CheckExEnv')
cleanEx()
nameEx("Bootstrap_example")
### * Bootstrap_example

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: Bootstrap_example
### Title: Output example for a bootstrapping analysis conducted with the
###   ResIN package
### Aliases: Bootstrap_example
### Keywords: datasets

### ** Examples


data(Bootstrap_example)
str(Bootstrap_example)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("Bootstrap_example", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("BrJSocPsychol_2024")
### * BrJSocPsychol_2024

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: BrJSocPsychol_2024
### Title: Source data for Lüders, A., Carpentras, D. and Quayle, M., 2024.
###   Attitude networks as intergroup realities: Using network‐modelling to
###   research attitude‐identity relationships in polarized political
###   contexts. British Journal of Social Psychology, 63(1), pp.37-51.
### Aliases: BrJSocPsychol_2024
### Keywords: datasets

### ** Examples


data(BrJSocPsychol_2024)
head(BrJSocPsychol_2024)





base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("BrJSocPsychol_2024", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("ResIN")
### * ResIN

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: ResIN
### Title: Flagship function that implements Response Item Network (ResIN)
###   analysis
### Aliases: ResIN

### ** Examples


## Load the 12-item simulated Likert-type toy dataset
data(lik_data)

# Apply the ResIN function to toy Likert data:
ResIN_obj <- ResIN(lik_data, network_stats = TRUE, remove_nonsignificant = TRUE)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("ResIN", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("ResIN_boots_execute")
### * ResIN_boots_execute

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: ResIN_boots_execute
### Title: Carry out prepared bootstrap analyses on ResIN networks
### Aliases: ResIN_boots_execute

### ** Examples

## Load the 12-item simulated Likert-type toy dataset
data(lik_data)

# Apply the ResIN function to toy Likert data:
ResIN_obj <- ResIN(lik_data, network_stats = TRUE,
                      generate_ggplot = FALSE, plot_ggplot = FALSE)

## No test: 
# Prepare for bootstrapping
prepped_boots <- ResIN_boots_prepare(ResIN_obj, n=100, boots_type="resample")

# Execute the prepared bootstrap list
executed_boots <-  ResIN_boots_execute(prepped_boots, parallel = TRUE, detect_cores = TRUE)

# Extract results - here for example, the network (global)-clustering coefficient
ResIN_boots_extract(executed_boots, what = "global_clustering", summarize_results = TRUE)
## End(No test)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("ResIN_boots_execute", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("ResIN_boots_extract")
### * ResIN_boots_extract

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: ResIN_boots_extract
### Title: Extract and summarize the results of a bootstrap simulation
###   undertaken on a ResIN object
### Aliases: ResIN_boots_extract

### ** Examples

## Load the 12-item simulated Likert-type toy dataset
data(lik_data)

# Apply the ResIN function to toy Likert data:
ResIN_obj <- ResIN(lik_data, network_stats = TRUE,
                      generate_ggplot = FALSE, plot_ggplot = FALSE)

## No test: 
# Prepare for bootstrapping
prepped_boots <- ResIN_boots_prepare(ResIN_obj, n=100, boots_type="resample")

# Execute the prepared bootstrap list
executed_boots <-  ResIN_boots_execute(prepped_boots, parallel = TRUE, detect_cores = TRUE)

# Extract results - here for example, the network (global)-clustering coefficient
ResIN_boots_extract(executed_boots, what = "global_clustering", summarize_results = TRUE)
## End(No test)





base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("ResIN_boots_extract", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("ResIN_boots_prepare")
### * ResIN_boots_prepare

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: ResIN_boots_prepare
### Title: Create a bootstrap plan for re-estimating ResIN objects to
###   derive statistical uncertainty estimates
### Aliases: ResIN_boots_prepare

### ** Examples

## Load the 12-item simulated Likert-type toy dataset
data(lik_data)

# Apply the ResIN function to toy Likert data:
ResIN_obj <- ResIN(lik_data, network_stats = TRUE,
                      generate_ggplot = FALSE, plot_ggplot = FALSE)

## No test: 
# Prepare for bootstrapping
prepped_boots <- ResIN_boots_prepare(ResIN_obj, n=100, boots_type="resample")

# Execute the prepared bootstrap list
executed_boots <-  ResIN_boots_execute(prepped_boots, parallel = TRUE, detect_cores = TRUE)

# Extract results - here for example, the network (global)-clustering coefficient
ResIN_boots_extract(executed_boots, what = "global_clustering", summarize_results = TRUE)
## End(No test)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("ResIN_boots_prepare", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("ResIN_to_gephi")
### * ResIN_to_gephi

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: ResIN_to_gephi
### Title: (Deprecated.) Convert ResIN networks to Gephi-readable csv
###   tables. Use 'as.gephi()' method instead.
### Aliases: ResIN_to_gephi

### ** Examples

## Load the 12-item simulated Likert-type ResIN toy dataset
data(lik_data)

## Estimate a ResIN network
res <- ResIN(lik_data, generate_ggplot = FALSE)

## Create Gephi edge table without writing files
edges <- as.gephi(res, dont_save_csv = TRUE)
head(edges)

## Not run: 
##D ## Write CSV file(s) for import to Gephi
##D ## (writes "ResIN_gephi.csv" by default)
##D as.gephi(res, file = "ResIN_gephi.csv")
##D 
##D ## Write both edges and nodes tables
##D ## (writes "ResIN_gephi_edges.csv" and "ResIN_gephi_nodes.csv")
##D as.gephi(res, file = "ResIN_gephi.csv", edges_only = FALSE)
## End(Not run)





base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("ResIN_to_gephi", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("ResIN_to_igraph")
### * ResIN_to_igraph

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: ResIN_to_igraph
### Title: (Deprecated.) Convert a ResIN network into an igraph object. Use
###   'as.igraph()' method instead.
### Aliases: ResIN_to_igraph

### ** Examples


## Load the 12-item simulated Likert-type ResIN toy dataset
data(lik_data)

## Run the function:

igraph_output <-  as.igraph(ResIN(lik_data, plot_ggplot = FALSE))

class(igraph_output)

## Plot and/or investigate as you wish:
## No test: 
igraph::plot.igraph(igraph_output)
## End(No test)





base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("ResIN_to_igraph", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("ResIN_to_qgraph")
### * ResIN_to_qgraph

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: ResIN_to_qgraph
### Title: (Deprecated.) Convert a ResIN network into an qgraph object. Use
###   'as.qgraph()' method instead.
### Aliases: ResIN_to_qgraph

### ** Examples

## Load the 12-item simulated Likert-type ResIN toy dataset
data(lik_data)

## Run the function:
ResIN_qgraph <-  as.qgraph(ResIN(lik_data, plot_ggplot = FALSE))

class(ResIN_qgraph)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("ResIN_to_qgraph", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("as.gephi.ResIN")
### * as.gephi.ResIN

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: as.gephi.ResIN
### Title: Coerce a ResIN object to Gephi CSV table(s)
### Aliases: as.gephi.ResIN

### ** Examples

## Load the 12-item simulated Likert-type ResIN toy dataset
data(lik_data)

## Estimate a ResIN network
res <- ResIN(lik_data, plot_ggplot = FALSE)

## Create Gephi edge table without writing files
edges <- as.gephi(res, dont_save_csv = TRUE)
head(edges)

## Not run: 
##D ## Write CSV file(s) for import to Gephi
##D ## (writes "ResIN_gephi.csv" by default)
##D as.gephi(res, file = "ResIN_gephi.csv")
##D 
##D ## Write both edges and nodes tables
##D ## (writes "ResIN_gephi_edges.csv" and "ResIN_gephi_nodes.csv")
##D as.gephi(res, file = "ResIN_gephi.csv", edges_only = FALSE)
## End(Not run)





base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("as.gephi.ResIN", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("as.graphsjl.ResIN")
### * as.graphsjl.ResIN

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: as.graphsjl.ResIN
### Title: Export a ResIN object to Graphs.jl (Julia) tables
### Aliases: as.graphsjl.ResIN

### ** Examples

## Not run: 
##D data(lik_data)
##D res <- ResIN(lik_data, generate_ggplot = FALSE, plot_ggplot = FALSE)
##D 
##D # Return tables only (no files written)
##D jl_tbls <- as.graphsjl(res, dont_save_csv = TRUE, edges_only = FALSE)
##D 
##D # Default behavior writes CSV files
##D # as.graphsjl(res, file = "ResIN_graphsjl.csv", edges_only = FALSE)
## End(Not run)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("as.graphsjl.ResIN", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("as.igraph.ResIN")
### * as.igraph.ResIN

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: as.igraph.ResIN
### Title: Coerce a ResIN object to an igraph graph
### Aliases: as.igraph.ResIN

### ** Examples

## Load the 12-item simulated Likert-type ResIN toy dataset
data(lik_data)

## Run the function:

igraph_output <-  as.igraph(ResIN(lik_data, plot_ggplot = FALSE))

class(igraph_output)

## Plot and/or investigate as you wish:
## No test: 
igraph::plot.igraph(igraph_output)
## End(No test)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("as.igraph.ResIN", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("as.network.ResIN")
### * as.network.ResIN

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: as.network.ResIN
### Title: Convert a ResIN object to a statnet/network object
### Aliases: as.network.ResIN

### ** Examples

data(lik_data)
res <- ResIN(lik_data, generate_ggplot = FALSE, plot_ggplot = FALSE)

# ResIN re-exports network::as.network()
net <- as.network.ResIN(res) ## alternatively: as.network(res)
net




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("as.network.ResIN", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("as.networkx.ResIN")
### * as.networkx.ResIN

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: as.networkx.ResIN
### Title: Export a ResIN object to NetworkX (Python) tables
### Aliases: as.networkx.ResIN

### ** Examples

## Not run: 
##D data(lik_data)
##D res <- ResIN(lik_data, generate_ggplot = FALSE, plot_ggplot = FALSE)
##D 
##D # Return tables only (no files written)
##D nx_tbls <- as.networkx(res, dont_save_csv = TRUE, edges_only = FALSE)
##D 
##D # Default behavior writes CSV files
##D # as.networkx(res, file = "ResIN_networkx.csv", edges_only = FALSE)
## End(Not run)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("as.networkx.ResIN", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("as.qgraph.ResIN")
### * as.qgraph.ResIN

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: as.qgraph.ResIN
### Title: Coerce a ResIN object to a qgraph object
### Aliases: as.qgraph.ResIN

### ** Examples

## Load the 12-item simulated Likert-type ResIN toy dataset
data(lik_data)

## Run the function:
ResIN_qgraph <-  as.qgraph(ResIN(lik_data, plot_ggplot = FALSE))

class(ResIN_qgraph)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("as.qgraph.ResIN", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("as.tidygraph.ResIN")
### * as.tidygraph.ResIN

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: as.tidygraph.ResIN
### Title: Coerce a ResIN object to a tidygraph graph
### Aliases: as.tidygraph.ResIN

### ** Examples

## Load toy data and estimate ResIN
data(lik_data)
res <- ResIN(lik_data, network_stats = TRUE, detect_clusters = TRUE,
             plot_ggplot = FALSE)

## Convert to tidygraph
tg <- as.tidygraph(res)
tg




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("as.tidygraph.ResIN", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("lik_data")
### * lik_data

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: lik_data
### Title: Likert-type, mock-response data for "ResIN" package examples
### Aliases: lik_data
### Keywords: datasets

### ** Examples


data(lik_data)
head(lik_data)





base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("lik_data", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
### * <FOOTER>
###
cleanEx()
options(digits = 7L)
base::cat("Time elapsed: ", proc.time() - base::get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')
