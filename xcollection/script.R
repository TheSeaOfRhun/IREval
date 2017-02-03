library("reshape2")
library("ggplot2")

## --------------------------------------------------------------------------------------
## NAMING
## --------------------------------------------------------------------------------------
## T = Test-collection
## M = Measure
## Q = Query
## S = Score
## A = Algorithm (that which is usually 'system')
## --------------------------------------------------------------------------------------
## VARIABLE PREFIX:
## --------------------------------------------------------------------------------------
## v = vector
## l = list
## m = matrix
## a = array
## s = string (should be 'c' as in 'character' for R?)
## d = data frame
## w/l = wide/long table format
## f = file name (then what is a string?)
## --------------------------------------------------------------------------------------

# TRECEVAL output -> matrix

## Read treceval file into a table, which happens to be in long-format, but, because of the 'runid' line, which has a string in the 3rd column, all columns are read in as characters. So, aftern reading in the file, mark and drop rows with 'all' in column 2. Then convert it to a wide-table using dcast(). Finally, create a matrix from the data frame.

# Build the Measure x Query x Score Matrix
MQSMatrix <- function(fEval) {
    vEvalHeader = c("measure", "query", "score")
    dlMQS = read.table(fEval, header = FALSE, col.names = vEvalHeader, na.strings = c("runid", "all"))
    dlMQS = na.omit(dlMQS)
    ## Convert long-format table to wide-format table.
    ## Use col 1 as row names and then drop it.
    ## Create matrix from table
    dwMQS = dcast(dlMQS, measure ~ query, value.var = "score")
    rownames(dwMQS) = dwMQS[, 1]
    dwMQS = dwMQS[, -1]
    # write.table(dwMQS, "tables/DEMO.a.p.bm25.20.D.x", quote = FALSE, row.names = FALSE)
    mMQS = data.matrix(dwMQS)
    return(mMQS)
}

### PLOTS

## Prototype 3D plot for measures x queries x scores. Rows 14, 15 and 16 (num_rel, num_rel_ret, num_ret) were dropped to keep scores within a range that creates a decent picture.

#mEval = mEval[ - c(14, 15, 16),]
#persp(x = 1:nrow(mEval), y = 1:ncol(mEval), z = mEval, xlab = "Measures", ylab = "Queries", zlab = "Scores", theta = 50, phi = 45, shade = 0.1, ticktype = "detailed")

## Boxplot rows
# plot(as.factor(rownames(mEval)[ - c(14, 15, 16)]), mEval[ - c(14, 15, 16),]) ## Scatterplot matrix

## Scatterplot measures x queries
# pairs(mEval[-c(14,15,16),])

### Multiple TRECEVAL files -> system x query matrix of scores for a single measure

# Build the Algorithm x Query x Score matrix
AQSMatrix <- function(vfEval) {
    lmEval = lapply(vfEval, function(x) MQSMatrix(x))
    lmAQS = lapply(lmEval, function(z) z["map",])
    vAName = basename(vfEval)
    vQName = names(lmAQS[[1]])
    mAQS = matrix(unlist(lmAQS), nrow = length(vfEval), byrow = T, dimnames = list(vAName, vQName))
    return(mAQS)
}

# Get list of eval files whose names match a regex.
getEvalFileList <- function(regex) {
    vfEval = list.files("data/LTR/evals", pattern = regex, full.names = TRUE)
}

## DEBUG
# fEval = "data/LTR/evals/AP.d.p.bm25.196.T.x"
# mMQS = MQSMatrix(fEval)
# vfEval = getEvalFileList("^AP\\..*")
# mAQS = AQSMatrix(vfEval)

## Build list of mAQS matrices for multiple test-collections.

## DEBUG
vTName = c("AP", "DOE", "FR")
#vfEvalRgx = c("^AP\\.", "^DOE\\.", "^FR\\.")

#vTName = c("AP",   "DOE",     "FR",       "FR94",     "NOFR94",
           #"TREC", "TREC678", "TREC678a", "TREC678b", "TREC678c",
           #"WSJ",  "ZF",      "ZIFF1",    "ZIFF2")
lTIndex = setNames(as.list(1:length(vTName)), vTName)
vfEvalRgx = paste("^", vTName, "\\.", sep = "")
lmAQS = lapply(vfEvalRgx, function(x) {y = getEvalFileList(x); AQSMatrix(y)})

## DEBUG
# Index into the list of mAQS matrices
#print(lmAQS[[lTIndex[["DOE"]]]][1:10, 1:10])

## TODO: get row and column E[X] and Var(X).
lvRowMean = lapply(vTName, function(x) rowMeans(lmAQS[[lTIndex[[x]]]]))

## Chris's table: Algorithm x Testcol x Mean Score
vColName = rownames(lmAQS[[lTIndex[[1]]]])
vAName = sapply(strsplit(vColName, "[.]"), function(x) paste(x[2:4], collapse = "."))
mATS = matrix(unlist(lvRowMean), nrow = length(vTName), byrow = T, dimnames = list(vTName, vAName))

## Plot Chris's table.
dATS = data.frame(mATS)
dATS[, "Algorithm"] = rownames(dATS)
dlATS = melt(dATS)
ggplot(dlATS, aes(variable, value, fill = variable)) + geom_bar(width = 0.4, stat = "identity") + facet_grid(Algorithm ~ .) + theme(strip.text.x = element_text(size = 8, angle = 90), axis.text.x = element_text(angle = 90, vjust = 0.5, size = 6))
