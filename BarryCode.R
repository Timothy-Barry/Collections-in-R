# Timothy Barry, University of Maryland College Park, November 2017

# This script generates the data and plots used in the manuscript "Collections 
# in R: Review and Proposal" by T Barry. Note that these scripts were originally
# written across several .R files. We combine all code into a single script to 
# facilitate code review. The code below is broken up into 7 sections.

# You should be able to run this script on your computer. The script
# will generate 7 data frames. The data frames will be saved upon generation, and then removed from
# the workspace.

# This script will use the following values of nElementsVect (collection sizes at which 
# function execution times are tested) and nRepeats (number of times tests are repeated). 

setwd("/Users/TimBarry/Box Sync/RCollections/Data/Sample/") # set directory to your own empty folder

nElementsVect <- c(seq(from=100, to = 900, by = 100),
                   seq(from=1000, to = 9000, by = 1000),
                   seq(from=10000, to = 100000, by = 10000))
nRepeats <- 25

if (TRUE) { # alternative option -- much shorter
  nElementsVect <- seq(from=100, to = 300, by = 100)
  nRepeats <- 5
}

# The values chosen here will cause the script to take a long
# time to run. Change them to smaller values to speed up the script (e.g., 
# nElementsVect <- seq(from=100, to = 500, by = 100), nRepeats <- 5).
# If you DO change the values of nElementsVect, then you will need to update 
# the range of values plotted by the calls to ggplot in section 8.

# Note that you will need the following packages to run the script: microbenchmark, plyr, gtools,
# sets, hash, hashmap, filehash, rstackdeque, liqueueR, dequer, stdvectors, listenv, S4Vectors, ggplot2.

####################################
## SECTION 1: Load data and packages
####################################

# laod packages that help with processing
require(microbenchmark)
require(plyr)
require(gtools)

# generate test data
charPerms <- apply(permutations(n = 13, r = 5, v = letters[1:13]), 
                   1, function(x) paste0(x, collapse = ""))[1:(1e5+1)] # a character vector
numVals <- rnorm(1e5 + 1) # a numeric vector

####################################
## SECTION 2: Define some basic functions to help with benchmarking
####################################

# A function to return elements from the test data
getDataElements <- function(what, nElements) {
  switch(what,
         keysToInstantiate = charPerms[1:nElements],
         valuesToInstantiate = numVals[1:nElements],
         singleKeyToAdd = charPerms[nElements + 1],
         singleValueToAdd = numVals[nElements + 1],
         singleKeyToGet = charPerms[nElements/2],
         singleKeyToRemove = charPerms[nElements/2])
}


# The "workhorse" function of the script. This function takes a 
# list of functions (functList), operations (oplist), vector specifying the 
# number of elements to test (nElementsVect), and atomic numeric vector specifying
# the number of times to repeat each benchmark (nTimesRepeat). It returns a 
# dataframe containing the results of the benchmark test.
runBenchmarks <- function(functList, opList, nElementsVect, nTimesRepeat) {
  df <- expand.grid(package = names(functList), op = opList, nElements = nElementsVect)
  resultForRow <- function(package, op, nElements, functList, nTimesRepeat) {
    if (package == "filehash" && op == "get") { # a special case for the "get" operation of filehash
      funct <- functList[[package]]
      output <- as.data.frame(matrix(data = NA, ncol = nTimesRepeat, nrow = 2))
      colnames(output) <- paste0("trial", 1:nTimesRepeat)
      print(paste0("package: ", package, ", operation: ", op, ", n: ", nElements))
      for(i in 1:nTimesRepeat) {
        res <- do.call(funct, args = list(operation=as.character(op), nElements=nElements))
        output[1,i] <- summary(res[[1]], unit = "us")[["mean"]]
        output[2,i] <- summary(res[[2]], unit = "us")[["mean"]]
      }
      cbind(data.frame(TimeInRow = 1:2), output)
    } else { # the standard code that is executed
      funct <- functList[[package]]
      output <- as.data.frame(matrix(data = NA, ncol = nTimesRepeat, nrow = 1))
      colnames(output) <- paste0("trial", 1:nTimesRepeat)
      print(paste0("package: ", package, ", operation: ", op, ", n: ", nElements))
      for(i in 1:nTimesRepeat) {
        res <- do.call(funct, args = list(operation=as.character(op), nElements=nElements))
        output[[i]] <- ifelse(is.null(res), NA, summary(res, unit = "us")[["mean"]])
      }
      cbind(data.frame(TimeInRow = 1), output)
    }
  }
  mdply(df, resultForRow, functList = functList, nTimesRepeat = nTimesRepeat)
}

####################################
## SECTION 3: Run benchmark tests on the packages that implement sets
####################################
# Recall the set API:
# 1) insert
# 2) remove
# 3) search

# Load test packages
require(sets)

# Benchmarks to run for each package...

# 1. sets package
setsBM <- function(operation, nElements) {
  # instantiate object
  y <- as.set(getDataElements("keysToInstantiate", nElements))
  # benchmark
  switch(operation,
         insert = microbenchmark(y <- set_union(y, getDataElements("singleKeyToAdd", nElements)), times = 1),
         remove = microbenchmark(y <- set_complement(getDataElements("singleKeyToRemove", nElements), y), times = 1),
         search = microbenchmark(set_contains_element(getDataElements("singleKeyToGet", nElements), y), times = 1)
  )
}

# 2. base
baseVectorBM <- function(operation, nElements) {
  # instantiate object
  y <- as.list(getDataElements("keysToInstantiate", nElements))
  # benchmark
  switch(operation,
         insert = microbenchmark(y <- if (!getDataElements("singleKeyToAdd", nElements) %in% y) 
                                   c(y, getDataElements("singleKeyToAdd", nElements)) else y, times = 1),
         remove = microbenchmark(y <- y[!(y ==  getDataElements("singleKeyToRemove", nElements))], times = 1),
         search = microbenchmark(getDataElements("singleKeyToGet", nElements) %in% y, times = 1)
  )
}

# actually run the benchmarks
functList <- list(sets=setsBM, baseVector=baseVectorBM) # define functList
opList <- c("insert", "remove", "search") # define opList
result <- runBenchmarks(functList, opList, nElementsVect, nRepeats) # run benchmarks
saveRDS(result, "set.rds")

# clean up
detach("package:sets", unload = T) # detach package
rm(setsBM) # rm package functions
rm(baseVectorBM)
rm(functList) # rm everything else 
rm(opList)
rm(result)

####################################
## SECTION 4: Run benchmark tests on the packages that implement maps
####################################
# Recall the map API
# 1. put
# 2. get
# 3. remove

# Load test packages
require(hash)
require(hashmap)
require(filehash)

# Benchmarks to run for each package...

# hash
hashBM <- function(operation, nElements) {
  # instantiate object
  y <- hash(setNames(getDataElements("valuesToInstantiate", nElements),
                     getDataElements("keysToInstantiate", nElements)))
  # run benchmark
  switch(operation,
         put = microbenchmark(y[[getDataElements("singleKeyToAdd", nElements)]] <- getDataElements("singleValueToAdd", nElements), times = 1),
         get = microbenchmark(y[[getDataElements("singleKeyToGet", nElements)]], times = 1),
         remove = microbenchmark(delete(getDataElements("singleKeyToRemove", nElements), y), times = 1)
  )
}

# hashmap
hashmapBM <- function(operation, nElements) {
  # instantiate object
  y <- hashmap(getDataElements("keysToInstantiate", nElements), getDataElements("valuesToInstantiate", nElements))
  # run benchmark
  switch(operation,
         put = microbenchmark(y[[getDataElements("singleKeyToAdd", nElements)]] <- getDataElements("singleValueToAdd", nElements), times = 1),
         get = microbenchmark(y[[getDataElements("singleKeyToGet", nElements)]], times = 1),
         remove = microbenchmark(y$erase(getDataElements("singleKeyToRemove", nElements)), times = 1)
  )
}

# base
baseBM <- function(operation, nElements) {
  # instantiate object
  y <- as.list(setNames(getDataElements("valuesToInstantiate", nElements),
                getDataElements("keysToInstantiate", nElements)))
  # benchmarks
  switch(operation,
         put = microbenchmark(y[getDataElements("singleKeyToAdd", nElements)] <- getDataElements("singleValueToAdd", nElements), times = 1),
         get = microbenchmark(y[getDataElements("singleKeyToGet", nElements)], times = 1),
         remove = microbenchmark(y <- y[!(names(y) %in% getDataElements("singleKeyToRemove", nElements))], times = 1)
  )
}

# filehash
filehashBM <- function(operation, nElements) {
  # instantiate object
  if (file.exists("myDB")) {
    file.remove("myDB")
  }
  # allow time for file removal
  Sys.sleep(0.5)
  dbCreate("myDB")
  y <- dbInit("myDB")
  keys <- getDataElements("keysToInstantiate", nElements)
  values <- getDataElements("valuesToInstantiate", nElements)
  for (i in 1:length(keys)) {
    dbInsert(y, keys[i], values[i])
  }
  # run benchmark
  if (operation == "get") { # we test "get" twice for this package
    bm1 <- microbenchmark(dbFetch(y, getDataElements("singleKeyToGet", nElements)), times = 1)
    bm2 <- microbenchmark(dbFetch(y, getDataElements("singleKeyToGet", nElements)), times = 1)
    return(list(bm1=bm1, bm2=bm2))
  }
  switch(operation,
         put = microbenchmark(dbInsert(y, getDataElements("singleKeyToAdd", nElements), getDataElements("singleValueToAdd", nElements)), times = 1),
         remove = microbenchmark(dbDelete(y, getDataElements("singleKeyToRemove", nElements)), times = 1)
  )
}

# actually run the benchmarks
functList <- list(hash=hashBM, hashmap=hashmapBM, filehash=filehashBM, baseVector=baseBM)
opList <- c("put", "get", "remove")
result <- runBenchmarks(functList, opList, nElementsVect, nRepeats)
saveRDS(result, "maps.rds")
if (file.exists("myDB")) { # remove file created by "filehash" stored on hard drive
  file.remove("myDB")
}

# clean up

detach("package:hash", unload = T)# detach package
detach("package:hashmap", unload = T)
detach("package:filehash", unload = T)
rm(hashBM) # rm package functions
rm(hashmapBM)
rm(baseBM)
rm(filehashBM)
rm(functList) # rm everything else 
rm(opList)
rm(result)

####################################
## SECTION 5: Run benchmark tests on the packages that implement stacks
####################################
# Recall the stack API
# 1. push
# 2. pop

# load test packages
require(rstackdeque)
require(liqueueR)

# The code to execute for each package...

# rstackdeque
rstackdequeBM <- function(operation, nElements) {
  # instantiate object
  y <- rstackdeque::as.rstack(getDataElements("keysToInstantiate", nElements)[nElements:1])
  # benchmark
  switch(operation,
         push = microbenchmark(y <- rstackdeque::insert_top(y, getDataElements("singleKeyToAdd", nElements)), times = 1),
         pop = microbenchmark(y <- rstackdeque::without_top(y), times = 1)
  )
}

# liqueueR
liqueueRBM <- function(operation, nElements) {
  # instantiate object
  y <- liqueueR::Stack()
  y$data <- as.list(getDataElements("keysToInstantiate", nElements)[nElements:1])
  # benchmark
  switch(operation,
         push = microbenchmark(y$push(getDataElements("singleKeyToAdd", nElements)), times = 1),
         pop = microbenchmark(y$pop(), times = 1)
  )
}

# base
baseVectorBM <- function(operation, nElements) {
  # instantiate object
  y <- as.list(getDataElements("keysToInstantiate", nElements))
  # benchmark
  switch(operation,
         push = microbenchmark(y <- c(y, getDataElements("singleKeyToAdd", nElements)), times = 1),
         pop = microbenchmark(y <- y[1:(length(y)-1)], times = 1)
  )
}

# actually run the benchmarks
functList <- list(rstackdeque=rstackdequeBM, liqueueR=liqueueRBM, baseVector=baseVectorBM)
opList <- c("push", "pop")
result <- runBenchmarks(functList, opList, nElementsVect, nRepeats)
saveRDS(result, "stack.rds")

detach("package:rstackdeque", unload = T) # detach package
detach("package:liqueueR", unload = T)
rm(rstackdequeBM) # rm package functions
rm(liqueueRBM)
rm(baseVectorBM)
rm(functList) # rm everything else 
rm(opList)
rm(result)

 # Unfortunately, the dequer package only supports instantiation of a single stack object. This means that
 # we must benchmark this pacakge separately.
require(dequer)
output <- data.frame()
y <- dequer::stack()
for (i in nElementsVect) {
  print(i)
  for (j in (length(y)+1):i) {
    push(y, charPerms[j])
  } # now, y has length i
  # do the push benchmark
  pushRes <- data.frame(matrix(nrow = 1, ncol = nRepeats))
  colnames(pushRes) <- paste0("trial", 1:nRepeats)
  for (j in 1:nRepeats) {
    temp <- microbenchmark(push(y, getDataElements("singleKeyToAdd", i)), times = 1)
    pushRes[[j]] <- summary(temp, unit = "us")[["mean"]]
    pop(y)
  }
  # do the pop benchmark
  popRes <- data.frame(matrix(nrow = 1, ncol = nRepeats))
  colnames(popRes) <- paste0("trial", 1:nRepeats)
  for (j in 1:nRepeats) {
    temp <- microbenchmark(pop(y), times = 1)
    popRes[[j]] <- summary(temp, unit = "us")[["mean"]]
    push(y, getDataElements("singleKeyToAdd", length(y)))
  }
  # add push and pop results to the dataframe
  pushRow <- cbind(data.frame(package = "dequer", op = "push", nElements = i), pushRes)
  popRow <- cbind(data.frame(package = "dequer", op = "pop", nElements = i), popRes)
  output <- rbind(output, pushRow, popRow)
}
result <- output
saveRDS(result, "stackDequer.rds")
detach("package:dequer", unload = T) 
rm(output, popRes, popRow, pushRes, pushRow, result, temp, i, j, y)

####################################
## SECTION 6: Run benchmark tests on the packages that implement queues
####################################
# Recall the queue API
# 1. enqueue
# 2. dequeue
let <- "aaaaa"

# load test packages
require(rstackdeque)
require(liqueueR)

# the benchmark code for each package...

# rstackdeque
rstackdequeBM <- function(operation, nElements) {
  # instantiate object
  y <- rstackdeque::as.rpqueue(as.list(rep(let, nElements)))
  # benchmark
  switch(operation,
         enqueue = microbenchmark(y <- insert_back(y, let), times = 1),
         dequeue = microbenchmark(y <- (without_front(y)), times = 1)
  )
}

# liqueueR
liqueueRBM <- function(operation, nElements) {
  # instantiate object
  y <- liqueueR::Queue()
  y$data <- as.list(rep(let, nElements))
  # benchmark
  switch(operation,
         enqueue = microbenchmark(y$push(let), times = 1),
         dequeue = microbenchmark(y$pop(), times = 1)
  )
}

# base
baseVectorBM <- function(operation, nElements) {
  # instantiate object
  y <- as.list(rep(let, nElements))
  # benchmark
  switch(operation,
         enqueue = microbenchmark(y <- c(let, y), times = 1),
         dequeue = microbenchmark(y <- y[1:(length(y)-1)], times = 1)
  )
}

# actually run the benchmarks
functList <- list(rstackdeque=rstackdequeBM, liqueueR=liqueueRBM,
                  baseVector=baseVectorBM)
opList <- c("enqueue", "dequeue")
result <- runBenchmarks(functList, opList, nElementsVect, nRepeats)
saveRDS(result, "queue.rds")

detach("package:rstackdeque", unload = T) # detach package
detach("package:liqueueR", unload = T)
rm(rstackdequeBM) # rm package functions
rm(liqueueRBM)
rm(baseVectorBM)
rm(functList) # rm everything else 
rm(opList)
rm(result)

# Again, the dequer package unfortunately only supports the instantiation of a single
# queue object. This means that we must benchmark the dequer queue class separately.
require(dequer)
output <- data.frame()
y <- dequer::queue()
let <- "aaaaa"
for (i in nElementsVect) {
  print(i)
  for (j in (length(y)+1):i) {
    pushback(y, let)
  } # now, y has length i
  # do the enqueue benchmark
  enqueueRes <- data.frame(matrix(nrow = 1, ncol = nRepeats))
  colnames(enqueueRes) <- paste0("trial", 1:nRepeats)
  for (j in 1:nRepeats) {
    temp <- microbenchmark(pushback(y, let), times = 1)
    enqueueRes[[j]] <- summary(temp, unit = "us")[["mean"]]
    pop(y)
  }
  # do the dequeue benchmark
  dequeueRes <- data.frame(matrix(nrow = 1, ncol = nRepeats))
  colnames(dequeueRes) <- paste0("trial", 1:nRepeats)
  for (j in 1:nRepeats) {
    temp <- microbenchmark(pop(y), times = 1)
    dequeueRes[[j]] <- summary(temp, unit = "us")[["mean"]]
    pushback(y, let)
  }
  # add push and pop results to the dataframe
  enqueueRow <- cbind(data.frame(package = "dequer", op = "enqueue", nElements = i), enqueueRes)
  dequeueRow <- cbind(data.frame(package = "dequer", op = "dequeue", nElements = i), dequeueRes)
  output <- rbind(output, enqueueRow, dequeueRow)
}
result <- output
saveRDS(result, "dequerQueue.rds")
detach("package:dequer", unload = T) 
rm(dequeueRes, dequeueRow, enqueueRes, enqueueRow, output, result, temp, i, j, let, y)

####################################
## SECTION 8: Run benchmark tests on the packages that implement sequences
####################################
# Load test packages
require(stdvectors)
require(listenv)
require(S4Vectors)

# the code that will be benchmarked for each package...
# stdvectors package
stdvectorsBM <- function(operation, nElements) {
  # instantiate object
  y <- stdvectorCreate("character")
  stdvectorPushBack(y, getDataElements("keysToInstantiate", nElements))
  # benchmark
  switch(operation,
         append = microbenchmark(stdvectorPushBack(y, getDataElements("singleKeyToAdd", nElements)), times = 1),
         replace = microbenchmark(stdvectorReplace(y, nElements/2, getDataElements("singleKeyToAdd", nElements)), times = 1),
         getElementAtIndex = microbenchmark(stdvectorSubset(y, nElements/2), times = 1),
         remove = microbenchmark(stdvectorErase(y, nElements/2, nElements/2), times = 1)
  )
}

# listenv package
listenvBM <- function(operation, nElements) {
  # instantiate object
  y <- as.listenv(as.list(getDataElements("keysToInstantiate", nElements)))
  # benchmark
  switch(operation,
         append = microbenchmark(y[[length(y) + 1]] <- getDataElements("singleKeyToAdd", nElements), times = 1),
         replace = microbenchmark(y[[nElements/2]] <- getDataElements("singleKeyToAdd", nElements), times = 1),
         getElementAtIndex = microbenchmark(y[[nElements/2]], times = 1),
         remove = microbenchmark(y[[nElements/2]] <- NULL, times = 1)
  )
}

# baseVector
baseVectorBM <- function(operation, nElements) {
  # instantiate object
  y <- as.list(getDataElements("keysToInstantiate", nElements))
  # benchmark
  switch(operation,
         append = microbenchmark(y <- c(y, getDataElements("singleKeyToAdd", nElements)), times = 1),
         replace = microbenchmark(y[nElements/2] <- getDataElements("singleKeyToAdd", nElements), times = 1),
         getElementAtIndex = microbenchmark(y[nElements/2], times = 1),
         remove = microbenchmark(y <- y[-nElements/2], times = 1)
  )
}

# S4Vectors
S4VectorsBM <- function(operation, nElements) {
  # instantiate object
  y <- SimpleList(as.list(getDataElements("keysToInstantiate", nElements)))
  # benchmark
  switch(operation,
         append = microbenchmark(y[[length(y) + 1]] <- getDataElements("singleKeyToAdd", nElements), times = 1),
         replace = microbenchmark(y[[nElements/2]] <- getDataElements("singleKeyToAdd", nElements), times = 1),
         getElementAtIndex = microbenchmark(y[[nElements/2]], times = 1),
         remove = microbenchmark(y[[nElements/2]] <- NULL, times = 1)
  )
}

# run the benchmarks
functList <- list(stdvectors=stdvectorsBM, listenv=listenvBM, baseVector=baseVectorBM, S4Vectors = S4VectorsBM)
opList <- c("append", "replace", "getElementAtIndex", "remove")
result <- runBenchmarks(functList, opList, nElementsVect, nRepeats)
saveRDS(result, "sequence.rds")

# clean up
detach("package:stdvectors", unload = T) # detach package
detach("package:listenv", unload = T)
rm(stdvectorsBM) # rm package functions
rm(listenvBM)
rm(baseVectorBM)
rm(S4VectorsBM)
rm(functList) # rm everything else 
rm(opList)
rm(result)

###################
## Plot the results
###################

require(ggplot2)
mapBenchmarks <- readRDS("maps.rds")
setBenchmarks <- readRDS("set.rds")
stackBenchmarks <- readRDS("stack.rds")
queueBenchmarks <- readRDS("queue.rds")
sequenceBenchmarks <- readRDS("sequence.rds")
stackDequer <- readRDS("stackDequer.rds")
queueDequer <- readRDS("dequerQueue.rds")
stackBenchmarks <- rbind(stackBenchmarks, mutate(stackDequer, TimeInRow = 1))
queueBenchmarks <- rbind(queueBenchmarks, mutate(queueDequer, TimeInRow = 1))

# We write a few functions to help with the processing and plotting of data...

# function 1: This function takes a data frame and processes it for plotting with ggplot.
# In particular, it changes units of time to ms, corrects the package names, and produces a column
# for mean execution time.
process <- function(df) {
  out <- adply(.data = df, .margins = 1, .fun = function(row) {
    r <- 1e-3 * as.numeric(row[grep("trial", colnames(df))]) # convert to ms
    data.frame(mean=mean(r), median=median(r), sd=sd(r), twentyFifth=quantile(r, 0.25), 
               seventyFifth = quantile(r, 0.75), max = max(r), min = min(r),
               ninety = quantile(r, 0.9), ten = quantile(r, 0.1))
  })
  capitalize <- function(x) {
    x <- as.character(x)
    substr(x, 1, 1) <- toupper(substr(x, 1, 1))
    levs <- sort(unique(x))
    factor(x, levels = levs)
  }
  modifyBase <- function(x) {
    x <- as.character(x)
    x <- gsub(pattern = "baseVector", replacement = "base", x)
    u <- unique(x)
    levs <- c(sort(u[u != "base"]), "base")
    factor(x, levels = levs)
  }
  if ("TimeInRow" %in% colnames(df)) {
    mutate(out, package = modifyBase(package), op = capitalize(op), TimeInRow = factor(TimeInRow))
  } else {
    mutate(out, package = modifyBase(package), op = capitalize(op), TimeInRow = 1)
  }
}

# 2. A function that formats the numbers displayed on the x-axis of ggplot.
kFormat <- function(num) {
  ch <- format(num, scientific = F)
  paste0(substr(ch, 1, nchar(ch)-3), "k")
}

# 3. A function to easily compare base execution times against those of other packages
compareAgainstBase <- function(df, pack, nStart, nEnd, repeatNo = 1) {
  temp <- rbind(subset(df, package %in% pack & nElements >= nStart & nElements <= nEnd & TimeInRow == repeatNo), subset(df, package == "base" & nElements >= nStart & nElements <= nEnd))
  ggplot(data = temp, mapping = aes(nElements, mean, col = package)) + geom_point() + geom_line() + facet_wrap("op", scales = "free_y") + theme_bw() + theme(plot.title = element_text(hjust = 0.5)) + ggtitle("Benchmark comparison") + xlab("Number of elements in collection") + ylab("Time (ms)") + guides(col=guide_legend(title="Package"))
}

map <- process(mapBenchmarks)
set <- process(setBenchmarks)
stack <- process(stackBenchmarks)
queue <- process(queueBenchmarks)
sequence <- process(sequenceBenchmarks)

# Plots
require(colorspace)
lineCols <- rainbow_hcl(n=4, l = 10, start = 200, end = 300)
fillCols <- rainbow_hcl(n=4, l = 80, start = 200, end = 300)

# set
setPlot <- ggplot(data = set, mapping = aes(nElements, median)) + geom_ribbon(aes(ymin = twentyFifth, ymax = seventyFifth, fill = package)) + geom_line(aes(color=package)) +geom_point() + facet_grid(package ~ op, scales = "free_y") + theme_bw() + theme(plot.title = element_text(hjust = 0.5)) + ggtitle("Set benchmark") + xlab("Number of elements in collection") + ylab("Time (ms)") + guides(col=guide_legend(title="Times repeated")) + scale_x_continuous(labels = kFormat) +  scale_fill_manual(values=fillCols) + scale_colour_manual(values=lineCols) + theme(legend.position="none") + theme(strip.background =element_rect(fill="white"))

# map
mapPlot <- ggplot(data = map, mapping = aes(nElements, median, shape = TimeInRow)) + geom_ribbon(aes(ymin = twentyFifth, ymax = seventyFifth, fill = package)) + geom_line(aes(color=package)) +geom_point() + facet_grid(package ~ op, scales = "free_y") + theme_bw() + theme(plot.title = element_text(hjust = 0.5)) + ggtitle("Map benchmark") + xlab("Number of elements in collection") + ylab("Time (ms)") + guides(col=guide_legend(title="Times repeated")) + scale_x_continuous(labels = kFormat) +  scale_fill_manual(values=fillCols) + scale_colour_manual(values=lineCols) + theme(legend.position="none") + theme(strip.background =element_rect(fill="white"))

# stack
stackPlot <- ggplot(data = stack, mapping = aes(nElements, median)) + geom_ribbon(aes(ymin = twentyFifth, ymax = seventyFifth, fill = package)) + geom_line(aes(color=package)) +geom_point() + facet_grid(package ~ op, scales = "free_y") + theme_bw() + theme(plot.title = element_text(hjust = 0.5)) + ggtitle("Stack benchmark") + xlab("Number of elements in collection") + ylab("Time (ms)") + guides(col=guide_legend(title="Times repeated")) + scale_x_continuous(labels = kFormat) +  scale_fill_manual(values=fillCols) + scale_colour_manual(values=lineCols) + theme(legend.position="none") + theme(strip.background =element_rect(fill="white"))

# queue
queuePlot <- ggplot(data = queue, mapping = aes(nElements, median)) + geom_ribbon(aes(ymin = twentyFifth, ymax = seventyFifth, fill = package)) + geom_line(aes(color=package)) +geom_point() + facet_grid(package ~ op, scales = "free_y") + theme_bw() + theme(plot.title = element_text(hjust = 0.5)) + ggtitle("Queue benchmark") + xlab("Number of elements in collection") + ylab("Time (ms)") + guides(col=guide_legend(title="Times repeated")) + scale_x_continuous(labels = kFormat) +  scale_fill_manual(values=fillCols) + scale_colour_manual(values=lineCols) + theme(legend.position="none") + theme(strip.background =element_rect(fill="white"))

# sequence
sequencePlot <- ggplot(data = sequence, mapping = aes(nElements, median)) + geom_ribbon(aes(ymin = twentyFifth, ymax = seventyFifth, fill = package)) + geom_line(aes(color=package)) +geom_point() + facet_grid(package ~ op, scales = "free_y") + theme_bw() + theme(plot.title = element_text(hjust = 0.5)) + ggtitle("Sequence benchmark") + xlab("Number of elements in collection") + ylab("Time (ms)") + guides(col=guide_legend(title="Times repeated")) + scale_x_continuous(labels = kFormat) +  scale_fill_manual(values=fillCols) + scale_colour_manual(values=lineCols) + theme(legend.position="none") + theme(strip.background =element_rect(fill="white"))

# Plot
setPlot
mapPlot
stackPlot
queuePlot
sequencePlot