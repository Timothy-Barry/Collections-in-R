# Timothy Barry, University of Maryland College Park, November 2017

# This script generates the data and plots used in the manuscript "Collections 
# in R: Review and Proposal" by T Barry. Note that these scripts were originally
# written across several .R files. We combine all code into a single script to 
# facilitate code review. The code below is broken up into 7 sections.

# You should be able to run this script on your computer. The script
# will generate 7 data frames and save them in a list. Relevant results from
# these data frames will be plotted. 

# This script will use the following values of nElementsVect (collection sizes at which 
# function execution times are tested) and nRepeats (number of times tests are repeated). 

nElementsVect <- c(seq(from=100, to = 900, by = 100),
                   seq(from=1000, to = 9000, by = 1000),
                   seq(from=10000, to = 100000, by = 10000))
nRepeats <- 25

# The values chosen here will cause the script to take a long
# time to run. Change them to smaller values to speed up the script (e.g., 
# nElementsVect <- seq(from=100, to = 500, by = 100), nRepeats <- 5).
# If you DO change the values of nElementsVect, then you will need to update 
# the range of values plotted by the calls to ggplot in section 8.

# Note that you will need the following packages to run the script: microbenchmark, plyr, gtools,
# sets, hash, hashmap, filehash, rstackdeque, liqueueR, dequer, stdvectors, listenv, ggplot2.

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

# a list to store all results
dfList <- list()
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
  y <- getDataElements("keysToInstantiate", nElements)
  # benchmark
  switch(operation,
         insert = microbenchmark(y <- c(y, getDataElements("singleKeyToAdd", nElements)), times = 1),
         remove = microbenchmark(y <- y[!(y ==  getDataElements("singleKeyToRemove", nElements))], times = 1),
         search = microbenchmark(getDataElements("singleKeyToGet", nElements) %in% y, times = 1)
  )
}

# actually run the benchmarks
functList <- list(sets=setsBM, baseVector=baseVectorBM) # define functList
opList <- c("insert", "remove", "search") # define opList
dfList[["setsBenchmarks"]] <- runBenchmarks(functList, opList, nElementsVect, nRepeats) # run benchmarks

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
  y <- setNames(getDataElements("valuesToInstantiate", nElements),
                getDataElements("keysToInstantiate", nElements))
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
dfList[["mapBenchmarks"]] <- runBenchmarks(functList, opList, nElementsVect, nRepeats)
if (file.exists("myDB")) { # remove file created by "filehash" stored on hard drive
  file.remove("myDB")
}

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
  y <- getDataElements("keysToInstantiate", nElements)
  # benchmark
  switch(operation,
         push = microbenchmark(y <- c(y, getDataElements("singleKeyToAdd", nElements)), times = 1),
         pop = microbenchmark(y <- y[1:(length(y)-1)], times = 1)
  )
}

# actually run the benchmarks
functList <- list(rstackdeque=rstackdequeBM, liqueueR=liqueueRBM, baseVector=baseVectorBM)
opList <- c("push", "pop")
dfList[["stackBenchmarks"]] <- runBenchmarks(functList, opList, nElementsVect, nRepeats)

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
dfList[["dequerStackBenchmarks"]] <- output

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
  y <- rep(let, nElements)
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
dfList[["queueBenchmarks"]] <- runBenchmarks(functList, opList, nElementsVect, nRepeats)

# Again, the dequer package unfortunately only supports the instantiation of a single
# queue object. This means that we must benchmark the dequer queue class separately.
require(dequer)
output <- data.frame()
y <- dequer::queue()
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
dfList[["dequerQueueBenchmarks"]] <- output

####################################
## SECTION 8: Run benchmark tests on the packages that implement sequences
####################################
# Load test packages
require(stdvectors)
require(listenv)

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
  y <- getDataElements("keysToInstantiate", nElements)
  # benchmark
  switch(operation,
         append = microbenchmark(y <- c(y, getDataElements("singleKeyToAdd", nElements)), times = 1),
         replace = microbenchmark(y[nElements/2] <- getDataElements("singleKeyToAdd", nElements), times = 1),
         getElementAtIndex = microbenchmark(y[nElements/2], times = 1),
         remove = microbenchmark(y <- y[-nElements/2], times = 1)
  )
}

# run the benchmarks
functList <- list(stdvectors=stdvectorsBM, listenv=listenvBM, baseVector=baseVectorBM)
opList <- c("append", "replace", "getElementAtIndex", "remove")
dfList[["sequenceBenchmarks"]] <- runBenchmarks(functList, opList, nElementsVect, nRepeats)

####################################
## SECTION 9: Plot results
####################################
# In this section, we plot the results of the benchmark tests.
# Of course we need ggplot! Load it.
require(ggplot2)


# combine the "dequer stack" benchmark results with the stack benchmark resuts; likewise, combine the 
# "dequer queue" benchmark results with the queue benchmark results.
rm(i, bmFiles, temp)
dfList[["stackBenchmarks"]] <- rbind(dfList[["stackBenchmarks"]], mutate(dfList$dequerStackBenchmarks, TimeInRow = 1))
dfList[["queueBenchmarks"]] <- rbind(dfList[["queueBenchmarks"]], mutate(dfList$dequerQueueBenchmarks, TimeInRow = 1))
dfList$dequerQueueBenchmarks <- NULL; dfList$dequerStackBenchmarks <- NULL

# We write a few functions to help with the processing and plotting of data...

# function 1: This function takes a data frame and processes it for plotting with ggplot.
# In particular, it changes units of time to ms, corrects the package names, and produces a column
# for mean execution time.
process <- function(df) {
  out <- adply(.data = df, .margins = 1, .fun = function(row) {
    r <- 1e-3 * as.numeric(row[grep("trial", colnames(df))]) # convert to ms
    data.frame(mean=mean(r), median=median(r), sd=sd(r))
  })
  capitalize <- function(x) {
    x <- as.character(x)
    substr(x, 1, 1) <- toupper(substr(x, 1, 1))
    levs <- sort(unique(x))
    factor(x, levels = levs)
  }
  modifyBase <- function(x) {
    x <- as.character(x)
    x <- gsub(pattern = "Vector", replacement = "", x)
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
  temp <- rbind(subset(df, package %in% pack & nElements >= nStart & nElements <= nEnd & TimeInRow == repeatNo), subset(df, package == "Base" & nElements >= nStart & nElements <= nEnd))
  ggplot(data = temp, mapping = aes(nElements, mean, col = package)) + geom_point() + geom_line() + facet_wrap("op", scales = "free_y") + theme_bw() + theme(plot.title = element_text(hjust = 0.5)) + ggtitle("Benchmark comparison") + xlab("Number of elements in collection") + ylab("Time (ms)") + guides(col=guide_legend(title="Package"))
}

# Process each data frame, and extract each data frame from the list.
for(i in names(dfList)) {
  dfList[[i]] <- process(dfList[[i]])
}

map <- dfList[["mapBenchmarks"]]
set <- dfList[["setsBenchmarks"]]
stack <- rbind(dfList[["stackBenchmarks"]])
queue <- rbind(dfList[["queueBenchmarks"]])
sequence <- dfList[["sequenceBenchmarks"]]

# Now, make the actual plots
# map
a <- ggplot(data = subset(map, nElements >= 10000), mapping = aes(nElements, mean, col = TimeInRow)) + geom_point() + geom_line() + facet_grid(package ~ op, scales = "free_y") + theme_bw() + theme(plot.title = element_text(hjust = 0.5)) + ggtitle("Map benchmark") + xlab("Number of elements in collection") + ylab("Time (ms)") + scale_color_manual(values = c("black", "blue")) + guides(col=guide_legend(title="Times repeated")) + scale_x_continuous(labels = kFormat) + guides(col = FALSE)
# compare each pacakge individually to base
compareAgainstBase(map, "hash", 1000, 5000)
compareAgainstBase(map, "hashmap", 2000, 50000)
compareAgainstBase(map, "filehash", 10000, 100000, repeatNo = 2)

# set
setPlot <- ggplot(data = subset(set, nElements >= 10000 & package != "hash"), mapping = aes(nElements, mean)) + geom_point() + geom_line() + facet_grid(package ~ op, scales = "free_y") + theme_bw() + theme(plot.title = element_text(hjust = 0.5)) + ggtitle("Set benchmark") + xlab("Number of elements in collection") + ylab("Time (ms)") + guides(col=guide_legend(title="Times repeated")) + scale_x_continuous(labels = kFormat)
# compare each package individually to base
compareAgainstBase(set, "hash", 10000, 50000)
compareAgainstBase(set, "sets", 100, 1000)

# stack
stackPlot <- ggplot(data = subset(stack, nElements >= 10000 & !(package %in% c("filehash", "rstack"))), mapping = aes(nElements, mean)) + geom_point() + geom_line() + facet_grid(package ~ op, scales = "free_y") + theme_bw() + theme(plot.title = element_text(hjust = 0.5)) + ggtitle("Stack benchmark") + xlab("Number of elements in collection") + ylab("Time (ms)") + guides(col=guide_legend(title="Times repeated")) + scale_x_continuous(labels = kFormat)
# compare each package individually to base
compareAgainstBase(stack, "dequer", 1000, 10000)
compareAgainstBase(stack, "liqueueR", 10000, 100000)
compareAgainstBase(stack, "rstackdequeue", 10000, 20000)

# queue
queuePlot <- ggplot(data = subset(queue, nElements >= 10000), mapping = aes(nElements, mean)) + geom_point() + geom_line() + facet_grid(package ~ op, scales = "free_y") + theme_bw() + theme(plot.title = element_text(hjust = 0.5)) + ggtitle("Queue benchmark") + xlab("Number of elements in collection") + ylab("Time (ms)") + guides(col=guide_legend(title="Times repeated")) + scale_x_continuous(labels = kFormat)
# compare each package individually to base
compareAgainstBase(queue, "dequer", 1000, 10000)
compareAgainstBase(queue, "liqueueR", 100, 100000)
compareAgainstBase(queue, "rstackdequeue", 100, 100000)

# sequence
sequencePlot <- ggplot(data = subset(sequence, nElements >= 10000 & nElements <= 100000), mapping = aes(nElements, mean)) + geom_point() + geom_line() + facet_grid(package ~ op, scales = "free_y") + theme_bw() + theme(plot.title = element_text(hjust = 0.5)) + ggtitle("Sequence benchmark") + xlab("Number of elements in collection") + ylab("Time (ms)") + guides(col=guide_legend(title="Times repeated")) + scale_x_continuous(labels = kFormat)
# compare each package individually to base
compareAgainstBase(sequence, "stdvectors", 1000, 100000)
compareAgainstBase(sequence, "listenv", 100, 100000)