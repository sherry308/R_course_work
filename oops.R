# objects in R
library(ALL)
library(GenomicRanges)

# S3 object example (linear Regression)
df <- data.frame(y = rnorm(10), x = rnorm(10))
lm.object <- lm(y ~ x, data = df)
lm.object
class(lm.object)
names(lm.object)

#####
xx <- list(a = letters[1:3], b = rnorm(3))
xx
xx$a
class(xx) <- "lm"
xx$b

#if (!require("BiocManager", quietly = TRUE))
 # install.packages("BiocManager")

#BiocManager::install("ALL")
library(ALL)
data(ALL)
ALL
class(ALL)
isS4(ALL)
show(ALL)
getClass("ExpressionSet")
#Data inside an S4 class are organized into slots. You access slots by using either ‘@’ or the ’slots()` function, like
#### create expression sets and access data from expression sets

# View available datasets
data(package = "ALL")

# Load the ALL dataset
# Access the ExpressionSet object
eset <- ALL
# Access expression data
expression_data <- exprs(eset)
expression_data[1:5,1:5]
dim(expression_data)
# Access sample metadata
sample_metadata <- pData(eset)
print(summary(pData(ALL)))

# Access feature metadata
feature_metadata <- fData(eset)

##### create your own expressionset object

# Load the necessary library
library(Biobase)

# Define sample data
sample_data <- data.frame(
  SampleID = c("S1", "S2", "S3", "S4"),
  Condition = c("Control", "Control", "Treatment", "Treatment")
)
library(Biobase)
# Define expression matrix
expression_data <- matrix(
  c(10, 15, 20, 25,    # Expression values for gene A
    5, 8, 12, 18,      # Expression values for gene B
    30, 35, 40, 45),   # Expression values for gene C
  nrow = 3,            # Number of genes
  ncol = 4,            # Number of samples
  byrow = TRUE         # Fill matrix by row
)

# Define feature metadata (gene metadata)
feature_data <- data.frame(
  GeneID = c("GeneA", "GeneB", "GeneC"),                     # Gene identifiers
  GeneSymbol = c("ABC1", "DEF2", "GHI3"),                    # Gene symbols
  Chromosome = c("Chr1", "Chr2", "Chr3"),                    # Chromosome information
  Description = c("Example gene A", "Example gene B", "Example gene C")  # Gene descriptions
)


minimalSet <- ExpressionSet(assayData=expression_data)
# Create ExpressionSet object with feature metadata
my_expression_set <- ExpressionSet(
  assayData = expression_data,
  phenoData = AnnotatedDataFrame(sample_data),
  featureData=AnnotatedDataFrame(feature_data),
)
my_expression_set
# Print the ExpressionSet object
print(my_expression_set)

exprs(my_expression_set)
pData(my_expression_set)
fData(my_expression_set)


#### 
my_expression_set <- ExpressionSet(
  assayData = expression_data,
  #phenoData = AnnotatedDataFrame(sample_data),
  featureData=AnnotatedDataFrame(feature_data),
)

