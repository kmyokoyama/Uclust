x=matrix(rnorm(1000,-2),ncol =100)
x1=matrix(rnorm(1000,-3),ncol =100)
y=matrix(rnorm(300,6,1),ncol =100)
y1=matrix(rnorm(300,4,1),ncol =100)
z=matrix(rnorm(400,-5,1),ncol =100)
X=(rbind(y,z,x1,y1,x))
#rownames(X)=c("a","b","c","d","e","f","g")

source("uhclust.R")
v=uhclust(X) 



#rownames(X)=c(1:13)
h=hclust(dist((X)))

plot(h)
names(h)
h$merge
h$height
h$order
h$labels
h$call
h$dist.method


a <- list()  # initialize empty object
# define merging pattern: 
#    negative numbers are leaves, 
#    positive are merged clusters (defined by row number in $merge)
a$merge <- matrix(c(-1, -2,
                    -3, -4,
                    1,  2), nc=2, byrow=TRUE ) 
a$height <- c(1, 1.5, 3)    # define merge heights
a$order <- 1:4              # order of leaves(trivial if hand-entered)
a$labels <- LETTERS[1:4]    # labels of leaves
class(a) <- "hclust"        # make it an hclust object
plot(a)                     # look at the result   

#convert to a dendrogram object if needed
ad <- as.dendrogram(a)