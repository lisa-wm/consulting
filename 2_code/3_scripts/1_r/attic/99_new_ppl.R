# colapply not really appropriate bc it applies function ... well, on every
# column
# neither is mutate, supposedly, bc it can only handle a single output
# PipeOpTaskPreproc could work

# make_topic_modeling <- function(x) x
# topic_modeling <- mlr3pipelines::po("colapply")
# topic_modeling$param_set$values$applicator <- make_topic_modeling
# topic_modeling$param_set$values$affect_columns <- selector_name(x, y)
# 
# make_feature_extraction <- function(x) x
# feature_extraction <- mlr3pipelines::po("colapply")
# feature_extraction$param_set$values$applicator <- make_feature_extraction
# feature_extraction$param_set$values$affect_columns <- selector_name(x, y)
# 
# graph <- topic_modeling %>>%
#   feature_extraction %>>%
#   mlr3pipelines::mlr_pipeops$get("learner", lrn("classif.svm"))

task = mlr_tasks$get("iris")

# f2 = function(x) {
#   cbind(floor = floor(x), ceiling = ceiling(x))
# }

# poca = po("colapply")
# poca$param_set$values$applicator = f2
# poca$param_set$values$affect_columns = selector_grep("^Petal")
# poca$train(list(task))[[1]]$data()

f3 <- function(x, y, z) {data.table(a = x + y + z, b = y - z)}



pom <- po("mutate")
pom$param_set$values$mutation = list(
  Sepal.Area = ~ Sepal.Width * Sepal.Length,
  "foo%s" = ~ f3(Sepal.Width, Sepal.Length, Petal.Length)
)

pom$train(list(task))[[1]]$data()

graph = pom %>>%
  mlr_pipeops$get(
    "learner",
    learner = mlr_learners$get("classif.rpart"))

glrn <- GraphLearner$new(graph)

cv3 = rsmp("cv", folds = 3)
rr <- resample(task, glrn, cv3)

