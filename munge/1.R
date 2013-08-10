# Load Ingredients Data ---------------------------------------------------
ingredients <- read.csv("./data/Model Data.xlsm - Ingredient.csv",stringsAsFactors = FALSE)  #read CSV data from google spreadsheet
ingredients[is.na(ingredients)] <- 0
header <- as.list(read.csv("./data/Model Data.xlsm - Ingredient.csv", header = FALSE, stringsAsFactors=FALSE)[1,])
colnames(ingredients) <- header
row <- as.list(ingredients[,2])
rownames(ingredients) <- row
ingredients <- as.matrix(ingredients[4:ncol(ingredients)])
n_i <- length(ingredients[,1])		# number of ingredients

# Load Dishes Data --------------------------------------------------------
dishes <- read.csv("./data/Model Data.xlsm - Dish.csv",stringsAsFactors = FALSE)  #read CSV data from google spreadsheet
dishes[is.na(dishes)] <- 0
header <- as.list(read.csv("./data/Model Data.xlsm - Dish.csv", header = FALSE, stringsAsFactors=FALSE)[1,])
colnames(dishes) <- c(header[1:3],row)
row <- as.list(dishes[,1])
rownames(dishes) <- row
dishType <- dishes[,2]
dishes <- as.matrix(dishes[,4:ncol(dishes)]/dishes[,"Servings"])

# # Load Meals Data ---------------------------------------------------------
# meals <- read.csv("./data/Model Data.xlsm - Meal.csv",stringsAsFactors = FALSE)  #read CSV data from google spreadsheet
# meals[is.na(meals)] <- 0
# header <- as.list(read.csv("./data/Model Data.xlsm - Meal.csv", header = FALSE, stringsAsFactors=FALSE)[1,])
# colnames(meals) <- c(header[1:3],row)
# row <- as.list(meals[,1])
# rownames(meals) <- row
# mealType <- meals[,3]
# meals <- as.matrix(meals[4:ncol(meals)])

# Load Nutritional Limit Data ---------------------------------------------
nutrition <- read.csv("./data/Model Data.xlsm - Nutrition Limits.csv",stringsAsFactors = FALSE)  #read CSV data from google spreadsheet
header <- as.list(read.csv("./data/Model Data.xlsm - Nutrition Limits.csv", header = FALSE, stringsAsFactors=FALSE)[1,])
colnames(nutrition) <- c(header)
row <- as.list(nutrition[,1])
rownames(nutrition) <- row
nutrition <- as.matrix(nutrition[,2:ncol(nutrition)])
nutrition <- nutrition[,-1]
nutrition <- melt(nutrition,na.rm = TRUE)
i <- sapply(nutrition, is.factor)
nutrition[i] <- lapply(nutrition[i], as.character)
# clean up ----------------------------------------------------------------

rm(list=c("header","row","i"))