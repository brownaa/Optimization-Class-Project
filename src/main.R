library(ProjectTemplate)
load.project()

n_d <- length(dishes[,1])					# number of dishes
# n_m <- length(meals[,1])					# number of meals

# define ingredient column names associated with the DV names
# X for ingredient; XG for green ingredient; D for dishes; M for meals
# Build Ingredient Constraints --------------------------------------------

# Green ingredient coefficient matrix
gCost <- ingredients[,"Green Cost"]		# Cost of "green" ingredients
gCoeff <- diag(length(gCost))
i <- 1		# initializing for indexing
ii <- 1		# tracks ingredient index
XG <- c()	# initializing to empty before adding DV names
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
## The following loop identifies the green ingredients with some    ##
## cost >0.  If the item cost = 0, it is deleted from the matrix.   ##
## If the item cost > 0, a DV name is added to the variable XG.     ##
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
while (i <= length(gCoeff[1,])){
	if(gCost[i] == 0) {
		gCoeff <- as.matrix(gCoeff[,-i])  # deletes ingredient column if cost = 0
		gCost  <- gCost[-i]
	}
	else{
		XG <- c(XG,paste("XG_",ii,sep = "")) # adds a DV to XG for all non-zero ingredient costs
		i <- i + 1	# keeps track of the column and index of gCoeff and gCost only
	}
	ii <- ii + 1  # keeps track of the index of the ingredient
}

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##

iCoeff <- cbind(diag(n_i),gCoeff) * -1   # ingredient coefficient matrix
dishes <- t(dishes)
iConstraints <- cbind(iCoeff,dishes)
RHS <- data.frame(rhs=rep(0,length(iConstraints[,1])))
RHS[,2] <- "="
colnames(RHS) <- c("value","sense")

# # Build Dish Constraints --------------------------------------------------
# tmp <- matrix(0,nrow = n_d, ncol = n_i+ncol(gCoeff))	# fills in 0 in coeff for ingredients
# 
# dCoeff <- diag(n_d) * -1
# meals <- t(meals)
# dConstraints <- cbind(tmp,dCoeff,meals)
# tmp <- data.frame(cbind(rep(0, length(dConstraints[,1])),rep("=",length(dConstraints[,1]))))
# colnames(tmp) <- c("value","sense")
# RHS <- rbind(RHS, tmp)

# Build Meal Constraints --------------------------------------------
###### need to return vector with 0 and 1 for each type of dish/eater type

# Veggie Meal - main, sides, dessert
vegMeal <- matrix(c(as.numeric(dishType == "VM" | dishType == "MVM"),		# Veg Main
										as.numeric(dishType == "VS" | dishType == "MVS"),		# Veg Side
										as.numeric(dishType == "VD" | dishType == "MVD")),	# Veg Dessert
									,nrow=3, byrow=TRUE)
colnames(vegMeal) <- rownames(dishes)
rownames(vegMeal) <- c("Veg Main", "Veg Side", "Veg Dessert")

# Nonveg Meal - main, sides, dessert
nonvegMeal <- matrix(c(as.numeric(dishType == "MM" | dishType == "MVM"),	# Nonveg Main
										 as.numeric(dishType == "MS" | dishType == "MVS"),		# Nonveg Side
										 as.numeric(dishType == "MD" | dishType == "MVD")),		# Nonveg Dessert
									 ,nrow=3, byrow=TRUE)
colnames(nonvegMeal) <- rownames(dishes)
rownames(nonvegMeal) <- c("Nonveg Main", "Nonveg Side", "Nonveg Dessert")

# Build Meal Nutritional Constraints --------------------------------------

dishNutrition <- crossprod(ingredients,dishes)
# mealNutrition <- crossprod(t(dishNutrition),meals)

r_name <- c()
len <- ncol(dConstraints) - n_m
lead <- rep(0,len)
n_DV <- ncol(dConstraints)
nConstraints <- data.frame()			# Nutritional Constraints
for(i in 1:n_m){
	for(j in 1:nrow(nutrition)){
		ID <- nutrition[j,1]
		limit <- nutrition[j,2]
		nConstraints <- rbind(nConstraints, c(lead, mealNutrition[ID,i], rep(0,n_DV-len-1)) )
		r_name <- c(r_name, paste("M_",i," ",ID," ",limit,": ", sep = ""))
		if ( limit == "Upper Limit"){
			lim <- as.list(subset(subset(nutrition, Var2 == "Upper Limit"),Var1 == ID)["value"])
			tmp <- data.frame(lim,"<=")
			colnames(tmp) <- c("value","sense")
			RHS <- rbind(RHS,tmp)
			
		} else {
			lim <- as.list(subset(subset(nutrition, Var2 == "Lower Limit"),Var1 == ID)["value"])
			tmp <- data.frame(lim,">=")
			colnames(tmp) <- c("value","sense")
			RHS <- rbind(RHS,tmp)
		}
	}
	len <- len + 1
	lead <- rep(0,len)
}
rownames(nConstraints) <- r_name


# Build Meal Type Constraints ---------------------------------------------------
## This section identifies how many of each type of meal must be created

n_veg <- 4
n_nonveg <- 6

len <- ncol(dConstraints) - n_m
lead <- rep(0,len)
meatConstraint <- c()
vegConstraint  <- c()
for (i in 1:length(mealType)){
	if(mealType[i] == "Meat"){
		meatConstraint <- c(meatConstraint,1)
		vegConstraint <- c(vegConstraint,0)
	} else {
		meatConstraint <- c(meatConstraint,0)
		vegConstraint <- c(vegConstraint,1)
	}}

mConstraints <- rbind(c(lead,meatConstraint),
											c(lead,vegConstraint))
rownames(mConstraints) <- c("Carnivores","Vegetarians")
RHS <- rbind(RHS,c(n_nonveg,"="),c(n_veg,"="))


# Building LP Data --------------------------------------------------------
DV <- c(paste("X_",c(1:n_i), sep = ""),           # decision variable names
				XG,          
				paste("D_",c(1:n_d),sep = ""),
				paste("M_",c(1:n_m), sep = ""))
conNames <- c(paste("Ingredient:", rownames(iConstraints)),
							paste("Dish:      ", rownames(dConstraints)),
							rownames(nConstraints), rownames(mConstraints))
colnames(iConstraints) <- DV
colnames(dConstraints) <- DV
colnames(nConstraints) <- DV
colnames(mConstraints) <- DV
CONSTRAINTS <- rbind(iConstraints,dConstraints,nConstraints,mConstraints)

costObjective <- c(ingredients[,1],gCost[gCost>0],
									 rep(0,n_d + n_m))
lp <- make.lp(0,length(DV))
set.objfn(lp, costObjective)
for ( i in 1:nrow(CONSTRAINTS)){
	add.constraint(lp,CONSTRAINTS[i,],RHS[i,"sense"],RHS[i,"value"])
}
dimnames(lp) <- list(conNames,DV)
#******************* need to update the lower bounds!!!!!!!
set.bounds(lp, lower = 0, columns = 1)
write.lp(lp,'./reports/model.lp',type='lp')

solve(lp)
get.objective(lp)
get.variables(lp)
get.constraints(lp)