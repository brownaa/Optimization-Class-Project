library(ProjectTemplate)
load.project()
n_d <- length(dishes[,1])					# number of dishes
n_veg <- 4												# number of vegetarians
n_nonveg <- 6											# number of non-vegetarians

# define ingredient column names associated with the DV names
# X for ingredient; XG for green ingredient; D for dishes
# Build Ingredient Constraints --------------------------------------------

# Green ingredient coefficient matrix
gCost <- ingredients[,"Green Cost"]		# Cost of "green" ingredients
gCal <- ingredients[,"CAL"]
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
		gCal   <- gCal[-i]
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
cols <- c(paste( "X_", c(1:n_i), sep = ""),
					XG,
					paste( "D_", c(1:n_d), sep = ""))
colnames(iConstraints) <- cols

RHS <- data.frame(rhs=rep(0,nrow(iConstraints)))
RHS[,2] <- "="
colnames(RHS) <- c("value","sense")

rm(list=c("gCoeff", "iCoeff", "ii"))

# Build Dish Constraints --------------------------------------------
# Set total dish D equal to number of servings for (Veg + Non-veg) for each dish 
# This is done similarly to the 'iConstraints' matrix
tmp <- matrix(0, nrow = n_d, ncol = n_i + length(XG))  # initialize a 0 matrix
dConstraints <- cbind(diag(n_d) * -1, diag(n_d), diag(n_d))
rownames(dConstraints) <- colnames(dishes)
cols <- c(cols, 
					paste( "D_", c(1:20), ",veg", sep = "" ),
					paste( "D_", c(1:20), ",!veg", sep = "" ))
dConstraints <- cbind(tmp, dConstraints)
colnames(dConstraints) <- cols

tmp <- rbind( cbind(rep(0, n_d), "="))
colnames(tmp) <- c("value", "sense")
RHS <- rbind(RHS, tmp)
RHS[,"value"] <- as.numeric(RHS[,"value"])

# Build Meal Constraints --------------------------------------------
# Veggie Meal - main, sides, dessert
vegMeal <- matrix(c(as.numeric(dishType == "VM" | dishType == "MVM"),		# Veg Main
										as.numeric(dishType == "VS" | dishType == "MVS"),		# Veg Side for lower bound
										as.numeric(dishType == "VS" | dishType == "MVS"),		# Veg Side for upper bound
										as.numeric(dishType == "VD" | dishType == "MVD")),	# Veg Dessert
									,ncol=n_d, byrow=TRUE)
# Nonveg Meal - main, sides, dessert
nonvegMeal <- matrix(c(as.numeric(dishType == "MM" | dishType == "MVM"),	# Nonveg Main
										 as.numeric(dishType == "MS" | dishType == "MVS"),		# Nonveg Side for lower bound
										 as.numeric(dishType == "MS" | dishType == "MVS"),		# Nonveg Side for upper bound
										 as.numeric(dishType == "MD" | dishType == "MVD")),		# Nonveg Dessert
									 ,ncol=n_d, byrow=TRUE)

# number of meals for veg and non-veg are set by n_veg & n_nonveg
tmp <- matrix(0, nrow = 8, ncol = n_i + length(XG) + n_d)
tmp2 <- matrix(0, nrow = 4, ncol = n_d)

mConstraints <- cbind(rbind(vegMeal,tmp2),
											rbind(tmp2,nonvegMeal))		# combine veg and non-veg constraints
mConstraints <- cbind(tmp, mConstraints)		# getting it to the size needed for the LP
rownames(mConstraints) <- c("Veg Main", "Veg Side LB", 
														"Veg Side UB", "Veg Dessert",
														"Nonveg Main", "Nonveg Side LB", 
														"Nonveg Side UB", "Nonveg Dessert")
colnames(mConstraints) <- cols

tmp <- rbind( cbind(c(n_veg, n_veg*2, n_veg*3, n_veg), 
										c("=",   ">=",    "<=",    "=")),
							cbind(c(n_nonveg, n_nonveg*2, n_nonveg*3, n_nonveg), 
										c("=",   ">=",    "<=",    "=")))
colnames(tmp) <- c("value", "sense")
RHS <- rbind(RHS, tmp)
RHS[,"value"] <- as.numeric(RHS[,"value"])


rm(list=c("nonvegMeal", "vegMeal", "dishType", "tmp", "tmp2"))

# Build Nutritional Constraints --------------------------------------
# 
dishNutrition <- crossprod(ingredients,dishes)
write.csv(dishNutrition, "./data/dishNutrition.csv")

r_name <- c()    # row names for constraints
# nConstraints <- matrix()			# Nutritional Constraints
tmp <- c()
for(j in 1:nrow(nutrition)){
	ID <- nutrition[j,1]
	limit <- nutrition[j,2]
	if(j != 1)
		nConstraints <- rbind(nConstraints, dishNutrition[ID,])
	else
		nConstraints <- matrix(dishNutrition[ID,],ncol = n_d,byrow = FALSE)
	r_name <- c(r_name, paste(ID," ",limit,": ", sep = ""))
	lim <- as.list(subset(subset(nutrition, Var2 == limit),Var1 == ID)["value"])
	
	if ( limit == "Upper Limit")
		tmp <- rbind(tmp, c(lim,"<="))
	else
		tmp <- rbind(tmp, c(lim,">="))
}
colnames(tmp) <- c("value", "sense")
RHSveg <- as.data.frame(tmp)
RHSveg[,"value"] <- as.numeric(RHSveg[,"value"])
RHSnonveg <- RHSveg

RHSveg$value <- RHSveg$value * n_veg
RHSnonveg$value <- RHSnonveg$value * n_nonveg
RHS <- rbind(RHS,RHSveg,RHSnonveg)

tmp <- matrix(0, nrow = nrow(nutrition), ncol = n_d)
tmp_veg <- cbind(nConstraints,tmp)
colnames(tmp_veg) <- cols[(length(cols)+1-2*n_d):length(cols)]
tmp_nonveg <- cbind(tmp,nConstraints)
colnames(tmp_nonveg) <- cols[(length(cols)+1-2*n_d):length(cols)]
nConstraints <- rbind(tmp_veg,tmp_nonveg)

r_name <- c(paste("Veg",r_name),paste("Nonveg",r_name))
rownames(nConstraints) <- r_name

tmp <- matrix(0, nrow = nrow(nConstraints), ncol = n_i +n_d + length(XG))
nConstraints <- cbind(tmp,nConstraints)
colnames(nConstraints) <- cols

rm(list=c("r_name", "ID", "i", "lim", "limit", "tmp", "j",
					"tmp_veg", "tmp_nonveg", "RHSveg", "RHSnonveg"))

# Build CONSTRAINTS matrix ------------------------------------------------
tmp <- matrix(0, nrow = nrow(iConstraints), ncol = ncol(nConstraints) - ncol(iConstraints))
iConstraints <- cbind(iConstraints,tmp)
colnames(iConstraints) <- cols
CONSTRAINTS <- rbind(iConstraints,dConstraints,mConstraints,nConstraints)

rm(list=c("iConstraints", "dConstraints", "mConstraints", "nConstraints", 
					"tmp"))

# Building LP --------------------------------------------------------
# RHS$value <- as.numeric(RHS$value)
costObjective <- c(ingredients[,1], 
									 gCost, 
									 rep(0,n_d * 3))
greenObjective <- c(ingredients[,3], 
										rep(0,n_d * 3 +length(XG)))
lp <- make.lp(0,length(cols), verbose = "full")
for ( i in 1:nrow(CONSTRAINTS)){
	add.constraint(lp,
								 as.numeric(CONSTRAINTS[i,]),
								 as.character(RHS[i,"sense"]),
								 as.numeric(RHS[i,"value"]))
}
colnames(lp) <- cols
rownames(lp) <- rownames(CONSTRAINTS)
set.bounds(lp, lower = rep(0,length(cols)), 
					 columns = c(1:length(cols)))
w <- 0
inc <- 10			# number of increments to increase 'w' by until w=1
sequence <- seq(0,1,1/inc)
for(w in sequence){
	objFN <- w * costObjective +(1 - w) * greenObjective
	set.objfn(lp, objFN)
# 	write.lp(lp,paste("./reports/model w=", w,".lp", sep = ""))
	solve(lp)
	if(w==0){
		obj <- get.objective(lp)
		DV <- matrix(get.variables(lp),nrow = 1,byrow = FALSE)
		const <- matrix(get.constraints(lp))
		
	} else {
		obj <- c(obj, get.objective(lp))
		DV <- rbind(DV, get.variables(lp))
		const <- cbind(const, get.constraints(lp))
	}
}
w <- sequence
colnames(DV) <- cols
rownames(DV) <- w
rownames(const) <- rownames(lp)
colnames(const) <- w
names(obj) <- w

# Summary Results ---------------------------------------------------------
DV_i 				<- c(1:(n_i+length(XG)))			#ingredient DVs
DV_ig				<- c((n_i+1):length(DV_i))		#green ingredient DVs
DV_d 				<- c((n_i+length(XG)+1):(n_i+length(XG)+20))
DV_dveg 		<- c((n_i+length(XG)+21):(n_i+length(XG)+40))
DV_dnonveg	<- c((n_i+length(XG)+41):(n_i+length(XG)+60))

# u_DV <- DV[!duplicated(DV),]
# u_const <- const[,!duplicated(const, MARGIN=2)]
u_DV <- DV
u_const <- const

# 1 - Report all X and XG
DV   <- t(DV)					# all weights
u_DV <- t(u_DV)				# unique solutions

# 2 - Report Total Cost
Tot_Cost   <- DV   * costObjective			# all weights
u_Tot_Cost <- u_DV * costObjective			# unique solutions
u_NonGreenCal <- u_DV * greenObjective

results_X 				<- u_DV[DV_i,]
results_TotCost 	<- apply(u_Tot_Cost, 2, sum)
results_NonGreenCal <- apply(u_NonGreenCal, 2, sum) / (n_nonveg + n_veg)
results_Dishes 		<- u_DV[DV_d,]
results_Nutrition <- u_const[(nrow(u_const)-(22-1)):nrow(u_const),][-c(8,19),]
results_GreenCal	<- apply( u_DV[DV_ig,] * gCal, 2, sum) / (n_nonveg + n_veg)

write.csv(results_X,         "./reports/results_X.csv")
write.csv(results_TotCost,   "./reports/results_TotCost.csv")
write.csv(results_Dishes,    "./reports/results_Dishes.csv")
write.csv(results_Nutrition, "./reports/results_Nutrition.csv")
write.csv(results_GreenCal,  "./reports/results_GreenCal.csv")

jpeg("./graphs/Meal Cost.jpg")
plot(w,results_TotCost,  type = "l",
		 xlab=expression(W[1]), ylab="Total Cost",
		 main="Meal Cost")
dev.off()
jpeg("./graphs/Non-green Cal.jpg")
plot(w,results_NonGreenCal, type = "l",
		 xlab=expression(W[1]), ylab="Non-green Calories",
		 main="Non-Green Calories (per meal)")
dev.off()
jpeg("./graphs/Green Calories.jpg")
plot(w,results_GreenCal, type = "l",
		 xlab=expression(W[1]), ylab="Green Calories",
		 main="Green Calories (per meal)")
dev.off()
###### plotting tradeoffs between cost and calories
jpeg("./graphs/Non-Green CAL vs. Meal Cost.jpg")
plot(results_TotCost,results_NonGreenCal, type = "l",
		 xlab="Total Cost", ylab="Non-green Calories",
		 main="Non-green Calories vs. Total Cost")
dev.off()
jpeg("./graphs/Green CAL vs. Meal Cost.jpg")
plot(results_TotCost,results_GreenCal, type = "l",
		 xlab="Total Cost", ylab="Green Calories",
		 main="Green Calories vs. Total Cost")
dev.off()