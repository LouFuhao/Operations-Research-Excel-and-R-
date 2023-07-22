regression_tree= rpart(formula = Demand~., data = mydata3, method = "anova")

Prices= c(25, 30, 35)
P = rep(Prices, nrow(Data_test))

possible_k= seq(length(Prices)*min(Prices), length(Prices)*max(Prices), by=5)
possible_k




Cons_coeff
for (n in 1:length(possible_k)){
  for (i in 1:length(P)){
    Data_test2$Price= P[i]
    Data_test2$Relative_Price_of_Competing_Styles= P[i]/(possible_k[n]/3)
    Demand_pred[i]= predict(regression_tree, Data_test2[i,])
  }
  Obj_coeff= Demand_pred*P
  Cons_coeff= matrix(c(1,1,1,0,0,0,0,0,0,
                       0,0,0,1,1,1,0,0,0,
                       0,0,0,0,0,0,1,1,1,
                       P[1],P[2],P[3],
                       P[1],P[2],P[3],
                       P[1],P[2],P[3],P[4],P[5],P[6],P[7],P[8],P[9]),nrow=6, byrow=TRUE)
  Dir = c("==",
          "==",
          "==",
          ">",
          "<",
          "==")
  RHS= c(1,1,1,25,25,possible_k[n])
  Model= lp("max", Obj_coeff, Cons_coeff, Dir, RHS, all.bin= TRUE)
  Objectives[n]= Model$objval
  Solutions[n,]= Model$solution
}
Solutions[match(max(Objectives),Objectives),]
# 1 1 1 0 1 1 1 0
cbind(Solutions, Objectives)
Demand_pred
Obj_coeff
# I put additional constraints, but it doesn't work.
