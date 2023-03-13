
#load required packages

library(arules)
library(arulesViz)
library(dplyr)
library(reshape2) 

# load data set

Groceries<-read.csv(file.choose(),header = T)

#Remove column

Groceries<-Groceries[,-2]

dim(Groceries)


# convert data set from a long format to a wide format

Groceries2 <- dcast(Groceries, Member_number ~ itemDescription,
                    fun.aggregate = length,value.var = "itemDescription")

dim(Groceries2)


#replace "Yes" and "No instead to 1 and 0 using ifelse function

Groceries2[,2:168]<-ifelse((Groceries2[,2:168]==1)|(Groceries2[,2:168]==2),
                           "Yes","No")


#remove column 1st column after replacement

Groceries2<-Groceries2[,2:168]


# Convert all columns to factor

Groceries2<- as.data.frame(unclass(Groceries2),stringsAsFactors = TRUE)

str(Groceries2)


#for barplot

yes<-colSums(Groceries2=="Yes")
yes

no<-colSums(Groceries2=="No")
no
purchased<-rbind(yes,no)
purchased


barplot(purchased,legend=rownames(purchased))



#finding rules
rules<-apriori(Groceries2)

rules1 <- apriori(Groceries2, 
                 parameter =list(minlen=2,maxlen=3,conf = 0.95)) 


#find most frequent item ( whole milk) using barplot

barplot(purchased[,159:167],legend=rownames(purchased))


#find important rules

rules2 <- apriori(Groceries2, 
                 parameter = list(minlen=2, maxlen=3,conf = 0.485), 
                 appearance= list(rhs=c("whole.milk=Yes"),default="lhs")) 


inspect(rules2)


#find most important rules


rules3<-apriori(Groceries2, 
                parameter = list(minlen=2, maxlen=4,conf = 0.42), 
                appearance= list(rhs=c("whole.milk=Yes"),
                lhs=c("yogurt=Yes","zwieback=Yes","abrasive.cleaner=Yes","artif..sweetener=Yes", 
                      "baby.cosmetics=Yes","bags=Yes","baking.powder=Yes", "bathroom.cleaner=Yes",                        
                      "beef=Yes","berries=Yes","beverages=Yes","bottled.beer=Yes","whipped.sour.cream=Yes",                                      
                      "bottled.water=Yes","brandy=Yes","brown.bread=Yes","butter=Yes","white.bread=Yes",                        
                      "butter.milk=Yes","cake.bar=Yes","candles=Yes","candy=Yes","soft.cheese=Yes","softener=Yes",                                   
                      "canned.beer=Yes","canned.fish=Yes","canned.fruit=Yes","canned.vegetables=Yes",                            
                      "cat.food=Yes","cereals=Yes","chewing.gum=Yes","chicken=Yes","white.wine=Yes",                                 
                      "chocolate=Yes","chocolate.marshmallow=Yes","citrus.fruit=Yes","cleaner=Yes",                                  
                      "cling.film.bags=Yes","cocoa.drinks=Yes","coffee=Yes","condensed.milk=Yes",                                
                      "cooking.chocolate=Yes","cookware=Yes","cream=Yes","cream.cheese.=Yes",                                  
                      "curd=Yes","curd.cheese=Yes" ,"decalcifier=Yes","dental.care=Yes",                              
                      "dessert=Yes","detergent=Yes","dish.cleaner=Yes","dishes=Yes","spread.cheese=Yes","sugar=Yes",                       
                      "dog.food=Yes","domestic.eggs=Yes","female.sanitary.products=Yes","finished.products=Yes",                                
                      "fish=Yes","flour=Yes","flower..seeds.=Yes","flower.soil.fertilizer=Yes",                                  
                      "frankfurter=Yes","frozen.chicken=Yes","frozen.dessert=Yes","frozen.fish=Yes",                    
                      "frozen.fruits=Yes","frozen.meals=Yes","frozen.potato.products=Yes","frozen.vegetables=Yes",                                  
                      "fruit.vegetable.juice=Yes","grapes=Yes","hair.spray=Yes","ham=Yes",                             
                      "hamburger.meat=Yes","hard.cheese=Yes" , "herbs=Yes","honey=Yes","vinegar=Yes","waffles=Yes",                          
                      "house.keeping.products=Yes","hygiene.articles=Yes","ice.cream=Yes","instant.coffee=Yes" ,                                 
                      "Instant.food.products=Yes","jam=Yes","ketchup=Yes","kitchen.towels=Yes",                            
                      "kitchen.utensil=Yes","light.bulbs=Yes" ,"liqueur=Yes","liquor=Yes","whisky=Yes",                             
                      "liquor..appetizer.=Yes","liver.loaf=Yes","long.life.bakery.product=Yes","make.up.remover=Yes",                              
                      "male.cosmetics=Yes","margarine=Yes","mayonnaise=Yes","meat=Yes","mustard=Yes","napkins=Yes" ,                                
                      "meat.spreads=Yes","misc..beverages=Yes","newspapers=Yes","nut.snack=Yes",                                    
                      "nuts.prunes=Yes","oil=Yes","onions=Yes","organic.products=Yes","turkey=Yes","UHT.milk=Yes",                             
                      "organic.sausage=Yes","other.vegetables=Yes","packaged.fruit.vegetables=Yes", "pasta=Yes",                      
                      "pastry=Yes", "pet.care=Yes","photo.film=Yes","pickled.vegetables=Yes",                                    
                      "pip.fruit=Yes","popcorn=Yes", "pork=Yes","pot.plants=Yes","soap=Yes","soda=Yes",                  
                      "potato.products=Yes","preservation.products=Yes","processed.cheese=Yes","prosecco=Yes",                           
                      "pudding.powder=Yes","ready.soups=Yes","red.blush.wine=Yes","rice=Yes",                              
                      "roll.products.=Yes","rolls.buns=Yes","root.vegetables=Yes","rubbing.alcohol=Yes",                            
                      "rum=Yes","salad.dressing=Yes","salt=Yes", "salty.snack=Yes","tea=Yes","tidbits=Yes",
                      "sauces=Yes","sausage=Yes","seasonal.products=Yes","semi.finished.bread=Yes",
                      "shopping.bags=Yes","skin.care=Yes","sliced.cheese=Yes","snack.products=Yes",
                      "soups=Yes","sparkling.wine=Yes","specialty.bar=Yes","specialty.cheese=Yes",
                      "specialty.chocolate=Yes","specialty.fat=Yes","specialty.vegetables=Yes","spices=Yes" ,
                      "sweet.spreads=Yes","syrup=Yes","toilet.cleaner=Yes","tropical.fruit=Yes"),default="none"))

                                                         
                                                      
inspect(rules3)

#visualizing the rules

plot(rules3)
plot(rules3, method="grouped")
                                                   

#Explore association rules using interactive manipulations and visualization using shiny.

rules_ex <-apriori(Groceries2, 
                   parameter =list(minlen=2,maxlen=4,conf=0.75))


ruleExplorer(rules_ex)                                                      
                                                           
