
library(ggplot2)
library(dplyr)

data(diamonds)

# simple plotting : plot original data set as is
plotDF <- diamonds

# look at size, fields of plot DF
dim(plotDF)
names(plotDF)
# 
qplot(carat,price,colour=color,shape=cut,size=z,data=plotDF)


#now, aggregate by X, which is carat, and the variables corresponding to
#geoms color, shape and size - these are 
# color, cut and z.
# note that color and cut are factors and z is numeric.
# take the mean of Y, which is price as the aggregated value

groupDiamonds <- group_by(diamonds,carat,color,cut,z)
plotDF <- summarize(groupDiamonds,mean_price=mean(price))

# have a look at the field names, and the size of plotDF
# plotDF has less rows than diamonds : because diamonds has non-unique combinations
# of carat, color, cut and z.
# plotDF only has unique combinations per row
# the only aggregated field is z: where we now have the mean of z for each combination
# of the other 4 fields.
dim(plotDF)
names(plotDF)
# now we run EXACTLY the same qplot command as last time, except we replace price with mean_price
qplot(carat,mean_price,colour=color,shape=cut,size=z,data=plotDF)

# there is nothing special about any geom field other than some geoms must be numeric,
# others must be categoric.


# now, what do we mean by additional aggregators ?
# this means additional fields to DISaggregate the data, but not appearing as geoms/facets etc
# for example - let's say we wanted to disaggregate the data by clarity, along with the 
# four fields already used...
# we just add clarity to the list of aggregators coming from geoms or X

groupDiamonds <- group_by(diamonds,carat,color,cut,z,clarity)
plotDF <- summarize(groupDiamonds,mean_price=mean(price))

dim(plotDF)
names(plotDF)

# plotDF is now bigger than the previous example, because each unique combination of the 
# 5 aggregation variables (X, 3 geoms, and one additional)
# gets its own row
# it is still smaller than the original data set, because there are cases of multiple rows
# with the same combination of the five fields.

# The plot command is exactly the same as last time : we only have the three geoms from before
# but clarity was used in the step to build the data set plotDF
# to disaggregate the data set from the previous example by clarity

qplot(carat,mean_price,colour=color,shape=cut,size=z,data=plotDF)
[7/11/15, 11:27:10 PM] Eugene Dubossarsky: correction to the above : the only aggregated field is PRICE not Z !
  [7/11/15, 11:27:13 PM] Eugene Dubossarsky: apologies.
[7/11/15, 11:28:01 PM] Eugene Dubossarsky: lines 29 and 30 should read:
  [7/11/15, 11:28:02 PM] Eugene Dubossarsky: # the only aggregated field is price: where we now have the mean of price for each combination
  # of the other 4 fields.