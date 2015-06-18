# ggplot(mtcars, aes(x=am, y=mpg, fill=mpg)) + 
#   geom_bar(stat='identity', position='dodge') 
# x <- subset(mtcars, am==0)
# y <- subset(mtcars, am==1)
# table(x$mpg)
# table(y$mpg)

# # for testing purposes
# df <- mtcars
# df$cyl <- as.factor(df$cyl)
# df$am <- as.factor(df$am)
# df$gear <- as.factor(df$gear)
# 
# aggBy <- c('cyl', 'am')
# aggTarget <- c('mpg', 'hp')
# aggMeth <- c('mean', 'sum', 'count')
# aggregate(df, aggBy, aggTarget, aggMeth)
# 
# aggBy <- c('cyl')
# aggTarget <- c('drat')
# #aggMeth <- c('mean')
# aggMeth <- c('mean', 'count')
# 
# aggBy <- c('am')
# aggTarget <- c('drat')
# aggMeth <- c('sum', 'count')
# x <- aggregate(df, aggBy, aggTarget, aggMeth)
# x
# renameAggColNames(x, aggBy, aggTarget, aggMeth)


# aggBy <- 'cyl'
# aggTarget <- 'mpg'
# aggMeth <- 'mean'
# x <- aggregate(df, aggBy, aggTarget, aggMeth)

# aggBy <- c('cyl', 'am')
# aggTarget <- 'mpg'
# aggMeth <- 'mean'
# x <- aggregate(df, aggBy, aggTarget, aggMeth)
# 
# ggplot(x, aes(x=cyl, y=am))

#rm(list =ls())



# # for testing purposes
# df <- mtcars
# df$cyl <- as.factor(df$cyl)
# df$am <- as.factor(df$am)
# df$gear <- as.factor(df$gear)
# 
# aggBy <- c('cyl', 'am')
# aggTarget <- 'mpg'
# aggMeth <- c('mean', 'count')
# shareOf <- 'cyl'
# shareTarget <- 'mpg_mean'
# 
# df <- aggregate(df, aggBy, aggTarget, aggMeth)
# df <- calcShare(df, shareOf, shareTarget)
