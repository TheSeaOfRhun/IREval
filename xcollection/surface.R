
### Interpolated surface.

## Set up a matrix and convert it to a data frame. Add the row means as a new column to the frame. A 2D interpolated surface is meant to interpolate response variables (z) using a pair of predictors  (x,y). This example is therefore contrived.

set.seed(1303)
mVal = matrix(rnorm(100), 20, 5)
rownames(mVal) = letters[1:20]
colnames(mVal) = toupper(letters[1:5])
dVal = data.frame(mVal)
dVal[, "Mean"] = rowMeans(dVal)

## Mean ~ A + B implies Mean is the response variable and A and B are the predictors. Vectors of x and y axis values are constructed from the range of values in column A and B respectively. I don't know exactly what loess and outer does, but they get me to the point of seeing a nice surface.
 
model = loess(Mean ~ A + B, data = dVal)
x = range(dVal$A)
x = seq(x[1], x[2], length.out = 50)
y = range(dVal$B)
y = seq(y[1], y[2], length.out = 50)
z = outer(x, y, function(A, B) predict(model, data.frame(A, B)))
p = persp(x, y, z, theta = 30, phi = 30, col = "lightblue", expand = 0.5, shade = 0.2)

## TODO: explain this block that plots points in 3D, through which the surface was fitted.

#obs = trans3d(income2$Education, income2$Seniority, income2$Income, p)
#pred = trans3d(income2$Education, income2$Seniority, fitted(model), p)
#points(obs, col = "red", pch = 16)
#segments(obs$x, obs$y, pred$x, pred$y)
