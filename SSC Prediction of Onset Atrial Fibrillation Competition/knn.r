# NOTE: You need to extract the all the functions on the section 8

# K-NN Method (for 2 PCs - Feel free to change them)
# Extract PC1 and PC2
PC1 <- pca$x[, "PC1"]
PC2 <- pca$x[, "PC2"]

# Create train.data with PCs
train.data <- data.frame(PC1, PC2) 

# Assuming pca_cbind_1$Class is the correct class vector
train.class <- pca_cbind_1$Class  

# Check lengths to ensure they match
print(nrow(train.data))
print(length(train.class))

# Convert train.class to a factor for k-NN compatibility
train.class <- as.factor(train.class)

# Now, run the k-NN function
f <- knn(train.data, points, train.class, k = 5)

# Print the k-NN output
print(f)

# Expressions for model and prediction for k = 5
model.exp <- expression({})
predict.exp <- expression(unclass(knn(train.data, points, train.class, k = 5)) - 1)

# Set up the display layout
oldpar <- par(mfrow = c(2, 3))

# Ensure 'class' and 'test.class' are defined and passed to example.display
class <- train.class

# Call example.display without expand.grid
example.display(train.data, class, 
                test.data = {}, test.class = {}, 
                numb.xy.pts = c(100, 100), 
                title = "1-NN", 
                model.exp, 
                predict.exp, 
                extra = 1)
