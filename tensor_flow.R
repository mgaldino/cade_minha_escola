

### Tensor Flow

## Learning

# source https://rstudio.github.io/tensorflow/

## Precisa do Python. No windows, tem que fzer algumas coisas

## https://www.londonappdeveloper.com/setting-up-your-windows-10-system-for-python-development-pydev-eclipse-python/

## primeiro passo instalar JDE (java)
# baixar eclipse neon
# baixar python 3.5.2 executable (quando executar, clicar em add to path)
# instalar
# instalar pip ver https://stackoverflow.com/questions/4750806/how-do-i-install-pip-on-windows
# rodar depois pip install tensorflow

Sys.setenv(TENSORFLOW_PYTHON="/usr/local/bin/python")
library(devtools)
devtools::install_github("rstudio/tensorflow")

library(tensorflow)

# Create 100 phony x, y data points, y = x * 0.1 + 0.3
x_data <- runif(100, min=0, max=1)
y_data <- x_data * 0.1 + 0.3

# Try to find values for W and b that compute y_data = W * x_data + b
# (We know that W should be 0.1 and b 0.3, but TensorFlow will
# figure that out for us.)
W <- tf$Variable(tf$random_uniform(shape(1L), -1.0, 1.0))
b <- tf$Variable(tf$zeros(shape(1L)))
y <- W * x_data + b

# Minimize the mean squared errors.
loss <- tf$reduce_mean((y - y_data) ^ 2)
optimizer <- tf$train$GradientDescentOptimizer(0.5)
train <- optimizer$minimize(loss)

# Launch the graph and initialize the variables.
sess = tf$Session()
sess$run(tf$initialize_all_variables())

# Fit the line (Learns best fit is W: 0.1, b: 0.3)
for (step in 1:201) {
  sess$run(train)
  if (step %% 20 == 0)
    cat(step, "-", sess$run(W), sess$run(b), "\n")
}