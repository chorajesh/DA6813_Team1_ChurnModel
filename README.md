# Churn Modeling

Our primary goal is to apply our techniques acquired in the program, and build classification models on a open source dataset to answer a real-world business problem. Secondarily, our interest was to learn and apply new techniques for dealing with imbalanced classes in the response variable and study its impact on the accuracy of the predicting the event and the overall accuracy of the model.

## Getting Started

These instructions will get you a copy of the project up and running on your local machine for development and testing purposes. See deployment for notes on how to deploy the project on a live system.

### Prerequisites

System Requirements
•An Intel-compatible platform running Windows 2000, XP/2003/Vista/7/8/2012 Server/8.1/10.

•At least 32 MB of RAM, a mouse, and enough disk space for recovered files, image files, etc.

•The administrative privileges are required to install and run R-Studio utilities under Windows 2000/XP/2003/Vista/7/8/2012 Server/8.1/10.

•A network connection for data recovering over network.


### Installing

Download the ChurnModeling_CaretEnsembleFinal.Rmd and model_functions.R files along with the data set ChurnDataset1.csv
Using RStudio run the ChurnModeling_CaretEnsembleFinal.Rmd which will install all the required packages for this project.

This project typically requires the following libraries.

	GGally
	ggplot2
	corrplot
	caret
	MASS
	class 
	gam
	tree
	randomForest
	earth
	dplyr
	pROC
	caretEnsemble
	nnet
	e1071
	caretEnsemble
	data.table
	doMC
	DMwR

## Running the tests

We need to validate the models using the test data set which is done as part of the modeling.
We don't have any JUnit code to validate our project. Will update soon.


## Deployment

Just run with RStudio or you can place the required files - ChurnModeling_CaretEnsembleFinal.Rmd and model_functions.R 
along with the data set ChurnDataset1.csv and can run the project.

## Built With

* R
* MS-Office(365)


## Versioning

We use [Git Hub](https://github.com/chorajesh/DA6813_Team1_ChurnModel). 

## Authors

* **Rajesh Chodavarapu** - *Initial work* - [Rajesh Chodavarapu](https://github.com/chorajesh)
* **Biljana Jovanova** - *Initial work* - [Biljana Jovanova](https://github.com/biljanajov)


## License

This project is copy left and open to public.

## Acknowledgments

* [kaggle](https://www.kaggle.com/c/customer-churn-prediction/data)
* [Analytics Vidhya](https://www.analyticsvidhya.com/blog/2017/02/introduction-to-ensembling-along-with-implementation-in-r)
* [ISLR](https://www.springer.com/us/book/9781461471370)
* [Rpubs](https://rpubs.com/zxs107020/370699)


