# AI PROJECT DECISION TREE

## How to run our project

### Install necessary libraries

Please use Visual Studio or whatever necessary to get F# libraries installed. For this project, we are using ```FSharp.Data``` and ```MathNet.Numerics```. After that, change your reference paths on top of the ```Index.fsx``` file.

### Change directory path

Please update your directory path on variable ```BASE_PATH``` so it can find its relative paths of the ```.names``` and ```.data``` files.

#### How to run

In Visual Studio Code, plase make sure the F# package is installed, then select all of ```Index.fsx``` code and press ```Alt + Enter```

In Visual Studio, similar approach can be used. However, one may also press the ```run``` button to have a compiled executable running alone.

### Generated files

#### Training decision tree

The generated JSON file called ```train_result.json``` contains the necessary information for predicting the 70% training data in hold-out tests.

#### Testing decision tree

The generated JSON file called ```test_result.json``` contains the information needed for predicting the 30% testing data in hold-out tests.
