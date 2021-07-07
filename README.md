# oscbvar

This is a package containing code and data for the paper *local prediction pools*. Turns out that it mostly contains the parts relating to the empirical parts. Maybe I should reorg things.


## Generating the predictions of the agents

The predictions of the agents, which I will sometimes refer to as "atomic" predictions (as opposed to aggregate predictions made with pooling methods) are generated using notebook function (this nomenclature is maybe outdated by now), one notebook exists for each model.

This is probably a stupid way of doing things, as I am basically creating a function (the notebook-generator) that contains all the code required to run a specific model, and then generates a bunch of predictions from that function. BASICALLY, what I should be doing instead is having a general "prediction generating function" that takes a specific model as input and then does a switch or something similar.

BUT, things being as they are, here is the logic:

* when we create predictions, we are going to want some settings to be the same for all the models. These "parameters" are stored in a list that we send to the notebook function called agc (atomic generation controllers) and include which observation to consider as the first observation (in terms of the variable t), whether to use a rolling estimation window or not, and which variable is the response variable.
* we also supply a data set. This should contain a column named t that contains "time index" since we are dealing with time series data in the application.
* The rest of the stuff that is supplied are settings for the specific model.

### Todo
 * Break out the actual model (such as "BVAR") from the notebook function.