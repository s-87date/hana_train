# check dir
cd("/Users/hanadate/work/hana_train/Kaggle/titanic/src")
println(pwd())
using DataFrames
using DecisionTree
using Printf
using Statistics
using CSV
# Load in the data
train = CSV.read("../dat/train.csv", copycols=true)
test = CSV.read("../dat/test.csv", copycols=true)

#=
# iris Example
features, labels = load_data("iris")    # also see "adult" and "digits" datasets

# the data loaded are of type Array{Any}
# cast them to concrete types for better performance
features = float.(features)
labels   = string.(labels)

# train depth-truncated classifier
model = DecisionTreeClassifier(max_depth=2)
fit!(model, features, labels)
# pretty print of the tree, to a depth of 5 nodes (optional)
print_tree(model, 5)
# apply learned model
predict(model, [5.9,3.0,5.1,1.9])
# get the probability of each label
predict_proba(model, [5.9,3.0,5.1,1.9])
println(get_classes(model)) # returns the ordering of the columns in predict_proba's output
# run n-fold cross validation over 3 CV folds
# See ScikitLearn.jl for installation instructions
using ScikitLearn.CrossValidation: cross_val_score
accuracy = cross_val_score(model, features, labels, cv=3)

# random forest regression
# train regression forest, using 2 random features, 10 trees,
# averaging of 5 samples per leaf, and 0.7 portion of samples per tree
model = build_forest(labels, features, 2, 10, 0.7, 5)
# apply learned model
apply_forest(model, [-0.9,3.0,5.1,1.9,0.0])
# run 3-fold cross validation on regression forest, using 2 random features per split
n_subfeatures=2; n_folds=3
r2 = nfoldCV_forest(labels, features, n_folds, n_subfeatures)

# set of regression build_forest() parameters and respective default values
# n_subfeatures: number of features to consider at random per split (default: -1, sqrt(# features))
# n_trees: number of trees to train (default: 10)
# partial_sampling: fraction of samples to train each tree on (default: 0.7)
# max_depth: maximum depth of the decision trees (default: no maximum)
# min_samples_leaf: the minimum number of samples each leaf needs to have (default: 5)
# min_samples_split: the minimum number of samples in needed for a split (default: 2)
# min_purity_increase: minimum purity needed for a split (default: 0.0)
n_subfeatures=-1; n_trees=10; partial_sampling=0.7; max_depth=-1
min_samples_leaf=5; min_samples_split=2; min_purity_increase=0.0

model = build_forest(labels, features,
                     n_subfeatures,
                     n_trees,
                     partial_sampling,
                     max_depth,
                     min_samples_leaf,
                     min_samples_split,
                     min_purity_increase)

r2 =  nfoldCV_forest(labels, features,
                     n_folds,
                     n_subfeatures,
                     n_trees,
                     partial_sampling,
                     max_depth,
                     min_samples_leaf,
                     min_samples_split,
                     min_purity_increase)

# saving model
using JLD2
@save "../model/model_file.jld2" model
=#

show(first(train,5))
show(first(test,5))

y_Array=convert(Array, train[!, :Survived])
for i in unique(train[!,:Sex])
    train[!,Symbol(i)] = Int.(train[!,:Sex] .== i) # one_hot
    #train[!,:Sex_Int] = Int.(train[!,:Sex] .== i) # one_col
end

x_Df=convert(DataFrame, train[!, [:male,:Age,:Pclass]])
# filling missing
x_Df[ismissing.(x_Df[!, :Age]), :Age] .= mean(skipmissing(x_Df[!, :Age]))
println(describe(x_Df))
x_Array = convert(Array, x_Df)

model = build_forest(y_Array, x_Array, 2, 10, 0.7, 5)
