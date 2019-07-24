# recomenderSystem
UCT Data Science for industry group assignment 1


Data Science for Industry Project 1				           Due date: 12 August 2019

A company wants to curate a dataset of user reviews that it will then use to construct and sell recommendations. Its business model is to sell individual recommendations based on different subsets of this dataset – to offer, for example, an entry-level recommendation based on say 100 users; a mid-tier recommendation based on more (perhaps 1000) users; and a “pro recommendation” based on even more users (perhaps 5000). Customers would have to pay more to access recommendations constructed from more users.

This idea only works if predictions constructed from more users are more accurate. The company currently has no idea whether this is true, or in general how the accuracy of predicted recommendations might change as a function of the size of the dataset used to construct the prediction. The goal of this project is to help them to answer this question. Since they do not have any data yet, you’ll base your analysis on the MovieLens dataset. Specific project objectives are:

1.	Build a model that predicts the rating a user will give to a movie. The model should be an ensemble model averaging the predictions from user-based, item-based and matrix-decomposition-based collaboration filtering recommendation algorithms.
2.	Evaluate how the accuracy of the model constructed in (1) above changes as a function of the size of the dataset used to construct the prediction. Model accuracy should be assessed using the RMSE. Summarize your results in the form of a plot of expected RMSE against number of users, and use your plot to identify whether the RMSE continues to decrease as more users are added, or whether it reaches some asymptote. Try to provide some advice on the relative change in RMSE per user added.

The MovieLens dataset is available at https://grouplens.org/datasets/movielens/. The company thinks there is no chance that it will have more data for any more than 5 000 users.

Write up your work in the form of a report written in R Markdown. The report should contain a description of the problem, the approach you took, and your results. Your code should be included in the document but this should only be so that your results can be reproduced – the code itself should not be displayed in the final typeset document (use “echo = FALSE”).

Notes and hints:
1.	This project must be done in R without any special purpose recommender systems packages (e.g. recommenderlab)
2.	The factorization of large matrices is computationally intensive, and the methods we used in class may take a very long time to run and/or run into memory problems: they are not efficient. You will need some way of making the code provided in class more efficient – see the R package NNLM.
3.	Anyone else should be able to run the code in your R markdown document to completion – if necessary use set.seed() to set a random seed so that your final results don't change. Check that this works on a computer other than your own.

The submission deadline is on or before 23:59:59 on 12 August 2019. Submissions are made via the Assignments tab on Vula.
