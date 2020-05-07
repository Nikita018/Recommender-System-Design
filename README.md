# Recommender-System-Design
Designing a Recommender System using the User based Collaborative Filtering technique




	User Based Collaborative Filtering : We will use the ratings provided by similar users to a target user A to make recommendations for A. The predicted ratings of A will be computed as the weighted average values of these “peer group” ratings for each item.
	
	Approach
	
		1. Now we shall build a recommender system using a User based approach. Our aim is to predict the missing ratings of users in the dataset.
		2. We will use the normalized ratings for computation. The first step is to identify 'k' similar users for every other user. We shall use Cosine similarity and K-nearest neighbors approach
		3. After we have identified 'k' similar users, we will compute the missing ratings for each user by taking the weighted average ratings of the 'k' nearest users as per the similarity score
	

