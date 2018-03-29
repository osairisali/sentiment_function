score.sentiment = function(sentences, pos.words, neg.words, .progress='none')
{
	#require(plyr)
	require(dplyr)
	require(stringr)

	scores = laply(sentences, function(sentence, pos.words, neg.words) {
    
		word.list = str_split(sentence, '\\s+')
		words = unlist(word.list)
    
		#pos.matches = match(words, pos.words)  
		#neg.matches = match(words, neg.words)
	
		#pos.matches = !is.na(pos.matches)
		#neg.matches = !is.na(neg.matches)
		
		# I propose below approach to count the occurences of words based on its sentiments
		# match() only gives POSITIONS of matched elements, not its occurances
		freq <- as.tibble(table(sentence)) # prefer tibble than DF
		pos.matches <- filter(sentence %in% pos.words)
		neg.matches <- filter(sentence %in% neg.words)
		
		score = sum(pos.matches$n) - sum(neg.matches$n)

		return(score)
	}, pos.words, neg.words, .progress=.progress )

	scores.df = data.frame(score=scores, text=sentences)
	return(scores.df)
}
