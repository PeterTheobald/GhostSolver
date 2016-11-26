# ghostSolver.py
# Peter@PeterTheobald.com 2016
#
# Ghost is a word game in which players take turns adding letters to a growing word fragment, trying not to be the one to complete a valid word. Each fragment must be the beginning of an actual word, there is a minimum of four letters of a word that counts. The player who completes a word loses the round.
#
# This program reads a dictionary and figures out words that will guarantee a win both for the player who plays first and the player who plays second.
#
# Input: ghostSolver.py wordlist.txt
# Output: Player goes first: list of winning words. Player goes second: list of winning words.
#

import pprint
import fileinput
import re

# read through a list of words, split each word into letters, traverse the letters in the tree and add any new letters found
# throw away any words less than 4 letters long
# throw away any words that are supersets of existing words
def buildTree():
	tree={}
	for line in fileinput.input():
		word=line.rstrip()
		if len(word)<4:
			continue
		# clean out some cruft from online dictionaries, anything with Capitals or punctuation
		if re.search( '[^a-z]', word):
			continue
		p=tree
		newnode=True
		for letter in word:
			if letter in p:
				# this letter has already been created (for a different word), follow it
				p=p[letter]
				newnode=False
			else:
				# This letter isn't here. Is it because we are adding new letters onto the tree
				# or because we've reached the end of a complete existing word?
				if p or newnode:
					# create new node for this letter
					p[letter]={}
					newnode=True
					p=p[letter]
				else:
					# this is a superset of an existing word
					continue
	return tree

def findWinners( goesFirst, tree, word):
	# traverse the tree, find words that lose and prune them. Only pass back up the winning words
	if (goesFirst and isOdd( len( word))) or (not goesFirst and isEven( len(word))):
		# Opponent's move
		# played 'word' so far, next letter is one of tree.keys
		# if ALL children are winning words, prune losers return ALL win word branches
		# if ANY children lose, this is a dangerous branch return None
		# if opponent must complete a word, this is a win word
		resultNode={}
		for l in tree.keys(): # search all children branches
			if not tree[l]: # end of word keep a win
				resultNode[l]={}
			else:
				subResult=findWinners( goesFirst, tree[l], word+l)
				if not subResult: # found a losing branch, prune this
					return None
				else:
					resultNode[l]=subResult # keep winning branches
		return resultNode	
	else:
		# Player's move
		# played 'word' so far, next letter is one of tree.keys
		# if ANY children are winning words, choose the best one
		# prune the rest
		resultNode={}
		for l in tree.keys(): # search all children branches
			if tree[l]: # skip losing end-of-words
				subResult=findWinners( goesFirst, tree[l], word+l)
				if subResult:
					resultNode[l]=subResult
		if (resultNode):
			return bestWords(word, resultNode)
		else:
			return None

def bestWords( word, tree): # I have words. I have the best words.
	# in: a partial word, and a tree of completions of that word
	# out: a tree with the best branch in it
	# best is defined as:
	#	1. the branch with the fewest sub-branches (words)
	#	2. the branch with the shortest word
	#	3. the branch with alphabetically first word
	# Note: this is somewhat subjective:
	#		1 long word is 'better' than 2 short words
	#		3 short words and 3 long words are 'better' than 6 medium words
	wordList=[]
	countWords={}
	shortestWord={}
	alphaFirstWord={}
	for l in tree.keys():
		wordList=allWords( word+l, tree[l]) # each element is a list of words
		countWords[l]=len(wordList)
		shortestWord[l]=min(map(len,wordList))
		alphaFirstWord[l]=sorted(wordList)[0]
	resultkey=sorted( tree, 
					key=lambda x: ( countWords[x], shortestWord[x], alphaFirstWord[x])
					)[0]
	return {resultkey: tree[resultkey]}

def allWords( word, tree):
	if not tree:
		return [word]
	resultList=[]
	for l in tree.keys():
		resultList.extend( allWords( word+l, tree[l]))
	return resultList

def isOdd(x):
		return x%2
def isEven(x):
		return not x%2

def main():
	tree=buildTree( )
	print( "Player goes first: winning words:\n")
	for l in sorted(tree.keys()):
		winningTree=findWinners( True, tree[l], l)
		if (winningTree):
			print(l+": %s" % sorted(allWords(l, winningTree)))
		else:
			print(l+": No winning words")
	print( "Adversary goes first: winning words:\n")
	for l in sorted(tree.keys()):
		winningTree=findWinners( False, tree[l], l)
		if (winningTree):
			print(l+": %s" % sorted(allWords(l,winningTree)))
		else:
			print(l+": No winning words")

if __name__ == "__main__":
	main()
