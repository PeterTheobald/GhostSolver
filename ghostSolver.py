# ghostSolver.py
# Peter@PeterTheobald.com 2016
#
# Ghost is a word game in which players take turns adding letters to a growing
# word fragment, trying not to be the one to complete a valid word. Each
# fragment must be the beginning of an actual word, there is a minimum of four
# letters of a word that counts. The player who completes a word loses the round.
#
# This program reads a dictionary and figures out words that will guarantee
# a win both for the player who plays first and the player who plays second.
#
# Input: ghostSolver.py < wordlist.txt
# Output: Player goes first: list of winning words.
#         Player goes second: list of winning words.

import pprint
import fileinput
import re

# Our trie is a tree of letters forming words.
# Implemented as a hash-map with letters as keys; values are sub-tries or nil
# In the ghost game no words are allowed with complete words as prefixes
#   eg: if we have "cart" we can never reach "cartridge"

def trieLetters( trie):
  return trie.keys()
  
def subTrie( trie, letter):
  return trie[letter]

# read through a list of words, split each word into letters,
# traverse the letters in the tree and add any new letters found
# throw away any words less than 4 letters long
# throw away any words that are supersets of existing words
# this is made easier because we assume the input is always sorted
# dont need any end-of-word markers because there are no supersets.
# leaf nodes are always end-of-word. non-leaf nodes are never end-of-word.

def buildTrie():
  trie={}
  for line in fileinput.input():
    word=line.rstrip()
    if len(word)<4:
      continue
    # clean out some cruft from online dictionaries, anything with Capitals or punctuation
    if re.search( '[^a-z]', word):
      continue
    p=trie
    newnode=True
    for letter in word:
      if letter in p:
        # this letter has already been created (for a different word), follow it
        p=subTrie( p, letter)
        newnode=False
      else:
        # This letter isn't here. Is it because we are adding new letters onto the tree
        # or because we've reached the end of a complete existing word?
        if p or newnode:
          # create new node for this letter
          p[letter]={}
          newnode=True
          p=subTrie( p, letter)
        else:
          # this is a superset of an existing word
          continue # skip it-Ghost can't use it and saves lots of memory
  return trie

def isOdd(x):
  return x%2
def isEven(x):
  return not x%2
def isPlayersMove( partialWord, playerGoesFirst):
  return ((playerGoesFirst and isEven( len( partialWord)))
          or ( not( playerGoesFirst) and isOdd( len( partialWord))))

# returns list of all words branching from this trie
# can also be used for all words branching from a sub-trie and a partial word
def allWords( trie, partialWord):
  if not trie:
    return [partialWord]
  resultList=[]
  for l in trieLetters( trie):
    resultList.extend( allWords( subTrie(trie, l), partialWord+l))
  return resultList
           
def bestWords( trie, partialWord): # I have words. I have the best words.
  # in: a partial word, and a tree of completions of that word
  # out: a tree with the best branch in it
  # best is defined as:
  #   1. the branch with the fewest sub-branches (words)
  #   2. the branch with the shortest word
  #   3. the branch with alphabetically first word
  # Note: this is somewhat subjective:
  #       1 long word is 'better' than 2 short words
  #       3 short words and 3 long words are 'better' than 6 medium words
  wordList=[]
  countWords={}
  shortestWord={}
  alphaFirstWord={}
  for l in trieLetters( trie):
    # pre-calculate sort keys for each letter and list of words
    wordList=allWords( subTrie( trie, l), partialWord+l)
    countWords[l]=len( wordList)
    shortestWord[l]=min(map(len,wordList))
    alphaFirstWord[l]=sorted(wordList)[0]
  bestBranchLetter=sorted( trie, 
                  key=lambda x: ( countWords[x], shortestWord[x], alphaFirstWord[x])
                  )[0]
  return { bestBranchLetter: subTrie( trie, bestBranchLetter)}

def winningMoves( trie, partialWord, playerGoesFirst):
  # traverse the tree, find words that lose and prune them. Only pass back up the winning words
  if ( isPlayersMove( partialWord, playerGoesFirst)):
    # Player's move
    # played 'word' so far, next letter is one of tree.keys
    # if ANY children are winning words, choose the best one
    # prune the rest
    resultNode={}
    for l in trieLetters( trie): # search all children branches
      if subTrie( trie, l): # skip losing end-of-words
        subResult=winningMoves( subTrie( trie, l), partialWord+l, playerGoesFirst)
        if subResult:
          resultNode[l]=subResult
    if (resultNode):
      return bestWords( resultNode, partialWord)
    else:
      return None
  else:
    # Opponent's move
    # played 'word' so far, next letter is one of tree.keys
    # if ALL children are winning words, prune losers return ALL win word branches
    # if ANY children lose, this is a dangerous branch return None
    # if opponent must complete a word, this is a win word
    resultNode={}
    for l in trieLetters( trie): # search all children branches
      if not subTrie( trie, l): # end of word keep a win
        resultNode[l]={}
      else:
        subResult=winningMoves( subTrie( trie, l), partialWord+l, playerGoesFirst)
        if not subResult: # found a losing branch, prune this
          return None
        else:
          resultNode[l]=subResult # keep winning branches
    return resultNode   

def printWinners( trie, playerGoesFirst):
    for l in sorted( trieLetters( trie)):
        winningTrie=winningMoves( subTrie( trie, l), l, playerGoesFirst)
        if (winningTrie):
            print(l+": %s" % sorted(allWords( winningTrie, l)))
        else:
            print(l+": No winning words")

def main():
    trie=buildTrie( )
    print( "Player goes first: winning words:\n")
    printWinners( trie, True)
    print( "\nAdversary goes first: winning words:\n")
    printWinners( trie, False)

if __name__ == "__main__":
    main()
