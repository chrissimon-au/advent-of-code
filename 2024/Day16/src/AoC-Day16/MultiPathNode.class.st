Class {
	#name : #MultiPathNode,
	#superclass : #AIPathDistanceNode,
	#instVars : [
		'previousNodes'
	],
	#category : #'AoC-Day16-Graph'
}

{ #category : #'as yet unclassified' }
MultiPathNode >> clearPreviousNodes [
	previousNodes := OrderedCollection new.
]

{ #category : #'as yet unclassified' }
MultiPathNode >> previousNodes [
	(previousNodes = nil) ifTrue:
		[self clearPreviousNodes].
	^previousNodes.
]
