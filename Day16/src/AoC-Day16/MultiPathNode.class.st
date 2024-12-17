Class {
	#name : #MultiPathNode,
	#superclass : #AIPathDistanceNode,
	#instVars : [
		'previousNodes'
	],
	#category : #'AoC-Day16-Graph'
}

{ #category : #'as yet unclassified' }
MultiPathNode >> previousNodes [
	(previousNodes = nil) ifTrue:
		[previousNodes := OrderedCollection new.].
	^previousNodes.
]
