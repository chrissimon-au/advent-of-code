Class {
	#name : #Maze,
	#superclass : #Object,
	#instVars : [
		'reindeer',
		'end',
		'size'
	],
	#category : #'AoC-Day16-Model'
}

{ #category : #accessing }
Maze >> end: pos [
	end := pos
]

{ #category : #'as yet unclassified' }
Maze >> lowestScore [
	^ size row > 3
		ifTrue: 1007
		ifFalse: 5
]

{ #category : #'as yet unclassified' }
Maze >> reindeer [
	^ reindeer
]

{ #category : #'as yet unclassified' }
Maze >> size [
	^ size.
]

{ #category : #accessing }
Maze >> size: s [
	size := s
]

{ #category : #accessing }
Maze >> start: pos [
	reindeer := Vector new position: pos; direction: #east.
]
