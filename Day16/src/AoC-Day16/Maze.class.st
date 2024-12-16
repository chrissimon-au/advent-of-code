Class {
	#name : #Maze,
	#superclass : #Object,
	#instVars : [
		'width',
		'height',
		'reindeer',
		'end'
	],
	#category : #'AoC-Day16-Model'
}

{ #category : #accessing }
Maze >> end: pos [
	end := pos
]

{ #category : #'as yet unclassified' }
Maze >> height [
	^ height
]

{ #category : #accessing }
Maze >> height: h [
	height := h
]

{ #category : #'as yet unclassified' }
Maze >> lowestScore [
	^ 5
]

{ #category : #'as yet unclassified' }
Maze >> reindeer [
	^ reindeer
]

{ #category : #accessing }
Maze >> start: pos [
	reindeer := Vector new position: pos; direction: #east.
]

{ #category : #'as yet unclassified' }
Maze >> width [
	^ width
]

{ #category : #accessing }
Maze >> width: w [
	width := w
]
