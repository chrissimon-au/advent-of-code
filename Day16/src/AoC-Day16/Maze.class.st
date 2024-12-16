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
Maze class >> parse: input [
	| maze rows currentPos |
	rows := input splitOn: Character cr.
	
	maze := Maze new 
		size: {(rows at: 1) size . rows size } asCoordinates.
	
	rows doWithIndex: [ :row :rowIdx |
		row doWithIndex: [ :cell :colIdx |
			currentPos := {colIdx . rowIdx } asCoordinates.
			(cell = $S)
				ifTrue: [ 
					maze start: currentPos.
				].
			(cell = $E)
				ifTrue: [
					maze end: currentPos.
				]
		]
	].
	^maze.
]

{ #category : #'as yet unclassified' }
Maze >> end [
	^end.
]

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
